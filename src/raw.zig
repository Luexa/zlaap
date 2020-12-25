const std = @import("std");
const wasi = std.os.wasi;
const Allocator = std.mem.Allocator;
const unexpectedErrno = std.os.unexpectedErrno;
const GetCommandLineW = std.os.windows.kernel32.GetCommandLineW;

/// An iterator over an array of null-terminated argument strings.
pub const ArgvIterator = struct {
    argv: []const [*:0]const u8,

    /// NOTE: This method is only intended on POSIX systems or in applications that link libc.
    /// On Windows use WindowsArgIterator and on WASI use WasiArgs.iterator();
    pub fn init() ArgvIterator {
        return .{ .argv = std.os.argv };
    }

    /// Retrieve the next argument passed to the process.
    pub fn next(self: *ArgvIterator) ?[:0]const u8 {
        if (self.argv.len == 0)
            return null;
        defer self.argv = self.argv[1..];
        return std.mem.span(self.argv[0]);
    }
};

/// A structure that manages allocation, retrieval, and deallocation of WASI arguments.
pub const WasiArgs = struct {
    buf: []align(@alignOf([*:0]u8)) u8,
    argv: [][*:0]u8,

    /// The initialization error, taken from the stdlib WASI iterator.
    pub const InitError = std.process.ArgIteratorWasi.InitError;

    /// Initialize the argv buffer using the WASI argument retrieval API.
    /// Should be paired with a call to `deinit()` to free the memory.
    /// NOTE: if linking libc, use `ArgvIterator.init()` to avoid allocation.
    pub fn init(allocator: *Allocator) InitError!WasiArgs {
        if (std.builtin.os.tag != .wasi)
            @compileError("Cannot initialize WASI argument iterator on non-WASI target.");

        // Retrieve the number of arguments and size of the arguments buffer.
        var argc: usize = undefined;
        var argv_size: usize = undefined;
        var err = wasi.args_sizes_get(&argc, &argv_size);
        if (err != wasi.ESUCCESS)
            return unexpectedErrno(err);

        // Allocate space required for the arguments and their pointers.
        const self = try allocBuf(allocator, argc, argv_size);
        errdefer self.deinit(allocator);

        // Populate the argv slice and arguments buffer with the appropriate data.
        err = wasi.args_get(self.argv.ptr, self.buf.ptr + argc * @sizeOf([*:0]u8));
        if (err != wasi.ESUCCESS)
            return unexpectedErrno(err);
        return self;
    }

    /// Free the memory allocated by `init()` or `allocBuf()`.
    pub fn deinit(self: *WasiArgs, allocator: *Allocator) void {
        defer self.* = undefined;
        allocator.free(self.buf);
    }

    /// Return an iterator over the argv buffer. This iterator is compatible with
    /// the iterator used on POSIX systems as the argv buffer has the same layout.
    pub fn iterator(self: WasiArgs) ArgvIterator {
        return .{ .argv = self.argv };
    }

    /// Allocate memory for `buf` and `argv` based on argument count and number
    /// of bytes required to store the concatenated, null-terminated `argv`. This
    /// should usually not be called directly as it serves little purpose outside
    /// of implementing `WasiArgs.init()` or testing on another platform.
    pub fn allocBuf(allocator: *Allocator, argc: usize, argv_size: usize) error{OutOfMemory}!WasiArgs {
        const buf_len = argc * @sizeOf([*:0]u8) + argv_size;
        const buf = try allocator.allocWithOptions(u8, buf_len, @alignOf([*:0]u8), null);
        const argv = @ptrCast([*][*:0]u8, buf)[0..argc];
        return WasiArgs{
            .buf = buf,
            .argv = argv,
        };
    }
};

/// An iterator over arguments specified in a Windows command line string.
pub const WindowsArgIterator = struct {
    command_line: [*:0]const u16,

    /// Initialize the iterator using the kernel32 function GetCommandLineW.
    pub fn init() WindowsArgIterator {
        if (std.builtin.os.tag != .windows)
            @compileError("Cannot initialize Windows argument iterator on non-Windows target.");
        return .{ .command_line = GetCommandLineW() };
    }

    /// Retrieve the next argument in the command line. The text is unprocessed
    /// other than being split according to the Windows backslash and quoting
    /// rules, so to actually determine the contents of the argument it will
    /// likely be useful to use the other methods provided by this library.
    pub fn next(self: *WindowsArgIterator) ?[]const u16 {
        return self.parseInternal(.raw, {}) catch unreachable;
    }

    /// Parse the argument from the command line string into the provided buffer.
    /// Return value is a slice into the buffer; use `readNext` to advance the
    /// original buffer past the newly read argument.
    ///
    /// Returns an error if the buffer does not have room for the parsed argument.
    /// Note that Windows command lines are limited to 32768 WTF-16 code units in
    /// length, including quotes and escape sequences, so the buffer need only be
    /// [32768]u16 or [65536]u8, depending on encoding choice.
    pub fn decodeNext(
        self: *WindowsArgIterator,
        comptime encoding: Encoding,
        buf: encoding.v(null).Param(),
    ) error{OutOfMemory}!?encoding.v(null).Ret() {
        return self.parseInternal(comptime encoding.v(null), buf);
    }

    /// Parse the argument from the command line string into the provided buffer,
    /// additionally appending the requested sentinel. See `decodeNext` for more
    /// information.
    pub fn decodeNextZ(
        self: *WindowsArgIterator,
        comptime encoding: Encoding,
        comptime sentinel: encoding.Sentinel(),
        buf: encoding.v(sentinel).Param(),
    ) error{OutOfMemory}!?encoding.v(sentinel).Ret() {
        return self.parseInternal(comptime encoding.v(sentinel), buf);
    }

    /// Parse the argument from the command line string into the provided buffer,
    /// additionally advancing the original buffer past the newly read argument.
    /// See `decodeNext` for more information.
    pub fn readNext(
        self: *WindowsArgIterator,
        comptime encoding: Encoding,
        buf: *encoding.v(null).Param(),
    ) error{OutOfMemory}!?encoding.v(null).Ret() {
        if (try self.decodeNext(encoding, buf.*)) |result| {
            buf.* = buf.*[result.len..];
            return result;
        } else return null;
    }

    /// Parse the argument from the command line string into the provided buffer,
    /// additionally advancing the original buffer past the newly read argument
    /// and appending the requested sentinel. See `decodeNext` for more information.
    pub fn readNextZ(
        self: *WindowsArgIterator,
        comptime encoding: Encoding,
        comptime sentinel: encoding.Sentinel(),
        buf: *encoding.v(sentinel).Param(),
    ) error{OutOfMemory}!?encoding.v(sentinel).Ret() {
        if (try self.decodeNextZ(encoding, sentinel, buf.*)) |result| {
            buf.* = buf.*[(result.len + 1)..];
            return result;
        } else return null;
    }

    /// The encoding to parse command line arguments in, used by `decodeNext`,
    /// `decodeNextZ`, `readNext`, and `readNextZ`.
    pub const Encoding = enum {
        /// The native encoding used by Windows to represent non-ASCII characters.
        /// This is the recommended encoding to read command line arguments in, but
        /// requires platform-specific code as non-Windows OSes do not use this.
        /// However, using WTF-8 entails a series of back-and-forth conversions for
        /// winapi calls and file IO so it is highly recommended to use WTF-16.
        wtf16,

        /// An encoding roughly compatible with UTF-8, but additionally allowing
        /// unpaired surrogate codepoints to be encoded. This encoding should not
        /// be used unless truly required; WTF-16 is the format used by winapi,
        /// and for network transmission, display to user, and file IO it should
        /// be converted to validated UTF-8 (possibly with U+FFFD) instead. To
        /// reiterate: using this encoding is almost always incorrect.
        wtf8,

        fn Sentinel(comptime self: Encoding) type {
            return switch (self) {
                .wtf16 => u16,
                .wtf8 => u8,
            };
        }

        fn v(comptime self: Encoding, comptime sentinel: ?self.Sentinel()) OutputVariant {
            return @unionInit(OutputVariant, @tagName(self), sentinel);
        }
    };

    fn parseInternal(self: *WindowsArgIterator, comptime v: OutputVariant, buf: v.Param()) !?v.Ret() {
        // Initialize variables used in the parser loop.
        var c = self.command_line;
        var i: usize = 0;

        // Null terminator means there is nothing to iterate, so return null.
        if (c[0] == 0)
            return null;

        // The MSDN docs say that if the command line string starts with a space
        // or tab, then the first argument shall be parsed as a 0-length string.
        var out = ParseOutput(v).init(if (v == .raw) c else buf);
        if (c[0] != ' ' and c[0] != '\t') {
            var in_quotes = false;
            var num_backslashes: usize = 0;
            while (c[0] != 0 and (in_quotes or (c[0] != ' ' and c[0] != '\t'))) : (c += 1) {
                switch (c[0]) {
                    '\\' => {
                        if (v == .raw)
                            try out.add({});
                        num_backslashes += 1;
                    },
                    '"' => {
                        defer num_backslashes = 0;
                        if (num_backslashes % 2 == 0)
                            in_quotes = !in_quotes;
                        if (v != .raw) {
                            if (num_backslashes > 0)
                                try out.addBackslashes(num_backslashes / 2);
                            if (num_backslashes % 2 == 1)
                                try out.add('"');
                        } else try out.add({});
                    },
                    else => {
                        defer num_backslashes = 0;
                        if (v != .raw) {
                            if (num_backslashes > 0)
                                try out.addBackslashes(num_backslashes);
                            try out.add(c[0]);
                        } else try out.add({});
                    },
                }
            }
            if (v != .raw and num_backslashes > 0) {
                try out.addBackslashes(num_backslashes);
            }
        }

        // Extract the final result from the parser state and strip trailing whitespace.
        const result = try out.finalize();
        while (c[0] == ' ' or c[0] == '\t') : (c += 1) {}
        self.command_line = c;
        return result;
    }

    /// Internal structure that represents the buffer. Performs bounds checks
    /// and, when applicable, converts WTF-16 code units into WTF-8 bytes.
    fn ParseOutput(comptime v: OutputVariant) type {
        return struct {
            cap: if (v == .raw) void else usize,
            prev: if (v == .wtf8) u16 else void,
            buf: v.Buf(),

            /// Initialize the ParseOutput using the relevant buffer type.
            fn init(buf: if (v == .raw) [*]const u16 else v.Buf()) @This() {
                const buf2: v.Buf() = buf[0..0];
                return switch (v) {
                    .raw => .{
                        .cap = {},
                        .prev = {},
                        .buf = buf2,
                    },
                    .wtf16 => .{
                        .cap = buf.len,
                        .prev = {},
                        .buf = buf2,
                    },
                    .wtf8 => .{
                        .cap = buf.len,
                        .prev = 0,
                        .buf = buf2,
                    },
                };
            }

            /// Encode a WTF-16 code unit into a 1-byte WTF-8 sequence.
            fn encode1(self: *@This(), c: u16) !void {
                if (self.cap == self.buf.len)
                    return error.OutOfMemory;
                self.buf.len += 1;
                self.buf[self.buf.len - 1] = @intCast(u8, c);
            }

            /// Encode a WTF-16 code unit into a 2-byte WTF-8 sequence.
            fn encode2(self: *@This(), c: u16) !void {
                if (self.cap < self.buf.len + 2)
                    return error.OutOfMemory;
                self.buf.len += 2;
                self.buf[(self.buf.len - 2)..][0..2].* = [_]u8{
                    0xc0 | @intCast(u8, c >> 6),
                    0x80 | @intCast(u8, c & 0x3f),
                };
            }

            /// Encode a WTF-16 code unit into a 3-byte WTF-8 sequence.
            fn encode3(self: *@This(), c: u16) !void {
                if (self.cap < self.buf.len + 3)
                    return error.OutOfMemory;
                self.buf.len += 3;
                self.buf[(self.buf.len - 3)..][0..3].* = [_]u8{
                    0xe0 | @intCast(u8, c >> 12),
                    0x80 | @intCast(u8, (c >> 6) & 0x3f),
                    0x80 | @intCast(u8, c & 0x3f),
                };
            }

            /// Encode a WTF-16 surrogate pair into a 4-byte WTF-8 sequence.
            fn encode4(self: *@This(), lead: u16, trail: u16) !void {
                if (self.cap < self.buf.len + 4)
                    return error.OutOfMemory;
                const c = 0x10000 + (@as(u21, lead - 0xd800) << 10) + (trail - 0xdc00);
                self.buf.len += 4;
                self.buf[(self.buf.len - 4)..][0..4].* = [_]u8{
                    0xf0 | @intCast(u8, c >> 18),
                    0x80 | @intCast(u8, (c >> 12) & 0x3f),
                    0x80 | @intCast(u8, (c >> 6) & 0x3f),
                    0x80 | @intCast(u8, c & 0x3f),
                };
            }

            /// Append the provided WTF-16 code unit to the buffer, returning an
            /// error if the buffer is out of room. Performs WTF-16 to WTF-8
            /// conversion if ParseOutput is in WTF-8 mode.
            fn add(self: *@This(), c: if (v == .raw) void else u16) !void {
                switch (v) {
                    .raw => self.buf.len += 1,
                    .wtf16 => {
                        // Verify buffer capacity, then append the code unit.
                        if (self.cap == self.buf.len)
                            return error.OutOfMemory;
                        self.buf.len += 1;
                        self.buf[self.buf.len - 1] = c;
                    },
                    .wtf8 => {
                        // Perform WTF-16 to WTF-8 conversion.
                        switch (c) {
                            0xd800...0xdbff => {
                                if (self.prev != 0) {
                                    // Encode unpaired leading surrogate.
                                    try self.encode3(self.prev);
                                }
                                // Store this leading surrogate for later.
                                self.prev = c;
                            },
                            0xdc00...0xdfff => {
                                if (self.prev == 0) {
                                    // Encode unpaired trailing surrogate.
                                    try self.encode3(c);
                                } else {
                                    // Encode supplementary codepoint.
                                    try self.encode4(self.prev, c);
                                    self.prev = 0;
                                }
                            },
                            else => {
                                if (self.prev != 0) {
                                    // Encode unpaired leading surrogate.
                                    try self.encode3(self.prev);
                                    self.prev = 0;
                                }
                                // Encode basic multilingual plane codepoint.
                                switch (c) {
                                    0x0000...0x007f => try self.encode1(c),
                                    0x0080...0x07ff => try self.encode2(c),
                                    0x0800...0xffff => try self.encode3(c),
                                }
                            },
                        }
                    },
                }
            }

            /// Append the specified number of backslashes to end of the buffer.
            fn addBackslashes(self: *@This(), count: usize) !void {
                comptime std.debug.assert(v != .raw);
                if (v == .wtf8 and self.prev != 0) {
                    try self.encode3(self.prev);
                    self.prev = 0;
                }
                if (self.cap < self.buf.len + count)
                    return error.OutOfMemory;
                self.buf.len += count;
                for (self.buf[(self.buf.len - count)..]) |*target|
                    target.* = '\\';
            }

            /// Returns the buffer that was filled via repeated `add()` calls.
            /// This will fail if there is not enough capacity to encode the
            /// final codepoint and/or the null terminator.
            fn finalize(self: *@This()) !v.Ret() {
                if (v == .wtf8 and self.prev != 0)
                    try self.encode3(self.prev);
                if (comptime v.sentinel()) |s| {
                    if (self.cap == self.buf.len)
                        return error.OutOfMemory;
                    self.buf.len += 1;
                    self.buf[self.buf.len - 1] = s;
                    return self.buf[0..(self.buf.len - 1) :s];
                } else return self.buf;
            }
        };
    }

    /// Helper enum to make the above code a bit less verbose.
    const OutputVariant = union(enum) {
        raw: void,
        wtf16: ?u16,
        wtf8: ?u8,

        fn sentinel(comptime v: OutputVariant) switch (v) {
            .wtf16 => ?u16,
            .wtf8 => ?u8,
            else => ?void,
        } {
            if (v == .raw) return null;
            return @field(v, @tagName(v));
        }

        fn Param(comptime v: OutputVariant) type {
            return switch (v) {
                .raw => void,
                .wtf16 => []u16,
                .wtf8 => []u8,
            };
        }

        fn Buf(comptime v: OutputVariant) type {
            return switch (v) {
                .raw => []const u16,
                .wtf16 => []u16,
                .wtf8 => []u8,
            };
        }

        fn Ret(comptime v: OutputVariant) type {
            return switch (v) {
                .raw => []const u16,
                .wtf16 => |s_| if (s_) |s| [:s]u16 else []u16,
                .wtf8 => |s_| if (s_) |s| [:s]u8 else []u8,
            };
        }
    };
};

test "zlaap.args.ArgvIterator" {
    // Ensure [][*:0]u8 can cast to []const [*:0]const u8.
    _ = @as([]const [*:0]const u8, std.os.argv);

    // Random argv buffer to test in a platform-independent environment.
    const argv_str = [_][:0]const u8{
        "abc, xyz, 123",
        "592392 jgjsk l2ql1",
        "al2laktrwalktl",
    };
    const argv_ptr = [_][*:0]const u8{
        argv_str[0].ptr,
        argv_str[1].ptr,
        argv_str[2].ptr,
    };

    // The iterator should return exactly 3 arguments.
    var iterator = ArgvIterator{ .argv = &argv_ptr };
    const arg_1 = iterator.next() orelse return error.ExpectedArgument;
    const arg_2 = iterator.next() orelse return error.ExpectedArgument;
    const arg_3 = iterator.next() orelse return error.ExpectedArgument;

    // Validate the returned arguments and ensure no 4th argument is returned.
    std.testing.expect(iterator.next() == null);
    std.testing.expectEqualStrings(argv_str[0], arg_1);
    std.testing.expectEqualStrings(argv_str[1], arg_2);
    std.testing.expectEqualStrings(argv_str[2], arg_3);
}

test "zlaap.args.WasiArgs" {
    // Random argv buffer to test in a platform-independent environment.
    const test_buf: []const u8 = "abcxyz123456000+++\x00111222333444---555---+++\x00999___212___622\x00";
    const test_argv = [_][]const u8{
        test_buf[0..18],
        test_buf[19..][0..24],
        test_buf[19..][25..][0..15],
    };

    // Initialize WasiArgs manually to allow this test to be cross-platform.
    var args = try WasiArgs.allocBuf(std.testing.allocator, test_argv.len, test_buf.len);
    defer args.deinit(std.testing.allocator);

    // Ensure that the allocation was performed correctly.
    std.testing.expectEqual(@sizeOf([*:0]u8) * test_argv.len + test_buf.len, args.buf.len);
    std.testing.expectEqual(test_argv.len, args.argv.len);

    // Initialize WasiArgs' argument buffer and argv array.
    const argv_buf = args.buf[(@sizeOf([*:0]u8) * test_argv.len)..];
    std.mem.copy(u8, argv_buf, test_buf);
    args.argv[0] = @ptrCast([*:0]u8, argv_buf.ptr);
    args.argv[1] = @ptrCast([*:0]u8, argv_buf.ptr) + 19;
    args.argv[2] = @ptrCast([*:0]u8, argv_buf.ptr) + 19 + 25;

    // The iterator should return exactly 3 arguments.
    var iterator = args.iterator();
    const arg_1 = iterator.next() orelse return error.ExpectedArgument;
    const arg_2 = iterator.next() orelse return error.ExpectedArgument;
    const arg_3 = iterator.next() orelse return error.ExpectedArgument;

    // Validate the returned arguments and ensure no 4th argument is returned.
    std.testing.expect(iterator.next() == null);
    std.testing.expectEqualStrings(test_argv[0], arg_1);
    std.testing.expectEqualStrings(test_argv[1], arg_2);
    std.testing.expectEqualStrings(test_argv[2], arg_3);
}

test "zlaap.args.WindowsArgIterator" {
    inline for (.{
        .{ windows_cmd16, .{ windows_raw16, windows_wtf16, windows_wtf8 } },
        .{ emoji_cmd16, .{ emoji_raw16, emoji_wtf16, emoji_wtf8 } },
    }) |args| {
        try testWindowsVariant(.raw, args[0], args[1][0]);
        inline for (.{ null, 0 }) |s| {
            try testWindowsVariant(.{ .wtf16 = s }, args[0], args[1][1]);
            try testWindowsVariant(.{ .wtf8 = s }, args[0], args[1][2]);
        }
    }
}

// Windows test helper used to reduce code duplication while still testing the
// various combinations of API usage possible with WindowsArgIterator.
fn testWindowsVariant(
    comptime v: WindowsArgIterator.OutputVariant,
    comptime command_line: [*:0]const u16,
    comptime expected: if (v == .wtf8) [3][]const u8 else [3][]const u16,
) !void {
    // The scalar type used in the buffer and output strings.
    const U = switch (v) {
        .raw, .wtf16 => u16,
        .wtf8 => u8,
    };
    // The sentinel terminating the output string, if any.
    const sentinel = comptime v.sentinel();
    // The requested encoding of the output string, if any.
    const encoding = if (v == .raw) {} else @field(WindowsArgIterator.Encoding, @tagName(v));

    // Validate the results of iterating over each argument.
    var iterator = WindowsArgIterator{ .command_line = command_line };
    inline for (expected) |str, i| {
        if (v == .raw) {
            // Test `iterator.next()`.
            const result = iterator.next() orelse return error.ExpectedArgument;
            std.testing.expectEqualSlices(U, str, result);
        } else {
            const iterator_state = iterator;
            var buf: [str.len + @boolToInt(sentinel != null)]U = undefined;
            var slice: []U = &buf;
            {
                // Test `iterator.decodeNext()`.
                const result = (if (sentinel) |s|
                    try iterator.decodeNextZ(encoding, s, slice)
                else
                    try iterator.decodeNext(encoding, slice)) orelse return error.ExpectedArgument;
                std.testing.expectEqualSlices(U, str, result);
            }
            {
                // Test `iterator.readNext()`.
                iterator = iterator_state;
                const result = (if (sentinel) |s|
                    try iterator.readNextZ(encoding, s, &slice)
                else
                    try iterator.readNext(encoding, &slice)) orelse return error.ExpectedArgument;
                std.testing.expectEqual(@as(usize, 0), slice.len);
                std.testing.expectEqualSlices(U, str, result);
            }
        }
    }

    // Ensure the iterator does not attempt to return a fourth element.
    std.testing.expect(iterator.next() == null);
    std.testing.expect((try iterator.decodeNext(.wtf8, &[_]u8{})) == null);
    std.testing.expect((try iterator.decodeNext(.wtf16, &[_]u16{})) == null);
    std.testing.expect((try iterator.decodeNextZ(.wtf8, 0, &[_]u8{})) == null);
    std.testing.expect((try iterator.decodeNextZ(.wtf16, 0, &[_]u16{})) == null);
}

// An ASCII command line that contains the "gotchas" that one may run into when
// implementing a Windows command line argument parser. Useful to isolate errors
// caused by parsing from errors caused by Unicode conversion.
const windows_cmd8 = "  \t \t  \t HELLO\"WORLD\\\" \t  \\\\\"\\\\\" \\\t \\\\ \"  \t  \tfoo\\\\\\\\\"bar\" ";
const windows_raw8 = [_][]const u8{ "", "HELLO\"WORLD\\\" \t  \\\\\"\\\\\" \\\t \\\\ \"", "foo\\\\\\\\\"bar\"" };
const windows_wtf8 = [_][]const u8{ "", "HELLOWORLD\" \t  \\\\ \\\t \\\\ ", "foo\\\\bar" };
const windows_cmd16 = std.unicode.utf8ToUtf16LeStringLiteral(windows_cmd8);
const windows_raw16 = [_][]const u16{
    std.unicode.utf8ToUtf16LeStringLiteral(windows_raw8[0]),
    std.unicode.utf8ToUtf16LeStringLiteral(windows_raw8[1]),
    std.unicode.utf8ToUtf16LeStringLiteral(windows_raw8[2]),
};
const windows_wtf16 = [_][]const u16{
    std.unicode.utf8ToUtf16LeStringLiteral(windows_wtf8[0]),
    std.unicode.utf8ToUtf16LeStringLiteral(windows_wtf8[1]),
    std.unicode.utf8ToUtf16LeStringLiteral(windows_wtf8[2]),
};

// A Unicode command line with UTF-8 byte sequences ranging from 1 to 4 bytes
// in length; additionally, when converted to UTF-16, there are both surrogate
// and non-surrogate code units. TODO: test unpaired surrogates.
const emoji_cmd8 = "_ⱵSƎƎⱵƎ  \t  \"Z😜☺_\\\" \" \t  T☺v😜\\Ⱶ   \t ";
const emoji_raw8 = [_][]const u8{ "_ⱵSƎƎⱵƎ", "\"Z😜☺_\\\" \"", "T☺v😜\\Ⱶ" };
const emoji_wtf8 = [_][]const u8{ "_ⱵSƎƎⱵƎ", "Z😜☺_\" ", "T☺v😜\\Ⱶ" };
const emoji_cmd16 = std.unicode.utf8ToUtf16LeStringLiteral(emoji_cmd8);
const emoji_raw16 = [_][]const u16{
    std.unicode.utf8ToUtf16LeStringLiteral(emoji_raw8[0]),
    std.unicode.utf8ToUtf16LeStringLiteral(emoji_raw8[1]),
    std.unicode.utf8ToUtf16LeStringLiteral(emoji_raw8[2]),
};
const emoji_wtf16 = [_][]const u16{
    std.unicode.utf8ToUtf16LeStringLiteral(emoji_wtf8[0]),
    std.unicode.utf8ToUtf16LeStringLiteral(emoji_wtf8[1]),
    std.unicode.utf8ToUtf16LeStringLiteral(emoji_wtf8[2]),
};
