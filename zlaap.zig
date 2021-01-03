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
/// Construct using `var wasi_args = WasiArgs{}`, then access the argv slice via
/// `wasi_args.argv()` or `wasi_args.iterator()`. Make sure to call `wasi_args.deinit()`.
pub const WasiArgs = struct {
    /// The buffer that stores the argument strings and pointers into them.
    buf: []align(buf_align) u8 = &[_]u8{},

    /// The initialization error, taken from the stdlib WASI iterator.
    pub const InitError = error{OutOfMemory} || std.os.UnexpectedError;

    /// The alignment required to store the array of pointers or slices.
    const buf_align = std.math.max(@alignOf([*:0]u8), @alignOf([:0]u8));

    /// Initialize the argv buffer using the WASI argument retrieval API.
    /// Caller is responsible for calling `WasiArgs.deinit()` to free memory.
    /// Do not free the returned slice separately from `WasiArgs`; the slice
    /// is contained within the same buffer as the argument data.
    /// If `span` is false, return value is a slice of [*:0]u8.
    /// If `span` is true, return value is a slice of [:0]u8.
    pub fn argv(
        self: *WasiArgs,
        allocator: *Allocator,
        comptime span: bool,
    ) InitError!(if (span) [][:0]u8 else [][*:0]u8) {
        if (std.builtin.os.tag != .wasi)
            @compileError("Cannot initialize WASI argument buffer on non-WASI target.");

        // Retrieve argument count and required buffer size.
        var argc: usize = undefined;
        var argv_size: usize = undefined;
        var err = wasi.args_sizes_get(&argc, &argv_size);
        if (err != wasi.ESUCCESS)
            return unexpectedErrno(err);

        // Allocate the buffer with enough space for argv slice and argument data.
        const buf = try self.allocBuf(allocator, argc, argv_size, span);
        errdefer self.deinit(allocator);

        // Retrieve the argument data, storing it in the newly-allocated buffer.
        err = wasi.args_get(buf.argv.ptr, buf.buf.ptr);
        if (err != wasi.ESUCCESS)
            return unexpectedErrno(err);

        // Return the argv buffer, spanning it if requested.
        return if (span) buf.span() else buf.argv;
    }

    /// Retrieve the process arguments and return an iterator over them. The
    /// returned iterator is an ArgvIterator, which is used on POSIX systems.
    /// Caller is responsible for calling `WasiArgs.deinit()` to free memory.
    pub fn iterator(self: *WasiArgs, allocator: *Allocator) InitError!ArgvIterator {
        return ArgvIterator{
            .argv = try self.argv(allocator, false),
        };
    }

    /// Free the memory allocated by `argv()` or `iterator()`.
    pub fn deinit(self: *WasiArgs, allocator: *Allocator) void {
        defer self.* = undefined;
        allocator.free(self.buf);
    }

    /// Allocate memory for `buf` and `argv` based on argument count and number
    /// of bytes required to store the concatenated, null-terminated `argv`. This
    /// should usually not be called directly as it serves little purpose outside
    /// of implementing `WasiArgs.argv()` and `WasiArgs.iterator()`.
    pub fn allocBuf(
        self: *WasiArgs,
        allocator: *Allocator,
        argc: usize,
        argv_size: usize,
        comptime span: bool,
    ) error{OutOfMemory}!AllocBuf {
        const ArgvT = if (span) [:0]u8 else [*:0]u8;
        const item_size = comptime std.math.max(@sizeOf(ArgvT), @sizeOf([*:0]u8));
        const buf_len = argc * item_size + argv_size;
        self.buf = try allocator.allocWithOptions(u8, buf_len, buf_align, null);
        return AllocBuf{
            .argv = @ptrCast([*][*:0]u8, self.buf.ptr)[0..argc],
            .buf = self.buf[(item_size * argc)..],
        };
    }

    /// Return value of `allocBuf`, storing the requested allocation.
    pub const AllocBuf = struct {
        /// A slice of argument strings, indexing into the underlying buffer.
        argv: [][*:0]u8,
        /// The buffer of argument data indexed by the `argv` slice.
        buf: []u8,

        /// Return the result of spanning each pointer in the argv slice.
        /// Caller asserts that the underlying allocation has space for this.
        pub fn span(self: AllocBuf) [][:0]u8 {
            comptime std.debug.assert(@sizeOf([:0]u8) >= @sizeOf([*:0]u8));
            var source = self.argv;
            const dest = @ptrCast([*][:0]u8, source.ptr)[0..source.len];
            while (source.len > 0) : (source.len -= 1)
                dest[source.len - 1] = std.mem.span(source[source.len - 1]);
            return dest;
        }
    };
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
        return self.parseInternal(.raw, null, {}, {}) catch unreachable;
    }

    /// Parse the argument from the command line string into the provided buffer.
    /// Returns an error if the buffer does not have room for the parsed argument.
    /// Note that Windows command lines are limited to 32768 WTF-16 code units in
    /// length, including quotes and escape sequences, so the buffer need only be
    /// [32768]u16 or [98304]u8, depending on encoding choice.
    pub fn decodeNext(
        self: *WindowsArgIterator,
        comptime encoding: Encoding,
        buf: encoding.v(null).Param(),
    ) error{OutOfMemory}!?encoding.v(null).Ret() {
        return self.parseInternal(comptime encoding.v(null), null, {}, buf);
    }

    /// Parse the argument from the command line string into a newly allocated buffer.
    /// Windows argument decoding can be done with a fixed buffer; see `decodeNext`.
    pub fn decodeAlloc(
        self: *WindowsArgIterator,
        allocator: *Allocator,
        comptime encoding: Encoding,
    ) error{OutOfMemory}!?encoding.v(null).Ret() {
        return self.parseAlloc(allocator, encoding, null);
    }

    /// Parse the argument from the command line string into the provided buffer,
    /// additionally appending the requested sentinel. See `decodeNext` for more
    /// information.
    pub fn decodeNextZ(
        self: *WindowsArgIterator,
        comptime encoding: Encoding,
        buf: encoding.v(null).Param(),
        comptime sentinel: encoding.Sentinel(),
    ) error{OutOfMemory}!?encoding.v(sentinel).Ret() {
        return self.parseInternal(comptime encoding.v(sentinel), null, {}, buf);
    }

    /// Parse the argument from the command line string into a newly allocated buffer.
    /// Windows argument decoding can be done with a fixed buffer; see `decodeNextZ`.
    pub fn decodeAllocZ(
        self: *WindowsArgIterator,
        allocator: *Allocator,
        comptime encoding: Encoding,
        comptime sentinel: encoding.Sentinel(),
    ) error{OutOfMemory}!?encoding.v(sentinel).Ret() {
        return self.parseAlloc(allocator, encoding, sentinel);
    }

    /// Return a decoder which tracks state and thus can be resumed after out of
    /// memory errors instead of failing with no chance of recovery.
    pub fn advancedDecoder(
        self: *WindowsArgIterator,
        comptime encoding: Encoding,
        comptime sentinel: ?encoding.Sentinel(),
    ) AdvancedDecoder(encoding, sentinel) {
        return .{ .iterator = self };
    }

    pub const AdvancedDecoder = AdvancedDecoder_;

    /// The encoding to parse command line arguments in, used by `decodeNext`.
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

    fn parseAlloc(
        self: *WindowsArgIterator,
        allocator: *Allocator,
        comptime encoding: Encoding,
        comptime sentinel: ?encoding.Sentinel(),
    ) !?encoding.v(sentinel).Ret() {
        var decoder = self.advancedDecoder(encoding, sentinel);
        var buf = std.ArrayListUnmanaged(encoding.Sentinel()){};
        errdefer buf.deinit(allocator);
        while (true) {
            if (decoder.decodeNext(buf.items)) |result| {
                if (result) |str| {
                    const new_len = str.len + @boolToInt(sentinel != null);
                    _ = allocator.shrink(buf.items, new_len);
                }
                return result;
            } else |_| {
                try buf.ensureCapacity(allocator, buf.capacity + 1);
                buf.items.len = buf.capacity;
            }
        }
    }

    fn parseInternal(
        self: *WindowsArgIterator,
        comptime v: OutputVariant,
        comptime Advanced: ?type,
        advanced: if (Advanced) |A| *A else void,
        buf: v.Param(),
    ) !?v.Ret() {
        // Initialize variables used in the parser loop.
        var c = self.command_line;
        if (Advanced != null)
            c += advanced.cmd_offset;

        // Null terminator means there is nothing to iterate, so return null.
        if (c[0] == 0 and (Advanced == null or advanced.cmd_offset == 0))
            return null;

        // Set up output parsing utility.
        var out = ParseOutput(v).init(if (v == .raw) c else buf);
        if (Advanced != null) {
            out.buf.len = advanced.buf_offset;
            out.prev = advanced.prev;
        }

        // The MSDN docs say that if the command line string starts with a space
        // or tab, then the first argument shall be parsed as a 0-length string.
        if (c[0] != ' ' and c[0] != '\t' or (Advanced != null and advanced.cmd_offset > 0)) {
            var in_quotes = false;
            var num_backslashes: usize = 0;
            if (Advanced != null) {
                in_quotes = advanced.in_quotes;
                num_backslashes = advanced.num_backslashes;
            }
            while (c[0] != 0 and (in_quotes or (c[0] != ' ' and c[0] != '\t'))) : (c += 1) {
                switch (c[0]) {
                    '\\' => {
                        if (v == .raw)
                            try out.add({});
                        num_backslashes += 1;
                    },
                    '"' => {
                        if (num_backslashes % 2 == 0)
                            in_quotes = !in_quotes;
                        if (v != .raw) {
                            if (num_backslashes > 0)
                                try out.addBackslashes(num_backslashes / 2);
                            if (num_backslashes % 2 == 1)
                                try out.add('"');
                        } else try out.add({});
                        num_backslashes = 0;
                    },
                    else => {
                        if (v != .raw) {
                            if (num_backslashes > 0)
                                try out.addBackslashes(num_backslashes);
                            try out.add(c[0]);
                        } else try out.add({});
                        num_backslashes = 0;
                    },
                }
                if (Advanced != null) {
                    advanced.cmd_offset += 1;
                    advanced.buf_offset = out.buf.len;
                    advanced.prev = out.prev;
                    advanced.in_quotes = in_quotes;
                    advanced.num_backslashes = num_backslashes;
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
        if (Advanced != null)
            advanced.* = .{ .iterator = self };
        return result;
    }

    /// Internal structure that represents the buffer. Performs bounds checks
    /// and, when applicable, converts WTF-16 code units into WTF-8 bytes.
    fn ParseOutput(comptime v: OutputVariant) type {
        return struct {
            cap: if (v == .raw) void else usize,
            prev: if (v == .wtf8) u16 else u0 = 0,
            buf: v.Buf(),

            /// Initialize the ParseOutput using the relevant buffer type.
            fn init(buf: if (v == .raw) [*]const u16 else v.Buf()) @This() {
                const buf2: v.Buf() = buf[0..0];
                return switch (v) {
                    .raw => .{
                        .cap = {},
                        .buf = buf2,
                    },
                    .wtf16 => .{
                        .cap = buf.len,
                        .buf = buf2,
                    },
                    .wtf8 => .{
                        .cap = buf.len,
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

fn AdvancedDecoder_(comptime encoding: WindowsArgIterator.Encoding, comptime sentinel: ?encoding.Sentinel()) type {
    const v = encoding.v(sentinel);
    return struct {
        iterator: *WindowsArgIterator,
        in_quotes: bool = false,
        num_backslashes: usize = 0,
        prev: if (v == .wtf8) u16 else u0 = 0,
        cmd_offset: usize = 0,
        buf_offset: usize = 0,

        /// Decode the next argument. This function can be retried with a reallocated
        /// buffer when an error is returned, but will otherwise return the new buffer.
        pub fn decodeNext(self: *@This(), buf: v.Param()) error{OutOfMemory}!?v.Ret() {
            return self.iterator.parseInternal(v, @This(), self, buf);
        }
    };
}

test "zlaap.ArgvIterator" {
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

test "zlaap.WasiArgs" {
    // Random argv buffer to test in a platform-independent environment.
    const test_buf: []const u8 = "abcxyz123456000+++\x00111222333444---555---+++\x00999___212___622\x00";
    const test_argv = [_][]const u8{
        test_buf[0..18],
        test_buf[19..][0..24],
        test_buf[19..][25..][0..15],
    };

    // Run the test with both spanned argv and unspanned argv.
    inline for ([_]bool{ false, true }) |span| {
        // Simulate WASI argument retrieval but for any target.
        var wasi_args = WasiArgs{};
        const buf = try wasi_args.allocBuf(
            std.testing.allocator,
            test_argv.len,
            test_buf.len,
            span,
        );
        defer wasi_args.deinit(std.testing.allocator);

        // Ensure the allocation was performed correctly.
        const ArgvT = if (span) [:0]u8 else [*:0]u8;
        const ptr_size = comptime std.math.max(@sizeOf(ArgvT), @sizeOf([*:0]u8));
        std.testing.expectEqual(test_buf.len, buf.buf.len);
        std.testing.expectEqual(test_argv.len, buf.argv.len);
        std.testing.expectEqual(test_buf.len + test_argv.len * ptr_size, wasi_args.buf.len);
        std.mem.copy(u8, buf.buf, test_buf);
        buf.argv[0] = @ptrCast([*:0]u8, buf.buf.ptr);
        buf.argv[1] = @ptrCast([*:0]u8, buf.buf.ptr) + 19;
        buf.argv[2] = @ptrCast([*:0]u8, buf.buf.ptr) + 19 + 25;
        var argv: [3][:0]const u8 = undefined;
        if (span) {
            const argv_ = buf.span();
            std.testing.expectEqual(test_argv.len, argv_.len);
            argv = argv_[0..test_argv.len].*;
        } else {
            var iterator = ArgvIterator{ .argv = buf.argv };
            for (argv) |*arg, i|
                arg.* = iterator.next() orelse return error.ExpectedArgument;
            std.testing.expect(iterator.next() == null);
        }
        for (argv) |arg, i|
            std.testing.expectEqualStrings(test_argv[i], arg);
    }
}

test "zlaap.WindowsArgIterator" {
    inline for (.{
        windows_test_ascii,
        windows_test_emoji,
        windows_test_unpaired,
    }) |test_case| {
        try test_case.execute();
    }
}

/// A Windows test case and its expected outputs.
const WindowsTest = struct {
    command_line: [:0]const u16,
    argv_raw: [3][]const u16,
    argv_wtf16: [3][]const u16,
    argv_wtf8: [3][]const u8,

    /// Convert a UTF-8 command line and expected results into a test case
    /// that supports WTF-16 testing as well.
    fn fromUtf8(
        comptime command_line: []const u8,
        comptime argv_raw: [3][]const u8,
        comptime argv_wtf8: [3][]const u8,
    ) WindowsTest {
        @setEvalBranchQuota(100 * (command_line.len +
            argv_raw[0].len + argv_raw[1].len + argv_raw[2].len +
            argv_wtf8[0].len + argv_wtf8[1].len + argv_wtf8[2].len));
        comptime return .{
            .command_line = std.unicode.utf8ToUtf16LeStringLiteral(command_line),
            .argv_wtf8 = argv_wtf8,
            .argv_raw = [_][]const u16{
                std.unicode.utf8ToUtf16LeStringLiteral(argv_raw[0]),
                std.unicode.utf8ToUtf16LeStringLiteral(argv_raw[1]),
                std.unicode.utf8ToUtf16LeStringLiteral(argv_raw[2]),
            },
            .argv_wtf16 = [_][]const u16{
                std.unicode.utf8ToUtf16LeStringLiteral(argv_wtf8[0]),
                std.unicode.utf8ToUtf16LeStringLiteral(argv_wtf8[1]),
                std.unicode.utf8ToUtf16LeStringLiteral(argv_wtf8[2]),
            },
        };
    }

    fn execute(comptime self: WindowsTest) !void {
        const iterator_init = WindowsArgIterator{ .command_line = self.command_line };
        {
            // Test case for `iterator.next()`.
            var iterator = iterator_init;
            for (self.argv_raw) |str| {
                const result = iterator.next() orelse return error.ExpectedArgument;
                std.testing.expectEqualSlices(u16, str, result);
            }

            // Ensure that there is no fourth argument.
            std.testing.expect(iterator.next() == null);
            std.testing.expect((try iterator.decodeNext(.wtf8, &[_]u8{})) == null);
            std.testing.expect((try iterator.decodeNext(.wtf16, &[_]u16{})) == null);
            std.testing.expect((try iterator.decodeNextZ(.wtf8, &[_]u8{}, 0)) == null);
            std.testing.expect((try iterator.decodeNextZ(.wtf16, &[_]u16{}, 0)) == null);
        }
        const allocator = std.testing.allocator;
        inline for (.{ "wtf16", "wtf8" }) |encoding_| {
            // Test case for the `iterator.decode` family of functions.
            var iterators = [_]WindowsArgIterator{iterator_init} ** 4;
            const encoding = @field(WindowsArgIterator.Encoding, encoding_);
            const S = encoding.Sentinel();
            inline for (@field(self, "argv_" ++ encoding_)) |str| {
                // Create buffers for `decodeNext` and `decodeNextZ`.
                var buf: [str.len]S = undefined;
                var buf_z: [str.len + 1]S = undefined;

                // Execute each function to be tested and store the results in an array.
                var results: std.meta.Tuple(&[_]type{[]S, [:0]S, []S, [:0]S}) = undefined;
                results[0] = (try iterators[0].decodeNext(encoding, &buf)) orelse return error.ExpectedArgument;
                results[1] = (try iterators[1].decodeNextZ(encoding, &buf_z, 0)) orelse return error.ExpectedArgument;
                results[2] = (try iterators[2].decodeAlloc(allocator, encoding)) orelse return error.ExpectedArgument;
                defer allocator.free(results[2]);
                results[3] = (try iterators[3].decodeAllocZ(allocator, encoding, 0)) orelse return error.ExpectedArgument;
                defer allocator.free(results[3]);

                // Ensure that the returned argument matches what is expected.
                inline for ([_]void{{}} ** results.len) |_, i|
                    std.testing.expectEqualSlices(S, str, results[i]);
            }
            // Ensure that there is no fourth argument.
            for (iterators) |*iterator| {
                std.testing.expect(iterator.next() == null);
                std.testing.expect((try iterator.decodeNext(.wtf8, &[_]u8{})) == null);
                std.testing.expect((try iterator.decodeNext(.wtf16, &[_]u16{})) == null);
                std.testing.expect((try iterator.decodeNextZ(.wtf8, &[_]u8{}, 0)) == null);
                std.testing.expect((try iterator.decodeNextZ(.wtf16, &[_]u16{}, 0)) == null);
            }
        }
    }
};

/// An ASCII command line that contains the "gotchas" that one may run into when
/// implementing a Windows command line argument parser. Useful to isolate errors
/// caused by parsing from errors caused by Unicode conversion.
const windows_test_ascii = WindowsTest.fromUtf8(
    "  \t \t  \t HELLO\"WORLD\\\" \t  \\\\\"\\\\\" \\\t \\\\ \"  \t  \tfoo\\\\\\\\\"bar\" ",
    [_][]const u8{ "", "HELLO\"WORLD\\\" \t  \\\\\"\\\\\" \\\t \\\\ \"", "foo\\\\\\\\\"bar\"" },
    [_][]const u8{ "", "HELLOWORLD\" \t  \\\\ \\\t \\\\ ", "foo\\\\bar" },
);

/// A Unicode command line with UTF-8 byte sequences ranging from 1 to 4 bytes
/// in length; additionally, when converted to UTF-16, there are both surrogate
/// and non-surrogate code units.
const windows_test_emoji = WindowsTest.fromUtf8(
    "_ⱵSƎƎⱵƎ  \t  \"Z😜☺_\\\" \" \t  T☺v😜\\Ⱶ   \t ",
    [_][]const u8{ "_ⱵSƎƎⱵƎ", "\"Z😜☺_\\\" \"", "T☺v😜\\Ⱶ" },
    [_][]const u8{ "_ⱵSƎƎⱵƎ", "Z😜☺_\" ", "T☺v😜\\Ⱶ" },
);

// A Unicode command line with unpaired surrogate codepoints.
const windows_test_unpaired = WindowsTest{
    .command_line = blk: {
        const buf = [_]u16{ ' ', 0xDC10, 0xD810, '"', ' ', 0xD810, 0xDC10, 's', 0xDC10, '\t', '"', ' ', 0xD810, ' ', 0x0 };
        break :blk buf[0..(buf.len - 1) :0];
    },
    .argv_raw = [_][]const u16{
        &[_]u16{},
        &[_]u16{ 0xDC10, 0xD810, '"', ' ', 0xD810, 0xDC10, 's', 0xDC10, '\t', '"' },
        &[_]u16{0xD810},
    },
    .argv_wtf16 = [_][]const u16{
        &[_]u16{},
        &[_]u16{ 0xDC10, 0xD810, ' ', 0xD810, 0xDC10, 's', 0xDC10, '\t' },
        &[_]u16{0xD810},
    },
    .argv_wtf8 = struct {
        const result = [_][]const u8{
            &[_]u8{},
            unpaired(0xDC10) ++ unpaired(0xD810) ++ " " ++
                paired(0xD810, 0xDC10) ++ "s" ++ unpaired(0xDC10) ++ "\t",
            &unpaired(0xD810),
        };

        fn paired(lead: u16, trail: u16) [4]u8 {
            const codepoint = 0x10000 + (@as(u21, lead - 0xD800) << 10) + (trail - 0xDC00);
            return [_]u8{
                0xF0 | @intCast(u8, codepoint >> 18),
                0x80 | @intCast(u8, (codepoint >> 12) & 0x3F),
                0x80 | @intCast(u8, (codepoint >> 6) & 0x3F),
                0x80 | @intCast(u8, codepoint & 0x3F),
            };
        }

        fn unpaired(codepoint: u16) [3]u8 {
            return [_]u8{
                0xE0 | @intCast(u8, codepoint >> 12),
                0x80 | @intCast(u8, (codepoint >> 6) & 0x3F),
                0x80 | @intCast(u8, codepoint & 0x3F),
            };
        }
    }.result,
};
