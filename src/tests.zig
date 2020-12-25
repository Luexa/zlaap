const std = @import("std");
const lib = @import("./main.zig");

comptime {
    std.testing.refAllDecls(lib);
}
