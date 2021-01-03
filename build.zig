const std = @import("std");
const CrossTarget = std.zig.CrossTarget;
const ReleaseMode = std.builtin.Mode;
const Builder = std.build.Builder;
const Step = std.build.Step;

pub fn build(b: *Builder) void {
    b.top_level_steps.shrinkRetainingCapacity(0);
    const mode_config = [_]?bool{
        b.option(bool, "debug", "Execute tests in Debug mode"),
        b.option(bool, "release-safe", "Execute tests in ReleaseSafe mode"),
        b.option(bool, "release-fast", "Execute tests in ReleaseFast mode"),
        b.option(bool, "release-small", "Execute tests in ReleaseSmall mode"),
    };
    const target: ?CrossTarget = blk: {
        const target_ = b.standardTargetOptions(.{});
        if (b.user_input_options.contains("target"))
            break :blk target_;
        break :blk null;
    };
    const default_mode = for (mode_config) |opt| {
        if (opt != null and opt.?)
            break false;
    } else true;

    const test_everything = b.option(bool, "test-everything", "Test on every mode and emulable target") orelse false;
    const test_filter = b.option([]const u8, "test-filter", "Filter which unit tests are to be executed");
    var cross_tests = b.option(bool, "cross-tests", "Execute tests through qemu, wasmtime, and wine") orelse test_everything;
    const wasi_tests = b.option(bool, "wasi-tests", "Execute tests through wasmtime") orelse cross_tests;
    const qemu_tests = (b.option(bool, "qemu-tests", "Execute tests through qemu") orelse cross_tests) and
        std.builtin.os.tag != .windows;
    const wine_tests = (b.option(bool, "wine-tests", "Execute tests through wine") orelse cross_tests) and
        std.builtin.os.tag != .windows and (std.builtin.cpu.arch == .x86_64 or std.builtin.cpu.arch == .i386);
    cross_tests = wasi_tests or qemu_tests or wine_tests;

    var num_modes: usize = 0;
    var modes: [4]ReleaseMode = undefined;
    for ([_]ReleaseMode{
        .Debug,
        .ReleaseSafe,
        .ReleaseFast,
        .ReleaseSmall,
    }) |mode, i| {
        const is_default = default_mode and (i == 0 or test_everything);
        if (mode_config[i] orelse is_default) {
            defer num_modes += 1;
            modes[num_modes] = mode;
        }
    }

    if (num_modes == 0 and !b.invalid_user_input) {
        if (test_everything) {
            std.debug.print("error: all build modes were manually disabled\n", .{});
        } else {
            std.debug.print("error: unable to infer requested build mode\n", .{});
        }
        std.process.exit(1);
    }

    const test_step = b.step("test", "Run library tests");
    var test_config = TestConfig{
        .b = b,
        .step = test_step,
        .modes = modes[0..num_modes],
        .filter = test_filter,
        .wasi_tests = wasi_tests,
        .qemu_tests = qemu_tests,
        .wine_tests = wine_tests,
    };

    if (cross_tests) {
        if (target) |t|
            test_config.addTest(t, true);
        for ([_]std.Target.Cpu.Arch{
            .x86_64,
            .i386,
            .aarch64,
            .arm,
            .riscv64,
        }) |arch| {
            if (arch.ptrBitWidth() > std.builtin.cpu.arch.ptrBitWidth())
                continue;
            if (arch == .riscv64 and std.builtin.os.tag != .linux)
                continue;
            if (qemu_tests or arch == std.builtin.cpu.arch)
                test_config.addTest(.{ .cpu_arch = arch, .os_tag = std.builtin.os.tag }, false);
        }
        if (wine_tests) {
            const cpu_arch = std.builtin.cpu.arch;
            if (cpu_arch == .x86_64)
                test_config.addTest(.{ .cpu_arch = .x86_64, .os_tag = .windows }, false);
            test_config.addTest(.{ .cpu_arch = .i386, .os_tag = .windows }, false);
        }
        if (wasi_tests)
            test_config.addTest(.{ .cpu_arch = .wasm32, .os_tag = .wasi }, false);
    } else test_config.addTest(target orelse .{}, true);

    b.default_step = test_step;
}

const TestConfig = struct {
    b: *Builder,
    step: *Step,
    num_tests: usize = 0,
    modes: []const ReleaseMode,
    filter: ?[]const u8,
    wasi_tests: bool,
    qemu_tests: bool,
    wine_tests: bool,

    fn addTest(self: *TestConfig, target: CrossTarget, default: bool) void {
        const filter_arg = [_][]const u8{
            if (self.filter != null) "\n  filter: " else "",
            if (self.filter) |f| f else "",
        };
        const target_str = target.zigTriple(self.b.allocator) catch unreachable;
        for (self.modes) |mode| {
            const log_step = self.b.addLog("unit tests\n  target: {}\n  mode: {}{}{}\n", .{
                target_str,
                @tagName(mode),
                filter_arg[0],
                filter_arg[1],
            });

            const test_step = self.b.addTest("./zlaap.zig");
            test_step.setFilter(self.filter);
            test_step.setTarget(target);
            test_step.setBuildMode(mode);
            test_step.enable_wasmtime = default or self.wasi_tests;
            test_step.enable_qemu = default or self.qemu_tests;
            test_step.enable_wine = default or self.wine_tests;

            if (self.newline()) |step|
                self.step.dependOn(step);
            self.step.dependOn(&log_step.step);
            self.step.dependOn(&test_step.step);
        }
    }

    fn newline(self: *TestConfig) ?*Step {
        defer self.num_tests += 1;
        if (self.num_tests > 0) {
            const log_step = self.b.addLog("\n", .{});
            return &log_step.step;
        } else return null;
    }
};
