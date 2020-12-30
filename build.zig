const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    b.top_level_steps.shrinkRetainingCapacity(0);
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const main_tests = b.addTest("zlaap.zig");
    main_tests.setFilter(b.option([]const u8, "test-filter", "Filter which tests to run"));
    main_tests.setBuildMode(mode);
    main_tests.setTarget(target);
    main_tests.enable_wasmtime = true;
    main_tests.enable_qemu = true;
    main_tests.enable_wine = true;

    const test_step = b.step("test", "Run all tests for the library");
    test_step.dependOn(&main_tests.step);

    b.default_step = test_step;
}
