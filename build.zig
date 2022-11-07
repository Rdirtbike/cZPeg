const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();

    const main_test = b.addTest("src/czpeg.zig");
    main_test.addPackagePath("czpeg", "src/czpeg.zig");
    main_test.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_test.step);
}
