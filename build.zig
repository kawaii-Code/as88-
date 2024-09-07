const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const vaxis_dep = b.dependency("vaxis", .{
        .target = target,
        .optimize = optimize,
    });
    const vaxis_mod = vaxis_dep.module("vaxis");

    const lib_as88 = b.addStaticLibrary(.{
        .name = "libas88",
        .root_source_file = b.path("src/as88.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib_as88);

    const as88_cli = b.addExecutable(.{
        .name = "as88",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(as88_cli);

    const as88_tui = b.addExecutable(.{
        .name = "t88",
        .root_source_file = b.path("src/main_tui.zig"),
        .target = target,
        .optimize = optimize,
    });
    as88_tui.root_module.addImport("vaxis", vaxis_mod);
    b.installArtifact(as88_tui);

    const run_cli_cmd = b.addRunArtifact(as88_cli);
    run_cli_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cli_cmd.addArgs(args);
    }

    const run_cli_step = b.step("run", "Run the app");
    run_cli_step.dependOn(&run_cli_cmd.step);

    const run_tui_cmd = b.addRunArtifact(as88_tui);
    run_tui_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_tui_cmd.addArgs(args);
    }

    const run_tui_step = b.step("tui", "Run the text user interface");
    run_tui_step.dependOn(&run_tui_cmd.step);

    const lib_as88_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_lib_as88_unit_tests = b.addRunArtifact(lib_as88_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_as88_unit_tests.step);
}