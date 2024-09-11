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

    const as88 = b.addExecutable(.{
        .name = "as88",
        .root_source_file = b.path("src/main_as88.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(as88);

    const s88 = b.addExecutable(.{
        .name = "s88",
        .root_source_file = b.path("src/main_s88.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(s88);

    const t88 = b.addExecutable(.{
        .name = "t88",
        .root_source_file = b.path("src/main_t88.zig"),
        .target = target,
        .optimize = optimize,
    });
    t88.root_module.addImport("vaxis", vaxis_mod);
    b.installArtifact(t88);

    {
        const run_s88 = b.addRunArtifact(s88);
        run_s88.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_s88.addArgs(args);
        }
        const run_s88_step = b.step("run", "Run the main program: assembles and runs the first argument");
        run_s88_step.dependOn(&run_s88.step);
    }

    {
        const run_as88 = b.addRunArtifact(as88);
        run_as88.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_as88.addArgs(args);
        }
        const run_as88_step = b.step("run-as88", "Run the assembler error checker");
        run_as88_step.dependOn(&run_as88.step);
    }

    {
        const run_t88_cmd = b.addRunArtifact(t88);
        run_t88_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_t88_cmd.addArgs(args);
        }
        const run_t88_step = b.step("run-tui", "Run the text user interface");
        run_t88_step.dependOn(&run_t88_cmd.step);
    }

    const lib_as88_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_lib_as88_unit_tests = b.addRunArtifact(lib_as88_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_as88_unit_tests.step);
}