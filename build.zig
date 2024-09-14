const std = @import("std");

const MOD_NAME = "comptime-clap";
const MODE_ENTRY_FILE = "comptime_clap.zig";

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule(MOD_NAME, .{
        .root_source_file = b.path(MODE_ENTRY_FILE),
        .target = target,
        .optimize = optimize,
    });
    const clap_mod = b.addModule(MOD_NAME, .{ .root_source_file = b.path(MODE_ENTRY_FILE) });

    const test_step = b.step("test", "Run all tests in all modes.");

    const tests = b.addTest(.{
        .root_source_file = b.path(MODE_ENTRY_FILE),
        .target = target,
        .optimize = optimize,
    });
    const run_tests = b.addRunArtifact(tests);
    test_step.dependOn(&run_tests.step);

    const example_step = b.step("examples", "Build examples");
    const check_step = b.step("check", "Build all examples without installing them");
    inline for ([_][]const u8{
        "simple-ex",
        "complex",
    }) |example_name| {
        const example = b.addExecutable(.{
            .name = example_name,
            .root_source_file = b.path(b.fmt("examples/{s}.zig", .{example_name})),
            .target = target,
            .optimize = optimize,
        });
        const install_example = b.addInstallArtifact(example, .{});
        example.root_module.addImport(MOD_NAME, clap_mod);
        example_step.dependOn(&example.step);
        example_step.dependOn(&install_example.step);

        check_step.dependOn(&example.step);

        const run_step = b.step("run-" ++ example_name, "Run the " ++ example_name ++ " example");
        const run_example = b.addRunArtifact(example);
        if (b.args) |args| {
            run_example.addArgs(args);
        }
        run_step.dependOn(&run_example.step);
    }

    const all_step = b.step("all", "Build everything and runs all tests");
    all_step.dependOn(test_step);
    all_step.dependOn(example_step);

    b.default_step.dependOn(example_step);
}
