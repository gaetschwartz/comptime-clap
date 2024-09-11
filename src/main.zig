const std = @import("std");
const config = @import("config.zig");
const ArgParser = @import("comptime_parser.zig").ArgParserWithOpts;

const Repl = struct {
    verbose: bool = false,
};
const Eval = struct {
    verbose: bool = false,
    json: bool = false,
    @"0": []const u8,
};
const Compile = struct {
    verbose: bool = false,
    @"0": []const u8,
};

const JIT = struct {
    verbose: bool = false,
    json: bool = false,
    @"0": []const u8,
    file: []const u8 = "output",
};

const App = struct {
    help: bool = false,
    command: ?union(enum) {
        repl: Repl,
        eval: Eval,
        compile: Compile,
        jit: JIT,
    },
};

pub fn main() !void {
    const log = std.log.scoped(.main);

    var gpa = config.gpa();
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const Parser = ArgParser(App, .{});
    var parsed = try Parser.parse(.{ .allocator = alloc });
    defer parsed.deinit();

    if (parsed.parsed.help) {
        try Parser.printUsage();
        std.process.exit(0);
    }
    inline for (@typeInfo(App).Struct.fields) |field| {
        const val = @field(parsed.parsed, field.name);
        std.log.debug("{s} = {any}", .{ field.name, val });
    }
    if (parsed.parsed.command) |c| {
        switch (c) {
            .repl => |o| log.info("Repl with args: {}\n", .{o}),
            .eval => |o| log.info("Eval with args: {}\n", .{o}),
            .compile => |o| log.info("Compile with args: {}\n", .{o}),
            .jit => |o| log.info("JIT with args: {}\n", .{o}),
        }
    }
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
