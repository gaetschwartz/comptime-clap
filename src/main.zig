const std = @import("std");
const config = @import("config.zig");
const ArgParser = @import("comptime_clap.zig").ArgParserWithOpts;
const MakeOption = @import("comptime_clap.zig").MakeOption;
const ValueParsingError = @import("comptime_clap.zig").ValueParsingError;

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

const Crazy = struct {
    // showHelp: MakeOption("{h,help}[Print help message]+=false", bool),
    // myUrl: MakeOption("{u,url}[URL to fetch data from]", []const u8),
    // verbosity: MakeOption("{v,verbose}[Increase verbosity level]+=0", u32),
    complex: ComplexType,

    pub fn format(v: @This(), comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = opts;
        _ = fmt;
        // try writer.print("Crazy{{showHelp: {}, myUrl: {s}, verbosity: {}}}", .{ v.showHelp, v.myUrl, v.verbosity });
        try writer.print("Crazy{{complex: {}}}", .{v.complex});
    }
};

const ComplexType = struct {
    value: u32,

    pub fn parse(v: []const u8) ValueParsingError!ComplexType {
        return .{ .value = std.fmt.parseInt(u32, v, 10) catch return error.invalid_int };
    }
};

const JIT = struct {
    verbose: bool = false,
    json: bool = false,
    int: u32 = 0,
    float: f32 = 0.0,
    @"0": []const u8,
    file: []const u8 = "output",
    name: []const u8,

    pub fn format(v: @This(), comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = opts;
        _ = fmt;
        try writer.print("Jit{}", .{std.json.Formatter(@This()){ .value = v, .options = .{} }});
    }
};

const App = struct {
    help: bool = false,
    command: ?union(enum) {
        repl: Repl,
        eval: Eval,
        compile: Compile,
        jit: JIT,
        crazy: Crazy,
    },
};

pub fn main() !void {
    const log = std.log.scoped(.main);
    _ = log; // autofix

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
            .repl => |o| std.debug.print("Repl with args: {}\n", .{o}),
            .eval => |o| std.debug.print("Eval with args: {}\n", .{o}),
            .compile => |o| std.debug.print("Compile with args: {}\n", .{o}),
            .jit => |o| std.debug.print("JIT with args: {}\n", .{o}),
            .crazy => |o| std.debug.print("Crazy with args: {}\n", .{o}),
        }
    }
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
