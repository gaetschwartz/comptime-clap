const std = @import("std");
const cclap = @import("comptime-clap");
const ArgParser = cclap.ArgParserWithOpts;
const MakeOption = cclap.MakeOption;
const ValueParsingError = cclap.ValueParsingError;
const debug = std.debug;

const App = struct {
    help: bool = false,
    number: u32 = 0,
    // answer: []const u8,
    string: []const u8,
};

pub fn main() !void {
    const log = std.log.scoped(.main);
    _ = log; // autofix

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    const Parser = ArgParser(App, .{});
    var res = try Parser.parse(.{ .allocator = alloc });
    defer res.deinit();

    if (res.parsed.help)
        debug.print("--help\n", .{});
    debug.print("--number = {}\n", .{res.parsed.number});
    // debug.print("--answer = {s}\n", .{res.parsed.answer});
    debug.print("--string = {s}\n", .{res.parsed.string});
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
