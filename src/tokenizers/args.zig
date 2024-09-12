const std = @import("std");
const this = @This();

pub const ArgumentIteratorOpts = struct {
    allowTooManyDashes: bool = false,
};

pub const ArgumentIteratorArg = union(enum) {
    kv: KV,
    long: []const u8,
    short: []const u8,
    value: []const u8,

    pub const KV = struct {
        key: []const u8,
        value: []const u8,
    };

    pub fn asStr(self: ArgumentIteratorArg) []const u8 {
        return switch (self) {
            .kv => self.kv.key,
            .long => self.long,
            .short => self.short,
            .value => self.value,
        };
    }

    pub fn format(self: ArgumentIteratorArg, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .kv => try writer.print("Arg{{kv:{s}={s}}}", .{ self.kv.key, self.kv.value }),
            .long => try writer.print("Arg{{long:{s}}}", .{self.long}),
            .short => try writer.print("Arg{{short:{s}}}", .{self.short}),
            .value => try writer.print("Arg{{value:{s}}}", .{self.value}),
        }
    }

    pub const DisplayOpts = struct {
        surround: ?u8,
    };
    pub fn Display(comptime opts: DisplayOpts) type {
        return struct {
            data: ArgumentIteratorArg,

            pub fn format(v: @This(), _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                if (opts.surround) |s| try writer.writeByte(s);
                switch (v.data) {
                    .kv => |kv| try writer.print("{s}={s}", .{ kv.key, kv.value }),
                    .long => |e| try writer.print("--{s}", .{e}),
                    .short => |e| try writer.print("-{s}", .{e}),
                    .value => |e| try writer.print("{s}", .{e}),
                }
                if (opts.surround) |s| try writer.writeByte(s);
            }
        };
    }
    pub fn displayQuoted(self: ArgumentIteratorArg) Display(.{ .surround = '"' }) {
        return .{ .data = self };
    }

    pub fn eql(a: ArgumentIteratorArg, b: ArgumentIteratorArg) bool {
        return switch (a) {
            .kv => switch (b) {
                .kv => std.mem.eql(u8, a.kv.key, b.kv.key) and std.mem.eql(u8, a.kv.value, b.kv.value),
                else => false,
            },
            .long => switch (b) {
                .long => std.mem.eql(u8, a.long, b.long),
                else => false,
            },
            .short => switch (b) {
                .short => std.mem.eql(u8, a.short, b.short),
                else => false,
            },
            .value => switch (b) {
                .value => std.mem.eql(u8, a.value, b.value),
                else => false,
            },
        };
    }
};

pub fn ArgumentIterator(opts: ArgumentIteratorOpts) type {
    return struct {
        source: []const []const u8,
        i: usize = 0,
        j: usize = 0,
        singleCharFlagMode: bool = false,
        alloc: std.mem.Allocator,

        const Self = @This();
        pub const Tag = enum { Key, Value };
        pub const options = opts;
        pub const Arg = this.ArgumentIteratorArg;

        pub fn init(alloc: std.mem.Allocator, source: []const []const u8) Self {
            return .{
                .source = source,
                .alloc = alloc,
            };
        }

        pub const Error = error{
            tooManyDashes,
        } || std.zig.string_literal.ParseError;

        pub fn next(self: *Self) Error!?Self.Arg {
            // std.debug.print("i: {}, j: {}, singleCharFlagMode: {}\n", .{ self.i, self.j, self.singleCharFlagMode });
            if (self.i >= self.source.len) {
                return null;
            }
            // std.debug.print("curr: {s}\n", .{self.source[self.i][self.j..]});
            const string = self.source[self.i];
            if (self.j >= string.len) {
                self.goNextString();
                // std.debug.print("going to next because j >= len\n", .{});
                return self.next();
            }

            if (self.singleCharFlagMode) {
                return try self.handleSingleCharFlagMode();
            }

            const d = self.countDashes();
            self.j += d;
            const curr = string[self.j..];
            // std.debug.print("d: {}, curr: {s}\n", .{ d, curr });

            switch (d) {
                0 => {
                    self.goNextString();
                    return .{ .value = curr };
                },
                1 => {
                    self.singleCharFlagMode = true;
                    return self.next();
                },
                2 => {
                    if (std.mem.indexOf(u8, curr, "=")) |eq| {
                        const k = curr[0..eq];
                        const v = curr[eq + 1 ..];
                        self.goNextString();
                        return try kv(k, v);
                    } else {
                        self.goNextString();
                        return .{ .long = curr };
                    }
                },
                else => {
                    if (options.allowTooManyDashes) {
                        self.goNextString();
                        return .{ .value = curr };
                    } else {
                        return error.tooManyDashes;
                    }
                },
            }
        }

        inline fn goNextString(self: *Self) void {
            self.i += 1;
            self.j = 0;
            self.singleCharFlagMode = false;
        }

        fn kv(key: []const u8, value: []const u8) !Self.Arg {
            std.log.debug("kv: key: {s}, value: {s}", .{ key, value });
            return .{ .kv = .{ .key = key, .value = value } };
        }

        fn countDashes(self: *Self) usize {
            var d: usize = 0;
            for (self.source[self.i][self.j..]) |c| {
                if (c == '-') {
                    d += 1;
                } else {
                    break;
                }
            }
            return d;
        }

        fn handleSingleCharFlagMode(self: *Self) !?Self.Arg {
            const flag = self.source[self.i][self.j..];
            if (flag.len > 1 and flag[1] == '=') {
                self.singleCharFlagMode = false;
                const key = flag[0..1];
                const value = flag[2..];
                self.goNextString();
                return try kv(key, value);
            } else {
                self.j += 1;
                return .{ .short = flag[0..1] };
            }
        }

        pub const Peeked = struct {
            j: usize,
            i: usize,
            iter: *Self,
            value: Self.Arg,

            pub fn consume(self: *const Peeked) []const u8 {
                self.iter.j = self.j;
                self.iter.i = self.i;
                return self.value.value;
            }
        };

        pub fn peek(self: *Self) Error!?Peeked {
            const j = self.j;
            const i = self.i;
            const res = try self.next();
            self.j = j;
            self.i = i;
            if (res) |r|
                return Peeked{ .j = j, .i = i, .iter = self, .value = r }
            else
                return null;
        }
    };
}

test "ArgumentIterator" {
    const source = [_][]const u8{
        "foo", // positional
        "-u", // single short flag
        "-bar", // multiple short flag
        "-n=10", // short key-value
        // "-z=\"7-5\"", // short key-value with quotes
        "-op=7", // short flag and short key-value
        "--baz", // long flag
        "--qux=quux", // long key-value
        // "--gaet=\"hello-world\"", // long key-value with quotes
        "corge", // positional
        "100-12", // positional with dash in it
    };
    const A = ArgumentIterator(.{});
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var iter = A.init(arena.allocator(), &source);

    var arr = std.ArrayList(A.Arg).init(std.testing.allocator);
    defer arr.deinit();

    while (try iter.next()) |arg| try arr.append(arg);

    const expected = &[_]A.Arg{
        .{ .value = "foo" },
        .{ .short = "u" },
        .{ .short = "b" },
        .{ .short = "a" },
        .{ .short = "r" },
        .{ .kv = .{ .key = "n", .value = "10" } },
        // .{ .kv = .{ .key = "z", .value = "7-5" } },
        .{ .short = "o" },
        .{ .kv = .{ .key = "p", .value = "7" } },
        .{ .long = "baz" },
        .{ .kv = .{ .key = "qux", .value = "quux" } },
        // .{ .kv = .{ .key = "gaet", .value = "hello-world" } },
        .{ .value = "corge" },
        .{ .value = "100-12" },
    };

    for (0..expected.len) |i| {
        const a = arr.items[i];
        const e = expected[i];

        if (!A.Arg.eql(a, e)) {
            std.debug.print(
                \\Missmatch at index {d}:
                \\  got:      {}
                \\  expected: {}
                \\
            ,
                .{ i, a, e },
            );
            std.debug.print("expected: {s}\n", .{expected});
            std.debug.print("actual:   {s}\n", .{arr.items});
            return error.Mismatch;
        }
    }
}
