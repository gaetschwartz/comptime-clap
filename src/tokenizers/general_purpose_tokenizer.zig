const std = @import("std");
const tks = @import("tokenizers.zig");
const cfg = @import("../config.zig");

pub const TokenizerOptions = struct {
    ignore: []const u8,
    delimiters: []const u8,

    const Self = @This();

    fn sizeOf(comptime item: anytype) usize {
        var size = 0;
        switch (@typeInfo(@TypeOf(item))) {
            .Array => |arr| switch (arr.child) {
                type => for (@as([arr.len]type, item)) |t| {
                    size += tks.fieldCharNames(t).len;
                },
                u8 => size = arr.len,
                else => @compileError("ignore must be an array of u8 or a single type but got " ++ @typeName(@TypeOf(item))),
            },
            .Struct => |s| {
                if (!s.is_tuple) @compileError("item must be a tuple struct");
                for (s.fields) |field| {
                    size += tks.fieldCharNames(@field(item, field.name)).len;
                }
            },
            .Pointer => |_| {
                size = sizeOf(item.*);
            },
            .Type => {
                size = tks.fieldCharNames(item).len;
            },
            else => @compileError("ignore must be an array or a single type but got " ++ @typeName(@TypeOf(item))),
        }
        return size;
    }

    fn arrayOf(comptime item: anytype) [sizeOf(item)]u8 {
        var array: [sizeOf(item)]u8 = undefined;
        var idx = 0;
        switch (@typeInfo(@TypeOf(item))) {
            .Array => |arr| switch (arr.child) {
                type => for (@as([arr.len]type, item)) |t| {
                    const names = tks.fieldCharNames(t);
                    @memcpy(array[idx .. idx + names.len], names);
                    idx += names.len;
                },
                u8 => array = item,
                else => @compileError("ignore must be an array of u8 or a single type"),
            },
            .Struct => |s| {
                if (!s.is_tuple) @compileError("item must be a tuple struct");
                for (s.fields) |field| {
                    const names = tks.fieldCharNames(@field(item, field.name));
                    @memcpy(array[idx .. idx + names.len], names);
                    idx += names.len;
                }
            },
            .Pointer => |_| {
                array = arrayOf(item.*);
            },
            .Type => {
                const names = tks.fieldCharNames(item);
                @memcpy(array[idx .. idx + names.len], names);
            },
            else => @compileError("ignore must be an array or a single type"),
        }
        return array;
    }

    pub fn forTypes(comptime ignore: anytype, comptime delimiters: anytype) Self {
        const finalIgnore = comptime arrayOf(ignore);
        const finalDelimiters = comptime arrayOf(delimiters);
        return .{
            .ignore = &finalIgnore,
            .delimiters = &finalDelimiters,
        };
    }
};

pub fn GeneralPurposeTokenizer(comptime options: TokenizerOptions) type {
    return struct {
        source: []const u8,
        index: usize,

        const Self = @This();

        pub fn init(src: []const u8) Self {
            return .{
                .source = src,
                .index = 0,
            };
        }

        pub fn next(self: *Self) ?[]const u8 {
            var start = self.index;
            // std.debug.print("tokenizing: '{s}'\n", .{self.source[start..]});
            if (self.index >= self.source.len) {
                return null;
            }
            var hasSeenRealChar = false;
            loop: while (self.index < self.source.len) {
                const c = self.source[self.index];

                inline for (options.delimiters) |d| {
                    if (c == d) {
                        if (start == self.index) {
                            self.index += 1;
                        }
                        return self.source[start..self.index];
                    }
                }

                // if we find one of the ignored characters, we dont want to skip it as it would be part of the token
                // so we return the current and skip it on the next iteration
                inline for (options.ignore) |i| {
                    if (c == i) {
                        if (!hasSeenRealChar) {
                            self.index += 1;
                            start = self.index;
                            continue :loop;
                        }
                        // std.debug.print("c: '{s}'\n", .{&[_]u8{c}});
                        const curr = self.source[start..self.index];
                        self.index += 1;
                        return curr;
                    }
                }
                hasSeenRealChar = true;
                self.index += 1;
            }
            return self.source[start..];
        }

        fn toArray(self: *Self, allocator: std.mem.Allocator) ![]const []const u8 {
            var tokens = std.ArrayList([]const u8).init(allocator);

            while (self.next()) |t| try tokens.append(t);

            return tokens.toOwnedSlice();
        }
    };
}

pub fn TokenizerFor(comptime ignore: anytype, comptime delimiters: anytype) type {
    return GeneralPurposeTokenizer(TokenizerOptions.forTypes(ignore, delimiters));
}

test "tokenizer" {
    const Delimiters = union(enum) { @"(", @")", @"{", @"}", @";", @"," };
    const Spaces = union(enum) { @" ", @"\t", @"\n" };
    const Operators = union(enum) { @"+", @"-", @"*", @"/" };
    var tokenizer = GeneralPurposeTokenizer(TokenizerOptions.forTypes(Spaces, .{ Delimiters, Operators })).init("12  - (3) ");
    const array = try tokenizer.toArray(std.testing.allocator);
    defer std.testing.allocator.free(array);

    try std.testing.expectEqualDeep(array, &[_][]const u8{ "12", "-", "(", "3", ")", "" });
}
