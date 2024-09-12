const std = @import("std");

pub fn Typing(comptime T: type) type {
    return struct {
        type: union(enum) {
            Literal: union(enum) { Int, Float, Bool, String: fn (T) []const u8 },
            Complex: enum { Struct, Enum, Union, Opaque },
            Unsupported,
        },
        optional: usize = 0,
    };
}
pub fn getTyping(comptime T: type) Typing(T) {
    if (AsSliceOfOpt(T, u8)) |f| {
        return .{ .type = .{ .Literal = .{ .String = f } } };
    }
    return switch (@typeInfo(T)) {
        .Bool => .{ .type = .{ .Literal = .Bool } },
        .Int => .{ .type = .{ .Literal = .Int } },
        .Float => .{ .type = .{ .Literal = .Float } },
        .Pointer => |ptr| getTyping(ptr.child),
        .Optional => |opt| blk: {
            const inner = getTyping(opt.child);
            break :blk .{ .type = inner.type, .optional = 1 + inner.optional };
        },
        .Struct => .{ .type = .{ .Complex = .Struct } },
        .Enum => .{ .type = .{ .Complex = .Enum } },
        .Union => .{ .type = .{ .Complex = .Union } },
        .Opaque => .{ .type = .{ .Complex = .Opaque } },
        .NoReturn, .Array, .ComptimeFloat, .ComptimeInt, .Undefined, .Null, .ErrorUnion, .ErrorSet, .Fn, .Frame, .AnyFrame, .Vector, .EnumLiteral, .Type, .Void => .{ .type = .Unsupported },
    };
}

fn canTupleBeAnArray(
    comptime T: type,
    comptime Child: type,
) bool {
    switch (@typeInfo(T)) {
        .Struct => |s| {
            if (s.is_tuple) {
                for (s.fields) |field| {
                    if (field.type != Child) {
                        return false;
                    }
                }
                return true;
            } else {
                return false;
            }
        },
        else => return false,
    }
}

pub const OptionUnion = struct { tpe: type, @"union": std.builtin.Type.Union };
pub fn asOptionalUnion(comptime T: type) ?OptionUnion {
    return switch (@typeInfo(T)) {
        .Optional => |o| switch (@typeInfo(o.child)) {
            .Union => |u| .{ .tpe = o.child, .@"union" = u },
            else => null,
        },
        else => null,
    };
}

pub fn AsSliceOfOpt(comptime T: type, comptime Child: type) ?fn (T) []const Child {
    const ti = @typeInfo(T);

    return switch (ti) {
        .Struct => if (comptime canTupleBeAnArray(T, Child)) AsSliceOfOpt([]const Child, Child) else null,
        .Pointer => |ptrInfo| switch (ptrInfo.size) {
            .Slice => if (ptrInfo.child == Child and ptrInfo.is_const) struct {
                pub fn f(slice: T) []const Child {
                    // @compileLog(std.fmt.comptimePrint("AsSliceOfOpt.Slice: {}\n", .{ptrInfo}));
                    return slice;
                }
            }.f else null,
            .One => if (AsSliceOfOpt(ptrInfo.child, Child)) |f| struct {
                pub fn ff(slice: T) []const Child {
                    return f(slice.*);
                }
            }.ff else null,
            .C => null,
            .Many => null,
        },
        .Array => |arrInfo| if (arrInfo.child == Child) struct {
            pub fn f(array: T) []const Child {
                return @as([arrInfo.len]Child, array)[0..arrInfo.len];
            }
        }.f else null,
        else => null,
    };
}

test "AsSliceOfOpt []const u8" {
    if (AsSliceOfOpt([]const u8, u8)) |f| {
        try std.testing.expectEqualStrings("hello", f("hello"));
    } else {
        std.debug.print("AsSliceOfOpt([]const u8, u8) is null\n", .{});
        return error.AsSliceOfOptIsNull;
    }
}

pub fn asSliceOfOpt(comptime Child: type, comptime obj: anytype) ?[]const Child {
    const ti = @typeInfo(@TypeOf(obj));

    // _ = std.io.getStdErr().write(std.fmt.comptimePrint("ti: {}\n", .{ti})) catch unreachable;

    switch (ti) {
        .Struct => |s| {
            if (comptime canTupleBeAnArray(@TypeOf(obj), Child)) {
                // @compileLog("Struct is a tuple and can be an array");
                const final = blk: {
                    var arr: [s.fields.len]Child = undefined;
                    inline for (s.fields, &arr) |f, *a| {
                        a.* = @field(obj, f.name);
                    }
                    break :blk arr;
                };
                return &final;
            }
            return null;
        },
        .Pointer => |ptrInfo| {
            switch (ptrInfo.size) {
                .Slice => {
                    if (ptrInfo.child == Child) {
                        return @as([]const Child, @constCast(obj));
                    } else {
                        return null;
                    }
                },
                .One => return asSliceOfOpt(Child, @as(*ptrInfo.child, @constCast(obj)).*),
                .C, .Many => return null,
            }
        },
        .Array => |arrInfo| {
            if (arrInfo.child == Child) {
                // const arr = if (arrInfo.sentinel) |ptr|
                //     @as([arrInfo.len:@as(*const arrInfo.child, @ptrCast(@alignCast(ptr))).*]T, obj)[0..]
                // else
                //     @as([arrInfo.len]T, obj);
                const arr = @as([arrInfo.len]Child, obj);
                return &arr;
            } else {
                return null;
            }
        },

        else => return null,
    }
}

fn expectAsSliceOfOpt(comptime T: type, expected: []const T, comptime actual: anytype) !void {
    const slice = comptime asSliceOfOpt(T, actual);
    if (slice) |s| {
        return std.testing.expectEqualSlices(T, expected, s);
    } else {
        const canBeSlice = canTupleBeAnArray(@TypeOf(actual), T);

        std.debug.print("Expected a slice of type '{s}', but got null (canBeSlice: {})\n", .{ @typeName(T), canBeSlice });
        return error.notASlice;
    }
}

test "asSliceOfOpt" {
    const Foo = struct { a: u8, b: u8 };
    const f1 = Foo{ .a = 1, .b = 2 };
    const f2 = Foo{ .a = 3, .b = 4 };
    const f3 = Foo{ .a = 5, .b = 6 };
    const arr = [_]Foo{ f1, f2, f3 };
    try expectAsSliceOfOpt(Foo, &.{ f1, f2, f3 }, .{ f1, f2, f3 });
    try expectAsSliceOfOpt(Foo, &.{ f1, f2, f3 }, arr);
    try expectAsSliceOfOpt(Foo, &.{ f1, f2, f3 }, &arr);
    try expectAsSliceOfOpt(Foo, &.{ f1, f2, f3 }, arr[0..]);
    try expectAsSliceOfOpt(Foo, &.{ f2, f3 }, arr[1..]);
}

pub fn asString(comptime obj: anytype) ?[]const u8 {
    return asSliceOfOpt(u8, obj);
}

test "asString 'structs'" {
    const Foo = struct {
        x: i32,
        y: i32,
    };

    const foo = Foo{ .x = 1, .y = 2 };
    const fooStr = asString(foo);
    try std.testing.expectEqual(null, fooStr);
}

test "asString const string" {
    const hello = "hello";

    const helloStr = asString(hello);
    try std.testing.expect(helloStr != null);
    try std.testing.expectEqualStrings(hello, helloStr.?);
}

test "asString const string ptr" {
    const hello = "hello";

    const helloStr = asString(&hello);
    try std.testing.expect(helloStr != null);
    try std.testing.expectEqualStrings(hello, helloStr.?);
}

test "asString const string ptr ptr" {
    const hello = "hello";

    const helloStr = asString(&&hello);
    try std.testing.expect(helloStr != null);
    try std.testing.expectEqualStrings(hello, helloStr.?);
}

test "asString array" {
    const array = [_]u8{ 'f', 'o', 'o' };
    const arrayStr = asString(array);
    try std.testing.expect(arrayStr != null);
    try std.testing.expectEqualStrings(&array, arrayStr.?);
}

test "asString array ptr" {
    const array = [_]u8{ 'f', 'o', 'o' };
    const arrayStr = asString(&array);
    try std.testing.expect(arrayStr != null);
    try std.testing.expectEqualStrings(&array, arrayStr.?);
}

test "asString slice" {
    const str = "hello";
    const sliceStr = asString(str[0..4]);
    try std.testing.expect(sliceStr != null);
    try std.testing.expectEqualStrings("hell", sliceStr.?);
}

test "asString slice ptr" {
    const str = "hello";
    const sliceStr = asString(&str[0..4]);
    try std.testing.expect(sliceStr != null);
    try std.testing.expectEqualStrings("hell", sliceStr.?);
}
