pub const ansi = @import("ansi.zig");
pub const testing = @import("testing.zig");
pub const fs = @import("fs.zig");
const std = @import("std");

pub fn Result(comptime T: type, comptime E: type) type {
    return union(enum) {
        Success: T,
        Error: E,

        const Self = @This();

        pub inline fn success(value: T) Self {
            return .{ .Success = value };
        }

        pub inline fn err(value: E) Self {
            return .{ .Error = value };
        }

        pub inline fn successOrNull(self: Self) ?T {
            return switch (self) {
                .Success => |s| s,
                .Error => null,
            };
        }

        pub inline fn errOrNull(self: Self) ?E {
            return switch (self) {
                .Success => null,
                .Error => |e| e,
            };
        }

        pub inline fn isSuccess(self: Self) bool {
            return switch (self) {
                .Success => true,
                .Error => false,
            };
        }

        pub inline fn isError(self: Self) bool {
            return !self.isSuccess();
        }

        pub fn unwrapWithError(self: Self, with: fn (E) noreturn) T {
            return switch (self) {
                .Success => |s| s,
                .Error => |e| with(e),
            };
        }

        fn printErrorAndExit(e: E) noreturn {
            std.io.getStdErr().writer().print("Error: {}\n", .{ErrorFormatter.init(e)}) catch @panic("Failed to print error");
            std.process.exit(1);
        }

        pub fn unwrap(self: Self) T {
            return self.unwrapWithError(printErrorAndExit);
        }

        pub fn deinit(self: *Self) void {
            switch (self.*) {
                .Success => |*s| if (std.meta.hasMethod(T, "deinit")) {
                    s.deinit();
                },
                .Error => |*e| if (std.meta.hasMethod(E, "deinit")) {
                    e.deinit();
                },
            }
        }

        pub const ErrorFormatter = struct {
            value: E,

            pub fn init(value: E) ErrorFormatter {
                return .{ .value = value };
            }

            pub fn format(v: ErrorFormatter, comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) !void {
                _ = opts;
                _ = fmt;
                if (std.meta.hasMethod(E, "display")) {
                    try E.display(&v.value, writer);
                } else {
                    try writer.print("{}", .{v.value});
                }
            }
        };
    };
}

pub fn WriterOf(comptime T: type) type {
    return std.io.GenericWriter(T, T.Error, T.writer);
}

pub fn Pair(comptime T: type, comptime U: type) type {
    return struct {
        first: T,
        second: U,

        const Self = @This();

        pub fn init(first: T, second: U) Self {
            return Self{
                .first = first,
                .second = second,
            };
        }
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

pub const StringSet = struct {
    inner: std.StaticStringMap(void),

    const Self = @This();

    pub fn init(comptime args: anytype) Self {
        if (asSliceOfOpt([]const u8, args)) |strings| {
            const kvs = comptime blk: {
                var innerArgs: [strings.len]struct { []const u8 } = undefined;
                for (strings, &innerArgs) |s, *a| {
                    a.* = .{s};
                }
                break :blk innerArgs;
            };
            return Self{
                .inner = std.StaticStringMap(void).initComptime(kvs),
            };
        } else {
            @compileError("Expected a slice-like type of strings but got " ++ @typeName(@TypeOf(args)));
        }
    }

    pub fn initFromString(comptime str: []const u8, sep: []const u8) Self {
        const l = std.mem.count(u8, str, sep) + 1;
        const kvs = comptime blk: {
            var strings = std.mem.split(str, sep);
            var innerArgs: [l]struct { []const u8 } = undefined;
            var i = 0;
            while (strings.next()) |s| {
                innerArgs[i] = .{s};
                i += 1;
            }
            break :blk innerArgs;
        };
        return Self{
            .inner = std.StaticStringMap(void).initComptime(kvs),
        };
    }

    pub fn contains(self: *const Self, key: []const u8) bool {
        return self.inner.get(key) != null;
    }

    pub fn getIndex(self: *const Self, key: []const u8) ?usize {
        return self.inner.getIndex(key);
    }

    pub fn keys(self: *const Self) []const []const u8 {
        return self.inner.keys();
    }

    pub inline fn len(self: *const Self) usize {
        return self.keys().len;
    }
};

pub fn SeparatedStringSet() type {
    return struct {
        inner: []const []const u8,
        sep: []const u8,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn initFromString(str: []const u8, sep: []const u8, alloc: std.mem.Allocator) !Self {
            var list = std.ArrayList([]const u8).init(alloc);
            errdefer list.deinit();
            var wordStart: usize = 0;
            while (wordStart < str.len) {
                if (str[wordStart] == '"') {
                    const closing = std.mem.indexOfScalarPos(u8, str, wordStart + 1, '"');
                    if (closing) |c| {
                        try list.append(str[wordStart + 1 .. c]);
                        wordStart = c + 1;
                    } else {
                        return error.unterminatedQuote;
                    }
                } else {
                    const nextSep = std.mem.indexOfPos(u8, str, wordStart, sep);
                    if (nextSep) |sepPos| {
                        try list.append(str[wordStart..sepPos]);
                        wordStart = sepPos + 1;
                    } else {
                        try list.append(str[wordStart..]);
                        break;
                    }
                }
            }
            return Self{
                .inner = try list.toOwnedSlice(),
                .sep = sep,
                .allocator = alloc,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.inner);
        }

        pub fn contains(self: *const Self, key: []const u8) bool {
            return self.getIndex(key) != null;
        }

        pub fn getIndex(self: *const Self, key: []const u8) ?usize {
            for (self.inner, 0..) |k, i| {
                if (std.mem.eql(u8, k, key)) {
                    return i;
                }
            }
            return null;
        }

        pub inline fn len(self: *const Self) usize {
            return self.inner.len;
        }

        pub fn keys(self: *const Self) std.mem.SplitIterator(u8, .sequence) {
            return std.mem.split(u8, self.inner, self.sep);
        }
    };
}

test "SeparatedStringSet" {
    var set = try SeparatedStringSet().initFromString("foo,bar,baz,\"owo,awa\",", ",", std.testing.allocator);
    defer set.deinit();
    const str = try std.mem.join(std.testing.allocator, "|", set.inner);
    defer std.testing.allocator.free(str);

    // std.debug.print("[{s}]\n", .{str});

    try std.testing.expectEqualDeep(&[_][]const u8{ "foo", "bar", "baz", "owo,awa", "" }, set.inner);
    try std.testing.expect(set.contains("foo"));
    try std.testing.expect(set.contains("bar"));
    try std.testing.expect(set.contains("baz"));
    try std.testing.expect(set.contains("owo,awa"));
    try std.testing.expect(set.contains(""));
    try std.testing.expect(!set.contains("qux"));
    try std.testing.expectEqual(5, set.len());
    try std.testing.expectEqual(0, set.getIndex("foo").?);
    try std.testing.expectEqual(1, set.getIndex("bar").?);
    try std.testing.expectEqual(2, set.getIndex("baz").?);
    try std.testing.expectEqual(3, set.getIndex("owo,awa").?);
    try std.testing.expectEqual(4, set.getIndex("").?);
    try std.testing.expectEqual(null, set.getIndex("qux"));
}

pub const StringCodec = struct {
    allocator: std.mem.Allocator,
    buffers: std.ArrayList(Buffer),

    pub const Len = u32;
    pub const LenSize = @sizeOf(Len);
    const Buffer = []const u8;

    pub const endianness = std.builtin.Endian.little;

    pub fn init(allocator: std.mem.Allocator) StringCodec {
        return .{
            .allocator = allocator,
            .buffers = std.ArrayList(Buffer).init(allocator),
        };
    }

    pub fn clearStrings(self: *StringCodec) void {
        for (self.buffers.items) |buffer| {
            self.allocator.free(buffer);
        }
        self.buffers.clearRetainingCapacity();
    }

    pub fn deinit(self: *StringCodec) void {
        for (self.buffers.items) |buffer| {
            self.allocator.free(buffer);
        }
        self.buffers.deinit();
    }

    pub fn encode(self: *StringCodec, s: []const u8) ![]const u8 {
        var enc = try Encoder.init(self);
        _ = try enc.write(s);
        return try enc.finish();
    }

    pub fn encodeJson(self: *StringCodec, value: anytype) ![]const u8 {
        var enc = try Encoder.init(self);
        try std.json.stringify(value, .{}, enc.writer());
        return try enc.finish();
    }

    pub fn writer(self: *StringCodec) !Encoder {
        return try Encoder.init(self);
    }

    pub const Encoder = struct {
        buffer: std.ArrayList(u8),
        codec: *StringCodec,

        const Self = @This();

        pub fn init(codec: *StringCodec) !Self {
            var arr = std.ArrayList(u8).init(codec.allocator);
            try arr.appendNTimes(0, LenSize);
            return Self{ .buffer = arr, .codec = codec };
        }

        pub fn finish(self: *Self) ![]const u8 {
            const ptr = try self.buffer.toOwnedSlice();
            try self.codec.buffers.append(ptr);
            return ptr;
        }

        pub fn finishOwned(self: *Self) ![]const u8 {
            return try self.buffer.toOwnedSlice();
        }

        pub fn writer(self: *const Self) std.io.AnyWriter {
            // context: *const anyopaque,
            // writeFn: *const fn (context: *const anyopaque, bytes: []const u8) anyerror!usize,
            return std.io.AnyWriter{
                .writeFn = &writeFn,
                .context = self,
            };
        }

        pub fn write(self: *const Self, s: []const u8) !usize {
            return writeFn(self, s);
        }

        fn writeFn(rawContext: *const anyopaque, s: []const u8) !usize {
            const self: *Self = @ptrCast(@alignCast(@constCast(rawContext)));
            try self.buffer.appendSlice(s);
            std.mem.writeInt(Len, self.buffer.items[0..LenSize], @intCast(self.buffer.items.len - LenSize), endianness);
            return s.len;
        }
    };

    pub fn decode(self: StringCodec, s: [*]const u8) []const u8 {
        _ = self;
        const length = std.mem.readInt(Len, s[0..LenSize], endianness);
        return s[LenSize .. length + LenSize];
    }
};

test "StringCodec" {
    var codec = StringCodec.init(std.testing.allocator);
    defer codec.deinit();

    try std.testing.expectEqualSlices(u8, "\x05\x00\x00\x00hello", try codec.encode("hello"));
    try std.testing.expectEqualSlices(u8, "hello", codec.decode("\x05\x00\x00\x00hello".ptr));
    var writer = try codec.writer();
    try writer.writer().print("foo={}", .{true});
    try writer.writer().writeByte(',');
    try writer.writer().print("bar={}", .{false});
    const expected = "foo=true,bar=false";
    try std.testing.expectEqualSlices(u8, &[_]u8{ expected.len, 0, 0, 0 } ++ expected, try writer.finish());
}
