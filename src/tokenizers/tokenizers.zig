const std = @import("std");
const testing = std.testing;
const utils = @import("../utils/utils.zig");

pub usingnamespace @import("general_purpose_tokenizer.zig");

pub fn IdentifiedInPlace(comptime T: type, comptime ID: type) type {
    return switch (@typeInfo(T)) {
        .Struct => |tpeInfo| {
            if (@hasField(T, "id")) {
                @compileError("Identified: type already has an 'id' field");
            }

            var fields: [tpeInfo.fields.len + 1]std.builtin.Type.StructField = undefined;
            fields[0] = std.builtin.Type.StructField{
                .name = "id",
                .type = ID,
                .default_value = null,
                .is_comptime = false,
                .alignment = 0,
            };
            inline for (tpeInfo.fields, 0..) |field, i| {
                fields[i + 1] = field;
            }

            return @Type(.{
                .Struct = std.builtin.Type.Struct{
                    .layout = tpeInfo.layout,
                    .fields = &fields,
                    .backing_integer = tpeInfo.backing_integer,
                    .decls = &[_]std.builtin.Type.Declaration{},
                    .is_tuple = tpeInfo.is_tuple,
                },
            });
        },
        .Union => {
            const tpeInfo = @typeInfo(T).Union;
            var fields: [tpeInfo.fields.len]std.builtin.Type.UnionField = undefined;
            inline for (tpeInfo.fields, 0..) |field, i| {
                fields[i] = .{
                    .name = field.name,
                    .type = IdentifiedInPlace(field.type, ID),
                    .alignment = field.alignment,
                };
            }

            return @Type(.{
                .Union = std.builtin.Type.Union{
                    .layout = tpeInfo.layout,
                    .fields = &fields,
                    .tag_type = tpeInfo.tag_type,
                    .decls = &[_]std.builtin.Type.Declaration{},
                },
            });
        },
        inline else => {
            @compileError("Identified: expected struct or union");
        },
    };
}

test "IdentifiedInPlace init" {
    const Foo = struct {
        x: i32,
        y: i32,
    };

    const Bar = IdentifiedInPlace(Foo, usize);

    const bar = Bar{ .id = 42, .x = 1, .y = 2 };
    try testing.expectEqual(bar.id, 42);
    try testing.expectEqual(bar.x, 1);
    try testing.expectEqual(bar.y, 2);
}

pub fn IdentifiedWrapping(comptime V: type, comptime ID: type) type {
    return struct {
        id: ID,
        value: V,

        const Self = @This();

        pub fn init(value: V, id: ID) Self {
            return Self{
                .id = id,
                .value = value,
            };
        }

        pub fn jsonStringify(self: *const Self, jw: anytype) !void {
            if (std.meta.hasMethod(V, "jsonStringify")) {
                return @field(V, "jsonStringify")(&self.value, jw);
            } else if (std.meta.hasMethod(V, "jsonStringifyWrapped")) {
                return @field(V, "jsonStringifyWrapped")(self, jw);
            } else {
                return jw.write(self.value);
            }
        }

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            if (std.meta.hasMethod(V, "format")) {
                return @field(V, "format")(self.value, fmt, options, writer);
            } else if (std.meta.hasMethod(V, "formatWrapped")) {
                return @field(V, "formatWrapped")(self, fmt, options, writer);
            } else {
                return writer.print("{}", .{self.value});
            }
        }
    };
}

test "IdentifiedWrapping init" {
    const Foo = struct {
        x: i32,
        y: i32,
    };

    const Bar = IdentifiedWrapping(Foo, usize);

    const bar = Bar.init(Foo{ .x = 1, .y = 2 }, 42);
    try testing.expectEqual(bar.id, 42);
    try testing.expectEqual(bar.value.x, 1);
    try testing.expectEqual(bar.value.y, 2);
}

inline fn InitParamType(comptime T: type) type {
    if (@hasDecl(T, "init")) {
        switch (@typeInfo(@TypeOf(@field(T, "init")))) {
            .Fn => |f| if (f.params.len == 1) {
                if (f.params[0].type) |t| {
                    return t;
                } else {
                    @compileError("InitParamType: expected struct.init to take 1 argument");
                }
            } else {
                @compileError("InitParamType: expected struct.init to take 1 argument");
            },
            inline else => @compileError("InitParamType: expected struct with init decl"),
        }
    } else {
        @compileError("argType: expected struct with init decl");
    }
}

inline fn NextReturnType(comptime T: type) type {
    if (@hasDecl(T, "next")) {
        switch (@typeInfo(@TypeOf(@field(T, "next")))) {
            .Fn => |f| if (f.return_type) |t| {
                return t;
            } else {
                @compileError("NextReturnType: expected struct with next decl");
            },
            inline else => @compileError("NextReturnType: expected struct with next decl"),
        }
    } else {
        @compileError("NextReturnType: expected struct with next decl");
    }
}

pub fn IdentifiedGenerator(comptime T: type, comptime Context: type) type {
    // ensure context has a generator function
    const Id = NextReturnType(Context);
    const Args = InitParamType(Context);

    var found = false;

    const nextFnName = "next";

    inline for (std.meta.declarations(Context)) |field| {
        if (std.mem.eql(u8, field.name, nextFnName)) {
            switch (@typeInfo(@TypeOf(@field(Context, field.name)))) {
                .Fn => |fnType| {
                    if (!std.meta.eql(fnType.return_type, Id)) {
                        @compileError(std.fmt.comptimePrint("{s}: 'next' decl must return {s} but returns {s}", .{ @typeName(Context), @typeName(Id), @typeName(fnType.return_type) }));
                    }

                    if (fnType.params.len != 2) {
                        @compileError(std.fmt.comptimePrint("{s}: 'next' decl must take 2 arguments", .{@typeName(Context)}));
                    }

                    if (!std.meta.eql(fnType.params[0].type, *Context)) {
                        @compileError(std.fmt.comptimePrint("{s}: 'next' decl first argument must be of type {s}", .{ @typeName(Context), @typeName(*Context) }));
                    }

                    if (fnType.params[1].type != null and !std.meta.eql(fnType.params[1].type, T)) {
                        @compileError(std.fmt.comptimePrint("{s}: 'next' decl second argument must be of type {s} (or anytype) but is {s}", .{ @typeName(Context), @typeName(T), @typeName(fnType.params[1].type) }));
                    }

                    found = true;
                },
                else => {
                    @compileError(std.fmt.comptimePrint("{s}: 'next' decl must be a function", .{@typeName(Context)}));
                },
            }
        }
    }
    if (!found) {
        @compileError(std.fmt.comptimePrint("{s}: context is missing a 'next' decl", .{@typeName(Context)}));
    }
    return struct {
        const Self = @This();

        context: Context,

        pub fn init(args: Args) Self {
            if (@hasDecl(Context, "init")) {
                const ctxInit = @as(fn (Args) Context, @field(Context, "init"));
                return Self{
                    .context = ctxInit(args),
                };
            } else {
                return Self{
                    .context = .{},
                };
            }
        }

        pub fn next(self: *Self, element: T) IdentifiedWrapping(T, Id) {
            const generator = @field(Context, nextFnName);
            const res = @as(Id, generator(&self.context, element));
            return IdentifiedWrapping(T, Id).init(element, res);
        }

        pub fn deinit(self: Self) void {
            if (@hasDecl(Context, "deinit")) {
                const ctxDeinit = @as(fn (Context) void, @field(Context, "deinit"));
                ctxDeinit(self.context);
            }
        }
    };
}

pub fn IncrementingIdGenerator(comptime T: type) type {
    const Incrementer = struct {
        val: usize,

        const Self = @This();

        pub fn init(args: usize) Self {
            return Self{
                .val = args,
            };
        }

        pub fn next(context: *Self, _: anytype) usize {
            const res = context.val;
            context.val += 1;
            return res;
        }
    };

    return IdentifiedGenerator(
        T,
        Incrementer,
    );
}

test "IncrementingIdGenerator" {
    const Foo = struct {
        x: i32,
        y: i32,
    };

    var gen = IncrementingIdGenerator(Foo).init(0);
    defer gen.deinit();

    const a = gen.next(Foo{ .x = 1, .y = 2 });
    try testing.expectEqual(a.id, 0);
    try testing.expectEqual(a.value.x, 1);
    try testing.expectEqual(a.value.y, 2);

    const b = gen.next(Foo{ .x = 3, .y = 4 });
    try testing.expectEqual(b.id, 1);
    try testing.expectEqual(b.value.x, 3);
    try testing.expectEqual(b.value.y, 4);
}

pub const IdentifyingMethod = enum {
    InPlace,
    Wrapping,
};

pub fn IdentifiedByMethod(comptime T: type, comptime Id: type, comptime method: IdentifyingMethod) type {
    return switch (method) {
        .InPlace => IdentifiedInPlace(T, Id),
        .Wrapping => IdentifiedWrapping(T, Id),
    };
}

pub fn PointerStore(comptime T: type, comptime method: IdentifyingMethod) type {
    const ID = usize;
    const Inner = IdentifiedByMethod(T, ID, method);
    const CreateResult = switch (method) {
        .InPlace => *const Inner,
        .Wrapping => *const T,
    };

    return struct {
        const Self = @This();

        store: std.ArrayList(*const Inner),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{
                .store = std.ArrayList(*const Inner).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn getId(_: *const Self, item: CreateResult) ID {
            return switch (method) {
                inline .InPlace => item.id,
                inline .Wrapping => @as(*const IdentifiedWrapping(T, ID), @fieldParentPtr("value", item)).id,
            };
        }

        pub fn create(self: *Self, element: T) *const Inner {
            const p = self.allocator.create(Inner) catch unreachable;
            const id = self.store.items.len;
            p.* = IdentifiedWrapping(T, ID).init(element, id);
            self.store.append(p) catch unreachable;
            return p;
        }

        pub fn get(self: *const Self, id: ID) ?*const Inner {
            if (id >= self.store.items.len) {
                return null;
            }
            const item = self.store.items[id];
            return item;
        }

        pub fn deinit(self: Self) void {
            for (self.store.items) |ptr| {
                self.allocator.destroy(ptr);
            }
            self.store.deinit();
        }
    };
}

pub fn ArrayListGenerator(comptime T: type) type {
    return IdentifiedGenerator(T, struct {
        const Self = @This();
        const Store = PointerStore(T, .InPlace);

        store: Store,

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{
                .store = Store.init(allocator),
            };
        }

        pub fn next(self: *Self, element: T) usize {
            return self.store.create(element).id;
        }

        pub fn deinit(self: Self) void {
            self.store.deinit();
        }
    });
}
// test "ArrayListGenerator" {
//     const Foo = struct {
//         x: i32,
//         y: i32,
//     };

//     var gen = ArrayListGenerator(Foo).init(std.testing.allocator);
//     defer gen.deinit();

//     const a = gen.next(Foo{ .x = 1, .y = 2 });
//     try testing.expectEqual(a.id, 0);
//     try testing.expectEqual(a.value.x, 1);
//     try testing.expectEqual(a.value.y, 2);

//     const b = gen.next(Foo{ .x = 3, .y = 4 });
//     try testing.expectEqual(b.id, 1);
//     try testing.expectEqual(b.value.x, 3);
//     try testing.expectEqual(b.value.y, 4);
// }

pub fn FixedSizeArray(comptime T: type, comptime N: usize) type {
    return struct {
        const Self = @This();
        const Array = [N]T;

        array: Array,
        count: usize,

        pub const FixedSizeArrayError = error{
            OutOfBounds,
        };

        pub fn init() Self {
            return Self{
                .array = undefined,
                .count = 0,
            };
        }

        pub fn append(self: *Self, value: T) FixedSizeArrayError!void {
            // check if we have space
            if (self.count >= N) {
                @panic("FixedSizeArray: out of space");
            }
            self.array[self.count] = value;
            self.count += 1;
        }

        pub fn appendSlice(self: *Self, slice: []const T) void {
            for (slice) |value| {
                self.append(value);
            }
        }

        pub fn toSlice(self: *Self) []const T {
            return self.array[0..self.count];
        }
    };
}

pub fn fieldNames(comptime T: type) [std.meta.fields(T).len][]const u8 {
    const fields = std.meta.fields(T);
    var array: [fields.len][]const u8 = undefined;
    for (&array, fields) |*a, field| a.* = field.name;
    return array;
}

pub fn firstCharOfEach(comptime array: []const []const u8) [array.len]u8 {
    var res: [array.len]u8 = undefined;
    for (&res, array) |*c, s| c.* = if (s.len > 0) s[0] else @compileError("field name is empty");
    return res;
}

pub fn fieldCharNames(comptime T: type) []const u8 {
    const fields = std.meta.fields(T);
    var array: [fields.len]u8 = undefined;
    for (&array, fields) |*a, field| {
        if (field.name.len != 1) {
            @compileError(@typeName(T) ++ "." ++ field.name ++ " should be a single character");
        }
        a.* = field.name[0];
    }
    return &array;
}

pub fn fieldOfName(comptime T: type, comptime name: []const u8) ?switch (@typeInfo(T)) {
    .Struct => []const std.builtin.Type.StructField,
    .Union => []const std.builtin.Type.UnionField,
    .ErrorSet => []const std.builtin.Type.Error,
    .Enum => []const std.builtin.Type.EnumField,
    else => @compileError("Expected struct, union, error set or enum type, found '" ++ @typeName(T) ++ "'"),
} {
    const fields = std.meta.fields(T);
    for (fields) |field| {
        if (std.mem.eql(u8, field.name, name)) {
            return field;
        }
    }
    return null;
}

pub fn StructFieldTracker(comptime T: type) type {
    const tpe_info = @typeInfo(T);
    const fields = tpe_info.Struct.fields;
    var arr: [fields.len]struct { []const u8, comptime_int } = undefined;
    inline for (fields, &arr, 0..) |f, *a, i| {
        a.* = .{ f.name, i };
    }
    const map = std.StaticStringMap(comptime_int).initComptime(arr);
    return struct {
        missing: Set,
        hasDefault: Set,
        inner: T,

        const Self = @This();
        const Set = std.bit_set.ArrayBitSet(usize, std.meta.fields(T).len);
        const names = fieldNames(T);

        fn fieldIndex(comptime field_name: []const u8) comptime_int {
            return map.get(field_name) orelse utils.debugCompileError("field '" ++ field_name ++ "' not found in " ++ @typeName(T));
        }

        pub fn initValue(value: anytype) Self {
            var self = Self{
                .missing = Set.initFull(),
                .hasDefault = Set.initEmpty(),
                .inner = std.mem.zeroInit(T, value),
            };
            inline for (fields, 0..) |field, i| {
                if (field.default_value) |_| {
                    self.hasDefault.set(i);
                }
            }
            return self;
        }

        pub fn zeroes() Self {
            return initValue(.{});
        }

        pub fn set(self: *Self, comptime field_name: []const u8, value: anytype) void {
            @field(self.inner, field_name) = value;
            const index = fieldIndex(field_name);
            self.missing.unset(index);
        }

        pub fn setArray(self: *Self, comptime field_name: []const u8, value: anytype) void {
            @memcpy(@field(self.inner, field_name)[0..value.len], value);
            const index = fieldIndex(field_name);
            self.missing.unset(index);
        }

        pub const FieldGetError = error{FieldNotSet};

        pub fn tryGet(self: *Self, comptime field_name: []const u8) FieldGetError!@TypeOf(@field(self.inner, field_name)) {
            if (self.isMissing(field_name)) {
                return FieldGetError.FieldNotSet;
            }
            return @field(self.inner, field_name);
        }

        pub fn get(self: *Self, comptime field_name: []const u8) @TypeOf(@field(self.inner, field_name)) {
            return @field(self.inner, field_name);
        }

        pub fn isSet(self: *Self, comptime field_name: []const u8) bool {
            return !self.isMissing(field_name);
        }

        pub fn isMissing(self: *Self, comptime field_name: []const u8) bool {
            const index = fieldIndex(field_name);
            return self.missing.isSet(index);
        }

        pub fn firstMissing(self: *Self) ?usize {
            return self.missing.findFirstSet();
        }

        pub const FieldNotSetError = struct {
            field_name: []const u8,
        };
        const R = utils.Result(T, FieldNotSetError);

        pub fn checkAllSetOrSetDefault(self: *Self) R {
            var iter = self.missing.iterator(.{ .kind = .set });
            while (iter.next()) |i| {
                if (self.hasDefault.isSet(i)) {
                    // this is ok
                    continue;
                } else {
                    return R.err(.{ .field_name = names[i] });
                }
            }

            return R.success(self.inner);
        }
    };
}

test "StructFieldTracker set get isSet firstUnset" {
    comptime {
        const Foo = struct {
            x: i32,
            y: i32,
        };

        var tracker = StructFieldTracker(Foo).initValue(Foo{ .x = 1, .y = 2 });

        tracker.set("x", 42);
        try testing.expectEqual(42, tracker.get("x"));
        try testing.expect(tracker.isSet("x"));

        tracker.set("y", 43);
        try testing.expectEqual(43, tracker.get("y"));
        try testing.expect(tracker.isSet("y"));

        try testing.expectEqual(null, tracker.firstMissing());
    }
}

const Foo2 = struct {
    x: u8,
    y: u16 = 42,
};
test "StructFieldTracker checkAllSetOrSetDefault" {
    var tracker = StructFieldTracker(Foo2).zeroes();

    tracker.set("x", 12);
    try testing.expectEqual(12, tracker.get("x"));
    try testing.expect(tracker.isSet("x"));
    try testing.expect(tracker.isMissing("y"));
    try testing.expectEqual(42, tracker.get("y"));

    switch (tracker.checkAllSetOrSetDefault()) {
        .Success => |s| {
            try testing.expectEqual(12, tracker.get("x"));
            try testing.expectEqual(42, tracker.get("y"));
            try testing.expectEqual(12, s.x);
            try testing.expectEqual(42, s.y);
        },
        .Error => return error.expectedSuccess,
    }
}

pub fn SliceIterator(comptime T: type) type {
    return struct {
        slice: []const T,
        index: usize,

        const Self = @This();

        pub fn init(slice: []const T) Self {
            return Self{
                .slice = slice,
                .index = 0,
            };
        }

        pub fn next(self: *Self) ?T {
            if (self.index >= self.slice.len) {
                return null;
            }
            const res = self.slice[self.index];
            self.index += 1;
            return res;
        }
    };
}

pub const FlagIterator = struct {
    source: []const u8,
    long: bool,

    const Self = @This();

    pub const Error = error{
        flagTooShort,
        notAFlag,
    };

    pub fn init(source: []const u8) Error!Self {
        if (source.len < 1) {
            return error.flagTooShort;
        }
        if (source[0] != '-') {
            return error.notAFlag;
        }
        if (source.len < 2) {
            return error.flagTooShort;
        }
        const long = source[1] == '-';
        return Self{
            .source = if (long) source[2..] else source[1..],
            .long = long,
        };
    }

    pub const KV = struct {
        key: []const u8,
        value: ?[]const u8,
    };

    pub fn next(self: *Self) ?KV {
        if (self.source.len == 0) {
            return null;
        }
        if (self.long) {
            if (std.mem.indexOf(u8, self.source, "=")) |i| {
                const key = self.source[0..i];
                const value = self.source[i + 1 ..];
                self.source = self.source[self.source.len..];
                return .{ .key = key, .value = value };
            } else {
                const key = self.source[0..];
                self.source = self.source[self.source.len..];
                return .{ .key = key, .value = null };
            }
        } else {
            const key = self.source[0..1];
            if (self.source.len > 1 and self.source[1] == '=') {
                const value = self.source[2..];
                self.source = self.source[self.source.len..];
                return .{ .key = key, .value = value };
            } else {
                self.source = self.source[1..];
                return .{ .key = key, .value = null };
            }
        }
    }
};

test "FlagIterator '--foo=bar'" {
    const source = "--foo=bar";
    var iter = FlagIterator.init(source) catch unreachable;

    const a = iter.next().?;
    try testing.expectEqualStrings("foo", a.key);
    try testing.expectEqualStrings("bar", a.value.?);

    const b = iter.next();
    try testing.expectEqual(null, b);
}

test "FlagIterator '--foo'" {
    const source = "--foo";
    var iter = FlagIterator.init(source) catch unreachable;

    const a = iter.next().?;
    try testing.expectEqualStrings("foo", a.key);
    try testing.expectEqual(null, a.value);

    const b = iter.next();
    try testing.expectEqual(null, b);
}

test "FlagIterator '-f'" {
    const source = "-f";
    var iter = FlagIterator.init(source) catch unreachable;

    const a = iter.next().?;
    try testing.expectEqualStrings("f", a.key);
    try testing.expectEqual(null, a.value);

    const b = iter.next();
    try testing.expectEqual(null, b);
}

test "FlagIterator '-f=bar'" {
    const source = "-f=bar";
    var iter = FlagIterator.init(source) catch unreachable;

    const a = iter.next().?;
    try testing.expectEqualStrings("f", a.key);
    try testing.expectEqualStrings("bar", a.value.?);

    const b = iter.next();
    try testing.expectEqual(null, b);
}

test "FlagIterator '-rvf'" {
    const source = "-rvf";
    var iter = FlagIterator.init(source) catch unreachable;

    const a = iter.next().?;
    try testing.expectEqualStrings("r", a.key);
    try testing.expectEqual(null, a.value);

    const b = iter.next().?;
    try testing.expectEqualStrings("v", b.key);
    try testing.expectEqual(null, b.value);

    const c = iter.next().?;
    try testing.expectEqualStrings("f", c.key);
    try testing.expectEqual(null, c.value);

    const d = iter.next();
    try testing.expectEqual(null, d);
}

fn TypePair(comptime T: type, comptime T2: type) type {
    return struct {
        value: T,
        type: T2,
    };
}
pub fn BetterType(comptime T: type) type {
    return union(enum) {
        Int: TypePair(T, std.builtin.Type.Int),
        Float: TypePair(T, std.builtin.Type.Double),
        String: TypePair([]const u8, std.builtin.Type),
        Bool: TypePair(bool, std.builtin.Type.Bool),
        Struct: TypePair(T, std.builtin.Type.Struct),
        Union: TypePair(T, std.builtin.Type.Union),
        Enum: TypePair(T, std.builtin.Type.Enum),
        Else: TypePair(T, std.builtin.Type),
    };
}

pub fn betterTypeInfo(comptime arg: anytype) BetterType(@TypeOf(arg)) {
    const tpe_info = @typeInfo(@TypeOf(arg));
    return switch (tpe_info) {
        .Int => |i| .{ .Int = .{ .value = arg, .type = i } },
        .Double => |f| .{ .Double = .{ .value = arg, .type = f } },
        .Pointer => |p| {
            const child = p.child;
            switch (@typeInfo(child)) {
                .Struct => |s| .{ .Struct = .{ .value = arg, .type = s } },
                .Union => |u| .{ .Union = .{ .value = arg, .type = u } },
                .Enum => |e| .{ .Enum = .{ .value = arg, .type = e } },
                else => .{ .Else = .{ .value = arg, .type = child } },
            }
        },
        .Array => |a| {
            const child = a.child;
            if (child == u8) {
                return .{ .String = .{ .value = @as([]const u8, arg), .type = a } };
            } else {
                return .{ .Else = .{ .value = arg, .type = a } };
            }
        },
        .Bool => |b| .{ .Bool = .{ .value = @as(bool, arg), .type = b } },
        .Struct => |s| .{ .Struct = .{ .value = arg, .type = s } },
        .Union => |u| .{ .Union = .{ .value = arg, .type = u } },
        .Enum => |e| .{ .Enum = .{ .value = arg, .type = e } },
        else => .{ .Else = .{ .value = arg, .type = tpe_info } },
    };
}
