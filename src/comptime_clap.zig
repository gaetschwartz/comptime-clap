const std = @import("std");
const testing = std.testing;
const tks = @import("tokenizers/tokenizers.zig");
const OneTokenTokenizer = @import("tokenizers/one_token.zig");
const utils = @import("utils/utils.zig");
const ansi = @import("utils/ansi.zig");
const Result = utils.Result;
pub const ArgumentIterator = tks.ArgumentIterator;
const config = @import("config.zig");

const ErrorType = union(enum) {
    ExpectedValue: struct { option: []const u8 },
    RequiredOptionNotSet: struct { option: []const u8 },
    FailedToParseValue: struct { option: []const u8, value: []const u8, err: ValueParsingError },
    UnexpectedFlag: struct { value: []const u8 },
    ExtraneousPositional: struct { value: []const u8 },
    Parsing: struct { value: []const u8, err: IngesterError },
};
const IngesterResult = Result(void, ErrorType);
const IngesterError = error{
    expected_value,
} || ValueParsingError || tks.ArgumentIterator.Error;

const CommandPath = []const u8;

pub const ShortFlags = union(enum) {
    auto: void,
    disabled: void,
    using: *const ShortFlagFn,
    map: ShortFlagMap,

    pub const ShortFlagMap = std.StaticStringMap(u8);
    pub const ShortFlagFn = fn ([]const u8) ?u8;

    pub fn get(self: ShortFlags, long: []const u8) ?u8 {
        return switch (self) {
            .auto => long[0],
            .disabled => null,
            .using => |f| f(long),
            .map => |m| m.get(long),
        };
    }
};

const ErrorFormatterFn = fn (ErrorType, anytype) void;
pub const ArgParserOptions = struct {
    short_flags: ShortFlags = .auto,
    commandFieldKey: []const u8 = "command",
    error_formatter: ErrorFormatterFn = formatError,
    on_error: fn (ErrorType, ErrorFormatterFn) noreturn = onError,
    allow_extraneous_positionals: bool = false,
};

fn formatError(err: ErrorType, writer: anytype) void {
    (switch (err) {
        .ExpectedValue => |e| writer.print("Expected value for option: {s}\n", .{e.option}),
        .RequiredOptionNotSet => |e| writer.print("Required option not set: {s}\n", .{e.option}),
        .FailedToParseValue => |e| writer.print("Failed to parse value for option {s}: \"{s}\" ({s})\n", .{ e.option, e.value, @errorName(e.err) }),
        .UnexpectedFlag => |e| writer.print("Unknown option: \"{s}\"\n", .{e.value}),
        .Parsing => |e| writer.print("Error {s} while parsing \"{s}\"\n", .{ @errorName(e.err), e.value }),
        .ExtraneousPositional => |e| writer.print("Extraneous positional argument: \"{s}\"\n", .{e.value}),
    }) catch @panic("Failed to format error");
}

fn onError(err: ErrorType, formatterFn: ErrorFormatterFn) noreturn {
    const writer = std.io.getStdErr().writer();

    writer.print(ansi.RED ++ "Error: " ++ ansi.RESET, .{}) catch @panic("Failed to print error");
    formatterFn(err, writer);
    writer.print(ansi.RESET ++ "\n", .{}) catch @panic("Failed to print error");
    std.process.exit(1);
}

pub fn ArgParser(comptime Args: type) type {
    return ArgParserWithOpts(Args, .{});
}
pub fn ArgParserWithOpts(comptime Args: type, comptime options: ArgParserOptions) type {
    return CommandParserInternal(Args, options, &[_]u8{});
}
fn CommandParserInternal(comptime A: type, comptime opts: ArgParserOptions, comptime path: CommandPath) type {
    const tpe_info = @typeInfo(A);
    const commandFieldKey = opts.commandFieldKey;

    switch (tpe_info) {
        .Struct => {},
        else => @compileError("Invalid type, expected a struct but got a " ++ @tagName(tpe_info)),
    }

    return struct {
        const Self = @This();

        const log = tks.prefixed_log("[ArgParser:" ++ @typeName(A) ++ "] ");

        pub const options = opts;
        pub const Args = A;

        pub const Parsed = struct {
            parsed: A,
            argv: std.process.ArgIterator,

            pub fn deinit(self: *Parsed) void {
                self.argv.deinit();
            }
        };

        pub const ParsingResult = Result(Parsed, ErrorType);

        pub const ParseOptions = struct {
            allocator: std.mem.Allocator = std.heap.page_allocator,
        };

        pub fn parse(parseOptions: ParseOptions) !Parsed {
            const res = try Self.parseFull(parseOptions);
            return switch (res) {
                .Success => |s| s,
                .Error => |e| opts.on_error(e, opts.error_formatter),
            };
        }

        pub fn parseFull(parseOptions: ParseOptions) !ParsingResult {
            const alloc = parseOptions.allocator;
            var argv = try std.process.argsWithAllocator(alloc);
            // defer argv.deinit();
            _ = argv.next();
            var list = std.ArrayList([]const u8).init(alloc);
            defer list.deinit();
            while (argv.next()) |arg| {
                try list.append(arg);
            }
            log.debug("Parsing arguments: {s}", .{list.items});
            var iter = ArgumentIterator.init(list.items);

            const res = Self.parseFrom(&iter) catch |err| {
                std.log.err("Unable to parse arguments: {s}\n", .{@errorName(err)});
                std.process.exit(1);
            };
            return switch (res) {
                .Error => |e| .{ .Error = e },
                .Success => |r| .{ .Success = .{ .parsed = r, .argv = argv } },
            };
        }

        pub fn parseFrom(iter: *ArgumentIterator) !Result(A, ErrorType) {
            var args = tks.StructFieldTracker(A).zeroes();

            log.debug("Parsing arguments with map {s}", .{argsMap.keys()});

            var positional: usize = 0;
            const positionals = comptime computePositionals(A);

            while (try iter.next()) |arg| {
                log.debug("Parsing argument: {s}", .{arg});
                if (argsMap.get(arg.value)) |res| {
                    const result = res.ingester(iter, &args, arg.value) catch |e|
                        return .{ .Error = .{ .Parsing = .{ .value = arg.value, .err = e } } };
                    switch (result) {
                        .Success => {},
                        .Error => |e| return .{ .Error = e },
                    }
                } else {
                    if (arg.value[0] == '-') {
                        return .{ .Error = .{ .UnexpectedFlag = .{ .value = arg.value } } };
                    } else {
                        log.debug("Parsing positional argument: {s} ({d}/{d})", .{ arg.value, 1 + positional, positionals.len });
                        if (!opts.allow_extraneous_positionals and positional >= positionals.len) {
                            return .{ .Error = .{ .ExtraneousPositional = .{ .value = arg.value } } };
                        }
                        inline for (positionals, 0..) |p, i| {
                            if (i == positional) {
                                args.set(p, arg.value);
                            }
                        }
                        positional += 1;
                    }
                }
            }

            return .{ .Success = args.inner };
        }

        pub fn unwrapWithError(res: ParsingResult) Parsed {
            return res.unwrapWithError(opts.on_error);
        }

        fn print(writer: std.fs.File.Writer, comptime fmt: []const u8, args: anytype) void {
            nosuspend writer.print(fmt, args) catch return;
        }

        pub fn printUsage() !void {
            const alloc = config.default_allocator;
            std.debug.lockStdErr();
            defer std.debug.unlockStdErr();
            const stderr = std.io.getStdErr().writer();
            var iter = try std.process.argsWithAllocator(alloc);
            defer iter.deinit();
            const exe = iter.next().?;
            print(stderr, "Usage: {s} [options]\n", .{std.fs.path.basename(exe)});
            print(stderr, "Options:\n", .{});
            inline for (tpe_info.Struct.fields) |field| {
                if (MakeOptionConfig.ofField(field)) |o| {
                    print(stderr, "  ", .{});
                    if (o.required) {
                        print(stderr, "!", .{});
                    }
                    if (o.long) |l| {
                        print(stderr, "--{s}", .{l});
                        if (o.short) |_| {
                            print(stderr, ", ", .{});
                        }
                    }
                    if (o.short) |s| {
                        print(stderr, "-{c}", .{s});
                    }
                    if (o.description) |d| {
                        print(stderr, " [{s}]", .{d});
                    }
                    if (o.default) |d| {
                        print(stderr, " (default: {s})", .{d});
                    }
                    print(stderr, "\n", .{});
                }
            }
        }

        fn accumulate(comptime T: type) ?fn (T, ?T) T {
            switch (@typeInfo(T)) {
                .Int, .Float => return struct {
                    pub fn accumulate(a: T, b: ?T) T {
                        return a + (if (b) |v| v else 1);
                    }
                }.accumulate,
                .Bool => return struct {
                    pub fn accumulate(a: T, b: ?T) T {
                        return a or (if (b) |v| v else true);
                    }
                }.accumulate,

                else => return null,
            }
        }

        const IngesterFn = fn (iter: *tks.ArgumentIterator, args: *tks.StructFieldTracker(A), arg: []const u8) IngesterError!IngesterResult;

        const ArgsMapValue = struct { ingester: *const IngesterFn };
        const ArgsMap = std.StaticStringMap(*const ArgsMapValue);
        const ArgsMapInitKV = struct { []const u8, *const ArgsMapValue };

        fn Ingester(comptime field: std.builtin.Type.StructField) type {
            return struct {
                pub const T = field.type;

                pub fn miam(iter: *tks.ArgumentIterator, out: *tks.StructFieldTracker(A), arg: []const u8) IngesterError!IngesterResult {
                    if (utils.AsSliceOfOpt(T, u8)) |asString| {
                        const next = if (try iter.next()) |n|
                            asString(n.value)
                        else
                            return IngesterError.expected_value;
                        std.log.debug("Setting {s} to {s}", .{ field.name, next });
                        out.set(field.name, next);
                        return .Success;
                    }
                    switch (@typeInfo(T)) {
                        .Struct => {
                            const parseFn = parseFnOf(T);
                            if (comptime MakeOptionConfig.maybeOf(T)) |o| {
                                log.debug("Found option: {s} at field {s}", .{ arg, field.name });
                                if (o.cumulative) {
                                    const isSet = out.isSet(field.name);
                                    const curr = switch (@typeInfo(ValueType(T))) {
                                        .Int => if (isSet)
                                            out.get(field.name).value
                                        else
                                            0,
                                        .Bool => if (isSet)
                                            out.get(field.name).value
                                        else
                                            false,
                                        else => @compileError("Unsupported type"),
                                    };

                                    if (accumulate(ValueType(T))) |acc| {
                                        const res = acc(curr, null);
                                        out.set(field.name, T{ .value = res });
                                    } else {
                                        @compileError("Type does not support accumulation");
                                    }
                                } else {
                                    const next = if (try iter.next()) |n|
                                        n.value
                                    else
                                        return IngesterError.expected_value;
                                    const value = try parseFn(next);

                                    out.set(field.name, T{ .value = value });

                                    log.debug("Set option {s} to {s}", .{ o.name(), value });
                                }
                            } else {
                                @compileError("Invalid flag, it must either be a primitive type or a field of type Flag(...)");
                            }
                        },
                        .Float, .Int, .Bool => {
                            log.debug("Parsing primitive type: {s}", .{arg});
                            const v = try PrimitiveHandler(T).handle(iter);
                            out.set(field.name, v);
                        },
                        .Optional => |o| {
                            switch (@typeInfo(o.child)) {
                                .Union => |u| {
                                    const Handler = CommandHandler(field, o.child, u);
                                    const res = try Handler.miam(iter, out, field.name);
                                    return res;
                                },
                                .Double, .Int, .Bool => {
                                    const v = try PrimitiveHandler(o.child).handle(iter);
                                    out.set(field.name, v);
                                },
                                else => @compileError("Type '" ++ @typeName(o.child) ++ "' is not supported yet"),
                            }
                        },
                        .Union => |u| {
                            const Handler = CommandHandler(field, T, u);
                            const res = try Handler.miam(iter, out, field.name);
                            return res;
                        },
                        else => @compileError("Type '" ++ @typeName(T) ++ "' is not supported yet"),
                    }
                    return .Success;
                }
            };
        }

        const argsMap = blk: {
            var array: [argsMapSize(A)]ArgsMapInitKV = undefined;
            var i = 0;

            for (tpe_info.Struct.fields) |field| {
                if (std.mem.eql(u8, commandFieldKey, field.name)) {
                    if (asOptionalUnion(field.type)) |u| {
                        for (u.@"union".fields) |f| {
                            const value = &ArgsMapValue{ .ingester = &CommandHandler(u, f.name).miam };

                            array[i] = .{ f.name, value };
                            i += 1;
                        }
                        continue;
                    }
                }
                if (utils.AsSliceOfOpt(field.type, u8)) |_| {
                    array[i] = .{ field.name, &ArgsMapValue{ .ingester = &Ingester(field).miam } };
                    i += 1;
                    continue;
                }

                switch (@typeInfo(field.type)) {
                    .Struct => {
                        const maybeOpts = MakeOptionConfig.maybeOf(field.type);
                        const value = &ArgsMapValue{ .ingester = &Ingester(field).miam };

                        if (maybeOpts) |o| {
                            if (o.long) |long| {
                                array[i] = .{ long, value };
                                i += 1;
                            }
                            if (o.short) |short| {
                                array[i] = .{ &[_]u8{short}, value };
                                i += 1;
                            }
                        }
                    },
                    .Float, .Int, .Bool => {
                        const value = &ArgsMapValue{ .ingester = &Ingester(field).miam };
                        array[i] = .{ field.name, value };
                        i += 1;
                        if (options.short_flags.get(field.name)) |short| {
                            array[i] = .{ &[_]u8{short}, value };
                            i += 1;
                        }
                    },

                    else => continue,
                }
            }

            break :blk ArgsMap.initComptime(array[0..i]);
        };

        fn CommandHandler(comptime U: OptionUnion, comptime command: []const u8) type {
            return struct {
                pub const T = U.tpe;

                const l = tks.prefixed_log(std.fmt.comptimePrint("[{s}:commands]", .{path}));
                pub fn miam(iter: *tks.ArgumentIterator, out: *tks.StructFieldTracker(A), _: []const u8) IngesterError!IngesterResult {
                    const Payload: type = std.meta.TagPayloadByName(T, command);

                    l.debug("CommandHandler: {s}", .{command});
                    const Parser = CommandParserInternal(Payload, opts, path ++ command);
                    const res = try Parser.parseFrom(iter);
                    switch (res) {
                        .Success => |r| {
                            out.set(commandFieldKey, @unionInit(T, command, r));
                            return .Success;
                        },
                        .Error => |e| return IngesterResult.err(e),
                    }
                }
            };
        }
    };
}

fn argsMapSize(comptime T: type) usize {
    var size = 0;
    const tpe_info = @typeInfo(T);
    for (tpe_info.Struct.fields) |field| {
        if (asOptionalUnion(field.type)) |u| {
            for (u.@"union".fields) |_| {
                size += 1;
            }
        } else {
            size += 2;
        }
    }
    return size;
}

pub fn PrimitiveHandler(comptime T: type) type {
    return struct {
        pub fn handle(iter: *tks.ArgumentIterator) IngesterError!T {
            const parser = defaultParse(T);

            switch (@typeInfo(T)) {
                .Int, .Float => {
                    if (try iter.peek()) |p| {
                        const val = try parser(p.value);
                        _ = try iter.next();
                        return val;
                    } else {
                        return IngesterError.expected_value;
                    }
                },
                .Bool => {
                    if (try iter.peek()) |p| {
                        std.log.debug("Parsing bool value: {s}", .{p.value});
                        if (p.value.tag != .Key) {
                            const val = try parser(p.consume());
                            return val;
                        } else {
                            return true;
                        }
                    } else {
                        return true;
                    }
                },
                else => @compileError("Unsupported type"),
            }
        }
    };
}

pub const ValueParsingError = error{
    invalid_int,
    invalid_float,
    invalid_bool,
    invalid_option_names,
};
fn ValueType(comptime T: type) type {
    inline for (std.meta.fields(T)) |f| {
        if (std.mem.eql(u8, f.name, "value")) {
            return f.type;
        }
    }
    @compileError("Type " ++ @typeName(T) ++ " does not have a field named 'value'");
}
fn ParseFn(comptime T: type) type {
    return fn ([]const u8) ValueParsingError!T;
}
fn TypeOfParseFnOf(comptime T: type) type {
    return ParseFn(ValueType(T));
}
fn parseFnOf(comptime T: type) TypeOfParseFnOf(T) {
    if (!@hasDecl(T, "parse")) {
        return defaultParse(@TypeOf(@field(T, "value")));
    }
    const parseFn = @field(T, "parse");
    if (@TypeOf(parseFn) != TypeOfParseFnOf(T)) {
        @compileError("Invalid parse function");
    }
    return @as(TypeOfParseFnOf(T), parseFn);
}
fn selfParseFn(comptime T: type) ?ParseFn(T) {
    switch (@typeInfo(T)) {
        .Struct, .Union, .Opaque, .Enum => {
            if (@hasDecl(T, "parse")) {
                const field = @field(T, "parse");
                if (@TypeOf(field) == ParseFn(T)) {
                    return @as(ParseFn(T), field);
                } else {
                    return null;
                }
            } else {
                return null;
            }
        },
        else => return null,
    }
}
fn defaultParse(comptime T: type) ParseFn(T) {
    switch (@typeInfo(T)) {
        .Int => return struct {
            pub fn parse(s: []const u8) !T {
                return std.fmt.parseInt(T, s, 10) catch
                    return error.invalid_int;
            }
        }.parse,
        .Float => return struct {
            pub fn parse(s: []const u8) !T {
                return std.fmt.parseFloat(s) catch
                    return error.invalid_float;
            }
        }.parse,
        .Pointer => |pi| {
            switch (pi.size) {
                .One => return defaultParse(pi.child),
                .Slice => if (pi.child == u8) {
                    return struct {
                        pub fn parse(s: []const u8) !T {
                            return s;
                        }
                    }.parse;
                } else @compileError("Unsupported type: " ++ @typeName(T)),
                else => @compileError("Unsupported type: " ++ @typeName(T)),
            }
        },
        .Bool => return parseBool,
        .Optional => |o| return defaultParse(o.child),
        else => @compileError("Type '" ++ @typeName(T) ++ "' doesn't have a default parser"),
    }
}
fn parseBool(s: []const u8) !bool {
    if (s.len == 0) return false;
    const BoolRepr = enum { @"0", @"1", false, true, f, t };
    return if (std.meta.stringToEnum(BoolRepr, s)) |p| switch (p) {
        .@"0", .false, .f => false,
        .@"1", .true, .t => true,
    } else error.invalid_bool;
}
test "ParseBool" {
    const parse = parseBool;
    try testing.expectEqual(false, parse("0"));
    try testing.expectEqual(true, parse("1"));
    try testing.expectEqual(false, parse("false"));
    try testing.expectEqual(true, parse("true"));
    try testing.expectEqual(false, parse("f"));
    try testing.expectEqual(true, parse("t"));
    try testing.expectError(ValueParsingError.invalid_bool, parse("2"));
}
fn MakeOption(comptime raw_options: anytype, comptime T: type) type {
    const parsed = blk: {
        if (@TypeOf(raw_options) == MakeOptionConfig) {
            break :blk @as(MakeOptionConfig, raw_options);
        }

        if (utils.asString(raw_options)) |s| {
            break :blk MakeOptionConfig.parse(s) catch |err| @compileError("Invalid options: " ++ @errorName(err));
        }
    };
    switch (@typeInfo(T)) {
        .Bool, .Int => {},
        else => if (parsed.cumulative) {
            @compileError("Only bool and int types can be cumulative");
        },
    }
    const parseFn: ParseFn(T) = if (selfParseFn(T)) |f| f else defaultParse(T);

    if (parsed.short == null and parsed.long == null) {
        @compileError("Either short or long must be provided");
    }

    return struct {
        value: T,

        pub fn parse(s: []const u8) ValueParsingError!T {
            return try parseFn(s);
        }

        pub const options = parsed;
    };
}

pub const FlagConfig = struct {
    name: []const u8,
    short: u8,
    description: []const u8,
};
pub inline fn Flag(comptime opts: FlagConfig) type {
    return MakeOption(
        MakeOptionConfig{
            .long = opts.name,
            .short = opts.short,
            .description = opts.description,
            .cumulative = true,
            .default = null,
        },
        bool,
    );
}

pub inline fn IntFlag(comptime opts: struct {
    name: ?[]const u8 = null,
    short: ?u8 = null,
    description: ?[]const u8 = null,
    default: usize = 0,
}) type {
    return MakeOption(
        MakeOptionConfig{
            .long = opts.name,
            .short = opts.short,
            .description = opts.description,
            .cumulative = true,
            .default = std.fmt.comptimePrint("{d}", .{opts.default}),
        },
        usize,
    );
}

pub inline fn HelpFlag(comptime opts: struct {
    name: []const u8 = "help",
    short: u8 = 'h',
    description: []const u8 = "Print help message",
}) type {
    return MakeOption(
        MakeOptionConfig{
            .long = opts.name,
            .short = opts.short,
            .description = opts.description,
            .cumulative = true,
            .default = std.fmt.comptimePrint("{}", .{false}),
        },
        bool,
    );
}

pub const OptionConfig = struct {
    name: ?[]const u8 = null,
    short: ?u8 = null,
    description: ?[]const u8 = null,
    default: ?[]const u8 = null,
};
pub inline fn Option(comptime T: type, comptime opts: OptionConfig) type {
    return MakeOption(
        MakeOptionConfig{
            .long = opts.name,
            .short = opts.short,
            .description = opts.description,
            .cumulative = false,
            .default = opts.default,
        },
        T,
    );
}

const MakeOptionConfig = struct {
    long: ?[]const u8,
    short: ?u8,
    description: ?[]const u8,
    cumulative: bool,
    default: ?[]const u8,
    required: bool = false,

    const Tokenizer = OneTokenTokenizer();

    pub fn name(self: MakeOptionConfig) []const u8 {
        return if (self.long) |l| l else &[_]u8{self.short.?};
    }

    pub fn parse(s: []const u8) !MakeOptionConfig {
        var tokenizer = Tokenizer.init(s);
        var out: MakeOptionConfig = .{
            .long = null,
            .short = null,
            .description = null,
            .cumulative = false,
            .default = null,
            .required = false,
        };

        // {h,help}[Print help message]+=false
        tokenizer.skipWhitespaces();

        switch (try tokenizer.peek()) {
            '!' => {
                _ = try tokenizer.next();
                out.required = true;
            },
            else => {},
        }

        switch (try tokenizer.next()) {
            '{' => {
                var names = try tokenizer.readUntil('}');

                if (std.mem.indexOf(u8, names, ",")) |comma| {
                    const s1 = names[0..comma];
                    const s2 = names[comma + 1 ..];
                    if (s1.len == 1) {
                        out.short = s1[0];
                        out.long = s2;
                    } else if (s2.len == 1) {
                        out.short = s2[0];
                        out.long = s1;
                    } else {
                        return error.invalid_option_names;
                    }
                }
            },
            else => return error.expected_open_brace,
        }

        switch (try tokenizer.peek()) {
            '[' => {
                _ = try tokenizer.next();
                out.description = try tokenizer.readUntil(']');
            },
            else => {},
        }

        const beforeEqual = tokenizer.readUntil('=') catch |err| switch (err) {
            Tokenizer.Error.no_matching_char => "",
            else => return err,
        };
        if (beforeEqual.len != 0) {
            for (beforeEqual) |c| {
                switch (c) {
                    '+' => out.cumulative = true,
                    else => return error.invalid_option_names,
                }
            }
        }
        tokenizer.skipWhitespaces();
        const res = try tokenizer.rest();
        if (res.len != 0) {
            out.default = res;
        }

        std.debug.assert(tokenizer.isEOF());

        return out;
    }

    pub const optsOptionsFieldName = "options";

    pub fn maybeOf(comptime T: type) ?MakeOptionConfig {
        switch (@typeInfo(T)) {
            .Struct, .Enum, .Union, .Opaque => {
                if (@hasDecl(T, optsOptionsFieldName)) {
                    const field = @field(T, optsOptionsFieldName);
                    if (@TypeOf(field) == MakeOptionConfig) {
                        return @as(MakeOptionConfig, field);
                    }
                }
            },
            else => {},
        }
        return null;
    }

    pub fn ofField(comptime f: std.builtin.Type.StructField) ?MakeOptionConfig {
        switch (@typeInfo(f.type)) {
            .Struct, .Enum, .Union, .Opaque => maybeOf(f.type),
            .Float, .Int, .Bool => {
                return MakeOptionConfig{
                    .long = f.name,
                    .short = f.name[0],
                    .description = null,
                    .cumulative = false,
                    .default = std.fmt.comptimePrint("{any}", .{if (f.default_value) |d| @as(*const f.type, @ptrCast(@alignCast(d))).* else null}),
                };
            },
            else => {},
        }
        return null;
    }
};

const o1 = "{h,help}[Print help message]+=false";
const op1 = MakeOptionConfig{
    .long = "help",
    .short = 'h',
    .description = "Print help message",
    .cumulative = true,
    .default = "false",
};

fn computePositionals(comptime T: type) []const []const u8 {
    return comptime blk: {
        var fieldsTmp: [@typeInfo(T).Struct.fields.len][]const u8 = undefined;
        for (&fieldsTmp, @typeInfo(T).Struct.fields) |*f, field| {
            f.* = field.name;
        }

        // @compileLog("Fields: {s}", .{fields});
        std.mem.sortUnstable([]const u8, &fieldsTmp, {}, structFieldLessThan);
        // @compileLog("Fields 2: {s}", .{fields});

        var pos_start: ?usize = null;
        var current_pos: usize = 0;
        var buffer: [std.math.log10_int(fieldsTmp.len) + 1]u8 = undefined;

        const fields = fieldsTmp;

        for (fields, 0..) |field, i| {
            const str = std.fmt.bufPrint(&buffer, "{}", .{current_pos}) catch @compileError(std.fmt.comptimePrint("Failed to print at index {}", .{i}));

            if (std.mem.eql(u8, field, str)) {
                // @compileLog("Found positional argument: {s}", .{field});
                if (pos_start == null) {
                    pos_start = i;
                    // @compileLog("Setting pos_start to {d}", .{i});
                }
                if (pos_start) |start| {
                    if (i - start != current_pos) {
                        @compileError("Positional arguments must be contiguous");
                    }
                }
                // check that (i - pos_start) == current_pos
                // @compileLog("Incrementing current_pos from {d} to {d}", .{ current_pos, current_pos + 1 });
                current_pos += 1;
            } else {
                break;
            }
        }

        if (pos_start) |start| {
            break :blk fields[start .. start + current_pos];
        } else {
            break :blk &.{};
        }
    };
}

fn structFieldLessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.ascii.lessThanIgnoreCase(lhs, rhs);
}

test "CommandOptions simple" {
    try testing.expectEqualDeep(MakeOptionConfig.parse(o1), op1);
}

test "Opts" {
    const o = MakeOption(o1, bool){ .value = false };

    try std.testing.expect(@hasDecl(@TypeOf(o), MakeOptionConfig.optsOptionsFieldName));
    try std.testing.expectEqualDeep(@field(@TypeOf(o), MakeOptionConfig.optsOptionsFieldName), op1);
}

test "ArgParser" {
    const Args = struct {
        showHelp: MakeOption("{h,help}[Print help message]+=false", bool),
        myUrl: MakeOption("{u,url}[URL to fetch data from]", []const u8),
        verbosity: MakeOption("{v,verbose}[Increase verbosity level]+=0", u32),
    };

    var iter = tks.ArgumentIterator.init(&[_][]const u8{ "-h", "-u", "https://example.com", "-v", "-v", "-v" });
    const result = try ArgParser(Args).parseFrom(&iter);
    const res = result.Success;

    try testing.expectEqual(true, res.showHelp.value);
    try testing.expectEqualStrings("https://example.com", res.myUrl.value);
    try testing.expectEqual(3, res.verbosity.value);
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

pub fn MultiArgsMap(comptime V: type) type {
    return struct {
        short: std.StaticStringMap(V),
        long: std.StaticStringMap(V),
        positionals: []const V,

        const Self = @This();

        pub fn initComptime(comptime shorts: anytype, comptime longs: anytype, comptime positionals: []const V) Self {
            const finalShorts = comptime blk: {
                var skvs: [shorts.len]struct { []const u8, V } = undefined;
                for (shorts, &skvs) |s, *kv| {
                    kv.* =
                        switch (@TypeOf(s.@"0")) {
                        u8, comptime_int => .{ &[_]u8{s.@"0"}, s.@"1" },
                        else => @compileError("Invalid type, expected u8 but got " ++ @typeName(@TypeOf(s.@"0"))),
                    };
                }
                break :blk skvs;
            };

            return .{
                .short = std.StaticStringMap(V).initComptime(finalShorts),
                .long = std.StaticStringMap(V).initComptime(longs),
                .positionals = positionals,
            };
        }

        pub fn getShort(self: Self, key: u8) ?V {
            return self.short.get(&[_]u8{key});
        }

        pub fn getLong(self: Self, key: []const u8) ?V {
            return self.long.get(key);
        }

        pub fn getPositional(self: Self, index: usize) ?V {
            if (index >= self.positionals.len) {
                return null;
            } else {
                return self.positionals[index];
            }
        }
    };
}

test MultiArgsMap {
    const m = MultiArgsMap(u32).initComptime(
        .{
            .{ 'v', 1 },
            .{ 'h', 2 },
        },
        .{
            .{ "verbose", 3 },
            .{ "help", 4 },
        },
        &[_]u32{ 42, 69 },
    );

    try testing.expectEqual(1, m.getShort('v').?);
    try testing.expectEqual(2, m.getShort('h').?);
    try testing.expectEqual(3, m.getLong("verbose").?);
    try testing.expectEqual(4, m.getLong("help").?);
    try testing.expectEqual(42, m.getPositional(0).?);
    try testing.expectEqual(69, m.getPositional(1).?);
    try testing.expectEqual(null, m.getShort('x'));
    try testing.expectEqual(null, m.getLong("unknown"));
    try testing.expectEqual(null, m.getPositional(2));
}
