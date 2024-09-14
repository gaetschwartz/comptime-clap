const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const tks = @import("src/tokenizers/tokenizers.zig");
const OneTokenTokenizer = @import("src/tokenizers/one_token.zig");
const utils = @import("src/utils/utils.zig");
const ansi = @import("src/utils/ansi.zig");
const Result = utils.Result;
const argsType = @import("src/tokenizers/args.zig");
pub const ArgumentIterator = argsType.ArgumentIterator(.{});
const ArgumentIteratorArg = argsType.ArgumentIteratorArg;
const config = @import("src/config.zig");
const plog = utils.prefixed_log;

const ErrorType = union(enum) {
    ExpectedValue: struct { option: []const u8 },
    RequiredOptionNotSet: struct { option: []const u8 },
    FailedToParseValue: struct { option: []const u8, value: []const u8, err: ValueParsingError },
    UnexpectedFlag: struct { value: ArgumentIteratorArg },
    ExtraneousPositional: struct { value: ArgumentIteratorArg },
    Parsing: struct { value: ArgumentIteratorArg, err: IngesterError },

    pub fn format(v: @This(), comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = opts;
        _ = fmt;
        switch (v) {
            .ExpectedValue => |e| try writer.print("ExpectedValue{{option: {s}}}", .{e.option}),
            .RequiredOptionNotSet => |e| try writer.print("RequiredOptionNotSet{{option: {s}}}", .{e.option}),
            .FailedToParseValue => |e| try writer.print("FailedToParseValue{{option: {s}, value: {s}, err: {s}}}", .{ e.option, e.value, @errorName(e.err) }),
            .UnexpectedFlag => |e| try writer.print("UnexpectedFlag{{value: {}}}", .{e.value.displayQuoted()}),
            .ExtraneousPositional => |e| try writer.print("ExtraneousPositional{{value: {}}}", .{e.value.displayQuoted()}),
            .Parsing => |e| try writer.print("Parsing{{value: {}, err: {s}}}", .{ e.value.displayQuoted(), @errorName(e.err) }),
        }
    }
};

const IngesterResult = Result(void, ErrorType);
const IngesterError = error{
    expected_value,
    buffer_too_small,
} || ValueParsingError || ArgumentIterator.Error;

const CommandPath = []const u8;

pub const ShortFlags = union(enum) {
    auto: void,
    disabled: void,
    using: *const ShortFlagFn,
    charmap: ShortFlagMap,

    pub const ShortFlagMap = std.StaticStringMap(u8);
    pub const ShortFlagFn = fn ([]const u8) ?u8;

    pub fn map(args: anytype) @This() {
        var arr: [@typeInfo(@TypeOf(args)).Struct.fields.len]struct { []const u8, u8 } = undefined;
        var i = 0;
        inline for (@typeInfo(@TypeOf(args)).Struct.fields) |field| {
            switch (field.type) {
                comptime_int => {
                    arr[i] = .{ field.name, @field(args, field.name) };
                    i += 1;
                },
                else => @compileError("Invalid type for short flag map: " ++ @typeName(field.type)),
            }
        }
        return .{ .charmap = ShortFlagMap.initComptime(arr[0..i]) };
    }

    pub fn get(self: ShortFlags, long: []const u8) ?u8 {
        return switch (self) {
            .auto => long[0],
            .disabled => null,
            .using => |f| f(long),
            .charmap => |m| m.get(long),
        };
    }
};

const ErrorFormatterFn = fn (ErrorType, anytype) void;
pub const ArgParserOptions = struct {
    short_flags: ShortFlags = .disabled,
    commandFieldKey: []const u8 = "command",
    error_formatter: ErrorFormatterFn = formatError,
    on_error: fn (ErrorType, ErrorFormatterFn) noreturn = onError,
    allow_extraneous_positionals: bool = false,
    auto_help: bool = true,
};

fn formatError(err: ErrorType, writer: anytype) void {
    (switch (err) {
        .ExpectedValue => |e| writer.print("Expected value for option: {s}\n", .{e.option}),
        .RequiredOptionNotSet => |e| writer.print("Required option not set: {s}\n", .{e.option}),
        .FailedToParseValue => |e| writer.print("Failed to parse value for option {s}: \"{s}\" ({s})\n", .{ e.option, e.value, @errorName(e.err) }),
        .UnexpectedFlag => |e| writer.print("Unknown option: {}\n", .{e.value.displayQuoted()}),
        .ExtraneousPositional => |e| writer.print("Extraneous positional argument: \"{}\"\n", .{e.value.displayQuoted()}),
        .Parsing => |e| writer.print("Error {s} while parsing {}\n", .{ @errorName(e.err), e.value.displayQuoted() }),
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

        const log = plog("[ArgParser:" ++ @typeName(A) ++ "] ");

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
            var iter = ArgumentIterator.init(alloc, list.items);

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
                const key = arg.asStr();
                log.debug("Looking for option '{s}' in map", .{key});
                if (argsMap.get(key)) |res| {
                    log.debug("Found option: {s}", .{key});
                    const result = res.ingester(iter, &args, arg) catch |e|
                        return .{ .Error = .{ .Parsing = .{ .value = arg, .err = e } } };
                    switch (result) {
                        .Success => {},
                        .Error => |e| return .{ .Error = e },
                    }
                } else {
                    if (opts.auto_help) {
                        switch (arg) {
                            .long => |l| if (std.mem.eql(u8, l, "help")) {
                                try Self.printUsage();
                                std.process.exit(0);
                            },
                            .short => |s| if (s == 'h') {
                                try Self.printUsage();
                                std.process.exit(0);
                            },
                            else => {},
                        }
                    }
                    log.debug("Parsing positional argument: {s} ({d}/{d})", .{ key, 1 + positional, positionals.len });
                    if (!opts.allow_extraneous_positionals and positional >= positionals.len) {
                        return .{ .Error = .{ .ExtraneousPositional = .{ .value = arg } } };
                    }
                    inline for (positionals, 0..) |p, i| {
                        if (i == positional) {
                            args.set(p, arg.asStr());
                        }
                    }
                    positional += 1;
                }
            }

            log.debug("Current args: {}", .{args.inner});
            return switch (args.checkAllSetOrSetDefault()) {
                .Success => |r| .{ .Success = r },
                .Error => |e| .{ .Error = .{ .RequiredOptionNotSet = .{ .option = e.field_name } } },
            };
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
            print(stderr, "Usage: {s}", .{std.fs.path.basename(exe)});
            var split = std.mem.splitScalar(u8, path, '.');
            while (split.next()) |c| {
                print(stderr, " {s}", .{c});
            }
            print(stderr, " [options]\n", .{});
            print(stderr, "Options:\n", .{});
            inline for (tpe_info.Struct.fields) |field| {
                if (MakeOptionConfig.ofField(field, opts)) |o| {
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
            if (opts.auto_help) {
                print(stderr, "  --help, -h\tPrint this help message\n", .{});
            }
            var hasPrintedCommands = false;
            inline for (tpe_info.Struct.fields) |field| {
                if (std.mem.eql(u8, field.name, commandFieldKey)) {
                    if (utils.typing.isUnion(field.type)) |u| {
                        if (!hasPrintedCommands) {
                            print(stderr, "Commands:\n", .{});
                            hasPrintedCommands = true;
                        }
                        inline for (u.@"union".fields) |f| {
                            print(stderr, "  {s}\n", .{f.name});
                        }
                    }
                }
            }
        }

        const IngesterFn = fn (iter: *ArgumentIterator, args: *tks.StructFieldTracker(A), arg: ArgumentIterator.Arg) IngesterError!IngesterResult;
        const ArgsMapValue = struct { ingester: *const IngesterFn, field_index: usize };
        const ArgsMapInitKV = struct { []const u8, *const ArgsMapValue };

        const argsMap: std.StaticStringMap(*const ArgsMapValue) = blk: {
            var array: [maxArgsMapSize(A)]ArgsMapInitKV = undefined;
            var cnt = 0;

            for (tpe_info.Struct.fields, 0..) |field, j| {
                if (std.mem.eql(u8, field.name, commandFieldKey)) {
                    if (utils.typing.isUnion(field.type)) |u| {
                        for (u.@"union".fields) |f| {
                            const value = &ArgsMapValue{
                                .ingester = &CommandHandler(u, f.name).miam,
                                .field_index = 0,
                            };

                            array[cnt] = .{ f.name, value };
                            cnt += 1;
                        }
                        continue;
                    } else {
                        @compileError("Field " ++ @typeName(A) ++ "." ++ field.name ++ " is not a union");
                    }
                }
                const value = &ArgsMapValue{
                    .ingester = &Ingester(A, field).miam,
                    .field_index = j,
                };

                const typing = utils.typing.getTyping(field.type);
                const fieldPath = if (path.len == 0) field.name else path ++ "." ++ field.name;
                switch (typing.type) {
                    .Literal => {
                        array[cnt] = .{ field.name, value };
                        cnt += 1;
                        if (options.short_flags.get(fieldPath)) |short| {
                            array[cnt] = .{ &[_]u8{short}, value };
                            cnt += 1;
                        }
                    },
                    .Complex => {
                        if (MakeOptionConfig.maybeOf(field.type)) |o| {
                            if (o.long) |long| {
                                array[cnt] = .{ long, value };
                                cnt += 1;
                            }
                            if (o.short) |short| {
                                array[cnt] = .{ &[_]u8{short}, value };
                                cnt += 1;
                            }
                        } else {
                            if (options.short_flags.get(fieldPath)) |short| {
                                array[cnt] = .{ &[_]u8{short}, value };
                                cnt += 1;
                            }
                            array[cnt] = .{ field.name, value };
                            cnt += 1;
                        }
                    },
                    .Unsupported => {
                        @compileError("Unsupported type for field " ++ @typeName(field.type) ++ " in " ++ @typeName(A));
                    },
                }
            }

            break :blk std.StaticStringMap(*const ArgsMapValue).initComptime(array[0..cnt]);
        };

        fn CommandHandler(comptime un: utils.typing.OptionUnion, comptime command: []const u8) type {
            const T = un.tpe;

            return struct {
                const l = plog(std.fmt.comptimePrint("[{s}:commands]", .{path}));
                pub fn miam(iter: *ArgumentIterator, out: *tks.StructFieldTracker(A), _: ArgumentIterator.Arg) IngesterError!IngesterResult {
                    const Payload: type = std.meta.TagPayloadByName(T, command);

                    l.debug("CommandHandler: {s}", .{command});
                    const Parser = CommandParserInternal(Payload, opts, if (path.len == 0) command else path ++ "." ++ command);
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

fn Ingester(comptime A: type, comptime field: std.builtin.Type.StructField) type {
    const T = field.type;

    return struct {
        pub fn miam(iter: *ArgumentIterator, out: *tks.StructFieldTracker(A), arg: ArgumentIterator.Arg) IngesterError!IngesterResult {
            const l = plog(std.fmt.comptimePrint("[Ingester:{s}]", .{field.name}));

            switch (comptime utils.typing.getTyping(T).type) {
                .Literal => |lit| {
                    l.debug("Parsing primitive type: {s}", .{arg});

                    switch (lit) {
                        .Int, .Float => {
                            const parser = defaultParse(T) orelse @compileError("Unsupported type for field " ++ @typeName(T) ++ " in " ++ @typeName(A));
                            const v = switch (arg) {
                                .kv => |kv| try parser(kv.value),
                                else => try parser((try iter.next() orelse return error.expected_value).asStr()),
                            };
                            l.debug("Set field {s} to {any}", .{ field.name, v });
                            out.set(field.name, v);
                        },
                        .Bool => {
                            const parser = defaultParse(T) orelse @compileError("Unsupported type for field " ++ @typeName(T) ++ " in " ++ @typeName(A));
                            const v = switch (arg) {
                                .kv => |kv| try parser(kv.value),
                                else => true,
                            };
                            l.debug("Set field {s} to {any}", .{ field.name, v });
                            out.set(field.name, v);
                        },
                        .Slice => {
                            const v = switch (arg) {
                                .kv => |kv| kv.value,
                                else => if (try iter.next()) |n| n.asStr() else return IngesterError.expected_value,
                            };
                            l.debug("Set field {s} to {any}", .{ field.name, v });
                            out.set(field.name, v);
                        },
                        .Array => |info| {
                            const v = switch (arg) {
                                .kv => |kv| kv.value,
                                else => if (try iter.next()) |n| n.asStr() else return IngesterError.expected_value,
                            };
                            if (v.len > info.size) {
                                return IngesterError.buffer_too_small;
                            }
                            l.debug("Set field {s} to {any}", .{ field.name, v });
                            out.setArray(field.name, v);
                        },
                    }
                },
                .Complex => {
                    const parseFn = parseFnOf(T);
                    const o = comptime MakeOptionConfig.maybeOf(T) orelse MakeOptionConfig.default;
                    l.debug("Found option: {} at field {s}", .{ arg, field.name });
                    const next: []const u8 = switch (arg) {
                        .kv => |kv| kv.value,
                        else => blk: {
                            l.debug("Expecting value for option {s}", .{arg});
                            if (try iter.next()) |n| {
                                l.debug("Got value: {s}", .{n.asStr()});
                                break :blk n.asStr();
                            } else {
                                return IngesterError.expected_value;
                            }
                        },
                    };
                    const value = try parseFn(next);

                    out.set(field.name, value);

                    l.debug("Set option {s} to {}", .{ o.name(), value });
                },
                .Unsupported => @compileError("Unsupported type"),
            }
            return .Success;
        }
    };
}

fn accumulate(comptime A: type, comptime field_name: []const u8, comptime T: type, s: *tks.StructFieldTracker(A)) void {
    switch (@typeInfo(T)) {
        .Int => if (s.isSet(field_name)) {
            s.set(field_name, s.get(field_name) + 1);
        } else {
            s.set(field_name, 1);
        },
        .Bool => s.set(field_name, true),

        else => @compileError("accumulate: unsupported type " ++ @typeName(T)),
    }
}

fn maxArgsMapSize(comptime T: type) usize {
    var size = 0;
    for (@typeInfo(T).Struct.fields) |field| {
        if (utils.typing.isUnion(field.type)) |u| {
            size += u.@"union".fields.len;
        } else {
            size += 2;
        }
    }
    return size;
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
    return fn (value: []const u8) ValueParsingError!T;
}

fn parseFnOf(comptime T: type) ParseFn(T) {
    if (@hasDecl(T, "parse")) {
        const parseFn = @field(T, "parse");
        if (@TypeOf(parseFn) != ParseFn(T)) {
            @compileError("Invalid parse function, expected a function with signature : \n`" ++ @typeName(ParseFn(T)) ++ "` but got \n`" ++ @typeName(@TypeOf(parseFn)) ++ "`");
        }
        return @as(ParseFn(T), parseFn);
    }
    @compileError("Type " ++ @typeName(T) ++ " does not have a parse function");
}

fn canBeCustomData(comptime T: type) type {
    switch (@typeInfo(T)) {
        .Struct, .Union, .Opaque, .Enum => {
            if (@hasDecl(T, "parse")) {
                const field = @field(T, "parse");
                if (@TypeOf(field) != ParseFn(T)) {
                    @compileError("Invalid parse function, expected a function with signature `" ++ @typeName(ParseFn(T)) ++ "`");
                }
            } else {
                @compileError("Type " ++ @typeName(T) ++ " does not have a parse function");
            }

            var found = false;
            inline for (std.meta.fields(T)) |f| {
                if (std.mem.eql(u8, f.name, "value")) {
                    if (@typeInfo(f.type) == .Optional) {
                        found = true;
                        break;
                    } else {
                        @compileError("Field 'value' must be of type 'Option' but got '" ++ @typeName(f.type) ++ "'");
                    }
                }
            }

            if (!found) {
                @compileError("Type " ++ @typeName(T) ++ " does not have a field named 'value'");
            }

            return T;
        },
        else => @compileError(@typeName(T) ++ " is not supported"),
    }
}
fn defaultParse(comptime T: type) ?ParseFn(T) {
    return switch (@typeInfo(T)) {
        .Int => struct {
            pub fn parse(s: []const u8) !T {
                return std.fmt.parseInt(T, s, 10) catch
                    return error.invalid_int;
            }
        }.parse,
        .Float => struct {
            pub fn parse(s: []const u8) !T {
                return std.fmt.parseFloat(T, s) catch
                    return error.invalid_float;
            }
        }.parse,
        .Bool => struct {
            fn parse(s: []const u8) !bool {
                if (s.len == 0) return false;
                const BoolRepr = enum { @"0", @"1", false, true, f, t };
                return if (std.meta.stringToEnum(BoolRepr, s)) |p| switch (p) {
                    .@"0", .false, .f => false,
                    .@"1", .true, .t => true,
                } else error.invalid_bool;
            }
        }.parse,
        .Optional => |o| defaultParse(o.child),
        else => null,
    };
}

test "ParseBool" {
    const parse = defaultParse(bool) orelse @compileError("No parse function for bool");
    try testing.expectEqual(false, parse("0"));
    try testing.expectEqual(true, parse("1"));
    try testing.expectEqual(false, parse("false"));
    try testing.expectEqual(true, parse("true"));
    try testing.expectEqual(false, parse("f"));
    try testing.expectEqual(true, parse("t"));
    try testing.expectError(ValueParsingError.invalid_bool, parse("2"));
}
pub fn MakeOption(comptime raw_options: anytype, comptime T: type) type {
    const parsedOptions = blk: {
        if (@TypeOf(raw_options) == MakeOptionConfig) {
            break :blk @as(MakeOptionConfig, raw_options);
        }

        if (utils.typing.asString(raw_options)) |s| {
            break :blk MakeOptionConfig.parse(s) catch |err| @compileError("Invalid options: " ++ @errorName(err));
        }

        @compileError("Invalid options type, expected a " ++ @typeName(MakeOptionConfig) ++ " or a string");
    };
    if (parsedOptions.cumulative and @typeInfo(T) != .Int and @typeInfo(T) != .Bool) {
        @compileError("Only bool and int types can be cumulative");
    }
    if (parsedOptions.short == null and parsedOptions.long == null) {
        @compileError("Either short or long must be provided");
    }

    if (defaultParse(T)) |p| {
        return struct {
            value: T,

            pub fn parse(s: []const u8) ValueParsingError!@This() {
                return .{ .value = try p(s) };
            }

            pub fn format(v: @This(), comptime fmt: []const u8, opts: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
                if (std.meta.hasMethod(T, "format")) {
                    return v.value.format(fmt, opts, writer);
                } else {
                    // @compileLog(std.fmt.comptimePrint("No format method for type {s}, fmt: {s}", .{ @typeName(@This()), fmt }));
                    return writer.print("{}", .{std.json.fmt(v.value, .{})});
                }
            }

            pub const options = parsedOptions;
        };
    } else {
        return canBeCustomData(T);
    }
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

    const Tokenizer = OneTokenTokenizer;

    pub const default = MakeOptionConfig{
        .long = null,
        .short = null,
        .description = null,
        .cumulative = false,
        .default = null,
    };

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
        if (@hasDecl(T, optsOptionsFieldName)) {
            const field = @field(T, optsOptionsFieldName);
            if (@TypeOf(field) == MakeOptionConfig) {
                return @as(MakeOptionConfig, field);
            }
        }
        return null;
    }

    pub fn ofField(comptime f: std.builtin.Type.StructField, comptime opts: ArgParserOptions) ?MakeOptionConfig {
        const typing = utils.typing.getTyping(f.type);
        return switch (typing.type) {
            .Complex => maybeOf(f.type),
            .Literal => MakeOptionConfig{
                .long = f.name,
                .short = opts.short_flags.get(f.name),
                .description = null,
                .cumulative = false,
                .default = if (f.default_value) |d| std.fmt.comptimePrint(typing.fmt(), .{@as(*const f.type, @ptrCast(@alignCast(d))).*}) else null,
            },
            .Unsupported => null,
        };
    }
};

fn isComplexType(comptime T: type) bool {
    return utils.typing.getTyping(T).type == .Complex;
}
fn isLiteralType(comptime T: type) bool {
    return utils.typing.getTyping(T).type == .Literal;
}

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
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var iter = ArgumentIterator.init(
        arena.allocator(),
        &.{ "-h", "-u", "https://example.com", "-v", "-v", "-v" },
    );
    const Parser = ArgParser(Args);
    const result = try Parser.parseFrom(&iter);
    if (result == .Error) {
        std.debug.print("Error: {}\n", .{result.Error});
        return error.UnexpectedError;
    }
    const res = result.Success;

    try testing.expectEqual(true, res.showHelp.value);
    try testing.expectEqualStrings("https://example.com", res.myUrl.value);
    try testing.expectEqual(3, res.verbosity.value);
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
