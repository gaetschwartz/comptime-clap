pub const ansi = @import("ansi.zig");
pub const testing = @import("testing.zig");
pub const fs = @import("fs.zig");
const std = @import("std");
const builtin = @import("builtin");

pub const typing = @import("typing.zig");

// This is essentially @compileError, but the message is only use in debug mode.
// In release mode, the message is replaced with a generic message.
// This allows for more informative error messages in debug mode, while hopefully
// improving the compile time in release mode by reducing the number of unique
// error messages.
pub inline fn debugCompileError(comptime msg: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        @compileError(msg);
    } else {
        @compileError("Compile error");
    }
}

pub fn prefixed_log(comptime prefix: []const u8) type {
    return struct {
        const scope = std.log.default_log_scope;
        const log = std.log.scoped(scope);
        /// Log an error message. This log level is intended to be used
        /// when something has gone wrong. This might be recoverable or might
        /// be followed by the program exiting.
        pub fn err(
            comptime format: []const u8,
            args: anytype,
        ) void {
            @setCold(true);
            log.err(prefix ++ format, args);
        }

        /// Log a warning message. This log level is intended to be used if
        /// it is uncertain whether something has gone wrong or not, but the
        /// circumstances would be worth investigating.
        pub fn warn(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log.warn(prefix ++ format, args);
        }

        /// Log an info message. This log level is intended to be used for
        /// general messages about the state of the program.
        pub fn info(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log.info(prefix ++ format, args);
        }

        /// Log a debug message. This log level is intended to be used for
        /// messages which are only useful for debugging.
        pub fn debug(
            comptime format: []const u8,
            args: anytype,
        ) void {
            log.debug(prefix ++ format, args);
        }
    };
}

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

pub fn maxFieldLength(comptime T: type) usize {
    var max = 0;
    inline for (std.meta.fields(T)) |field| {
        const len = field.name.len;
        if (len > max) {
            max = len;
        }
    }
    return max;
}
