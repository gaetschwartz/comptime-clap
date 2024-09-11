const std = @import("std");
const gmml = @import("../../gmml.zig");
const ast = gmml.ast;
const lexer = gmml.Lexer;
const compiler = gmml.compiler;
const tks = @import("tokenizers.zig");

source: []const u8,
index: usize,

const Self = @This();

pub const Error = error{ no_matching_char, end_of_file };

pub fn init(src: []const u8) Self {
    return .{
        .source = src,
        .index = 0,
    };
}

pub fn next(self: *Self) !u8 {
    if (self.index >= self.source.len) {
        return Error.end_of_file;
    }
    const c = self.source[self.index];
    self.index += 1;
    return c;
}

pub fn nextOpt(self: *Self) ?u8 {
    self.next() catch return null;
}

pub fn take(self: *Self, n: usize) ![]const u8 {
    const start = self.index;
    self.index += n;
    return self.source[start..@min(self.source.len, self.index)];
}

pub fn readUntil(self: *Self, c: u8) ![]const u8 {
    const start = self.index;
    var i = self.index;
    while (i < self.source.len) {
        if (self.source[i] == c) {
            self.index = i + 1;
            return self.source[start..i];
        }
        i += 1;
    }
    return Error.no_matching_char;
}

pub fn rest(self: *Self) ![]const u8 {
    if (self.index > self.source.len) {
        return Error.end_of_file;
    }
    const start = self.index;
    self.index = self.source.len;
    return self.source[start..];
}

pub fn peek(self: *Self) !u8 {
    if (self.index >= self.source.len) {
        return Error.end_of_file;
    }
    return self.source[self.index];
}

pub fn peekOpt(self: *Self) ?u8 {
    self.peek() catch return null;
}

pub fn peekN(self: *Self, n: usize) ![]const u8 {
    if (self.index + n >= self.source.len) {
        return null;
    }
    return self.source[self.index .. self.index + n];
}

pub fn peekUntil(self: *Self, c: u8) ![]const u8 {
    const start = self.index;
    var i = self.index;
    while (i < self.source.len) {
        if (self.source[i] == c) {
            return self.source[start..i];
        }
        i += 1;
    }
    return self.source[start..];
}

pub fn skipWhitespaces(self: *Self) void {
    while (self.index < self.source.len) {
        if (self.source[self.index] == ' ') {
            self.index += 1;
        } else {
            break;
        }
    }
}

pub fn isEOF(self: *Self) bool {
    return self.index >= self.source.len;
}

test "OneTokenizer simple" {
    const T = Self();
    var tokenizer = T.init("{uwu}=:owo(awa)");
    try std.testing.expectEqualStrings("{", try tokenizer.take(1));
    try std.testing.expectEqualStrings("uwu", try tokenizer.peekUntil('}'));
    try std.testing.expectEqualStrings("uwu", try tokenizer.readUntil('}'));
    try std.testing.expectEqualStrings("=", try tokenizer.take(1));
    try std.testing.expectEqualStrings(":owo", try tokenizer.readUntil('('));
    try std.testing.expectEqualStrings("awa", try tokenizer.readUntil(')'));
    try std.testing.expect(tokenizer.isEOF());
}

test "OneTokenizer errors" {
    const T = Self();
    var tokenizer = T.init("abcdef");
    try std.testing.expectError(error.no_matching_char, tokenizer.readUntil('='));
    try std.testing.expectEqualStrings("abcdef", try tokenizer.rest());
    try std.testing.expectError(T.Error.end_of_file, tokenizer.next());
    try std.testing.expectError(T.Error.end_of_file, tokenizer.peek());
}

pub const CharMatcher = struct {
    const Self2 = @This();

    match: fn (token: u8) bool,

    fn isNumber(token: u8) bool {
        return token >= '0' and token <= '9';
    }

    fn isLetter(token: u8) bool {
        return (token >= 'a' and token <= 'z') or (token >= 'A' and token <= 'Z');
    }

    fn isWhitespace(token: u8) bool {
        return token == ' ' or token == '\t' or token == '\n';
    }

    fn _or(comptime matchers: []const Self2) Self2 {
        return .{
            .match = struct {
                pub fn match(token: u8) bool {
                    inline for (matchers) |m| {
                        if (m.match(token)) {
                            return true;
                        }
                    }
                    return false;
                }
            }.match,
        };
    }

    fn _and(comptime matchers: []const Self2) Self2 {
        return .{
            .match = struct {
                pub fn match(token: u8) bool {
                    inline for (matchers) |m| {
                        if (!m.match(token)) {
                            return false;
                        }
                    }
                    return true;
                }
            }.match,
        };
    }

    fn _not(comptime matcher: Self2) Self2 {
        return .{
            .match = struct {
                pub fn match(token: u8) bool {
                    return !matcher.match(token);
                }
            }.match,
        };
    }
    pub const Number = .{ .match = isNumber };
    pub const Letter = .{ .match = isLetter };
    pub const Whitespace = .{ .match = isWhitespace };

    pub fn Range(comptime start: u8, comptime end: u8) Self2 {
        return .{
            .match = struct {
                pub fn match(token: u8) bool {
                    return switch (token) {
                        start...end => true,
                        else => false,
                    };
                }
            }.match,
        };
    }

    pub fn @"or"(matchers: []const Self2) Self2 {
        return _or(matchers);
    }

    pub fn @"and"(matchers: []const Self2) Self2 {
        return _and(matchers);
    }

    pub fn not(matcher: Self2) Self2 {
        return _not(matcher);
    }
};

test "CharMatcher" {
    const matcher = CharMatcher.@"or"(&.{ CharMatcher.Number, CharMatcher.Letter });
    try std.testing.expect(matcher.match('a'));
    try std.testing.expect(matcher.match('1'));
    try std.testing.expect(!matcher.match(' '));
    try std.testing.expect(!matcher.match('$'));
}

pub const Matcher = struct {
    match: fn (tokenizer: *Self2()) void,

    const Self2 = @This();

    fn _opt(comptime matcher: CharMatcher) Self2 {
        return .{
            .match = struct {
                pub fn match(tokenizer: *Self()) bool {
                    if (tokenizer.peekOpt()) |t| {
                        const matching = matcher.match(t);
                        if (matching) {
                            tokenizer.next();
                        }
                    }
                }
            }.match,
        };
    }

    fn _repeat(comptime matcher: CharMatcher) Self2 {
        return .{
            .match = struct {
                pub fn match(tokenizer: *Self()) void {
                    while (tokenizer.peekOpt()) |t| {
                        const matching = matcher.match(t);
                        if (!matching) {
                            break;
                        }
                        tokenizer.next();
                    }
                }
            }.match,
        };
    }
};
