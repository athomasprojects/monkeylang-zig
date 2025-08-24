const std = @import("std");
const testing = std.testing;
const expect = testing.expect;

pub const Tag = enum {
    identifier,
    string_literal,
    integer_literal,
    assign,
    plus,
    minus,
    equal,
    not_equal,
    bang,
    asterisk,
    slash,
    l_brace,
    l_bracket,
    l_paren,
    r_brace,
    r_bracket,
    r_paren,
    less_than,
    greater_than,
    comma,
    semicolon,
    colon,
    keyword_else,
    keyword_false,
    keyword_function,
    keyword_if,
    keyword_let,
    keyword_return,
    keyword_true,
    illegal,
    eof,

    pub fn lexeme(self: Token) ?[]const u8 {
        return switch (self) {
            .identifier,
            .integer_literal,
            .string_literal,
            .illegal,
            .eof,
            => null,
            .assign => "=",
            .plus => "+",
            .minus => "-",
            .equal => "==",
            .not_equal => "!=",
            .bang => "!",
            .asterisk => "*",
            .slash => "/",
            .l_brace => "{{",
            .l_bracket => "[",
            .l_paren => "(",
            .r_brace => "}}",
            .r_bracket => "]",
            .r_paren => ")",
            .less_than => "<",
            .greater_than => ">",
            .comma => ",",
            .semicolon => ";",
            .colon => ":",
            .keyword_else => "else",
            .keyword_false => "false",
            .keyword_function => "fn",
            .keyword_if => "if",
            .keyword_let => "let",
            .keyword_return => "return",
            .keyword_true => "true",
        };
    }

    pub fn symbol(tag: Tag) []const u8 {
        return tag.lexeme() orelse switch (tag) {
            .identifier => "an identifier",
            .integer_literal => "an integer literal",
            .string_literal => "a string literal",
            .illegal => "an invalid token",
            .eof => "EOF",
        };
    }
};

pub const Token = union(Tag) {
    identifier: []const u8,
    string_literal: []const u8,
    integer_literal: i64,
    // Operators
    assign,
    plus,
    minus,
    equal,
    not_equal,
    bang,
    asterisk,
    slash,
    // Delimiters
    l_brace,
    l_bracket,
    l_paren,
    r_brace,
    r_bracket,
    r_paren,
    less_than,
    greater_than,
    comma,
    semicolon,
    colon,
    keyword_else,
    keyword_false,
    keyword_function,
    keyword_if,
    keyword_let,
    keyword_return,
    keyword_true,
    illegal,
    eof,

    pub fn print(self: Token) void {
        switch (self) {
            .identifier, .string_literal => |str| std.debug.print("\"{s}\"", .{str}),
            .integer_literal => |value| std.debug.print("{d}", .{value}),
            .illegal => std.debug.print("ILLEGAL", .{}),
            .assign => std.debug.print("=", .{}),
            .plus => std.debug.print("+", .{}),
            .minus => std.debug.print("-", .{}),
            .equal => std.debug.print("==", .{}),
            .not_equal => std.debug.print("!=", .{}),
            .bang => std.debug.print("!", .{}),
            .asterisk => std.debug.print("*", .{}),
            .slash => std.debug.print("/", .{}),
            .l_brace => std.debug.print("{{", .{}),
            .l_bracket => std.debug.print("[", .{}),
            .l_paren => std.debug.print("(", .{}),
            .r_brace => std.debug.print("}}", .{}),
            .r_bracket => std.debug.print("]", .{}),
            .r_paren => std.debug.print(")", .{}),
            .less_than => std.debug.print("<", .{}),
            .greater_than => std.debug.print(">", .{}),
            .comma => std.debug.print(",", .{}),
            .semicolon => std.debug.print(";", .{}),
            .colon => std.debug.print(":", .{}),
            .keyword_else => std.debug.print("else", .{}),
            .keyword_false => std.debug.print("false", .{}),
            .keyword_function => std.debug.print("fn", .{}),
            .keyword_if => std.debug.print("if", .{}),
            .keyword_let => std.debug.print("let", .{}),
            .keyword_return => std.debug.print("return", .{}),
            .keyword_true => std.debug.print("true", .{}),
            .eof => std.debug.print("EOF", .{}),
        }
    }

    pub fn toString(self: Token, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .integer_literal => |value| try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .identifier, .string_literal => |str| try std.fmt.allocPrint(allocator, "{s}", .{str}),
            .illegal => try std.fmt.allocPrint(allocator, "ILLEGAL", .{}),
            .assign => try std.fmt.allocPrint(allocator, "=", .{}),
            .plus => try std.fmt.allocPrint(allocator, "+", .{}),
            .minus => try std.fmt.allocPrint(allocator, "-", .{}),
            .equal => try std.fmt.allocPrint(allocator, "==", .{}),
            .not_equal => try std.fmt.allocPrint(allocator, "!=", .{}),
            .bang => try std.fmt.allocPrint(allocator, "!", .{}),
            .asterisk => try std.fmt.allocPrint(allocator, "*", .{}),
            .slash => try std.fmt.allocPrint(allocator, "/", .{}),
            .l_brace => try std.fmt.allocPrint(allocator, "{{", .{}),
            .l_bracket => try std.fmt.allocPrint(allocator, "[", .{}),
            .l_paren => try std.fmt.allocPrint(allocator, "(", .{}),
            .r_brace => try std.fmt.allocPrint(allocator, "}}", .{}),
            .r_bracket => try std.fmt.allocPrint(allocator, "]", .{}),
            .r_paren => try std.fmt.allocPrint(allocator, ")", .{}),
            .less_than => try std.fmt.allocPrint(allocator, "<", .{}),
            .greater_than => try std.fmt.allocPrint(allocator, ">", .{}),
            .comma => try std.fmt.allocPrint(allocator, ",", .{}),
            .semicolon => try std.fmt.allocPrint(allocator, ";", .{}),
            .colon => try std.fmt.allocPrint(allocator, ":", .{}),
            .keyword_else => try std.fmt.allocPrint(allocator, "else", .{}),
            .keyword_false => try std.fmt.allocPrint(allocator, "false", .{}),
            .keyword_function => try std.fmt.allocPrint(allocator, "fn", .{}),
            .keyword_if => try std.fmt.allocPrint(allocator, "if", .{}),
            .keyword_let => try std.fmt.allocPrint(allocator, "let", .{}),
            .keyword_return => try std.fmt.allocPrint(allocator, "return", .{}),
            .keyword_true => try std.fmt.allocPrint(allocator, "true", .{}),
            .eof => try std.fmt.allocPrint(allocator, "EOF", .{}),
        };
    }

    pub fn eql(self: Token, other: Token) bool {
        switch (self) {
            .identifier, .string_literal => |actual_slice| {
                switch (other) {
                    .identifier, .string_literal => |other_slice| return std.mem.eql(u8, @tagName(self), @tagName(other)) and std.mem.eql(u8, actual_slice, other_slice),
                    else => {},
                }
            },
            else => {},
        }
        return std.meta.eql(self, other);
    }

    pub fn isOperator(token: Token) bool {
        return switch (token) {
            .assign,
            .bang,
            .plus,
            .minus,
            .asterisk,
            .slash,
            .equal,
            .not_equal,
            .less_than,
            .greater_than,
            => true,
            else => false,
        };
    }
};

const keywords = std.StaticStringMap(Token).initComptime(.{
    .{ "let", .keyword_let },
    .{ "fn", .keyword_function },
    .{ "if", .keyword_if },
    .{ "else", .keyword_else },
    .{ "true", .keyword_true },
    .{ "false", .keyword_false },
    .{ "return", .keyword_return },
});

/// Returns the corresponding keyword token if the string is a keyword, otherwise returns and identifier token.
pub fn fromIdentifierOrKeyword(str: []const u8) Token {
    return keywords.get(str) orelse Token{ .identifier = str };
}

// Tests
test "lookup keywords" {
    const strings = [_][]const u8{ "let", "fn", "if", "else", "true", "false", "return" };
    for (strings) |str| {
        const result = switch (fromIdentifierOrKeyword(str)) {
            .keyword_let,
            .keyword_function,
            .keyword_if,
            .keyword_else,
            .keyword_true,
            .keyword_false,
            .keyword_return,
            => true,
            else => false,
        };
        try expect(result);
    }
}
