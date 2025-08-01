const std = @import("std");
const testing = std.testing;
const expect = testing.expect;
const ArenaAllocator = std.heap.ArenaAllocator;

const StaticStringMap = std.static_string_map.StaticStringMap;
pub const TokenTag = enum {
    // End of file
    Eof,
    // Identifiers
    Ident,
    String,
    Integer,
    // Invalid
    Illegal,
    // Operators
    Assign,
    Plus,
    Minus,
    Equal,
    NotEqual,
    Bang,
    Asterisk,
    Slash,
    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LessThan,
    GreaterThan,
    Comma,
    Semicolon,
    // Keywords
    Let,
    Function,
    If,
    Else,
    True,
    False,
    Return,
};

pub const Token = union(TokenTag) {
    // End of file
    Eof,
    // Identifiers
    Ident: []const u8,
    String: []const u8,
    Integer: i64,
    // Invalid
    Illegal,
    // Operators
    Assign,
    Plus,
    Minus,
    Equal,
    NotEqual,
    Bang,
    Asterisk,
    Slash,
    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LessThan,
    GreaterThan,
    Comma,
    Semicolon,
    // Keywords
    Let,
    Function,
    If,
    Else,
    True,
    False,
    Return,

    pub fn print(self: Token) void {
        switch (self) {
            .Ident, .String => |str| std.debug.print("\"{s}\"", .{str}),
            .Integer => |value| std.debug.print("{d}", .{value}),
            .Illegal => std.debug.print("ILLEGAL", .{}),
            .Assign => std.debug.print("=", .{}),
            .Plus => std.debug.print("+", .{}),
            .Minus => std.debug.print("-", .{}),
            .Equal => std.debug.print("==", .{}),
            .NotEqual => std.debug.print("!=", .{}),
            .Bang => std.debug.print("!", .{}),
            .Asterisk => std.debug.print("*", .{}),
            .Slash => std.debug.print("/", .{}),
            .LeftParen => std.debug.print("(", .{}),
            .RightParen => std.debug.print(")", .{}),
            .LeftBrace => std.debug.print("{{", .{}),
            .RightBrace => std.debug.print("}}", .{}),
            .LeftBracket => std.debug.print("[", .{}),
            .RightBracket => std.debug.print("]", .{}),
            .LessThan => std.debug.print("<", .{}),
            .GreaterThan => std.debug.print(">", .{}),
            .Comma => std.debug.print(",", .{}),
            .Semicolon => std.debug.print(";", .{}),
            .Let => std.debug.print("let", .{}),
            .Function => std.debug.print("fn", .{}),
            .If => std.debug.print("if", .{}),
            .Else => std.debug.print("else", .{}),
            .True => std.debug.print("true", .{}),
            .False => std.debug.print("false", .{}),
            .Return => std.debug.print("return", .{}),
            .Eof => std.debug.print("EOF", .{}),
        }
    }

    pub fn toString(self: Token, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .Integer => |value| try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .Ident, .String => |str| try std.fmt.allocPrint(allocator, "{s}", .{str}),
            .Illegal => try std.fmt.allocPrint(allocator, "ILLEGAL", .{}),
            .Assign => try std.fmt.allocPrint(allocator, "=", .{}),
            .Plus => try std.fmt.allocPrint(allocator, "+", .{}),
            .Minus => try std.fmt.allocPrint(allocator, "-", .{}),
            .Equal => try std.fmt.allocPrint(allocator, "==", .{}),
            .NotEqual => try std.fmt.allocPrint(allocator, "!=", .{}),
            .Bang => try std.fmt.allocPrint(allocator, "!", .{}),
            .Asterisk => try std.fmt.allocPrint(allocator, "*", .{}),
            .Slash => try std.fmt.allocPrint(allocator, "/", .{}),
            .LeftParen => try std.fmt.allocPrint(allocator, "(", .{}),
            .RightParen => try std.fmt.allocPrint(allocator, ")", .{}),
            .LeftBrace => try std.fmt.allocPrint(allocator, "{{", .{}),
            .RightBrace => try std.fmt.allocPrint(allocator, "}}", .{}),
            .LeftBracket => try std.fmt.allocPrint(allocator, "[", .{}),
            .RightBracket => try std.fmt.allocPrint(allocator, "]", .{}),
            .LessThan => try std.fmt.allocPrint(allocator, "<", .{}),
            .GreaterThan => try std.fmt.allocPrint(allocator, ">", .{}),
            .Comma => try std.fmt.allocPrint(allocator, ",", .{}),
            .Semicolon => try std.fmt.allocPrint(allocator, ";", .{}),
            .Let => try std.fmt.allocPrint(allocator, "let", .{}),
            .Function => try std.fmt.allocPrint(allocator, "fn", .{}),
            .If => try std.fmt.allocPrint(allocator, "if", .{}),
            .Else => try std.fmt.allocPrint(allocator, "else", .{}),
            .True => try std.fmt.allocPrint(allocator, "true", .{}),
            .False => try std.fmt.allocPrint(allocator, "false", .{}),
            .Return => try std.fmt.allocPrint(allocator, "return", .{}),
            .Eof => try std.fmt.allocPrint(allocator, "EOF", .{}),
        };
    }

    pub fn isEqual(self: Token, other: Token) bool {
        switch (self) {
            .Ident, .String => |actual_slice| {
                switch (other) {
                    .Ident, .String => |other_slice| return std.mem.eql(u8, actual_slice, other_slice) and std.mem.eql(u8, @tagName(self), @tagName(other)),
                    else => {},
                }
            },
            else => {},
        }
        return std.meta.eql(self, other);
    }

    pub fn isOperator(token: Token) bool {
        return switch (token) {
            .Assign,
            .Bang,
            .Plus,
            .Minus,
            .Asterisk,
            .Slash,
            .Equal,
            .NotEqual,
            .LessThan,
            .GreaterThan,
            => true,
            else => false,
        };
    }
};

const keyword_map = StaticStringMap(Token).initComptime(.{
    .{ "let", .Let },
    .{ "fn", .Function },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "true", .True },
    .{ "false", .False },
    .{ "return", .Return },
});

/// Returns the corresponding keyword token if the string is a keyword, otherwise returns and identifier token.
pub fn keywordToIdentifier(str: []const u8) Token {
    return keyword_map.get(str) orelse Token{ .Ident = str };
}

// Tests
test "Token - lookup identifiers" {
    const strings = [_][]const u8{ "let", "fn", "if", "else", "true", "false", "return", "does not exist!", "15" };
    for (strings) |str| {
        const result = switch (keywordToIdentifier(str)) {
            .Let, .Function, .If, .Else, .True, .False, .Return => true,
            .Ident => |ident| std.mem.eql(u8, ident, str),
            else => false,
        };
        try expect(result);
    }
}
