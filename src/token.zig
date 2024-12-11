const std = @import("std");

const StaticStringMap = std.static_string_map.StaticStringMap;
pub const TokenTag = enum {
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
    // Identifiers
    Ident: []const u8,
    String: []const u8,
    Integer: i32,
    // Invalid
    Illegal: void,
    // Operators
    Assign: void,
    Plus: void,
    Minus: void,
    Equal: void,
    NotEqual: void,
    Bang: void,
    Asterisk: void,
    Slash: void,
    // Delimiters
    LeftParen: void,
    RightParen: void,
    LeftBrace: void,
    RightBrace: void,
    LeftBracket: void,
    RightBracket: void,
    LessThan: void,
    GreaterThan: void,
    Comma: void,
    Semicolon: void,
    // Keywords
    Let: void,
    Function: void,
    If: void,
    Else: void,
    True: void,
    False: void,
    Return: void,

    pub fn debugPrint(self: Token) void {
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
        }
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
            .Assign, .Bang, .Plus, .Minus, .Asterisk, .Slash, .Equal, .NotEqual, .LessThan, .GreaterThan => true,
            else => false,
        };
    }
};

const KeywordMap = StaticStringMap(Token).initComptime(.{
    .{ "let", .Let },
    .{ "fn", .Function },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "true", .True },
    .{ "false", .False },
    .{ "return", .Return },
});

/// Returns the corresponding keyword token if the string is a keyword, otherwise returns and identifier token.
pub fn lookupIdent(str: []const u8) Token {
    return KeywordMap.get(str) orelse Token{ .Ident = str };
}
