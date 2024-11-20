const std = @import("std");

const StaticStringMap = std.static_string_map.StaticStringMap;

pub const Token = union(enum) {
    Illegal,
    // Identifiers
    Integer: i32,
    Ident: []const u8,
    String: []const u8,
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

    pub fn debugPrint(self: Token) void {
        switch (self) {
            .Ident, .String => |str| std.debug.print("token.Token{{ .{s} = \"{s}\" }}\n", .{ @tagName(self), str }),
            else => std.debug.print("{}\n", .{self}),
        }
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
