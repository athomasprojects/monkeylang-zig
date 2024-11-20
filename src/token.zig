const std = @import("std");

const StaticStringMap = std.static_string_map.StaticStringMap;

pub const Token = union(enum) {
    Illegal: void,
    // Identifiers
    Integer: i32,
    Ident: []const u8,
    String: []const u8,
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
