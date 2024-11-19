const std = @import("std");

const StaticStringMap = std.static_string_map.StaticStringMap;

pub const Token = union(enum) {
    // Identifiers
    Illegal: void,
    Eof: void,
    Integer: i32,
    Ident: []const u8,
    String: []const u8,
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
};

const IdentMap = StaticStringMap(Token).initComptime(.{
    .{ "let", .Let },
    .{ "fn", .Function },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "true", .True },
    .{ "false", .False },
    .{ "return", .Return },
});

pub fn lookupIdent(str: []const u8) Token {
    return IdentMap.get(str) orelse Token{ .Ident = str };
}
