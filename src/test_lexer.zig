const std = @import("std");
const expect = std.testing.expect;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;

const input =
    \\let five = 5;
    \\let ten = 10;
    \\let add = fn(x, y){
    \\x + y;
    \\};
    \\let result = add(five, ten);
    \\if x != y {
    \\let foo_bar = y / x * 69;
    \\return true;
    \\} else {
    \\return false;
    \\}
    \\10 == 10;
    \\10 != 9;
    \\baz =!= 420
    \\"string"
;

test "init lexer" {
    const lexers = [_]Lexer{
        Lexer.init(""), Lexer.init(input),
    };
    const vals = [_]Lexer{ .{
        .input = "",
        .pos = 0,
        .ch = null,
    }, .{ .input = input, .pos = 0, .ch = input[0], .length = input.len } };
    for (lexers, vals) |lexer, val| {
        // lexer.debugPrint();
        try expect(std.meta.eql(lexer, val));
    }
}

test "next token" {
    const str =
        \\=      + -/    ,;{<][>)(   !-512.79 let    if else fn
        \\ true false
        \\  _foo_! !bar__
        \\"string!%"
        \\ != == === >=
    ;
    const tokens = [_]Token{ .Assign, .Plus, .Minus, .Slash, .Comma, .Semicolon, .LeftBrace, .LessThan, .RightBracket, .LeftBracket, .GreaterThan, .RightParen, .LeftParen, .Bang, .Minus, .{ .Integer = 512 }, .Illegal, .{ .Integer = 79 }, .Let, .If, .Else, .Function, .True, .False, .{ .Ident = "_foo_" }, .Bang, .Bang, .{ .Ident = "bar__" }, .{ .String = "string!%" }, .NotEqual, .Equal, .Equal, .Assign, .GreaterThan, .Assign };
    var lexer = Lexer.init(str);
    var idx: usize = 0;
    while (lexer.nextToken()) |next_tok| : (idx += 1) {
        const tok = tokens[idx];
        switch (next_tok) {
            .Ident, .String => |ident| {
                switch (tok) {
                    .Ident, .String => |v| try expect(std.mem.eql(u8, ident, v)),
                    else => try expect(std.meta.eql(next_tok, tokens[idx])),
                }
            },
            else => try expect(std.meta.eql(next_tok, tok)),
        }
        // next_tok.debugPrint();
        // tokens[idx].debugPrint();
    }
}
