const std = @import("std");
const expect = std.testing.expect;
const Lexer = @import("lexer.zig").Lexer;

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
        .position = 0,
        .ch = null,
    }, .{ .input = input, .position = 0, .ch = input[0], .length = input.len } };
    for (lexers, vals) |lexer, val| {
        // lexer.debugPrint();
        try expect(std.meta.eql(lexer, val));
    }
}

test "advance lexer" {
    var lexer1 = Lexer.init("");
    var lexer2 = Lexer.init(input);

    try expect(std.meta.eql(lexer1, Lexer{ .input = "" }));
    lexer1.advance();
    try expect(std.meta.eql(lexer1, Lexer{
        .input = "",
    }));

    // lexer1.debugPrint();
    // lexer2.debugPrint();
    try expect(std.meta.eql(lexer2, Lexer{ .input = input, .position = 0, .ch = input[0], .length = input.len }));
    lexer2.advance();
    try expect(std.meta.eql(lexer2, Lexer{ .input = input, .position = 1, .ch = input[1], .length = input.len }));
    // lexer2.debugPrint();
}

test "skip whitespace" {
    var lexer1 = Lexer.init("");
    const str = "   hello";
    var lexer2 = Lexer.init(str);

    lexer1.skip_whitespace();
    lexer2.skip_whitespace();

    try expect(std.meta.eql(lexer1, Lexer{ .input = "" }));
    try expect(std.meta.eql(lexer2, Lexer{ .input = str, .position = 3, .ch = 'h', .length = str.len }));
}
