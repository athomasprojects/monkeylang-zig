const std = @import("std");
const expect = std.testing.expect;
const Lexer = @import("lexer.zig").Lexer;

test "init lexer" {
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
    const lexers = [_]Lexer{
        Lexer.init(""), Lexer.init(input),
    };
    const vals = [_]Lexer{ .{ .input = "", .position = 0, .ch = null, .length = 0 }, .{ .input = input, .position = 0, .ch = input[0], .length = input.len } };
    for (lexers, vals) |lexer, val| {
        lexer.debugPrint();
        try expect(std.meta.eql(lexer, val));
    }
}
