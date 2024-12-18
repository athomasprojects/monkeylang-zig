const std = @import("std");
const repl = @import("repl.zig");

pub fn main() !void {
    try repl.start();
}

// Tests
const expect = std.testing.expect;
const token = @import("token.zig");
const Token = token.Token;
const TokenTag = token.TokenTag;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");

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

test "Token - lookup identifiers" {
    const strings = [_][]const u8{ "let", "fn", "if", "else", "true", "false", "return", "does not exist!", "15" };
    for (strings) |str| {
        const result = switch (token.lookupIdent(str)) {
            .Let, .Function, .If, .Else, .True, .False, .Return => true,
            .Ident => |ident| std.mem.eql(u8, ident, str),
            else => false,
        };
        try expect(result);
    }
}

test "Lexer - init lexer" {
    const lexers = [_]Lexer{
        Lexer.init(""), Lexer.init(input),
    };
    const vals = [_]Lexer{ .{
        .input = "",
        .pos = 0,
        .ch = null,
    }, .{ .input = input, .pos = 0, .ch = input[0], .length = input.len } };
    for (lexers, vals) |lexer, val| {
        try expect(std.meta.eql(lexer, val));
    }
}

// Note: `std.meta.eql` does not work for structs containing fields that are
// slices. This is because slices are 'fat pointers'. So, for example, if
// we are comparing two fields whose contents are slices, then `std.meta.eql`
// will compare their their lengths and _pointers_, NOT their contents! The
// pointers might not point to the same locations in memory, even if they
// happen to have the same contents. Since meaning of pointer comparison can be
// somewhat ambiguous and therefore is not handled by `std.meta.eql`. Instead
// the user has to implement the comparison.
//
// The `std.meta.eql` docs explicitly states that pointers are NOT followed! (https://ziglang.org/documentation/master/std/#std.meta.eql)
// See also: https://www.reddit.com/r/Zig/comments/17ug7l7/zigs_stdmetaeql_fails_to_find_tagged_union/
test "Lexer - next token" {
    const str =
        \\=      + -/    ,;{<][>)(   !-512.79 let    if else fn
        \\ true false
        \\  _foo_! !bar_baz__
        \\"string!%"
        \\ != == === >=
        \\return * =!=
    ;
    const expected_tokens = [_]Token{ .Assign, .Plus, .Minus, .Slash, .Comma, .Semicolon, .LeftBrace, .LessThan, .RightBracket, .LeftBracket, .GreaterThan, .RightParen, .LeftParen, .Bang, .Minus, .{ .Integer = 512 }, .Illegal, .{ .Integer = 79 }, .Let, .If, .Else, .Function, .True, .False, .{ .Ident = "_foo_" }, .Bang, .Bang, .{ .Ident = "bar_baz__" }, .{ .String = "string!%" }, .NotEqual, .Equal, .Equal, .Assign, .GreaterThan, .Assign, .Return, .Asterisk, .Assign, .NotEqual };
    var lexer = Lexer.init(str);
    var idx: usize = 0;
    var next_tok: Token = lexer.nextToken();
    while (next_tok != TokenTag.Eof) : ({
        next_tok = lexer.nextToken();
        idx += 1;
    }) {
        const expected_tok = expected_tokens[idx];
        try expect(expected_tok.isEqual(next_tok));
    }
}

test "Parser - init" {
    const src = "let x = \"foo\"";
    var lexer: Lexer = Lexer.init(src);
    const current_token: Token = .Let;
    const peek_token: Token = .{ .Ident = "x" };
    const expected: Parser = .{
        .lexer = &lexer,
        .current_token = current_token,
        .peek_token = peek_token,
        .allocator = std.testing.allocator,
    };

    const parser = Parser.init(&lexer, std.testing.allocator);
    try std.testing.expectEqualDeep(expected, parser);
}

test "Parser - identifiers" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "foo";
    var lexer: Lexer = Lexer.init(src);
    var parser = Parser.init(&lexer, allocator);
    const program = try parser.parse();
    const expr = switch (program.statements.items[0]) {
        .expression_statement => |e| e.expression.*,
        else => unreachable,
    };
    try expect(program.statements.items.len == 1);
    try std.testing.expectEqualDeep(expr, ast.Expression{ .identifier = ast.Identifier{ .value = src } });
}
