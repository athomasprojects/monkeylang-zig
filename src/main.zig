const std = @import("std");
const repl = @import("repl.zig");

pub fn main() !void {
    try repl.start();
}

// Tests
const expect = std.testing.expect;
const ArenaAllocator = std.heap.ArenaAllocator;
const token = @import("token.zig");
const Token = token.Token;
const TokenTag = token.TokenTag;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Evaluator = @import("evaluator.zig").Evaluator;
const Environment = @import("environment.zig").Environment;
const Object = @import("object.zig").Object;
const ast = @import("ast.zig");

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
    const lexers = [_]Lexer{ .init(""), .init(input) };
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
// slices. This is because slices are 'fat pointers'.
//
// So, for example, if we are comparing two fields whose contents are slices,
// then `std.meta.eql` will compare their their lengths and _pointers_, NOT
// their contents!
//
// The pointers might not point to the same locations in
// memory, even if they happen to have the same contents. Since meaning of
// pointer comparison can be somewhat ambiguous and therefore is not handled by
// `std.meta.eql`. Instead the user has to implement the comparison.
//
// The `std.meta.eql` docs explicitly states that pointers are NOT followed! - (see: https://ziglang.org/documentation/master/std/#std.meta.eql)
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
    const expected_tokens = [_]Token{
        .Assign,
        .Plus,
        .Minus,
        .Slash,
        .Comma,
        .Semicolon,
        .LeftBrace,
        .LessThan,
        .RightBracket,
        .LeftBracket,
        .GreaterThan,
        .RightParen,
        .LeftParen,
        .Bang,
        .Minus,
        .{ .Integer = 512 },
        .Illegal,
        .{ .Integer = 79 },
        .Let,
        .If,
        .Else,
        .Function,
        .True,
        .False,
        .{ .Ident = "_foo_" },
        .Bang,
        .Bang,
        .{ .Ident = "bar_baz__" },
        .{ .String = "string!%" },
        .NotEqual,
        .Equal,
        .Equal,
        .Assign,
        .GreaterThan,
        .Assign,
        .Return,
        .Asterisk,
        .Assign,
        .NotEqual,
    };
    var lexer: Lexer = .init(str);
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
    var lexer: Lexer = .init(src);
    const current_token: Token = .Let;
    const peek_token: Token = .{ .Ident = "x" };
    const expected: Parser = .{
        .lexer = &lexer,
        .current_token = current_token,
        .peek_token = peek_token,
        .allocator = std.testing.allocator,
    };

    const parser: Parser = .init(&lexer, std.testing.allocator);
    try std.testing.expectEqualDeep(expected, parser);
}

test "Parser - identifier expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "foo";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();
    const expr = switch (program.statements.items[0]) {
        .expression_statement => |e| e.expression.*,
        else => unreachable,
    };
    try expect(program.statements.items.len == 1);
    try std.testing.expectEqualDeep(expr, ast.Expression{ .identifier = ast.Identifier{ .value = src } });
}

test "Parser - integer expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "123456789; 0; 01";
    const expected = [_]i32{ 123456789, 0, 1 };
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == expected.len);
    for (program.statements.items, expected) |stmt, int| {
        const expr = switch (stmt) {
            .expression_statement => |e| e.expression.*,
            else => unreachable,
        };
        try std.testing.expectEqualDeep(expr, ast.Expression{ .integer = ast.Integer{ .value = int } });
    }
}

test "Parser - negative integer expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expected = [_]i32{ 123456789, 0, 1 };
    const src: []const u8 = "-123456789; -0; -1";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();
    try expect(program.statements.items.len == expected.len);
    for (program.statements.items, expected) |stmt, int| {
        const expr = switch (stmt) {
            .expression_statement => |e| e.expression.*,
            else => unreachable,
        };
        try expect(expr.prefix.operator == TokenTag.Minus);
        try std.testing.expectEqualDeep(expr.prefix.right.*.integer, ast.Integer{ .value = int });
    }
}

test "Parser - booleans expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expected = [_]bool{ true, false };
    const src: []const u8 = "true; false";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == expected.len);
    for (program.statements.items, expected) |stmt, b| {
        const expr = switch (stmt) {
            .expression_statement => |e| e.expression.*,
            else => unreachable,
        };
        try std.testing.expectEqualDeep(expr, ast.Expression{ .boolean = ast.Boolean{ .value = b } });
    }
}

test "Parser - string expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expected = [_][]const u8{ "hello", "x%foo23", "!_bar", "]*(BaZ)}]" };
    const src: []const u8 = "\"hello\"; \"x%foo23\"; \"!_bar\"; \"]*(BaZ)}]\"";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == expected.len);
    for (program.statements.items, expected) |stmt, str| {
        const expr = switch (stmt) {
            .expression_statement => |e| e.expression.*,
            else => unreachable,
        };
        try std.testing.expectEqualDeep(expr, ast.Expression{ .string = ast.String{ .value = str } });
    }
}

test "Parser - let statement" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let x = "foo" + "bar" * (-baz / (5 * 3))
    ;
    const expected =
        \\let x = ("foo" + ("bar" * ((-baz) / (5 * 3))));
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = switch (stmt) {
            .let_statement => |let_statement| try let_statement.toString(allocator),
            .return_statement => |return_statement| try return_statement.toString(allocator),
            .expression_statement => |expr| try expr.toString(allocator),
            else => unreachable,
        };
        // std.debug.print("{s}\n", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - return statement" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\return 123 * "foo" + "bar" * (-_baz / (5 * 3))
    ;
    const expected =
        \\return ((123 * "foo") + ("bar" * ((-_baz) / (5 * 3))));
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = switch (stmt) {
            .let_statement => |let_statement| try let_statement.toString(allocator),
            .return_statement => |return_statement| try return_statement.toString(allocator),
            .expression_statement => |expr| try expr.toString(allocator),
            else => unreachable,
        };
        // std.debug.print("{s}\n", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - expression statement" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // The lexer skips whitespace so weird formatting shouldn't matter here.
    const src: []const u8 =
        \\123 * "foo" + "bar"
        \\ * 
        \\    (-_baz / (5 * 3))
    ;
    const expected =
        \\((123 * "foo") + ("bar" * ((-_baz) / (5 * 3))))
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = switch (stmt) {
            .let_statement => |let_statement| try let_statement.toString(allocator),
            .return_statement => |return_statement| try return_statement.toString(allocator),
            .expression_statement => |expr| try expr.toString(allocator),
            else => unreachable,
        };
        // std.debug.print("{s}\n", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - block statement" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\{ foo;
        \\ let boo = "boo! this is a spooky string"; 123 * "foo" + "bar"
        \\ * 
        \\    (-_baz / (5 * 3));
        \\ return true * false;
        \\  };
    ;
    const expected =
        \\{
        \\foo
        \\let boo = "boo! this is a spooky string";
        \\((123 * "foo") + ("bar" * ((-_baz) / (5 * 3))))
        \\return (true * false);
        \\}
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = try stmt.toString(allocator);
        // std.debug.print("{s}", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - if expression" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\if (x != "foo" + "bar" - (5 * 3 / 2)) {
        \\let x = "foo";
        \\if (y > 2) { y };
        \\} else {
        \\return "baz";
        \\}
    ;
    const expected =
        \\if (x != (("foo" + "bar") - ((5 * 3) / 2))) {
        \\let x = "foo";
        \\if (y > 2) {
        \\y
        \\}
        \\} else {
        \\return "baz";
        \\}
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = try stmt.toString(allocator);
        // std.debug.print("{s}", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - function literal" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let add = fn(foo, bar, baz) {
        \\let x = 5;
        \\{ "foo" + "bar";
        \\fn(x,y) { x }
        \\}
        \\return foo * bar / baz;
        \\}
        \\
        \\
        \\
    ;
    const expected =
        \\let add = fn(foo, bar, baz) {
        \\let x = 5;
        \\{
        \\("foo" + "bar")
        \\fn(x, y) {
        \\x
        \\}
        \\}
        \\return ((foo * bar) / baz);
        \\};
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = try stmt.toString(allocator);
        // std.debug.print("{s}", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - function call expression, no arguments" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "add()";
    const expected = "add()";

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = try stmt.toString(allocator);
        // std.debug.print("{s}", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - function call expression, expression arguments" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "a + add(b * c) + d";
    const expected =
        \\((a + add((b * c))) + d)
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = try stmt.toString(allocator);
        // std.debug.print("{s}", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Parser - function call, function literal callee" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\ let func = fn(x,y) {
        \\  return x + y;
        \\ }(foo, -b / d + 5);
    ;
    const expected =
        \\let func = fn(x, y) {
        \\return (x + y);
        \\}(foo, (((-b) / d) + 5));
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == 1);
    for (program.statements.items) |stmt| {
        const s = try stmt.toString(allocator);
        // std.debug.print("{s}", .{s});
        try std.testing.expectEqualStrings(expected, s);
    }
}

test "Evaluator - null" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "null";
    const expected: []const u8 = "identifier not found: null";

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const obj: *Object = try evaluator.evalProgram(&program, &env);

    const result = try obj.toString(allocator);
    try std.testing.expectEqualStrings(expected, result);
}

test "Evaluator - boolean literal" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{ "true", "false" };

    for (src) |expected| {
        var lexer: Lexer = .init(expected);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        const result = try obj.toString(allocator);
        try std.testing.expectEqualStrings(expected, result);
    }
}

test "Evaluator - integer literal" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{ "1", "0", "-0", "-5", "-10", "69420" };
    const expected = [_]i32{ 1, 0, 0, -5, -10, 69420 };

    for (src, 0..) |str, i| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        try std.testing.expect(expected[i] == obj.integer);
    }
}

test "Evaluator - prefix operators" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "!true",
        "!false",
        "!5",
        "!!true",
        "!!false",
        "!!5",
        "!!-5",
        "!!!false",
    };
    const bools = [_]bool{
        false,
        true,
        false,
        true,
        false,
        true,
        true,
        true,
    };

    for (src, bools) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        try std.testing.expect(expected == obj.boolean);
    }
}

test "Evaluator - integer expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "5 + 5 + 5 + 5 - 10",
        "2 * 2 * 2 * 2 * 2",
        "-50 + 100 + 50",
        "5 * 2 + 10",
        "5 + 2 * 10",
        "2 * (5 + 10)",
        "3 * 3 * 3 + 10",
        "3 * (3 * 3) + 10",
        "(5 + 10 * 2 + 15 / 3) * 2 + -10",
    };
    const values = [_]i32{
        10,
        32,
        100,
        20,
        25,
        30,
        37,
        37,
        50,
    };

    for (src, values) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        try std.testing.expect(expected == obj.integer);
    }
}

test "Evaluator - boolean expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "true",
        "false",
        "1 < 2",
        "1 > 2",
        "1 < 1",
        "1 > 1",
        "1 == 1",
        "1 != 1",
        "1 == 2",
        "1 != 2",
        "true == true",
        "false == false",
        "true == false",
        "true != false",
        "false != true",
        "(1 < 2) == true",
        "(1 < 2) == false",
        "(1 > 2) == true",
        "(1 > 2) == false",
    };
    const values = [_]bool{
        true,
        false,
        true,
        false,
        false,
        false,
        true,
        false,
        false,
        true,
        true,
        true,
        false,
        true,
        true,
        true,
        false,
        false,
        true,
    };

    for (src, values) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        try std.testing.expect(expected == obj.boolean);
    }
}

test "Evaluator - conditional expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "if (true) { 10 }",
        "if (false) { 10 }",
        "if (1) { 10 }",
        "if (1 < 2) { 10 }",
        "if (1 > 2) { 10 }",
        "if (1 > 2) { 10 } else { 20 }",
        "if (1 < 2) { 10 } else { 20 }",
    };

    const values = [_][]const u8{
        "10",
        "null",
        "10",
        "10",
        "null",
        "20",
        "10",
    };

    for (src, values) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        const result = try obj.toString(allocator);
        try std.testing.expectEqualStrings(expected, result);
    }
}

test "Evaluator - nested conditional expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\ if (2 > 1) {
        \\   if (2 > 1) {
        \\     10
        \\   } else {
        \\     20
        \\   }
        \\ } else {
        \\   30
        \\ }
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const obj: *Object = try evaluator.evalProgram(&program, &env);

    try std.testing.expect(@as(i32, 10) == obj.integer);
}

test "Evaluator - error handling" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const nested: []const u8 =
        \\if (10 > 1) {
        \\    if (10 > 1) {
        \\        return true + false;
        \\    }
        \\    return 1;
        \\}
    ;
    const src = [_][]const u8{
        "5 + true;",
        "5 + true; 5;",
        "-true",
        "true + false",
        "5; true + false; 5",
        "if (10 > 1) { true + false; }",
        nested,
    };
    const error_messages = [_][]const u8{
        "type mismatch: INTEGER + BOOLEAN",
        "type mismatch: INTEGER + BOOLEAN",
        "unknown operator: -BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
    };

    for (src, error_messages) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        const result = try obj.toString(allocator);
        try std.testing.expectEqualStrings(expected, result);
    }
}

test "Evaluator - let statements" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "let a = 5; a;",
        "let a = 5 * 5; a;",
        "let a = 5; let b = a; b;",
        "let a = 5; let b = a; let c = a + b + 5; c;",
        "let a = 5; let b = a; let c = a + b + 5; let d = if (c > a) { 99 } else { 100 }; d;",
    };
    const error_messages = [_]i32{
        5,
        25,
        5,
        15,
        99,
    };

    for (src, error_messages) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const obj: *Object = try evaluator.evalProgram(&program, &env);

        try std.testing.expect(expected == obj.integer);
    }
}

test "Evaluator - function literals" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let add = fn(a,b) { a + b };
        \\let sub = fn(a,b) { a - b };
        \\let applyFunc = fn(a, b, func) { func(a,b) };
        \\applyFunc(2, 2, add)
    ;
    const expected: i32 = 4;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const obj: *Object = try evaluator.evalProgram(&program, &env);

    // const result = try obj.toString(allocator);
    try std.testing.expect(expected == obj.integer);
}
