const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Lexer = @import("Lexer.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const testing = std.testing;
const expect = testing.expect;
const ArenaAllocator = std.heap.ArenaAllocator;

pub const ParserError = error{
    ExpectedBoolean,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedInteger,
    ExpectedOperator,
    ExpectedPeek,
    ExpectedReturn,
    ExpectedStatement,
    ExpectedStringLiteral,
    InvalidExpressionList,
    InvalidInfix,
    InvalidPrefix,
    InvalidProgram,
    OutOfMemory,
};

lexer: *Lexer,
current_token: Token = .Eof,
peek_token: Token = .Eof,
allocator: Allocator,
const Parser = @This();

const Precedence = enum {
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
    // index,

    fn isLessThan(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    fn fromToken(token: Token) Precedence {
        return switch (token) {
            .Equal, .NotEqual => .equals,
            .LessThan, .GreaterThan => .less_greater,
            .Plus, .Minus => .sum,
            .Slash, .Asterisk => .product,
            .LeftParen => .call,
            // .LeftBracket => .index,
            else => .lowest,
        };
    }
};

/// Returns a newly initialized parser.
pub fn init(lexer: *Lexer, allocator: Allocator) Parser {
    var parser: Parser = .{
        .lexer = lexer,
        .allocator = allocator,
    };
    parser.advance();
    parser.advance();
    return parser;
}

/// Parses the input stream into a list of `ast.Statement` nodes.
pub fn parse(self: *Parser) !ast.Program {
    var statements = ArrayList(ast.Statement).init(self.allocator);
    return sw: switch (self.current_token) {
        .Eof => .{ .statements = statements },
        else => {
            // TODO: check if `parseStatement` returns an error and switch over the `ParserError` set to handle each respective case.
            // In --release=fast mode the program will just fail and Zig won't return the error stack trace.
            const s = try self.parseStatement();
            statements.append(s) catch return ParserError.InvalidProgram;
            self.advance();
            continue :sw self.current_token;
        },
    };
}

fn advance(self: *Parser) void {
    self.current_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn chompSemicolon(self: *Parser) void {
    switch (self.peek_token) {
        .Semicolon => self.advance(),
        else => {},
    }
}

fn chompComma(self: *Parser) void {
    switch (self.peek_token) {
        .Comma => self.advance(),
        else => {},
    }
}

fn parseStatement(self: *Parser) ParserError!ast.Statement {
    return switch (self.current_token) {
        .Let => .{ .let_statement = try self.parseLetStatement() },
        .Return => .{ .return_statement = try self.parseReturnStatement() },
        .LeftBrace => .{ .block_statement = try self.parseBlockStatement() },
        else => .{ .expression_statement = try self.parseExpressionStatement() },
    };
}

fn parseLetStatement(self: *Parser) ParserError!ast.LetStatement {
    try self.expectPeek(.Ident);
    const name = try self.parseIdentifier();
    try self.expectPeek(.Assign);

    // Move parser to beginning of expression
    self.advance();
    const expr = try self.parseExpression(.lowest);
    self.chompSemicolon();
    const expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.OutOfMemory;
    expr_ptr.* = expr;
    return .{ .name = name, .value = expr_ptr };
}

fn parseReturnStatement(self: *Parser) ParserError!ast.ReturnStatement {
    // Move parser to beginning of expression
    self.advance();
    const ret = try self.parseExpression(.lowest);
    self.chompSemicolon();
    const ret_ptr = self.allocator.create(ast.Expression) catch return ParserError.OutOfMemory;
    ret_ptr.* = ret;
    return .{ .value = ret_ptr };
}

fn parseExpressionStatement(self: *Parser) ParserError!ast.ExpressionStatement {
    const expr = try self.parseExpression(.lowest);
    self.chompSemicolon();
    const expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.OutOfMemory;
    expr_ptr.* = expr;
    return .{ .expression = expr_ptr };
}

fn parseBlockStatement(self: *Parser) ParserError!ast.BlockStatement {
    self.advance();
    var statements = ArrayList(ast.Statement).init(self.allocator);
    return sw: switch (self.current_token) {
        .RightBrace => {
            self.chompSemicolon();
            break :sw .{ .statements = statements };
        },
        else => {
            const s = try self.parseStatement();
            statements.append(s) catch return ParserError.OutOfMemory;
            self.advance();
            continue :sw self.current_token;
        },
    };
}

fn parseExpression(self: *Parser, precedence: Precedence) ParserError!ast.Expression {
    var left_expr = try self.parsePrefixToken(self.current_token);
    while (self.peek_token != TokenTag.Semicolon and precedence.isLessThan(Precedence.fromToken(self.peek_token))) {
        const left_expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.OutOfMemory;
        left_expr_ptr.* = left_expr;
        left_expr = try self.parseInfixToken(left_expr_ptr);
    }
    return left_expr;
}

fn parsePrefixToken(self: *Parser, token: Token) ParserError!ast.Expression {
    return switch (token) {
        .Ident => .{ .identifier = try self.parseIdentifier() },
        .Integer => .{ .integer = try self.parseInteger() },
        .String => .{ .string = try self.parseString() },
        .Bang, .Minus => .{ .prefix = try self.parsePrefixExpression() },
        .True, .False => .{ .boolean = try self.parseBoolean() },
        .LeftParen => try self.parseGroupedExpression(),
        .If => .{ .if_expression = try self.parseIfExpression() },
        .Function => .{ .function = try self.parseFunctionLiteral() },
        // .LeftBracket => .{ .array = try self.parseArray() },
        else => ParserError.InvalidPrefix,
    };
}

fn parseInfixToken(self: *Parser, left: *ast.Expression) ParserError!ast.Expression {
    const token = self.peek_token;
    self.advance();
    return switch (token) {
        .LeftParen => .{ .call = try self.parseCallExpression(left) },
        // .LeftBracket => .{ .index = try self.parseIndexExpression(left) },
        else => .{ .infix = try self.parseInfixExpression(left) },
    };
}

fn parsePrefixExpression(self: *Parser) ParserError!ast.PrefixExpression {
    if (self.current_token.isOperator()) {
        const current_token = self.current_token;
        self.advance();
        const right_expr = try self.parseExpression(.prefix);
        const right_ptr = self.allocator.create(ast.Expression) catch return ParserError.OutOfMemory;
        right_ptr.* = right_expr;
        return .{
            .operator = current_token,
            .right = right_ptr,
        };
    } else {
        return ParserError.ExpectedOperator;
    }
}

fn parseInfixExpression(self: *Parser, left: *ast.Expression) ParserError!ast.InfixExpression {
    if (self.current_token.isOperator()) {
        const current_token = self.current_token;
        self.advance();
        const right = try self.parseExpression(Precedence.fromToken(current_token));
        const right_ptr = self.allocator.create(ast.Expression) catch return ParserError.OutOfMemory;
        right_ptr.* = right;
        return .{
            .operator = current_token,
            .left = left,
            .right = right_ptr,
        };
    } else {
        return ParserError.ExpectedOperator;
    }
}

fn parseGroupedExpression(self: *Parser) !ast.Expression {
    self.advance();
    const expr = try self.parseExpression(.lowest);
    try self.expectPeek(.RightParen);
    return expr;
}

fn parseIfExpression(self: *Parser) ParserError!ast.IfExpression {
    try self.expectPeek(.LeftParen);
    const condition = try self.parseExpression(.lowest);
    const condition_ptr = self.allocator.create(ast.Expression) catch return ParserError.OutOfMemory;
    condition_ptr.* = condition;

    try self.expectPeek(.LeftBrace);
    const then_branch = try self.parseBlockStatement();
    const then_ptr = self.allocator.create(ast.BlockStatement) catch return ParserError.OutOfMemory;
    then_ptr.* = then_branch;

    return sw: switch (self.peek_token) {
        .Else => {
            self.advance();
            try self.expectPeek(.LeftBrace);
            const else_branch = try self.parseBlockStatement();
            const else_ptr = self.allocator.create(ast.BlockStatement) catch return ParserError.OutOfMemory;
            else_ptr.* = else_branch;
            break :sw .{
                .condition = condition_ptr,
                .then_branch = then_ptr,
                .else_branch = else_ptr,
            };
        },
        else => .{
            .condition = condition_ptr,
            .then_branch = then_ptr,
        },
    };
}

fn parseFunctionLiteral(self: *Parser) ParserError!ast.FunctionLiteral {
    try self.expectPeek(.LeftParen);
    self.advance();

    // var parameters: ?ArrayList(ast.Identifier) = null;
    const parameters: ?ArrayList(ast.Identifier) = blk: {
        if (self.current_token != .RightParen) {
            var idents = ArrayList(ast.Identifier).init(self.allocator);
            while (true) {
                const ident = try self.parseIdentifier();
                idents.append(ident) catch return ParserError.OutOfMemory;
                if (self.peek_token == .Comma) {
                    self.advance();
                    self.advance();
                } else {
                    break :blk idents;
                }
            }
        } else {
            break :blk null;
        }
    };

    // Parse the function body.
    try self.expectPeek(.RightParen);
    try self.expectPeek(.LeftBrace);
    const body = try self.parseBlockStatement();
    const body_ptr = self.allocator.create(ast.BlockStatement) catch return ParserError.OutOfMemory;
    body_ptr.* = body;
    return .{
        .parameters = parameters,
        .body = body_ptr,
    };
}

fn parseCallExpression(self: *Parser, callee: *ast.Expression) ParserError!ast.Call {
    self.advance(); // Consume left paren.

    // No arguments.
    if (self.current_token == .RightParen) {
        return .{ .callee = callee };
    }

    // Parse arguments.
    var args = ArrayList(ast.Expression).init(self.allocator);
    while (true) {
        const expr = try self.parseExpression(.lowest);
        args.append(expr) catch return ParserError.InvalidExpressionList;

        if (self.peek_token == .Comma) {
            self.advance();
            self.advance();
            continue;
        }

        if (self.peek_token == .RightParen) {
            self.advance();
            break;
        } else {
            return ParserError.ExpectedPeek;
        }
    }
    return .{ .callee = callee, .args = args };
}

// Expressions
fn parseIdentifier(self: *Parser) ParserError!ast.Identifier {
    return switch (self.current_token) {
        .Ident => |ident| .{ .value = ident },
        else => ParserError.ExpectedIdentifier,
    };
}

fn parseInteger(self: *Parser) ParserError!ast.Integer {
    return switch (self.current_token) {
        .Integer => |int| .{ .value = int },
        else => ParserError.ExpectedInteger,
    };
}

fn parseBoolean(self: *Parser) ParserError!ast.Boolean {
    return switch (self.current_token) {
        .True => .{ .value = true },
        .False => .{ .value = false },
        else => ParserError.ExpectedBoolean,
    };
}

fn parseString(self: *Parser) ParserError!ast.String {
    return switch (self.current_token) {
        .String => |str| .{ .value = str },
        else => ParserError.ExpectedStringLiteral,
    };
}

/// Advances the parser if `peek_token` matches `expected`. Otherwise returns a `ParserError`.
fn expectPeek(self: *Parser, expected: TokenTag) ParserError!void {
    if (self.peek_token == expected) {
        self.advance();
    } else {
        return ParserError.ExpectedPeek;
    }
}

pub fn printParserError(self: ParserError) void {
    std.debug.print("PARSER ERROR: {s}\n", .{@errorName(self)});
}

pub fn print(self: Parser) void {
    std.debug.print("parser.Parser{{\n", .{});
    std.debug.print("    lexer: ", .{});
    self.lexer.print();
    std.debug.print(",\n", .{});
    std.debug.print("    current_token: ", .{});
    self.current_token.print();
    std.debug.print(",\n    peek_token: ", .{});
    self.peek_token.print();
    std.debug.print("\n}}", .{});
}

// Tests
test "Parser - init" {
    const src = "let x = \"foo\"";
    var lexer: Lexer = .init(src);
    const current_token: Token = .Let;
    const peek_token: Token = .{ .Ident = "x" };
    const expected: Parser = .{
        .lexer = &lexer,
        .current_token = current_token,
        .peek_token = peek_token,
        .allocator = testing.allocator,
    };

    const parser: Parser = .init(&lexer, testing.allocator);
    try testing.expectEqualDeep(expected, parser);
}

test "Parser - identifier expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = "foo";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();
    const expr = switch (program.statements.items[0]) {
        .expression_statement => |e| e.expression.*,
        else => unreachable,
    };
    try expect(program.statements.items.len == 1);
    try testing.expectEqualDeep(
        expr,
        ast.Expression{ .identifier = .{ .value = src } },
    );
}

test "Parser - integer expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "123456789; 0; 01";
    const expected = [_]i64{ 123456789, 0, 1 };
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == expected.len);
    for (program.statements.items, expected) |stmt, int| {
        const expr = switch (stmt) {
            .expression_statement => |e| e.expression.*,
            else => unreachable,
        };
        try testing.expectEqualDeep(
            expr,
            ast.Expression{ .integer = .{ .value = int } },
        );
    }
}

test "Parser - negative integer expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expected = [_]i64{ 123456789, 0, 1 };
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
        try testing.expectEqualDeep(
            expr.prefix.right.integer,
            ast.Integer{ .value = int },
        );
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
        try testing.expectEqualDeep(
            expr,
            ast.Expression{ .boolean = .{ .value = b } },
        );
    }
}

test "Parser - string expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const expected = [_][]const u8{
        "hello",
        "x%foo23",
        "!_bar",
        "]*(BaZ)}]",
    };
    const src: []const u8 =
        \\"hello";
        \\"x%foo23";
        \\"!_bar";
        \\"]*(BaZ)}]"
    ;
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();

    try expect(program.statements.items.len == expected.len);
    for (program.statements.items, expected) |stmt, str| {
        const expr = switch (stmt) {
            .expression_statement => |e| e.expression.*,
            else => unreachable,
        };
        try testing.expectEqualDeep(
            expr,
            ast.Expression{ .string = .{ .value = str } },
        );
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
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
        try testing.expectEqualStrings(expected, s);
    }
}

test "Parser - function arguments with trailing comma" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        \\let foo = fn(x,y,) { x + y };
        ,
        \\let bar = fn(x,y) { x - y };
        \\bar(1,2,);
        ,
    };

    const errors = [_]ParserError{
        ParserError.ExpectedIdentifier,
        ParserError.InvalidPrefix,
    };

    for (src, errors) |input, expected| {
        var lexer: Lexer = .init(input);
        var parser: Parser = .init(&lexer, allocator);
        try testing.expectError(expected, parser.parse());
    }
}
