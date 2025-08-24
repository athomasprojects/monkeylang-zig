const std = @import("std");
const ast = @import("ast.zig");
const tok = @import("token.zig");
const Token = tok.Token;
const TokenTag = tok.Tag;
const Lexer = @import("Lexer.zig");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const testing = std.testing;
const expect = testing.expect;

lexer: *Lexer,
allocator: Allocator,
current_token: Token,
peek_token: Token,
const Parser = @This();

pub const ParserError = error{
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedInteger,
    ExpectedOperator,
    ExpectedStringLiteral,
    ExpectedIntegerLiteral,
    ExpectedAssign,
    ExpectedPlus,
    ExpectedMinus,
    ExpectedEqual,
    ExpectedNotEqual,
    ExpectedBang,
    ExpectedAsterisk,
    ExpectedSlash,
    ExpectedLeftBrace,
    ExpectedLeftBracket,
    ExpectedLeftParen,
    ExpectedeRightBrace,
    ExpectedRightBracket,
    ExpectedRightParen,
    ExpectedLessThan,
    ExpectedGreaterThan,
    ExpectedComma,
    ExpectedSemicolon,
    ExpectedColon,
    ExpectedKeywordElse,
    ExpectedKeywordFalse,
    ExpectedKeywordFunction,
    ExpectedKeywordIf,
    ExpectedKeywordLet,
    ExpectedKeywordReturn,
    ExpectedKeywordTrue,
    ExpectedIllegal,
    ExepectedEof,
    ExpectedReturn,
    ExpectedStatement,
    InvalidExpressionList,
    InvalidFunctionParameter,
    InvalidHashLiteral,
    InvalidInfix,
    InvalidPrefix,
    InvalidProgram,
    OutOfMemory,
};

const Precedence = enum {
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
    index,

    fn isLessThan(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    fn fromToken(token: Token) Precedence {
        return switch (token) {
            .equal, .not_equal => .equals,
            .less_than, .greater_than => .less_greater,
            .plus, .minus => .sum,
            .slash, .asterisk => .product,
            .l_paren => .call,
            .l_bracket => .index,
            else => .lowest,
        };
    }
};

/// Returns a newly initialized parser.
pub fn init(lexer: *Lexer, allocator: Allocator) Parser {
    var parser: Parser = .{
        .lexer = lexer,
        .allocator = allocator,
        .current_token = .eof,
        .peek_token = .eof,
    };
    parser.advance();
    parser.advance();
    return parser;
}

/// Parses the input stream into a list of `ast.Statement` nodes.
pub fn parse(self: *Parser) !ast.Program {
    var statements = ArrayList(ast.Statement).init(self.allocator);
    return state: switch (self.current_token) {
        .eof => .{ .statements = statements },
        else => {
            // TODO: check if `parseStatement` returns an error and switch over the `ParserError` set to handle each respective case.
            const s = try self.parseStatement();
            statements.append(s) catch return error.InvalidProgram;
            self.advance();
            continue :state self.current_token;
        },
    };
}

fn advance(self: *Parser) void {
    self.current_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn chompSemicolon(self: *Parser) void {
    switch (self.peek_token) {
        .semicolon => self.advance(),
        else => {},
    }
}

fn parseStatement(self: *Parser) !ast.Statement {
    return switch (self.current_token) {
        .keyword_let => .{ .let_statement = try self.parseLetStatement() },
        .keyword_return => .{ .return_statement = try self.parseReturnStatement() },
        // .l_brace => .{ .block_statement = try self.parseBlockStatement() },
        else => .{ .expression_statement = try self.parseExpressionStatement() },
    };
}

fn parseLetStatement(self: *Parser) !ast.LetStatement {
    try self.expectPeek(.identifier);
    const name = try self.parseIdentifier();
    try self.expectPeek(.assign);

    self.advance(); // Move parser to beginning of expression.
    const expr = try self.parseExpression(.lowest);
    self.chompSemicolon();
    const expr_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
    expr_ptr.* = expr;
    return .{ .name = name, .value = expr_ptr };
}

fn parseReturnStatement(self: *Parser) !ast.ReturnStatement {
    self.advance(); // Move parser to beginning of expression.
    const ret = try self.parseExpression(.lowest);
    self.chompSemicolon();
    const ret_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
    ret_ptr.* = ret;
    return .{ .value = ret_ptr };
}

fn parseExpressionStatement(self: *Parser) !ast.ExpressionStatement {
    const expr = try self.parseExpression(.lowest);
    self.chompSemicolon();
    const expr_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
    expr_ptr.* = expr;
    return .{ .expression = expr_ptr };
}

fn parseBlockStatement(self: *Parser) !ast.BlockStatement {
    self.advance();
    var statements = ArrayList(ast.Statement).init(self.allocator);
    return state: switch (self.current_token) {
        .r_brace => {
            self.chompSemicolon();
            break :state .{ .statements = statements };
        },
        else => {
            const stmt = try self.parseStatement();
            statements.append(stmt) catch return error.OutOfMemory;
            self.advance();
            continue :state self.current_token;
        },
    };
}

fn parseExpression(self: *Parser, precedence: Precedence) ParserError!ast.Expression {
    var left_expr = try self.parsePrefixToken(self.current_token);
    while (self.current_token != TokenTag.semicolon and precedence.isLessThan(Precedence.fromToken(self.peek_token))) {
        const left_expr_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
        left_expr_ptr.* = left_expr;
        left_expr = try self.parseInfixToken(left_expr_ptr);
    }
    return left_expr;
}

fn parsePrefixToken(self: *Parser, token: Token) !ast.Expression {
    return switch (token) {
        .identifier => |ident| .{ .identifier = .{ .value = ident } },
        .integer_literal => |integer_literal| .{
            .integer = .{ .value = integer_literal },
        },
        .string_literal => |string_literal| .{
            .string = .{ .value = string_literal },
        },
        .keyword_true => .{ .boolean = .{ .value = true } },
        .keyword_false => .{ .boolean = .{ .value = false } },
        .bang, .minus => .{ .prefix = try self.parsePrefixExpression() },
        .l_paren => try self.parseGroupedExpression(),
        .keyword_if => .{ .if_expression = try self.parseIfExpression() },
        .keyword_function => .{ .function = try self.parseFunctionLiteral() },
        .l_bracket => .{ .array_literal = try self.parseArrayLiteral() },
        .l_brace => .{ .hash_literal = try self.parseHashLiteral() },
        else => error.InvalidPrefix,
    };
}

fn parseInfixToken(self: *Parser, left: *ast.Expression) !ast.Expression {
    self.advance();
    return switch (self.current_token) {
        .l_paren => .{ .call = try self.parseCallExpression(left) },
        .l_bracket => .{ .index_expression = try self.parseIndexExpression(left) },
        else => .{ .infix = try self.parseInfixExpression(left) },
    };
}

fn parsePrefixExpression(self: *Parser) !ast.PrefixExpression {
    if (self.current_token.isOperator()) {
        const current_token = self.current_token;
        self.advance();
        const right_expr = try self.parseExpression(.prefix);
        const right_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
        right_ptr.* = right_expr;
        return .{
            .operator = current_token,
            .right = right_ptr,
        };
    } else return error.ExpectedOperator;
}

fn parseInfixExpression(self: *Parser, left: *ast.Expression) !ast.InfixExpression {
    if (self.current_token.isOperator()) {
        const current_token = self.current_token;
        self.advance();
        const right = try self.parseExpression(Precedence.fromToken(current_token));
        const right_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
        right_ptr.* = right;
        return .{
            .operator = current_token,
            .left = left,
            .right = right_ptr,
        };
    } else return error.ExpectedOperator;
}

fn parseGroupedExpression(self: *Parser) !ast.Expression {
    self.advance();
    const expr = try self.parseExpression(.lowest);
    try self.expectPeek(.r_paren);
    return expr;
}

fn parseIfExpression(self: *Parser) !ast.IfExpression {
    try self.expectPeek(.l_paren);
    const condition = try self.parseExpression(.lowest);
    const condition_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
    condition_ptr.* = condition;

    try self.expectPeek(.l_brace);
    const then_branch = try self.parseBlockStatement();
    const then_ptr = self.allocator.create(ast.BlockStatement) catch return error.OutOfMemory;
    then_ptr.* = then_branch;

    switch (self.peek_token) {
        .keyword_else => {
            self.advance();
            try self.expectPeek(.l_brace);
            const else_branch = try self.parseBlockStatement();
            const else_ptr = self.allocator.create(ast.BlockStatement) catch return error.OutOfMemory;
            else_ptr.* = else_branch;
            return .initElseBranch(condition_ptr, then_ptr, else_ptr);
        },
        else => return .init(condition_ptr, then_ptr),
    }
}

fn parseFunctionLiteral(self: *Parser) !ast.FunctionLiteral {
    // Consume left paren.
    try self.expectPeek(.l_paren);
    self.advance();

    // Parse function parameters.
    const parameters: ?ArrayList(ast.Identifier) = outer: {
        if (self.current_token == .r_paren) {
            break :outer null;
        }

        var parameter_list = ArrayList(ast.Identifier).init(self.allocator);
        const first_param = try self.parseIdentifier();
        parameter_list.append(first_param) catch return error.OutOfMemory;
        state: switch (self.peek_token) {
            .comma => {
                self.advance();
                self.advance();
                const param = try self.parseIdentifier();
                parameter_list.append(param) catch return error.OutOfMemory;
                continue :state self.peek_token;
            },
            .r_paren => {
                self.advance();
                break :outer parameter_list;
            },
            else => return error.InvalidFunctionParameter,
        }
    };

    // Parse the function body.
    try self.expectPeek(.l_brace);
    const body = try self.parseBlockStatement();
    const body_ptr = self.allocator.create(ast.BlockStatement) catch return error.OutOfMemory;
    body_ptr.* = body;
    return .{
        .parameters = parameters,
        .body = body_ptr,
    };
}

fn parseCallExpression(self: *Parser, callee: *ast.Expression) !ast.Call {
    self.advance(); // Consume left paren.

    // No arguments.
    if (self.current_token == .r_paren) {
        return .empty(callee);
    }

    // Parse function arguments.
    var args = ArrayList(ast.Expression).init(self.allocator);
    const first_arg = try self.parseExpression(.lowest);
    args.append(first_arg) catch return error.OutOfMemory;
    return state: switch (self.peek_token) {
        .comma => {
            self.advance();
            self.advance();
            const arg = try self.parseExpression(.lowest);
            args.append(arg) catch return error.OutOfMemory;
            continue :state self.peek_token;
        },
        .r_paren => {
            self.advance();
            break :state .init(callee, args);
        },
        else => break :state error.InvalidExpressionList,
    };
}

fn parseArrayLiteral(self: *Parser) !ast.ArrayLiteral {
    self.advance(); // Consume left bracket.

    // Empty array.
    if (self.current_token == .r_bracket) {
        return .empty;
    }

    // Parse array elements.
    var elements = ArrayList(ast.Expression).init(self.allocator);
    const first_element = try self.parseExpression(.lowest);
    elements.append(first_element) catch return error.OutOfMemory;
    return state: switch (self.peek_token) {
        .comma => {
            self.advance();
            self.advance();
            const element = try self.parseExpression(.lowest);
            elements.append(element) catch return error.OutOfMemory;
            continue :state self.peek_token;
        },
        .r_bracket => {
            self.advance();
            break :state .{ .elements = elements };
        },
        else => error.InvalidExpressionList,
    };
}

fn parseIndexExpression(self: *Parser, left: *ast.Expression) !ast.IndexExpression {
    self.advance();
    const index_expression = try self.parseExpression(.lowest);
    try self.expectPeek(.r_bracket);
    const index_ptr = self.allocator.create(ast.Expression) catch return error.OutOfMemory;
    index_ptr.* = index_expression;
    return .{ .left = left, .index = index_ptr };
}

fn parseHashLiteral(self: *Parser) !ast.HashLiteral {
    self.advance();

    // Empty hash.
    if (self.current_token == .r_brace) {
        return .empty;
    }

    // Parse key-value expression pairs.
    var entries: ArrayList(ast.HashLiteral.Entry) = .init(self.allocator);
    const first_key = try self.parseExpression(.lowest);
    try self.expectPeek(.colon);
    self.advance();
    const first_value = try self.parseExpression(.lowest);
    const first_entry: ast.HashLiteral.Entry = .{
        .key = first_key,
        .value = first_value,
    };
    entries.append(first_entry) catch return error.OutOfMemory;
    return state: switch (self.peek_token) {
        .comma => {
            self.advance();
            self.advance();
            const key = try self.parseExpression(.lowest);
            try self.expectPeek(.colon);
            self.advance();
            const value = try self.parseExpression(.lowest);
            const entry: ast.HashLiteral.Entry = .{
                .key = key,
                .value = value,
            };
            entries.append(entry) catch return error.OutOfMemory;
            continue :state self.peek_token;
        },
        .r_brace => {
            self.advance();
            break :state .{ .entries = entries };
        },
        else => error.InvalidHashLiteral,
    };
}

fn parseIdentifier(self: *Parser) !ast.Identifier {
    return switch (self.current_token) {
        .identifier => |ident| .{ .value = ident },
        else => error.ExpectedIdentifier,
    };
}

/// Advances the parser if `peek_token` matches `expected`. Otherwise returns a `ParserError`.
fn expectPeek(self: *Parser, expected: TokenTag) !void {
    if (self.peek_token == expected) {
        self.advance();
    } else {
        return switch (expected) {
            .identifier => error.ExpectedIdentifier,
            .string_literal => error.ExpectedStringLiteral,
            .integer_literal => error.ExpectedIntegerLiteral,
            .assign => error.ExpectedAssign,
            .plus => error.ExpectedPlus,
            .minus => error.ExpectedMinus,
            .equal => error.ExpectedEqual,
            .not_equal => error.ExpectedNotEqual,
            .bang => error.ExpectedBang,
            .asterisk => error.ExpectedAsterisk,
            .slash => error.ExpectedSlash,
            .l_brace => error.ExpectedLeftBrace,
            .l_bracket => error.ExpectedLeftBracket,
            .l_paren => error.ExpectedLeftParen,
            .r_brace => error.ExpectedeRightBrace,
            .r_bracket => error.ExpectedRightBracket,
            .r_paren => error.ExpectedRightParen,
            .less_than => error.ExpectedLessThan,
            .greater_than => error.ExpectedGreaterThan,
            .comma => error.ExpectedComma,
            .semicolon => error.ExpectedSemicolon,
            .colon => error.ExpectedColon,
            .keyword_else => error.ExpectedKeywordElse,
            .keyword_false => error.ExpectedKeywordFalse,
            .keyword_function => error.ExpectedKeywordFunction,
            .keyword_if => error.ExpectedKeywordIf,
            .keyword_let => error.ExpectedKeywordLet,
            .keyword_return => error.ExpectedKeywordReturn,
            .keyword_true => error.ExpectedKeywordTrue,
            .illegal => error.ExpectedIllegal,
            .eof => error.ExepectedEof,
        };
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
test "init" {
    const src = "let x = \"foo\"";
    var lexer: Lexer = .init(src);
    const current_token: Token = .keyword_let;
    const peek_token: Token = .{ .identifier = "x" };
    const expected: Parser = .{
        .lexer = &lexer,
        .current_token = current_token,
        .peek_token = peek_token,
        .allocator = testing.allocator,
    };

    const parser: Parser = .init(&lexer, testing.allocator);
    try testing.expectEqualDeep(expected, parser);
}

test "identifier expressions" {
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

test "integer expressions" {
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

test "negative integer expressions" {
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
        try expect(expr.prefix.operator == TokenTag.minus);
        try testing.expectEqualDeep(
            expr.prefix.right.integer,
            ast.Integer{ .value = int },
        );
    }
}

test "booleans expressions" {
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

test "string expressions" {
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

test "let statement" {
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

test "return statement" {
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

test "expression statement" {
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

// test "block statement" {
//     var arena = ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//     const allocator = arena.allocator();
//
//     const src: []const u8 =
//         \\{ foo;
//         \\ let boo = "boo! this is a spooky string"; 123 * "foo" + "bar"
//         \\ *
//         \\    (-_baz / (5 * 3));
//         \\ return true * false;
//         \\  };
//     ;
//     const expected =
//         \\{
//         \\foo
//         \\let boo = "boo! this is a spooky string";
//         \\((123 * "foo") + ("bar" * ((-_baz) / (5 * 3))))
//         \\return (true * false);
//         \\}
//     ;
//
//     var lexer: Lexer = .init(src);
//     var parser: Parser = .init(&lexer, allocator);
//     const program = try parser.parse();
//
//     try expect(program.statements.items.len == 1);
//     for (program.statements.items) |stmt| {
//         const s = try stmt.toString(allocator);
//         try testing.expectEqualStrings(expected, s);
//     }
// }

test "if expression" {
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

test "function literal" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let add = fn(foo, bar, baz) {
        \\let x = 5;
        \\return foo * bar / baz;
        \\}
    ;
    const expected =
        \\let add = fn(foo, bar, baz) {
        \\let x = 5;
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

test "function call expression, no arguments" {
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

test "function call expression, expression arguments" {
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

test "function call, function literal callee" {
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

test "function arguments with trailing comma" {
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
        error.ExpectedIdentifier,
        error.InvalidPrefix,
    };

    for (src, errors) |input, expected| {
        var lexer: Lexer = .init(input);
        var parser: Parser = .init(&lexer, allocator);
        try testing.expectError(expected, parser.parse());
    }
}

test "empty array literal" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = "[]";
    const expected = "[]";

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();
    try testing.expect(program.statements.items.len == 1);
    switch (program.statements.items[0]) {
        .expression_statement => |expression_statement| {
            switch (expression_statement.expression.*) {
                .array_literal => |array_literal| {
                    try expect(std.meta.eql(array_literal, .empty));
                    const string = try array_literal.toString(allocator);
                    try testing.expectEqualStrings(expected, string);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "array literals" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = "[1, 2, -10, 5, \"foo\", fn(x,y,baz) { x + y - baz} ]";
    const expected =
        \\[1, 2, (-10), 5, "foo", fn(x, y, baz) {
        \\((x + y) - baz)
        \\}]
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();
    try testing.expect(program.statements.items.len == 1);
    switch (program.statements.items[0]) {
        .expression_statement => |expression_statement| {
            switch (expression_statement.expression.*) {
                .array_literal => |array_literal| {
                    try expect(array_literal.elements.?.items.len == 6);
                    const string = try array_literal.toString(allocator);
                    try testing.expectEqualStrings(expected, string);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "array literal missing closing bracket" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = "[\"foo\", 1, -5,";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    try testing.expectError(ParserError.InvalidPrefix, parser.parse());
}

test "index expression" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = "let myArray = [1, -2, 5]; let x = 2; let y = 1; myArray[x-y]";
    const expected = "myArray[(x - y)]";
    const len: usize = 4;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();
    try testing.expect(program.statements.items.len == len);
    switch (program.statements.items[len - 1]) {
        .expression_statement => |expression_statement| {
            switch (expression_statement.expression.*) {
                .index_expression => |index_expression| {
                    const string = try index_expression.toString(allocator);
                    try testing.expectEqualStrings(expected, string);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "hash literals" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = "let key = \"name\"; { \"one\": 1, key: \"Monkey\", \"foo\": \"bar\"}";
    const expected = "{\"one\": 1, key: \"Monkey\", \"foo\": \"bar\"}";

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    const program = try parser.parse();
    switch (program.statements.items[1]) {
        .expression_statement => |expression_statement| {
            switch (expression_statement.expression.*) {
                .hash_literal => |hash_literal| {
                    const string = try hash_literal.toString(allocator);
                    try testing.expectEqualStrings(expected, string);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}
