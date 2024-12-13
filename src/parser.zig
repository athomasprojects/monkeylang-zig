const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const ParserError = error{
    FailedAlloc,
    ExpectedOperator,
    ExpectedPeek,
    ExpectedIdentifier,
    ExpectedInteger,
    ExpectedStringLiteral,
    ExpectedBoolean,
    ExpectedExpression,
    ExpectedStatement,
    ExpectedReturn,
    InvalidProgram,
    InvalidPrefix,
    InvalidInfix,
};

pub fn printParserError(self: ParserError) void {
    std.debug.print("PARSER ERROR: {s}\n", .{@errorName(self)});
}

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
};

fn tokenPrecedenceMap(token: Token) Precedence {
    return switch (token) {
        .Equal, .NotEqual => .equals,
        .LessThan, .GreaterThan => .less_greater,
        .Plus, .Minus => .sum,
        .Slash, .Asterisk => .product,
        .LeftParen => .call,
        .LeftBracket => .index,
        else => .lowest,
    };
}

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token = .Eof,
    peek_token: Token = .Eof,
    allocator: Allocator,

    pub fn init(lexer: *Lexer, allocator: Allocator) Parser {
        var parser: Parser = .{
            .lexer = lexer,
            .allocator = allocator,
        };
        parser.advance();
        parser.advance();
        return parser;
    }

    pub fn parse(self: *Parser) !ast.Program {
        var statements = ArrayList(ast.Statement).init(self.allocator);

        while (self.current_token != TokenTag.Eof) {
            const s = try self.parseStatement();
            statements.append(s) catch return ParserError.InvalidProgram;
            self.advance();
        }
        return .{ .statements = statements };
    }

    fn advance(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn chompSemicolon(self: *Parser) void {
        if (self.peek_token == TokenTag.Semicolon) {
            self.advance();
        }
    }

    fn parseStatement(self: *Parser) ParserError!ast.Statement {
        const current_token = self.current_token;
        return switch (current_token) {
            .Let => ast.Statement{ .let_statement = try self.parseLetStatement() },
            .Return => ast.Statement{ .return_statement = try self.parseReturnStatement() },
            else => ast.Statement{ .expression_statement = try self.parseExpressionStatement() },
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
        const expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
        expr_ptr.* = expr;
        return .{ .name = name, .value = expr_ptr };
    }

    fn parseReturnStatement(self: *Parser) ParserError!ast.ReturnStatement {
        // Move parser to beginning of expression
        self.advance();
        const ret = try self.parseExpression(.lowest);
        self.chompSemicolon();
        const ret_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
        ret_ptr.* = ret;
        return .{ .value = ret_ptr };
    }

    fn parseExpressionStatement(self: *Parser) ParserError!ast.ExpressionStatement {
        const expr = try self.parseExpression(.lowest);
        self.chompSemicolon();
        const expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
        expr_ptr.* = expr;
        return .{ .expression = expr_ptr };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) ParserError!ast.Expression {
        var left_expr = try self.parsePrefixToken(self.current_token);
        while (self.peek_token != TokenTag.Semicolon and precedence.isLessThan(tokenPrecedenceMap(self.peek_token))) {
            const left_expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
            left_expr_ptr.* = left_expr;
            left_expr = try self.parseInfixToken(self.peek_token, left_expr_ptr);
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
            // .LeftBracket => .{ .array = try self.parseArray() },
            // .If => .{ .if_expression = try self.parseIfExpression() },
            // .Function => .{ .function = try self.parseFunctionLiteral() },
            else => ParserError.InvalidPrefix,
        };
    }

    fn parseInfixToken(self: *Parser, token: Token, left: *ast.Expression) ParserError!ast.Expression {
        self.advance();
        return switch (token) {
            .Plus, .Minus, .Slash, .Asterisk, .Equal, .NotEqual, .LessThan, .GreaterThan => .{ .infix = try self.parseInfixExpression(left) },
            // .LeftParen => .{ .call = try self.parseCallExpression(left) },
            // .LeftBracket => .{ .index = try self.parseIndexExpression(left) },
            else => .{ .infix = try self.parseInfixExpression(left) },
        };
    }

    fn parsePrefixExpression(self: *Parser) ParserError!ast.PrefixExpression {
        if (self.current_token.isOperator()) {
            const current_token = self.current_token;
            self.advance();
            const right_expr = try self.parseExpression(.prefix);
            const right_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
            right_ptr.* = right_expr;
            return .{ .operator = current_token, .right = right_ptr };
        } else {
            return ParserError.ExpectedOperator;
        }
    }

    fn parseInfixExpression(self: *Parser, left: *ast.Expression) ParserError!ast.InfixExpression {
        if (self.current_token.isOperator()) {
            const current_token = self.current_token;
            self.advance();
            const right = try self.parseExpression(tokenPrecedenceMap(current_token));
            const right_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
            right_ptr.* = right;
            return .{ .operator = current_token, .left = left, .right = right_ptr };
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

    fn parseCallExpression(self: *Parser, callee: *ast.Expression) ParserError!ast.Call {
        return ast.Call{ .callee = callee, .args = try self.parseExpressionList(.RightParen) };
    }

    /// Advances the parser if `peek_token` matches `expected`. Otherwise returns a `ParserError`.
    fn expectPeek(self: *Parser, expected: TokenTag) ParserError!void {
        if (self.peek_token == expected) {
            self.advance();
        } else {
            return ParserError.ExpectedPeek;
        }
    }

    pub fn print(self: Parser) void {
        std.debug.print("parser.Parser{{\n", .{});
        std.debug.print("    lexer: ", .{});
        self.lexer.print();
        std.debug.print(",\n", .{});
        std.debug.print("    current_token: ", .{});
        if (self.current_token) |current_token| {
            current_token.print();
        } else {
            std.debug.print("null", .{});
        }
        std.debug.print(",\n    peek_token: ", .{});
        if (self.peek_token) |peek_token| {
            peek_token.print();
        } else {
            std.debug.print("null", .{});
        }
        std.debug.print("\n}}", .{});
    }
};
