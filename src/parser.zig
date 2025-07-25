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
    InvalidExpressionList,
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

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token = .Eof,
    peek_token: Token = .Eof,
    allocator: Allocator,

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

    /// Parses the parser's input stream into a list of `ast.Statement` nodes.
    pub fn parse(self: *Parser) !ast.Program {
        var statements = ArrayList(ast.Statement).init(self.allocator);

        while (self.current_token != TokenTag.Eof) {
            // Todo: check if parseStatement returns an error and switch over the ParserError set to handle each respective case.
            // In --release=fast mode the program will just fail and Zig won't return the error stack trace.
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

    fn chompToken(self: *Parser, token: TokenTag) void {
        if (self.peek_token == token) {
            self.advance();
        }
    }

    fn parseStatement(self: *Parser) ParserError!ast.Statement {
        const current_token = self.current_token;
        return switch (current_token) {
            .Let => ast.Statement{ .let_statement = try self.parseLetStatement() },
            .Return => ast.Statement{ .return_statement = try self.parseReturnStatement() },
            .LeftBrace => ast.Statement{ .block_statement = try self.parseBlockStatement() },
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

    fn parseBlockStatement(self: *Parser) ParserError!ast.BlockStatement {
        self.advance();
        var statements = ArrayList(ast.Statement).init(self.allocator);

        while (true) : (self.advance()) {
            switch (self.current_token) {
                .RightBrace => break,
                else => {
                    const s = try self.parseStatement();
                    statements.append(s) catch return ParserError.FailedAlloc;
                },
            }
        }
        self.chompSemicolon();
        return ast.BlockStatement{ .statements = statements };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) ParserError!ast.Expression {
        var left_expr = try self.parsePrefixToken(self.current_token);
        while (self.peek_token != TokenTag.Semicolon and precedence.isLessThan(Precedence.fromToken(self.peek_token))) {
            const left_expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
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
            const right = try self.parseExpression(Precedence.fromToken(current_token));
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

    fn parseIfExpression(self: *Parser) ParserError!ast.IfExpression {
        try self.expectPeek(.LeftParen);
        const condition = try self.parseExpression(.lowest);
        const condition_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
        condition_ptr.* = condition;

        try self.expectPeek(.LeftBrace);
        const then_branch = try self.parseBlockStatement();
        const then_ptr = self.allocator.create(ast.BlockStatement) catch return ParserError.FailedAlloc;
        then_ptr.* = then_branch;

        if (self.peek_token == TokenTag.Else) {
            self.advance();
            try self.expectPeek(.LeftBrace);
            const else_branch = try self.parseBlockStatement();
            const else_ptr = self.allocator.create(ast.BlockStatement) catch return ParserError.FailedAlloc;
            else_ptr.* = else_branch;
            return ast.IfExpression{ .condition = condition_ptr, .then_branch = then_ptr, .else_branch = else_ptr };
        } else {
            return ast.IfExpression{ .condition = condition_ptr, .then_branch = then_ptr };
        }

        self.chompSemicolon();
    }

    fn parseFunctionLiteral(self: *Parser) ParserError!ast.FunctionLiteral {
        try self.expectPeek(.LeftParen);
        self.advance();

        var parameters: ?ArrayList(ast.Identifier) = null;
        switch (self.current_token) {
            .RightParen => {}, // No parameters to parse.
            else => {
                var list = ArrayList(ast.Identifier).init(self.allocator);

                // Parse function params.
                while (self.current_token != .RightParen) : (self.advance()) {
                    const ident = try self.parseIdentifier();
                    list.append(ident) catch return ParserError.FailedAlloc;
                    self.chompToken(.Comma);
                }
                parameters = list;
            },
        }

        // Parse function body.
        try self.expectPeek(.LeftBrace);
        const body = try self.parseBlockStatement();
        const body_ptr = self.allocator.create(ast.BlockStatement) catch return ParserError.FailedAlloc;
        body_ptr.* = body;

        return .{ .parameters = parameters, .body = body_ptr };
    }

    fn parseCallExpression(self: *Parser, callee: *ast.Expression) ParserError!ast.Call {
        var args: ?ArrayList(ast.Expression) = null;
        if (self.peek_token == TokenTag.RightParen) {
            self.advance();
            return ast.Call{ .args = args, .callee = callee };
        }

        // Parse function arguments.
        self.advance();
        var list = ArrayList(ast.Expression).init(self.allocator);
        while (self.current_token != TokenTag.RightParen) : (self.advance()) {
            const expr = try self.parseExpression(.lowest);
            list.append(expr) catch return ParserError.InvalidExpressionList;
            self.chompToken(.Comma);
        }
        args = list;
        return ast.Call{ .args = args, .callee = callee };
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
};
