const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

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
        const parameters: ?ArrayList(ast.Identifier) = blk: {
            switch (self.current_token) {
                .RightParen => break :blk null, // No parameters to parse.
                else => {
                    var list = ArrayList(ast.Identifier).init(self.allocator);
                    sw: switch (self.current_token) {
                        .RightParen => break :blk list,
                        else => {
                            const ident = try self.parseIdentifier();
                            list.append(ident) catch return ParserError.OutOfMemory;
                            self.chompComma();
                            self.advance();
                            continue :sw self.current_token;
                        },
                    }
                },
            }
        };
        // Parse function body.
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
        self.advance();
        // No arguments to parse.
        switch (self.peek_token) {
            .RightParen => return .{ .callee = callee },
            else => {},
        }
        // Parse function arguments.
        var args = ArrayList(ast.Expression).init(self.allocator);
        return sw: switch (self.current_token) {
            .RightParen => .{ .args = args, .callee = callee },
            else => {
                const expr = try self.parseExpression(.lowest);
                args.append(expr) catch return ParserError.InvalidExpressionList;
                self.chompComma();
                self.advance();
                continue :sw self.current_token;
            },
        };
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
