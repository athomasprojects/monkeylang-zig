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
    current_token: ?Token = null,
    peek_token: ?Token = null,
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

        while (self.current_token) |_| {
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

    fn chompSemicolon(self: *Parser) !void {
        try self.expectPeek(.Semicolon);
    }

    fn parseStatement(self: *Parser) ParserError!ast.Statement {
        const current_token = self.current_token.?;
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
        if (self.peekTokenIs(.Semicolon)) {
            self.advance();
        }
        const expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
        expr_ptr.* = expr;
        return .{ .name = name, .value = expr_ptr };
    }

    fn parseReturnStatement(self: *Parser) ParserError!ast.ReturnStatement {
        // Move parser to beginning of expression
        self.advance();
        const ret = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.Semicolon)) {
            self.advance();
        }
        const ret_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
        ret_ptr.* = ret;
        return .{ .value = ret_ptr };
    }

    fn parseExpressionStatement(self: *Parser) ParserError!ast.ExpressionStatement {
        const expr = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.Semicolon)) {
            self.advance();
        }
        const expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
        expr_ptr.* = expr;
        return .{ .expression = expr_ptr };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) ParserError!ast.Expression {
        if (self.current_token) |current_token| {
            var left_expr = try self.parsePrefix(current_token);
            if (self.peekPrecedence()) |peek_precedence| {
                while (!self.peekTokenIs(.Semicolon) and precedence.isLessThan(peek_precedence)) {
                    const left_expr_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
                    left_expr_ptr.* = left_expr;
                    if (self.peek_token) |peek_token| {
                        left_expr = try self.parseInfix(peek_token, left_expr_ptr);
                    } else {
                        break;
                    }
                }
                return left_expr;
            } else |_| return left_expr;
        }
        return ParserError.ExpectedExpression;
    }

    fn parsePrefix(self: *Parser, token: Token) ParserError!ast.Expression {
        return switch (token) {
            .Ident => .{ .identifier = try self.parseIdentifier() },
            .Integer => .{ .integer = try self.parseInteger() },
            .String => .{ .string = try self.parseString() },
            .Bang, .Minus => .{ .prefix = try self.parsePrefixExpression() },
            .True, .False => .{ .boolean = try self.parseBoolean() },
            // .LeftParen => try self.parseGroupedExpression(),
            // .LeftBracket => .{ .array = try self.parseArray() },
            // .If => .{ .if_expression = try self.parseIfExpression() },
            // .Function => .{ .function = try self.parseFunctionLiteral() },
            else => ParserError.InvalidPrefix,
        };
    }

    fn parseInfix(self: *Parser, token: Token, left: *ast.Expression) ParserError!ast.Expression {
        self.advance();
        return switch (token) {
            .Plus, .Minus, .Slash, .Asterisk, .Equal, .NotEqual, .LessThan, .GreaterThan => .{ .infix = try self.parseInfixExpression(left) },
            // .LeftParen => .{ .call = try self.parseCallExpression(left) },
            // .LeftBracket => .{ .index = try self.parseIndexExpression(left) },
            else => ParserError.InvalidInfix,
        };
    }

    fn parsePrefixExpression(self: *Parser) ParserError!ast.PrefixExpression {
        if (self.current_token) |current_token| {
            if (current_token.isOperator()) {
                self.advance();
                const right = try self.parseExpression(.prefix);
                const right_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
                right_ptr.* = right;
                return .{ .operator = current_token, .right = right_ptr };
            } else {
                return ParserError.ExpectedOperator;
            }
        } else {
            unreachable;
        }
    }

    fn parseInfixExpression(self: *Parser, left: *ast.Expression) ParserError!ast.InfixExpression {
        if (self.current_token) |current_token| {
            if (current_token.isOperator()) {
                const curr_token_precedence = tokenPrecedenceMap(current_token);
                self.advance();
                const right = try self.parseExpression(curr_token_precedence);
                const right_ptr = self.allocator.create(ast.Expression) catch return ParserError.FailedAlloc;
                right_ptr.* = right;
                return .{ .operator = current_token, .left = left, .right = right_ptr };
            }
        }
        return ParserError.ExpectedOperator;
    }

    // Expressions
    fn parseIdentifier(self: *Parser) ParserError!ast.Identifier {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .Ident => |ident| .{ .value = ident },
                else => ParserError.ExpectedIdentifier,
            };
        } else {
            unreachable;
        }
    }

    fn parseInteger(self: *Parser) ParserError!ast.Integer {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .Integer => |int| .{ .value = int },
                else => ParserError.ExpectedInteger,
            };
        } else {
            unreachable;
        }
    }

    fn parseBoolean(self: *Parser) ParserError!ast.Boolean {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .True => .{ .value = true },
                .False => .{ .value = false },
                else => ParserError.ExpectedBoolean,
            };
        } else {
            unreachable;
        }
    }

    fn parseString(self: *Parser) ParserError!ast.String {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .String => |str| .{ .value = str },
                else => ParserError.ExpectedStringLiteral,
            };
        } else {
            unreachable;
        }
    }

    // fn parseGroupedExpression(self: *Parser) !ast.Expression {
    //     self.advance();
    //     const expr = try self.parseExpression(.lowest);
    //     try self.expectPeek(.RightParen);
    //     return expr;
    // }

    fn peekTokenIs(self: *Parser, token: TokenTag) bool {
        if (self.peek_token) |peek_token| {
            return peek_token == token;
        } else {
            return false;
        }
    }

    fn currentTokenIs(self: *Parser, token: TokenTag) bool {
        if (self.current_token) |current_token| {
            return current_token == token;
        } else {
            return false;
        }
    }

    /// Advances the parser if `peek_token` matches `expected`. Otherwise returns a `ParserError`.
    fn expectPeek(self: *Parser, expected: TokenTag) ParserError!void {
        if (self.peekTokenIs(expected)) {
            self.advance();
        } else {
            return ParserError.ExpectedPeek;
        }
    }

    fn peekPrecedence(self: *Parser) ParserError!Precedence {
        if (self.peek_token) |peek_token| {
            return tokenPrecedenceMap(peek_token);
        } else {
            return ParserError.ExpectedPeek;
        }
    }

    fn currentPrecedence(self: *Parser) ParserError!Precedence {
        if (self.current_token) |current_token| {
            return tokenPrecedenceMap(current_token);
        }
        return ParserError.ExpectedOperator;
    }

    pub fn debugPrint(self: Parser) void {
        std.debug.print("parser.Parser{{\n", .{});
        std.debug.print("    lexer: ", .{});
        self.lexer.debugPrint();
        std.debug.print(",\n", .{});
        std.debug.print("    current_token: ", .{});
        if (self.current_token) |current_token| {
            current_token.debugPrint();
        } else {
            std.debug.print("null", .{});
        }
        std.debug.print(",\n    peek_token: ", .{});
        if (self.peek_token) |peek_token| {
            peek_token.debugPrint();
        } else {
            std.debug.print("null", .{});
        }
        std.debug.print("\n}}", .{});
    }
};
