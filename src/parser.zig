const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ParserError = error{
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

const Precedence = enum {
    // fmt: off
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
    index,
    // fmt: on

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
    current_token: ?Token,
    peek_token: ?Token,
    allocator: Allocator,

    pub fn init(lexer: *Lexer, allocator: Allocator) Parser {
        return .{
            .lexer = lexer,
            .current_token = lexer.nextToken(),
            .peek_token = lexer.nextToken(),
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Parser) !ast.Program {
        var statements = ArrayList(ast.Statement).init(self.allocator);

        while (self.current_token) |_| {
            const s = try self.parseStatement();
            statements.append(s) catch return ParserError.InvalidProgram;
            s.debugPrint();
            std.debug.print("\n", .{});
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

    fn parseStatement(self: *Parser) !ast.Statement {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .Let => ast.Statement{ .let_statement = try self.parseLetStatement() },
                .Return => ast.Statement{ .return_statement = try self.parseReturnStatement() },
                else => ast.ExpressionStatement{ .expression = try self.parseExpressionStatement() },
            };
        } else {
            unreachable;
        }
    }

    fn parseLetStatement(self: *Parser) !ast.LetStatement {
        try self.expectPeek(.Ident);
        const name = try self.parseIdentifier();
        try self.expectPeek(.Assign);
        // Move parser to beginning of expression
        self.advance();
        // parse expression...
        // var expression = try self.ParseExpression(.lowest);

        // Todo: remove `self.peek_token != null` guard once we know how to parse expressions.
        while (!self.peekTokenIs(.Semicolon)) {
            if (self.peek_token == null or self.current_token == null) {
                return ParserError.ExpectedExpression;
            }
            self.advance();
        }
        const let: ast.LetStatement = .{ .name = name, .value = .{ .noop = self.current_token.? } };
        try self.expectPeek(.Semicolon);
        return let;
    }

    fn parseReturnStatement(self: *Parser) !ast.ReturnStatement {
        // Move parser to beginning of expression
        self.advance();
        // parse expression...
        // var expression = try self.ParseExpression(.lowest);

        // Todo: remove `self.peek_token != null` guard once we know how to parse expressions.
        while (!self.peekTokenIs(.Semicolon)) {
            if (self.peek_token == null or self.current_token == null) {
                return ParserError.ExpectedExpression;
            }
            self.advance();
        }
        const ret: ast.ReturnStatement = .{ .value = .{ .noop = self.current_token.? } };
        try self.expectPeek(.Semicolon);
        return ret;
    }

    fn parseExpressionStatement(self: *Parser) !ast.ExpressionStatement {
        const expr = try self.parseExpression(.lowest);
        if (self.peekTokenIs(.Semicolon)) {
            self.advance();
        }
        return .{ .expression = expr };
    }

    // Todo: Use explicit error set instead of inferred error set to resolve compiler error for recursive function!
    // See references listed in output of `zig build -freference=13` to see all 13 function references.
    fn parseExpression(self: *Parser, precedence: Precedence) !ast.Expression {
        if (self.current_token) |current_token| {
            var left_expr = try self.parsePrefix(current_token);
            var peek_precedence = try self.peekPrecedence();
            while (!self.peekTokenIs(.Semicolon) and precedence.isLessThan(peek_precedence)) {
                if (self.peek_token) |peek_token| {
                    left_expr = try self.parseInfix(peek_token, left_expr);
                }
                peek_precedence = try self.peekPrecedence();
            }
            return left_expr;
        } else {
            unreachable;
        }
    }

    fn parsePrefixExpression(self: *Parser) !ast.PrefixExpression {
        if (self.current_token) |current_token| {
            if (current_token.isOperator()) {
                self.advance();
                return .{ .operator = current_token, .right = try self.parseExpression(.prefix) };
            } else {
                return ParserError.InvalidPrefix;
            }
        } else {
            unreachable;
        }
    }

    fn parseInfixExpression(self: *Parser, left: ast.Expression) !ast.PrefixExpression {
        if (self.current_token) |current_token| {
            if (current_token.isOperator()) {
                const precedence = tokenPrecedenceMap(current_token);
                self.advance();
                const right = try self.parseExpression(precedence);
                return .{ .left = left, .operator = current_token, .right = right };
            } else {
                return ParserError.InvalidInfix;
            }
        } else {
            unreachable;
        }
    }

    fn parsePrefix(self: *Parser, token: TokenTag) !ast.Expression {
        return switch (token) {
            .Ident => .{ .identifier = try self.parseIdentifier() },
            .Integer => .{ .integer = try self.parseInteger() },
            .String => .{ .string = try self.parseString() },
            .True, .False => .{ .boolean = try self.parseBoolean() },
            .Bang, .Minus => .{ .prefix = try self.parsePrefixExpression() },
            .LeftParen => try self.parseGroupedExpression(),
            // .LeftBracket => .{ .array = try self.parseArray() },
            // .If => .{ .if_expression = try self.parseIfExpression() },
            // .Function => .{ .function = try self.parseFunctionLiteral() },
            else => unreachable,
        };
    }

    fn parseInfix(self: *Parser, token: TokenTag, left: ast.Expression) !ast.Expression {
        self.advance();
        return switch (token) {
            .Plus, .Minus, .Slash, .Asterisk, .Equal, .NotEqual, .LessThan, .GreaterThan => .{ .expression = try self.parseInfixExpression(left) },
            // .LeftParen => .{ .call = try self.parseCallExpression(left) },
            // .LeftBracket => .{ .index = try self.parseIndexExpression(left) },
            else => ParserError.InvalidInfix,
        };
    }

    // Expressions
    fn parseIdentifier(self: *Parser) !ast.Identifier {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .Ident => |ident| .{ .value = ident },
                else => ParserError.ExpectedIdentifier,
            };
        } else {
            unreachable;
        }
    }

    fn parseInteger(self: *Parser) !i32 {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .Integer => |int| int,
                else => ParserError.ExpectedInteger,
            };
        } else {
            unreachable;
        }
    }

    fn parseString(self: *Parser) ![]const u8 {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .String => |str| str,
                else => ParserError.ExpectedStringLiteral,
            };
        } else {
            unreachable;
        }
    }

    fn parseBoolean(self: *Parser) !bool {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .True => true,
                .False => false,
                else => ParserError.ExpectedBoolean,
            };
        } else {
            unreachable;
        }
    }

    fn parseGroupedExpression(self: *Parser) !ast.Expression {
        self.advance();
        const expr = try self.parseExpression(.lowest);
        try self.expectPeek(.RightParen);
        return expr;
    }

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
    fn expectPeek(self: *Parser, expected: TokenTag) !void {
        if (self.peekTokenIs(expected)) {
            self.advance();
        } else {
            return ParserError.ExpectedPeek;
        }
    }

    fn peekPrecedence(self: *Parser) !Precedence {
        if (self.peek_token) |peek_token| {
            return tokenPrecedenceMap(peek_token);
        } else {
            return ParserError.ExpectedPeek;
        }
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
        std.debug.print("    peek_token: ", .{});
        if (self.peek_token) |peek_token| {
            peek_token.debugPrint();
        } else {
            std.debug.print("null", .{});
        }
        std.debug.print("\n}}", .{});
    }
};
