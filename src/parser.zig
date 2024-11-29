const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const ParserError = error{
    ExpectOperator,
    ExpectPeek,
    ExpectedIdentifier,
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

    fn tokenPrecedence(token: Token) Precedence {
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
};

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

    fn parseExpression(self: *Parser, precedence: Precedence) !ast.Expression {
        return ParserError.ExpectedExpression;
    }

    fn parsePrefixExpression(self: *Parser) !ast.PrefixExpression {}

    fn parseInfixExpression(self: *Parser) !ast.PrefixExpression {}

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

    fn parsePrefix(self: *Parser, token: TokenTag) !ast.Expression {
        return switch (token) {
            .Ident => .{ .identifier = try self.parseIdentifier() },
            // .Integer => .{ .integer = try self.parseInteger() },
            // .String => .{ .string = try self.parseString() },
            // .True, .False => .{ .boolean = try self.parseBoolean() },
            // .Bang, .Minus => .{ .prefix = try self.parsePrefixExpression() },
            // .LeftParen => try self.parseGroupedExpression(),
            // .LeftBracket => .{ .array = try self.parseArray() },
            // .If => .{ .if_expression = try self.parseIfExpression() },
            // .Function => .{ .function = try self.parseFunctionLiteral() },
            else => ParserError.InvalidPrefix,
        };
    }

    fn parseInfix(self: *Parser, token: TokenTag, left: *ast.Expression) !ast.Expression {
        self.advance();
        return switch (token) {
            .Plus, .Minus, .Slash, .Asterisk, .Equal, .NotEqual, .LessThan, .GreaterThan => .{ .expression = try self.parseInfixExpression(left) },
            // .LeftParen => .{ .call = try self.parseCallExpression(left) },
            // .LeftBracket => .{ .index = try self.parseIndexExpression(left) },
            else => ParserError.InvalidInfix,
        };
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
            return ParserError.ExpectPeek;
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
