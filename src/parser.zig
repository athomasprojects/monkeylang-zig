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

const Operator = union(enum) {
    assign,
    bang,
    plus,
    minus,
    asterisk,
    slash,
    equal,
    not_equal,
    less_than,
    greater_than,

    fn fromToken(token: Token) !Operator {
        return switch (token) {
            .Assign => .assign,
            .Bang => .bang,
            .Plus => .plus,
            .Minus => .minus,
            .Asterisk => .asterisk,
            .Slash => .slash,
            .Equal => .equal,
            .NotEqual => .not_equal,
            .LessThan => .less_than,
            .GreaterThan => .greater_than,
            else => ParserError.ExpectOperator,
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
            s.debugPrint();
            std.debug.print("\n", .{});
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

    fn parseStatement(self: *Parser) !ast.Statement {
        if (self.current_token) |current_token| {
            return switch (current_token) {
                .Let => ast.Statement{ .let_statement = try self.parseLetStatement() },
                // .Return => ast.Statement{ .return_statement = try self.parseReturnStatement() },
                else => ParserError.ExpectedStatement,
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

    fn parseReturnStatement(_: *Parser) !ast.ReturnStatement {
        return ParserError.ExpectedReturn;
    }

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
