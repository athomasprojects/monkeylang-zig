const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Node = union(enum) {
    program: *Program,
    statement: *Statement,
    expression: *Expression,
};

pub const Program = struct {
    statements: ArrayList(Statement),
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    // block_statement: BlockStatement,

    pub fn debugPrint(self: Statement) void {
        std.debug.print("ast.Statement{{ .{s} = ", .{@tagName(self)});
        switch (self) {
            .let_statement => |let_statement| let_statement.debugPrint(),
            .return_statement => |return_statement| return_statement.debugPrint(),
            .expression_statement => |expr| expr.debugPrint(),
        }
        std.debug.print(" }}", .{});
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer: Integer,
    boolean: Boolean,
    string: String,
    prefix: PrefixExpression,
    infix: InfixExpression,
    // if_expression: IfExpression,
    // function: Function,
    // call: Call,
    // array: Array,
    // index: Index,

    pub fn debugPrint(self: Expression) void {
        std.debug.print("ast.Expression{{ .{s} = ", .{@tagName(self)});
        switch (self) {
            .identifier => |identifier| identifier.debugPrint(),
            .integer => |integer| integer.debugPrint(),
            .boolean => |boolean| boolean.debugPrint(),
            .string => |string| string.debugPrint(),
            .prefix => |prefix| prefix.debugPrint(),
            .infix => |infix| infix.debugPrint(),
            // if_expression => ,
            // function => ,
            // call => ,
            // array => ,
            // index => ,
        }
        std.debug.print(" }}", .{});
    }
};

pub const LetStatement = struct {
    name: Identifier,
    value: *Expression,

    pub fn debugPrint(self: LetStatement) void {
        std.debug.print("ast.Identifier{{\n    name: ", .{});
        self.name.debugPrint();
        std.debug.print(",\n    value: ", .{});
        self.value.debugPrint();
        std.debug.print("\n}}", .{});
    }
};

pub const ReturnStatement = struct {
    value: *Expression,

    pub fn debugPrint(self: ReturnStatement) void {
        std.debug.print("ast.ReturnStatement{{\n    value: ", .{});
        self.value.debugPrint();
        std.debug.print("\n}}", .{});
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,

    pub fn debugPrint(self: ExpressionStatement) void {
        std.debug.print("ast.ExpressionStatement{{\n    expression = ", .{});
        self.expression.debugPrint();
        std.debug.print("\n}}", .{});
    }
};

// Expressions
pub const Identifier = struct {
    value: []const u8,

    pub fn debugPrint(self: Identifier) void {
        std.debug.print("ast.Identifier{{ value = \"{s}\" }}", .{self.value});
    }
};

pub const Integer = struct {
    value: i32,

    pub fn debugPrint(self: Integer) void {
        std.debug.print("{}", .{self});
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn debugPrint(self: Boolean) void {
        std.debug.print("{}", .{self});
    }
};

pub const String = struct {
    value: []const u8,

    pub fn debugPrint(self: String) void {
        std.debug.print("ast.String{{ .value = \"{s}\" }}", .{self.value});
    }
};

pub const PrefixExpression = struct {
    operator: Token,
    right: *Expression,

    pub fn debugPrint(self: PrefixExpression) void {
        std.debug.print("ast.PrefixExpression{{\n", .{});
        std.debug.print("    operator: ", .{});
        self.operator.debugPrint();
        std.debug.print(",\n    right: ", .{});
        self.right.debugPrint();
        std.debug.print(" }}", .{});
    }
};

pub const InfixExpression = struct {
    operator: Token,
    left: *Expression,
    right: *Expression,

    pub fn debugPrint(self: InfixExpression) void {
        std.debug.print("ast.InfixExpression{{\n", .{});
        std.debug.print("    operator: ", .{});
        self.operator.debugPrint();
        std.debug.print(",\n    left: ", .{});
        self.left.debugPrint();
        std.debug.print(",\n    right: ", .{});
        self.right.debugPrint();
        std.debug.print(" }}", .{});
    }
};
