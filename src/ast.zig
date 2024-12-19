const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Program = struct {
    statements: ArrayList(Statement),

    pub fn printStatements(self: Program) void {
        for (self.statements.items) |s| {
            s.print();
            std.debug.print("\n", .{});
        }
    }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    // block_statement: BlockStatement,

    pub fn print(self: Statement) void {
        switch (self) {
            .let_statement => |let_statement| {
                let_statement.print();
                std.debug.print(";", .{});
            },
            .return_statement => |return_statement| {
                return_statement.print();
                std.debug.print(";", .{});
            },
            .expression_statement => |expr| expr.print(),
        }
    }

    // pub fn toString(self: Statement) ![]u8 {
    //     switch (self) {
    //         .letwstatement => |let_statement| {
    //             let_statement.toString();
    //             std.debug.print(";", .{});
    //         },
    //         .return_statement => |return_statement| {
    //             return_statement.toString();
    //             std.debug.print(";", .{});
    //         },
    //         .expression_statement => |expr| expr.toString(),
    //     }
    // }
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

    pub fn print(self: Expression) void {
        // std.debug.print("ast.Expression{{ .{s} = ", .{@tagName(self)});
        switch (self) {
            .identifier => |identifier| identifier.print(),
            .integer => |integer| integer.print(),
            .boolean => |boolean| boolean.print(),
            .string => |string| string.print(),
            .prefix => |prefix| prefix.print(),
            .infix => |infix| infix.print(),
            // if_expression => ,
            // function => ,
            // call => ,
            // array => ,
            // index => ,
        }
        // std.debug.print(" }}", .{});
    }
};

pub const LetStatement = struct {
    name: Identifier,
    value: *Expression,

    pub fn print(self: LetStatement) void {
        std.debug.print("let ", .{});
        self.name.print();
        std.debug.print(" = ", .{});
        self.value.print();
    }
};

pub const ReturnStatement = struct {
    value: *Expression,

    pub fn print(self: ReturnStatement) void {
        std.debug.print("return ", .{});
        self.value.print();
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,

    pub fn print(self: ExpressionStatement) void {
        self.expression.print();
    }
};

// Expressions
pub const Identifier = struct {
    value: []const u8,

    pub fn print(self: Identifier) void {
        std.debug.print("{s}", .{self.value});
    }
};

pub const Integer = struct {
    value: i32,

    pub fn print(self: Integer) void {
        std.debug.print("{}", .{self.value});
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn print(self: Boolean) void {
        std.debug.print("{}", .{self.value});
    }
};

pub const String = struct {
    value: []const u8,

    pub fn print(self: String) void {
        std.debug.print("\"{s}\"", .{self.value});
    }
};

pub const PrefixExpression = struct {
    operator: Token,
    right: *Expression,

    pub fn print(self: PrefixExpression) void {
        std.debug.print("(", .{});
        self.operator.print();
        self.right.print();
        std.debug.print(")", .{});
    }
};

pub const InfixExpression = struct {
    operator: Token,
    left: *Expression,
    right: *Expression,

    pub fn print(self: InfixExpression) void {
        std.debug.print("(", .{});
        self.left.print();
        std.debug.print(" ", .{});
        self.operator.print();
        std.debug.print(" ", .{});
        self.right.print();
        std.debug.print(")", .{});
    }
};

pub const Call = struct {
    callee: *Expression,
    args: ArrayList(Expression),
};
