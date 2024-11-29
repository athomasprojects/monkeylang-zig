// Monkeylang AST

const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

// The AST is list of `Statement` nodes that make up the tree.
// The `Program` node is the root node of the tree.
//
// Statement 'list' should be a heap allocated list
//
// A `Statement` can be one of the following:
// 1. Let statement
// 2. Return statement
// 3. ExpressionStatement
// 4. BlockStatement

// Todo:
// [ ] Parse `Program`
// [ ] Parse `Let` statement
// [ ] Parse Return statement
// [ ] Parse `Expression` statement
// [ ] Parse `Block` statement

// 1. Parsing let statements
// `let` statements bint a value to the given identifier
//
// let statements have the form:
// let <identifier> = <expression>;
//
// let x = 5;

pub const Node = union(enum) {
    program: Program,
    statement: Statement,
    expression: Expression,
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
            else => unreachable,
        }
        std.debug.print(" }}", .{});
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    // integer: Integer,
    // boolean: Boolean,
    // string: String,
    prefix: PrefixExpression,
    infix: InfixExpression,
    // if_expression: IfExpression,
    // function: Function,
    // call: Call,
    // array: Array,
    // index: Index,
    noop: Token,

    pub fn debugPrint(self: Expression) void {
        std.debug.print("ast.Expression{{ .{s} = ", .{@tagName(self)});
        switch (self) {
            .identifier => |identifier| identifier.debugPrint(),
            // integer => ,
            // boolean => ,
            // string => ,
            // prefix => ,
            // infix => ,
            // if_expression => ,
            // function => ,
            // call => ,
            // array => ,
            // index => ,
            .noop => |token| token.debugPrint(),
            // else => unreachable,
        }
        std.debug.print(" }}", .{});
    }
};

pub const LetStatement = struct {
    name: Identifier,
    value: Expression,

    pub fn debugPrint(self: LetStatement) void {
        std.debug.print("ast.Identifier{{\n    name: ", .{});
        self.name.debugPrint();
        std.debug.print(",\n    value: ", .{});
        self.value.debugPrint();
        std.debug.print("\n}}", .{});
    }
};

pub const ReturnStatement = struct {
    value: Expression,

    pub fn debugPrint(self: ReturnStatement) void {
        std.debug.print("ast.ReturnStatement{{\n    value: ", .{});
        self.value.debugPrint();
        std.debug.print("\n}}", .{});
    }
};

pub const ExpressionStatement = struct {
    expression: Expression,
};

// Expressions
pub const Identifier = struct {
    value: []const u8,

    pub fn debugPrint(self: Identifier) void {
        std.debug.print("ast.Identifier{{ value = \"{s}\" }}", .{self.value});
    }
};

pub const PrefixExpression = struct {
    operator: Token,
    right: Expression,
};

pub const InfixExpression = struct {
    left: Expression,
    operator: Token,
    right: Expression,
};
