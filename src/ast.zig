const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const AllocPrintError = std.fmt.AllocPrintError;

pub const Program = struct {
    statements: ArrayList(Statement),

    pub fn printStatements(self: Program) void {
        for (self.statements.items) |s| {
            s.print();
            std.debug.print("\n", .{});
        }
    }

    // pub fn toString(self: Program, allocator: Allocator) !void {
    //     for (self.statements.items) |s| {
    //         try s.toString();
    //         std.debug.print("\n", .{});
    //     }
    // }
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
            },
            .return_statement => |return_statement| {
                return_statement.print();
            },
            .expression_statement => |expr| expr.print(),
        }
    }

    pub fn toString(self: Statement, allocator: Allocator) ![]u8 {
        switch (self) {
            .let_statement => |let_statement| {
                let_statement.toString(allocator);
            },
            .return_statement => |return_statement| {
                return_statement.toString(allocator);
            },
            .expression_statement => |expr| expr.toString(allocator),
        }
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

    pub fn toString(self: Expression, allocator: Allocator) ![]u8 {
        // std.debug.print("ast.Expression{{ .{s} = ", .{@tagName(self)});
        return switch (self) {
            .identifier => |identifier| try identifier.toString(allocator),
            .integer => |integer| try integer.toString(allocator),
            .boolean => |boolean| try boolean.toString(allocator),
            .string => |string| try string.toString(allocator),
            .prefix => |prefix| try prefix.toString(allocator),
            .infix => |infix| try infix.toString(allocator),
            // if_expression => ,
            // function => ,
            // call => ,
            // array => ,
            // index => ,
        };
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
        std.debug.print(";", .{});
    }

    pub fn toString(self: LetStatement, allocator: Allocator) ![]u8 {
        const value = try self.value.toString(allocator);
        const s = try std.fmt.allocPrint(allocator, "let {s} = {s};", .{ self.name.value, value });
        return s;
    }
};

pub const ReturnStatement = struct {
    value: *Expression,

    pub fn print(self: ReturnStatement) void {
        std.debug.print("return ", .{});
        self.value.print();
        std.debug.print(";", .{});
    }

    pub fn toString(self: ReturnStatement, allocator: Allocator) ![]u8 {
        const value = try self.value.toString(allocator);
        const s = try std.fmt.allocPrint(allocator, "return {s};", .{value});
        return s;
    }
};

pub const ExpressionStatement = struct {
    expression: *Expression,

    pub fn print(self: ExpressionStatement) void {
        self.expression.print();
    }

    pub fn toString(self: ExpressionStatement, allocator: Allocator) ![]u8 {
        const expr = try self.expression.toString(allocator);
        const s = try std.fmt.allocPrint(allocator, "{s}", .{expr});
        return s;
    }
};

// Expressions
pub const Identifier = struct {
    value: []const u8,

    pub fn print(self: Identifier) void {
        std.debug.print("{s}", .{self.value});
    }

    pub fn toString(self: Identifier, allocator: Allocator) ![]u8 {
        const s = try std.fmt.allocPrint(allocator, "{s}", .{self.value});
        return s;
    }
};

pub const Integer = struct {
    value: i32,

    pub fn print(self: Integer) void {
        std.debug.print("{d}", .{self.value});
    }

    pub fn toString(self: Integer, allocator: Allocator) ![]u8 {
        const s = try std.fmt.allocPrint(allocator, "{d}", .{self.value});
        return s;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn print(self: Boolean) void {
        std.debug.print("{}", .{self.value});
    }

    pub fn toString(self: Boolean, allocator: Allocator) ![]u8 {
        const s = try std.fmt.allocPrint(allocator, "{}", .{self.value});
        return s;
    }
};

pub const String = struct {
    value: []const u8,

    pub fn print(self: String) void {
        std.debug.print("\"{s}\"", .{self.value});
    }

    pub fn toString(self: String, allocator: Allocator) ![]u8 {
        const s = try std.fmt.allocPrint(allocator, "\"{s}\"", .{self.value});
        return s;
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

    pub fn toString(self: PrefixExpression, allocator: Allocator) AllocPrintError![]u8 {
        const operator = try self.operator.toString(allocator);
        const right = try self.right.toString(allocator);
        const s = try std.fmt.allocPrint(allocator, "({s}{s})", .{ operator, right });
        return s;
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

    pub fn toString(self: InfixExpression, allocator: Allocator) AllocPrintError![]u8 {
        const left = try self.left.toString(allocator);
        const operator = try self.operator.toString(allocator);
        const right = try self.right.toString(allocator);
        const s = try std.fmt.allocPrint(allocator, "({s} {s} {s})", .{ left, operator, right });
        return s;
    }
};

pub const Call = struct {
    callee: *Expression,
    args: ArrayList(Expression),
};
