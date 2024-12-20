const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").TokenTag;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const AllocPrintError = std.fmt.AllocPrintError;

pub const ToStringError = std.mem.Allocator.Error || AllocPrintError;

pub const Program = struct {
    statements: ArrayList(Statement),

    pub fn printStatements(self: Program) void {
        for (self.statements.items) |s| {
            s.print();
            std.debug.print("\n", .{});
        }
    }

    // pub fn toString(self: Program, allocator: Allocator) ![]u8 {
    //     var list = ArrayList(u8).init(allocator);
    //     defer list.deinit();
    //     for (self.statements.items) |stmt| {
    //         const str = try stmt.toString(allocator);
    //         const s = try std.fmt.allocPrint(allocator, "{s}\n", .{str});
    //         try list.appendSlice(s);
    //     }
    //     const s = try std.fmt.allocPrint(allocator, "{s}", .{list.items});
    //     return s;
    // }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    block_statement: BlockStatement,

    pub fn print(self: Statement) void {
        switch (self) {
            .let_statement => |let_statement| {
                let_statement.print();
            },
            .return_statement => |return_statement| {
                return_statement.print();
            },
            .expression_statement => |expr| expr.print(),
            .block_statement => |block| block.print(),
        }
    }

    pub fn toString(self: Statement, allocator: Allocator) ![]u8 {
        return switch (self) {
            .let_statement => |let_statement| try let_statement.toString(allocator),
            .return_statement => |return_statement| try return_statement.toString(allocator),
            .expression_statement => |expr| try expr.toString(allocator),
            .block_statement => |block| try block.toString(allocator),
        };
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer: Integer,
    boolean: Boolean,
    string: String,
    prefix: PrefixExpression,
    infix: InfixExpression,
    if_expression: IfExpression,
    function: FunctionLiteral,
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
            .if_expression => |if_expr| if_expr.print(),
            .function => |func| func.print(),
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
            .if_expression => |if_expr| try if_expr.toString(allocator),
            .function => |func| try func.toString(allocator),
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

pub const BlockStatement = struct {
    statements: ArrayList(Statement),

    pub fn print(self: BlockStatement) void {
        std.debug.print("{{\n", .{});
        for (self.statements.items) |stmt| {
            stmt.print();
            std.debug.print("\n", .{});
        }
        std.debug.print("}}", .{});
    }

    pub fn toString(self: BlockStatement, allocator: Allocator) ToStringError![]u8 {
        var list = ArrayList(u8).init(allocator);
        defer list.deinit();
        try list.appendSlice("{\n");
        for (self.statements.items) |stmt| {
            const str = try stmt.toString(allocator);
            try list.appendSlice(str);
            try list.append('\n');
        }
        try list.append('}');
        const s = try std.fmt.allocPrint(allocator, "{s}", .{list.items});
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

pub const IfExpression = struct {
    condition: *Expression,
    then_branch: *BlockStatement,
    else_branch: ?*BlockStatement = null,

    pub fn print(self: IfExpression) void {
        std.debug.print("if ", .{});
        self.condition.print();
        std.debug.print(" ", .{});
        self.then_branch.print();
        if (self.else_branch) |else_branch| {
            std.debug.print(" else ", .{});
            else_branch.print();
        }
    }

    pub fn toString(self: IfExpression, allocator: Allocator) ToStringError![]u8 {
        const condition = try self.condition.toString(allocator);
        const then_block = try self.then_branch.toString(allocator);
        var str: []u8 = undefined;
        if (self.else_branch) |else_branch| {
            const else_block = try else_branch.toString(allocator);
            str = try std.fmt.allocPrint(allocator, "if {s} {s} else {s}", .{ condition, then_block, else_block });
        } else {
            str = try std.fmt.allocPrint(allocator, "if {s} {s}", .{ condition, then_block });
        }
        return str;
    }
};

pub const FunctionLiteral = struct {
    parameters: ?ArrayList(Identifier) = null,
    body: *BlockStatement,

    pub fn print(self: FunctionLiteral) void {
        std.debug.print("fn(", .{});
        if (self.parameters) |parameters| {
            for (0..parameters.items.len, parameters.items) |idx, ident| {
                ident.print();
                if (idx < parameters.items.len - 1) {
                    std.debug.print(", ", .{});
                }
            }
        }
        std.debug.print(") ", .{});
        self.body.print();
    }

    pub fn toString(self: FunctionLiteral, allocator: Allocator) ToStringError![]u8 {
        var list = ArrayList(u8).init(allocator);
        defer list.deinit();
        if (self.parameters) |parameters| {
            for (0..parameters.items.len, parameters.items) |idx, ident| {
                try list.appendSlice(ident.value);
                if (idx < parameters.items.len - 1) {
                    try list.appendSlice(", ");
                }
            }
        }
        const body = try self.body.toString(allocator);
        const s = try std.fmt.allocPrint(allocator, "fn({s}) {s}", .{ list.items, body });
        return s;
    }
};

pub const Call = struct {
    callee: *Expression,
    args: ArrayList(Expression),
};
