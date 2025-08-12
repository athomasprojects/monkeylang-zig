const std = @import("std");
const Token = @import("token.zig").Token;
const TokenTag = @import("token.zig").Tag;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const ToStringError = Allocator.Error || std.fmt.AllocPrintError;

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
    call: Call,
    array_literal: ArrayLiteral,
    index_expression: IndexExpression,
    hash_literal: HashLiteral,

    pub fn print(self: Expression) void {
        switch (self) {
            .identifier => |identifier| identifier.print(),
            .integer => |integer| integer.print(),
            .boolean => |boolean| boolean.print(),
            .string => |string| string.print(),
            .prefix => |prefix| prefix.print(),
            .infix => |infix| infix.print(),
            .if_expression => |if_expr| if_expr.print(),
            .function => |func| func.print(),
            .call => |call| call.print(),
            .array_literal => |array| array.print(),
            .index_expression => |index| index.print(),
            .hash_literal => |hash_literal| hash_literal.print(),
        }
    }

    pub fn toString(self: Expression, allocator: Allocator) ![]u8 {
        return switch (self) {
            .identifier => |identifier| try identifier.toString(allocator),
            .integer => |integer| try integer.toString(allocator),
            .boolean => |boolean| try boolean.toString(allocator),
            .string => |string| try string.toString(allocator),
            .prefix => |prefix| try prefix.toString(allocator),
            .infix => |infix| try infix.toString(allocator),
            .if_expression => |if_expr| try if_expr.toString(allocator),
            .function => |func| try func.toString(allocator),
            .call => |call| try call.toString(allocator),
            .array_literal => |array| try array.toString(allocator),
            .index_expression => |index| try index.toString(allocator),
            .hash_literal => |hash_literal| try hash_literal.toString(allocator),
        };
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
    value: i64,

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

    pub fn toString(self: PrefixExpression, allocator: Allocator) ToStringError![]u8 {
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

    pub fn toString(self: InfixExpression, allocator: Allocator) ToStringError![]u8 {
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
    else_branch: ?*BlockStatement,

    pub fn init(condition: *Expression, then_branch: *BlockStatement) IfExpression {
        return .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = null,
        };
    }

    pub fn initElseBranch(condition: *Expression, then_branch: *BlockStatement, else_branch: *BlockStatement) IfExpression {
        return .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
    }

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
    body: *BlockStatement,
    parameters: ?ArrayList(Identifier),

    pub fn print(self: FunctionLiteral) void {
        std.debug.print("fn(", .{});
        if (self.parameters) |parameters| {
            const max = parameters.items.len - 1;
            for (0..parameters.items.len, parameters.items) |idx, ident| {
                ident.print();
                if (idx < max) std.debug.print(", ", .{});
            }
        }
        std.debug.print(") ", .{});
        self.body.print();
    }

    pub fn toString(self: FunctionLiteral, allocator: Allocator) ToStringError![]u8 {
        var list = ArrayList(u8).init(allocator);
        defer list.deinit();
        if (self.parameters) |parameters| {
            const max = parameters.items.len - 1;
            for (0..parameters.items.len, parameters.items) |idx, ident| {
                try list.appendSlice(ident.value);
                if (idx < max) try list.appendSlice(", ");
            }
        }
        const body = try self.body.toString(allocator);
        const s = try std.fmt.allocPrint(allocator, "fn({s}) {s}", .{ list.items, body });
        return s;
    }
};

pub const Call = struct {
    callee: *Expression,
    args: ?ArrayList(Expression),

    pub fn init(callee: *Expression, args: ArrayList(Expression)) Call {
        return .{
            .callee = callee,
            .args = args,
        };
    }

    pub fn empty(callee: *Expression) Call {
        return .{
            .callee = callee,
            .args = null,
        };
    }

    pub fn print(self: Call) void {
        self.callee.print();
        std.debug.print("(", .{});
        if (self.args) |args| {
            const max = args.items.len - 1;
            for (0..args.items.len, args.items) |idx, arg| {
                arg.print();
                if (idx < max) std.debug.print(", ", .{});
            }
        }
        std.debug.print(")", .{});
    }

    pub fn toString(self: Call, allocator: Allocator) ToStringError![]u8 {
        const callee = try self.callee.toString(allocator);

        var list = ArrayList(u8).init(allocator);
        defer list.deinit();

        if (self.args) |args| {
            const max = args.items.len - 1;
            for (0..args.items.len, args.items) |idx, arg| {
                const expr = try arg.toString(allocator);
                try list.appendSlice(expr);
                if (idx < max) try list.appendSlice(", ");
            }
        }
        const s = try std.fmt.allocPrint(allocator, "{s}({s})", .{ callee, list.items });
        return s;
    }
};

pub const ArrayLiteral = struct {
    elements: ?ArrayList(Expression),

    pub const empty: ArrayLiteral = .{ .elements = null };

    pub fn print(self: ArrayLiteral) void {
        std.debug.print("[", .{});
        if (self.elements) |elements| {
            const max = elements.items.len - 1;
            for (0..elements.items.len, elements.items) |idx, elem| {
                elem.print();
                if (idx < max) std.debug.print(", ", .{});
            }
        }
        std.debug.print("]", .{});
    }

    pub fn toString(self: ArrayLiteral, allocator: Allocator) ToStringError![]u8 {
        var strings = ArrayList(u8).init(allocator);
        defer strings.deinit();

        if (self.elements) |elements| {
            const max = elements.items.len - 1;
            for (0..elements.items.len, elements.items) |idx, elem| {
                const expr = try elem.toString(allocator);
                try strings.appendSlice(expr);
                if (idx < max) try strings.appendSlice(", ");
            }
            return std.fmt.allocPrint(allocator, "[{s}]", .{strings.items});
        } else return std.fmt.allocPrint(allocator, "[]", .{});
    }
};

pub const IndexExpression = struct {
    left: *Expression,
    index: *Expression,

    pub fn print(self: IndexExpression) void {
        self.left.print();
        std.debug.print("[", .{});
        self.index.print();
        std.debug.print("]", .{});
    }

    pub fn toString(self: IndexExpression, allocator: Allocator) ToStringError![]u8 {
        const left = try self.left.toString(allocator);
        const index = try self.index.toString(allocator);
        return std.fmt.allocPrint(allocator, "{s}[{s}]", .{ left, index });
    }
};

pub const HashLiteral = struct {
    entries: ?ArrayList(Entry),

    pub const empty: HashLiteral = .{ .entries = null };

    pub const Entry = struct {
        key: Expression,
        value: Expression,

        pub fn print(self: Entry) void {
            self.key.print();
            std.debug.print(": ", .{});
            self.value.print();
        }

        pub fn toString(self: Entry, allocator: Allocator) ToStringError![]u8 {
            const key = try self.key.toString(allocator);
            const value = try self.value.toString(allocator);
            return std.fmt.allocPrint(
                allocator,
                "{s}: {s}",
                .{ key, value },
            );
        }
    };

    pub fn init(entries: ArrayList(Entry)) HashLiteral {
        return .{ .entries = entries };
    }

    pub fn print(self: HashLiteral) void {
        std.debug.print("{{", .{});
        if (self.entries) |entries| {
            const max = entries.items.len - 1;
            for (0..entries.items.len, entries.items) |idx, entry| {
                entry.print();
                if (idx < max) std.debug.print(", ", .{});
            }
        }
        std.debug.print("}}", .{});
    }

    pub fn toString(self: HashLiteral, allocator: Allocator) ToStringError![]u8 {
        var strings = ArrayList(u8).init(allocator);
        defer strings.deinit();
        if (self.entries) |entries| {
            const max = entries.items.len - 1;
            for (0..entries.items.len, entries.items) |idx, entry| {
                const s = try entry.toString(allocator);
                try strings.appendSlice(s);
                if (idx < max) try strings.appendSlice(", ");
            }
            return std.fmt.allocPrint(
                allocator,
                "{{{s}}}",
                .{strings.items},
            );
        }
        return std.fmt.allocPrint(allocator, "{{}}", .{});
    }
};
