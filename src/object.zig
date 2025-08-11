const std = @import("std");
const ast = @import("ast.zig");
const Environment = @import("environment.zig").Environment;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ToStringError = ast.ToStringError;
const EvaluatorError = @import("Evaluator.zig").EvaluatorError;
const BuiltinTagType = @import("builtins.zig").TagType;

pub const Object = union(enum) {
    null_,
    boolean: bool,
    integer: i64,
    string: []const u8,
    return_: ReturnValue,
    function: Function,
    error_: Error,
    builtin: BuiltinFunction,
    array: ArrayLiteral,

    pub fn print(self: Object) void {
        switch (self) {
            .null_ => std.debug.print("null", .{}),
            .integer => |integer| std.debug.print("{d}", .{integer}),
            .boolean => |boolean| std.debug.print("{}", .{boolean}),
            .string => |string| std.debug.print("\"{s}\"", .{string}),
            .function => |function| function.print(),
            .return_ => |return_value| return_value.value.print(),
            .error_ => |error_| error_.print(),
            .builtin => |builtin_fn| builtin_fn.print(),
            .array => |array_literal| array_literal.print(),
        }
    }

    pub fn toString(self: Object, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .null_ => try std.fmt.allocPrint(allocator, "null", .{}),
            .integer => |integer| try std.fmt.allocPrint(
                allocator,
                "{d}",
                .{integer},
            ),
            .boolean => |boolean| try std.fmt.allocPrint(
                allocator,
                "{}",
                .{boolean},
            ),
            .string => |string| try std.fmt.allocPrint(
                allocator,
                "\"{s}\"",
                .{string},
            ),
            .function => |function| function.toString(allocator),
            .return_ => |return_value| return_value.value.toString(allocator),
            .error_ => |error_| try error_.toString(allocator),
            .builtin => |builtin_fn| try builtin_fn.toString(allocator),
            .array => |array_literal| try array_literal.toString(allocator),
        };
    }

    pub fn typeName(self: Object) []const u8 {
        return switch (self) {
            .null_ => "NULL",
            .integer => "INTEGER",
            .boolean => "BOOLEAN",
            .string => "STRING",
            .function => "FUNCTION",
            .return_ => unreachable,
            .error_ => "ERROR",
            .builtin => "BUILTIN",
            .array => "ARRAY",
        };
    }
};

pub const ReturnValue = struct {
    value: *Object,
};

pub const Function = struct {
    parameters: ?ArrayList(ast.Identifier) = null,
    body: *ast.BlockStatement,
    env: *Environment,

    pub fn print(self: Function) void {
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

    pub fn toString(self: Function, allocator: Allocator) ToStringError![]u8 {
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
        return try std.fmt.allocPrint(allocator, "fn({s}) {s}", .{ list.items, body });
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn print(self: Error) void {
        std.debug.print("{s}", .{self.message});
    }

    pub fn toString(self: Error, allocator: std.mem.Allocator) ![]u8 {
        return try std.fmt.allocPrint(allocator, "{s}", .{self.message});
    }
};

pub const BuiltinFunction = struct {
    func: *const fn (Allocator, []*Object) EvaluatorError!*Object,
    tag: BuiltinTagType,

    pub fn call(self: BuiltinFunction, allocator: Allocator, args: []*Object) !*Object {
        return try self.func(allocator, args);
    }

    pub fn print(self: BuiltinFunction) void {
        std.debug.print("BuiltinFunction#{s}", .{@tagName(self.tag)});
    }

    pub fn toString(self: BuiltinFunction, allocator: Allocator) ToStringError![]u8 {
        return try std.fmt.allocPrint(
            allocator,
            "BuiltinFunction#{s}",
            .{@tagName(self.tag)},
        );
    }
};

pub const ArrayLiteral = struct {
    elements: ?ArrayList(*Object),

    pub const empty: ArrayLiteral = .{ .elements = null };

    pub fn print(self: ArrayLiteral) void {
        std.debug.print("[", .{});
        if (self.elements) |elements| {
            for (0..elements.items.len, elements.items) |idx, elem| {
                elem.print();
                if (idx < elements.items.len - 1) std.debug.print(", ", .{});
            }
        }
        std.debug.print("]", .{});
    }

    pub fn toString(self: ArrayLiteral, allocator: Allocator) ToStringError![]u8 {
        var strings = ArrayList(u8).init(allocator);
        defer strings.deinit();

        if (self.elements) |elements| {
            for (0..elements.items.len, elements.items) |idx, elem| {
                const expr = try elem.toString(allocator);
                try strings.appendSlice(expr);
                if (idx < elements.items.len - 1) try strings.appendSlice(", ");
            }
            return std.fmt.allocPrint(allocator, "[{s}]", .{strings.items});
        } else return std.fmt.allocPrint(allocator, "[]", .{});
    }
};
