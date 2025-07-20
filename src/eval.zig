const std = @import("std");
const ast = @import("ast.zig");
const obj = @import("object.zig");
const Token = @import("token.zig").Token;
const Allocator = std.mem.Allocator;
const StaticStringMap = std.static_string_map.StaticStringMap;

pub const EvaluatorError = error{
    FailedAlloc,
    FailedDivision,
    InvalidCondition,
};

const BUILTIN_TRUE: obj.Object = .{ .boolean = true };
const BUILTIN_FALSE: obj.Object = .{ .boolean = false };
const BUILTIN_NULL: obj.Object = .null_;

var TRUE_OBJECT: obj.Object = BUILTIN_TRUE;
var FALSE_OBJECT: obj.Object = BUILTIN_FALSE;
var NULL_OBJECT: obj.Object = BUILTIN_NULL;

pub const Evaluator = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Evaluator {
        return .{ .allocator = allocator };
    }

    pub fn evalProgram(self: *Evaluator, program: *ast.Program) !*obj.Object {
        var result: *obj.Object = undefined;
        for (program.statements.items) |*statement| {
            result = try self.evalStatement(statement);
            switch (result.*) {
                .return_ => |return_value| return return_value.value,
                .error_ => return result,
                else => {},
            }
        }
        return result;
    }

    fn evalStatement(self: *Evaluator, statement: *const ast.Statement) !*obj.Object {
        return switch (statement.*) {
            .expression_statement => |expression_statement| try self.evalExpression(expression_statement.expression),
            .block_statement => |*block_statement| {
                var result: *obj.Object = undefined;
                for (block_statement.statements.items) |*stmt| {
                    result = try self.evalStatement(stmt);
                    switch (result.*) {
                        .return_, .error_ => return result,
                        else => {},
                    }
                }
                return result;
            },
            .return_statement => |return_statement| {
                const value: *obj.Object = try self.evalExpression(return_statement.value);
                const object_ptr: *obj.Object = self.allocator.create(obj.Object) catch return EvaluatorError.FailedAlloc;
                object_ptr.* = obj.Object{ .return_ = obj.ReturnValue{ .value = value } };
                return object_ptr;
            },
            else => &NULL_OBJECT,
        };
    }

    fn evalExpression(self: *Evaluator, expr: *ast.Expression) !*obj.Object {
        return switch (expr.*) {
            .integer => |integer| try self.createIntegerObject(integer.value),
            .boolean => |boolean| Evaluator.createBooleanObject(boolean.value),
            .string => |string| try self.createStringObject(string.value),
            .prefix => |prefix_expr| {
                const right = try self.evalExpression(prefix_expr.right);
                switch (right.*) {
                    .error_ => return right,
                    else => {},
                }
                return try self.evalPrefixExpression(&prefix_expr.operator, right);
            },
            .infix => |infix_expr| {
                const left = try self.evalExpression(infix_expr.left);
                switch (left.*) {
                    .error_ => return left,
                    else => {},
                }

                const right = try self.evalExpression(infix_expr.right);
                switch (right.*) {
                    .error_ => return right,
                    else => {},
                }

                return try self.evalInfixExpression(&infix_expr.operator, left, right);
            },
            .if_expression => |if_expr| try self.evalIfExpression(&if_expr),
            else => &NULL_OBJECT,
        };
    }

    fn createIntegerObject(self: *Evaluator, integer: i32) !*obj.Object {
        const integer_ptr: *obj.Object = self.allocator.create(obj.Object) catch return EvaluatorError.FailedAlloc;
        integer_ptr.* = obj.Object{ .integer = integer };
        return integer_ptr;
    }

    fn createBooleanObject(boolean: bool) *obj.Object {
        if (boolean) {
            return &TRUE_OBJECT;
        } else {
            return &FALSE_OBJECT;
        }
    }

    fn createStringObject(self: *Evaluator, string: []const u8) !*obj.Object {
        const string_ptr: *obj.Object = self.allocator.create(obj.Object) catch return EvaluatorError.FailedAlloc;
        string_ptr.* = obj.Object{ .string = string };
        return string_ptr;
    }

    fn evalPrefixExpression(self: *Evaluator, operator: *const Token, right: *obj.Object) !*obj.Object {
        return switch (operator.*) {
            .Bang => try evalBangOperatorExpression(right),
            .Minus => try self.evalMinusOperatorExpression(right),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.FailedAlloc;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s}{s}",
                    .{ operator_string, right.typeName() },
                ) catch return EvaluatorError.FailedAlloc;
                return try self.newError(msg);
            },
        };
    }

    fn evalInfixExpression(self: *Evaluator, operator: *const Token, left: *obj.Object, right: *obj.Object) !*obj.Object {
        return switch (left.*) {
            .integer => switch (right.*) {
                .integer => try self.evalIntegerInfixExpression(operator, left, right),
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.FailedAlloc;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{ left.typeName(), operator_string, right.typeName() },
                    ) catch return EvaluatorError.FailedAlloc;
                    return try self.newError(msg);
                },
            },
            .string => switch (right.*) {
                .string => switch (operator.*) {
                    .Equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
                    .NotEqual => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
                    else => {
                        const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.FailedAlloc;
                        const msg: []const u8 = std.fmt.allocPrint(
                            self.allocator,
                            "unknown operator: {s} {s} {s}",
                            .{ left.typeName(), operator_string, right.typeName() },
                        ) catch return EvaluatorError.FailedAlloc;
                        return try self.newError(msg);
                    },
                },
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.FailedAlloc;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{ left.typeName(), operator_string, right.typeName() },
                    ) catch return EvaluatorError.FailedAlloc;
                    return try self.newError(msg);
                },
            },
            else => switch (operator.*) {
                .Equal => nativeBoolToBooleanObject(std.meta.eql(left.*, right.*)),
                .NotEqual => nativeBoolToBooleanObject(!std.meta.eql(left.*, right.*)),
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.FailedAlloc;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "unknown operator: {s} {s} {s}",
                        .{ left.typeName(), operator_string, right.typeName() },
                    ) catch return EvaluatorError.FailedAlloc;
                    return try self.newError(msg);
                },
            },
        };
    }

    fn evalIntegerInfixExpression(self: *Evaluator, operator: *const Token, left: *obj.Object, right: *obj.Object) !*obj.Object {
        const left_val = left.integer;
        const right_val = right.integer;
        return switch (operator.*) {
            .Plus => try self.createIntegerObject(left_val + right_val),
            .Minus => try self.createIntegerObject(left_val - right_val),
            .Asterisk => try self.createIntegerObject(left_val * right_val),
            .Slash => {
                const division = std.math.divExact(i32, left_val, right_val) catch return EvaluatorError.FailedDivision;
                return try self.createIntegerObject(division);
            },
            .GreaterThan => nativeBoolToBooleanObject(left_val > right_val),
            .LessThan => nativeBoolToBooleanObject(left_val < right_val),
            .Equal => nativeBoolToBooleanObject(left_val == right_val),
            .NotEqual => nativeBoolToBooleanObject(left_val != right_val),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.FailedAlloc;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s} {s} {s}",
                    .{ left.typeName(), operator_string, right.typeName() },
                ) catch return EvaluatorError.FailedAlloc;
                return try self.newError(msg);
            },
        };
    }

    fn evalStringInfixExpression(self: *Evaluator, operator: *const Token, left: *obj.Object, right: *obj.Object) !*obj.Object {
        return switch (operator.*) {
            .Equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
            .NotEqual => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.FailedAlloc;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s} {s} {s}",
                    .{ left.typeName(), operator_string, right.typeName() },
                ) catch return EvaluatorError.FailedAlloc;
                return try self.newError(msg);
            },
        };
    }

    fn evalBangOperatorExpression(right: *obj.Object) !*obj.Object {
        return switch (right.*) {
            .boolean => |boolean| if (boolean) &FALSE_OBJECT else &TRUE_OBJECT,
            else => &FALSE_OBJECT,
        };
    }

    fn evalMinusOperatorExpression(self: *Evaluator, right: *obj.Object) !*obj.Object {
        return switch (right.*) {
            .integer => |value| try self.createIntegerObject(-value),
            else => {
                const msg: []u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: -{s}",
                    .{right.typeName()},
                ) catch return EvaluatorError.FailedAlloc;
                return try self.newError(msg);
            },
        };
    }

    fn evalIfExpression(self: *Evaluator, if_expr: *const ast.IfExpression) EvaluatorError!*obj.Object {
        const condition: *obj.Object = self.evalExpression(if_expr.condition) catch return EvaluatorError.InvalidCondition;
        switch (condition.*) {
            .error_ => return condition,
            else => {},
        }

        if (isTruthy(condition)) {
            return try self.evalStatement(&ast.Statement{ .block_statement = if_expr.then_branch.* });
        } else if (if_expr.else_branch) |else_branch| {
            return try self.evalStatement(&ast.Statement{ .block_statement = else_branch.* });
        } else {
            return &NULL_OBJECT;
        }
    }

    fn nativeBoolToBooleanObject(native: bool) *obj.Object {
        return if (native) &TRUE_OBJECT else &FALSE_OBJECT;
    }

    fn isTruthy(object: *obj.Object) bool {
        return switch (object.*) {
            .null_ => false,
            .boolean => |boolean| boolean,
            else => true,
        };
    }

    fn newError(self: *Evaluator, message: []const u8) !*obj.Object {
        const error_ptr: *obj.Object = self.allocator.create(obj.Object) catch return EvaluatorError.FailedAlloc;
        error_ptr.* = .{ .error_ = obj.Error{ .message = message } };
        return error_ptr;
    }
};
