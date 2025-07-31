const std = @import("std");
const ast = @import("ast.zig");
const builtin = @import("builtin.zig");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;
const ErrorObject = @import("object.zig").Error;
const Function = @import("object.zig").Function;
const ReturnValue = @import("object.zig").ReturnValue;
const Environment = @import("environment.zig").Environment;

const Allocator = std.mem.Allocator;
const StaticStringMap = std.static_string_map.StaticStringMap;
const ArrayList = std.ArrayList;

pub const EvaluatorError = error{
    FailedDivision,
    InvalidCondition,
    OutOfMemory,
};

pub const Evaluator = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Evaluator {
        return .{ .allocator = allocator };
    }

    pub fn evalProgram(self: *Evaluator, program: *ast.Program, scope: *Environment) !*Object {
        var result: *Object = undefined;
        for (program.statements.items) |*statement| {
            result = try self.evalStatement(statement, scope);
            switch (result.*) {
                .return_ => |return_value| return return_value.value,
                .error_ => return result,
                else => {},
            }
        } else return result;
    }

    fn evalStatement(self: *Evaluator, statement: *const ast.Statement, scope: *Environment) !*Object {
        return sw: switch (statement.*) {
            .expression_statement => |expression_statement| try self.evalExpression(expression_statement.expression, scope),
            .let_statement => |let_statement| {
                const value = try self.evalExpression(let_statement.value, scope);
                switch (value.*) {
                    .error_ => break :sw value,
                    else => {
                        // Bind evaluated expression to identifier in the current scope.
                        scope.bind(let_statement.name.value, value) catch break :sw EvaluatorError.OutOfMemory;
                        break :sw &builtin.NULL;
                    },
                }
            },
            .block_statement => |*block_statement| {
                var result: *Object = &builtin.NULL;
                for (block_statement.statements.items) |*stmt| {
                    result = try self.evalStatement(stmt, scope);
                    switch (result.*) {
                        .return_, .error_ => break :sw result,
                        else => {},
                    }
                }
                break :sw result;
            },
            .return_statement => |return_statement| {
                const value: *Object = try self.evalExpression(return_statement.value, scope);
                const object_ptr: *Object = self.allocator.create(Object) catch break :sw EvaluatorError.OutOfMemory;
                object_ptr.* = .{ .return_ = .{ .value = value } };
                break :sw object_ptr;
            },
        };
    }

    fn evalExpression(self: *Evaluator, expr: *ast.Expression, scope: *Environment) !*Object {
        return sw: switch (expr.*) {
            .integer => |integer| try self.createIntegerObject(integer.value),
            .boolean => |boolean| Evaluator.createBooleanObject(boolean.value),
            .string => |string| try self.createStringObject(string.value),
            .prefix => |prefix_expr| {
                const right = try self.evalExpression(prefix_expr.right, scope);
                switch (right.*) {
                    .error_ => break :sw right,
                    else => break :sw try self.evalPrefixExpression(&prefix_expr.operator, right),
                }
            },
            .infix => |infix_expr| {
                const left = try self.evalExpression(infix_expr.left, scope);
                switch (left.*) {
                    .error_ => break :sw left,
                    else => {
                        const right = try self.evalExpression(infix_expr.right, scope);
                        switch (right.*) {
                            .error_ => break :sw right,
                            else => break :sw try self.evalInfixExpression(&infix_expr.operator, left, right),
                        }
                    },
                }
            },
            .if_expression => |if_expr| try self.evalIfExpression(&if_expr, scope),
            .identifier => |identifier| try self.evalIdentifier(&identifier, scope),
            .function => |function| try self.createFunctionLiteralObject(&function, scope),
            .call => |call| {
                const function = try self.evalExpression(call.callee, scope);
                switch (function.*) {
                    .error_ => break :sw function,
                    else => {
                        var evaled: *Object = &builtin.NULL;
                        var evaled_args = std.ArrayList(*Object).init(self.allocator);
                        if (call.args) |args| {
                            for (args.items) |*arg| {
                                evaled = try self.evalExpression(arg, scope);
                                switch (evaled.*) {
                                    .error_ => break :sw evaled,
                                    else => evaled_args.append(evaled) catch break :sw EvaluatorError.OutOfMemory,
                                }
                            }
                        }
                        break :sw try self.applyFunction(function, evaled_args.items);
                    },
                }
            },
        };
    }

    fn createIntegerObject(self: *Evaluator, integer: i64) !*Object {
        const integer_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.OutOfMemory;
        integer_ptr.* = .{ .integer = integer };
        return integer_ptr;
    }

    fn createBooleanObject(boolean: bool) *Object {
        return if (boolean) &builtin.TRUE else &builtin.FALSE;
    }

    fn createStringObject(self: *Evaluator, string: []const u8) !*Object {
        const string_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.OutOfMemory;
        string_ptr.* = .{ .string = string };
        return string_ptr;
    }

    fn createFunctionLiteralObject(self: *Evaluator, func_literal: *const ast.FunctionLiteral, scope: *Environment) !*Object {
        const func_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.OutOfMemory;
        func_ptr.* = .{
            .function = .{
                .parameters = func_literal.parameters,
                .body = func_literal.body,
                .env = scope,
            },
        };
        return func_ptr;
    }

    fn evalPrefixExpression(self: *Evaluator, operator: *const Token, right: *Object) !*Object {
        return sw: switch (operator.*) {
            .Bang => try evalBangOperatorExpression(right),
            .Minus => try self.evalMinusOperatorExpression(right),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.OutOfMemory;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s}{s}",
                    .{ operator_string, right.typeName() },
                ) catch break :sw EvaluatorError.OutOfMemory;
                break :sw try self.createError(msg);
            },
        };
    }

    fn evalInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
        return sw: switch (left.*) {
            .integer => switch (right.*) {
                .integer => try self.evalIntegerInfixExpression(operator, left, right),
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.OutOfMemory;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    ) catch break :sw EvaluatorError.OutOfMemory;
                    break :sw try self.createError(msg);
                },
            },
            .string => switch (right.*) {
                .string => switch (operator.*) {
                    .Equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
                    .NotEqual => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
                    .Plus => {
                        // Concatenate strings
                        const concatenated_string: []const u8 = std.fmt.allocPrint(
                            self.allocator,
                            "{s}{s}",
                            .{ left.string, right.string },
                        ) catch break :sw EvaluatorError.OutOfMemory;
                        break :sw try self.createStringObject(concatenated_string);
                    },
                    else => {
                        const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.OutOfMemory;
                        const msg: []const u8 = std.fmt.allocPrint(
                            self.allocator,
                            "unknown operator: {s} {s} {s}",
                            .{
                                left.typeName(),
                                operator_string,
                                right.typeName(),
                            },
                        ) catch break :sw EvaluatorError.OutOfMemory;
                        break :sw try self.createError(msg);
                    },
                },
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.OutOfMemory;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    ) catch break :sw EvaluatorError.OutOfMemory;
                    break :sw try self.createError(msg);
                },
            },
            else => switch (operator.*) {
                .Equal => nativeBoolToBooleanObject(std.meta.eql(left.*, right.*)),
                .NotEqual => nativeBoolToBooleanObject(!std.meta.eql(left.*, right.*)),
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.OutOfMemory;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "unknown operator: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    ) catch break :sw EvaluatorError.OutOfMemory;
                    break :sw try self.createError(msg);
                },
            },
        };
    }

    fn evalIntegerInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
        return sw: switch (operator.*) {
            .Plus => try self.createIntegerObject(left.integer + right.integer),
            .Minus => try self.createIntegerObject(left.integer - right.integer),
            .Asterisk => try self.createIntegerObject(left.integer * right.integer),
            .Slash => {
                const division = std.math.divExact(i64, left.integer, right.integer) catch break :sw EvaluatorError.FailedDivision;
                break :sw try self.createIntegerObject(division);
            },
            .GreaterThan => nativeBoolToBooleanObject(left.integer > right.integer),
            .LessThan => nativeBoolToBooleanObject(left.integer < right.integer),
            .Equal => nativeBoolToBooleanObject(left.integer == right.integer),
            .NotEqual => nativeBoolToBooleanObject(left.integer != right.integer),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.OutOfMemory;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                ) catch break :sw EvaluatorError.OutOfMemory;
                break :sw try self.createError(msg);
            },
        };
    }

    fn evalStringInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
        return sw: switch (operator.*) {
            .Equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
            .NotEqual => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.OutOfMemory;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s} {s} {s}",
                    .{ left.typeName(), operator_string, right.typeName() },
                ) catch break :sw EvaluatorError.OutOfMemory;
                break :sw try self.createError(msg);
            },
        };
    }

    fn evalBangOperatorExpression(right: *Object) !*Object {
        return switch (right.*) {
            .boolean => |boolean| if (boolean) &builtin.FALSE else &builtin.TRUE,
            else => &builtin.FALSE,
        };
    }

    fn evalMinusOperatorExpression(self: *Evaluator, right: *Object) !*Object {
        return sw: switch (right.*) {
            .integer => |value| try self.createIntegerObject(-value),
            else => {
                const msg: []u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: -{s}",
                    .{right.typeName()},
                ) catch break :sw EvaluatorError.OutOfMemory;
                break :sw try self.createError(msg);
            },
        };
    }

    fn evalIfExpression(self: *Evaluator, if_expr: *const ast.IfExpression, scope: *Environment) EvaluatorError!*Object {
        const condition: *Object = self.evalExpression(if_expr.condition, scope) catch return EvaluatorError.InvalidCondition;
        return sw: switch (condition.*) {
            .error_ => condition,
            else => {
                if (isTruthy(condition)) {
                    break :sw try self.evalStatement(&.{ .block_statement = if_expr.then_branch.* }, scope);
                } else if (if_expr.else_branch) |else_branch| {
                    break :sw try self.evalStatement(&.{ .block_statement = else_branch.* }, scope);
                } else {
                    break :sw &builtin.NULL;
                }
            },
        };
    }

    fn evalIdentifier(self: *Evaluator, identifier: *const ast.Identifier, scope: *Environment) !*Object {
        if (scope.get(identifier.value)) |value| {
            return value;
        }

        const msg: []const u8 = std.fmt.allocPrint(
            self.allocator,
            "identifier not found: {s}",
            .{identifier.value},
        ) catch return EvaluatorError.OutOfMemory;
        return try self.createError(msg);
    }

    fn applyFunction(self: *Evaluator, func: *Object, args: []*Object) EvaluatorError!*Object {
        return sw: switch (func.*) {
            .function => |*function| {
                if (function.parameters) |parameters| {
                    if (parameters.items.len != args.len) {
                        const msg: []const u8 = std.fmt.allocPrint(
                            self.allocator,
                            "incorrect number of arguments: expected {d}, got {d}",
                            .{ parameters.items.len, args.len },
                        ) catch break :sw EvaluatorError.OutOfMemory;
                        break :sw try self.createError(msg);
                    }
                } else {
                    if (args.len > 0) {
                        const msg: []const u8 = std.fmt.allocPrint(
                            self.allocator,
                            "incorrect number of arguments: expected 0, got {d}",
                            .{args.len},
                        ) catch return EvaluatorError.OutOfMemory;
                        return try self.createError(msg);
                    }
                }

                const extended_env = try self.extendFunctionEnvironment(function, args);
                var evaluated: *Object = &builtin.NULL;
                for (function.body.statements.items) |*stmt| {
                    evaluated = try self.evalStatement(stmt, extended_env);
                }
                break :sw switch (evaluated.*) {
                    .return_ => |return_value| return_value.value,
                    else => evaluated,
                };
            },
            else => {
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "not a function: {s}",
                    .{func.typeName()},
                ) catch break :sw EvaluatorError.OutOfMemory;
                break :sw try self.createError(msg);
            },
        };
    }

    fn extendFunctionEnvironment(self: *Evaluator, func: *Function, args: []*Object) !*Environment {
        const env_ptr: *Environment = self.allocator.create(Environment) catch return EvaluatorError.OutOfMemory;
        env_ptr.* = .initEnclosed(self.allocator, func.env);

        // Bind arguments of the function call to the function's parameter names.
        if (func.parameters) |parameters| {
            for (parameters.items, args) |param, value| {
                env_ptr.bind(param.value, value) catch return EvaluatorError.OutOfMemory;
            }
        }
        return env_ptr;
    }

    fn nativeBoolToBooleanObject(native: bool) *Object {
        return if (native) &builtin.TRUE else &builtin.FALSE;
    }

    fn isTruthy(object: *Object) bool {
        return switch (object.*) {
            .null_ => false,
            .boolean => |boolean| boolean,
            else => true,
        };
    }

    fn createError(self: *Evaluator, message: []const u8) !*Object {
        const error_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.OutOfMemory;
        error_ptr.* = .{ .error_ = ErrorObject{ .message = message } };
        return error_ptr;
    }
};
