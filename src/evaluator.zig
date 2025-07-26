const std = @import("std");
const ast = @import("ast.zig");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;
const Error = @import("object.zig").Error;
const Function = @import("object.zig").Function;
const ReturnValue = @import("object.zig").ReturnValue;
const Environment = @import("environment.zig").Environment;
const Allocator = std.mem.Allocator;
const StaticStringMap = std.static_string_map.StaticStringMap;
const ArrayList = std.ArrayList;

pub const EvaluatorError = error{
    FailedAlloc,
    FailedDivision,
    InvalidCondition,
};

const BUILTIN_TRUE: Object = .{ .boolean = true };
const BUILTIN_FALSE: Object = .{ .boolean = false };
const BUILTIN_NULL: Object = .null_;

var TRUE_OBJECT: Object = BUILTIN_TRUE;
var FALSE_OBJECT: Object = BUILTIN_FALSE;
var NULL_OBJECT: Object = BUILTIN_NULL;

pub const Evaluator = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Evaluator {
        return .{ .allocator = allocator };
    }

    pub fn evalProgram(self: *Evaluator, program: *ast.Program, env: *Environment) !*Object {
        var result: *Object = undefined;
        for (program.statements.items) |*statement| {
            result = try self.evalStatement(statement, env);
            switch (result.*) {
                .return_ => |return_value| return return_value.value,
                .error_ => return result,
                else => {},
            }
        } else return result;
    }

    fn evalStatement(self: *Evaluator, statement: *const ast.Statement, env: *Environment) !*Object {
        return sw: switch (statement.*) {
            .expression_statement => |expression_statement| try self.evalExpression(expression_statement.expression, env),
            .let_statement => |let_statement| {
                const value = try self.evalExpression(let_statement.value, env);
                switch (value.*) {
                    .error_ => break :sw value,
                    else => {
                        env.put(let_statement.name.value, value) catch break :sw EvaluatorError.FailedAlloc;
                        break :sw &NULL_OBJECT;
                    },
                }
            },
            .block_statement => |*block_statement| {
                var result: *Object = &NULL_OBJECT;
                for (block_statement.statements.items) |*stmt| {
                    result = try self.evalStatement(stmt, env);
                    switch (result.*) {
                        .return_, .error_ => break :sw result,
                        else => {},
                    }
                }
                break :sw result;
            },
            .return_statement => |return_statement| {
                const value: *Object = try self.evalExpression(return_statement.value, env);
                const object_ptr: *Object = self.allocator.create(Object) catch break :sw EvaluatorError.FailedAlloc;
                object_ptr.* = .{ .return_ = .{ .value = value } };
                break :sw object_ptr;
            },
        };
    }

    fn evalExpression(self: *Evaluator, expr: *ast.Expression, env: *Environment) !*Object {
        return sw: switch (expr.*) {
            .integer => |integer| try self.createIntegerObject(integer.value),
            .boolean => |boolean| Evaluator.createBooleanObject(boolean.value),
            .string => |string| try self.createStringObject(string.value),
            .prefix => |prefix_expr| {
                const right = try self.evalExpression(prefix_expr.right, env);
                switch (right.*) {
                    .error_ => break :sw right,
                    else => break :sw try self.evalPrefixExpression(&prefix_expr.operator, right),
                }
            },
            .infix => |infix_expr| {
                const left = try self.evalExpression(infix_expr.left, env);
                switch (left.*) {
                    .error_ => break :sw left,
                    else => {
                        const right = try self.evalExpression(infix_expr.right, env);
                        switch (right.*) {
                            .error_ => break :sw right,
                            else => break :sw try self.evalInfixExpression(&infix_expr.operator, left, right),
                        }
                    },
                }
            },
            .if_expression => |if_expr| try self.evalIfExpression(&if_expr, env),
            .identifier => |identifier| try self.evalIdentifier(&identifier, env),
            .function => |function| try self.createFunctionLiteralObject(&function, env),
            .call => |call| {
                const function = try self.evalExpression(call.callee, env);
                switch (function.*) {
                    .error_ => break :sw function,
                    else => {
                        var evaled: *Object = &NULL_OBJECT;
                        var evaled_args = std.ArrayList(*Object).init(self.allocator);
                        if (call.args) |args| {
                            for (args.items) |*arg| {
                                evaled = try self.evalExpression(arg, env);
                                switch (evaled.*) {
                                    .error_ => break :sw evaled,
                                    else => evaled_args.append(evaled) catch break :sw EvaluatorError.FailedAlloc,
                                }
                            }
                        }
                        break :sw try self.applyFunction(function, evaled_args.items);
                    },
                }
            },
        };
    }

    fn createIntegerObject(self: *Evaluator, integer: i32) !*Object {
        const integer_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.FailedAlloc;
        integer_ptr.* = .{ .integer = integer };
        return integer_ptr;
    }

    fn createBooleanObject(boolean: bool) *Object {
        return if (boolean) &TRUE_OBJECT else &FALSE_OBJECT;
    }

    fn createStringObject(self: *Evaluator, string: []const u8) !*Object {
        const string_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.FailedAlloc;
        string_ptr.* = .{ .string = string };
        return string_ptr;
    }

    fn createFunctionLiteralObject(self: *Evaluator, func_literal: *const ast.FunctionLiteral, env: *Environment) !*Object {
        const func_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.FailedAlloc;
        func_ptr.* = .{
            .function = .{
                .parameters = func_literal.parameters,
                .body = func_literal.body,
                .env = env,
            },
        };
        return func_ptr;
    }

    fn evalPrefixExpression(self: *Evaluator, operator: *const Token, right: *Object) !*Object {
        return sw: switch (operator.*) {
            .Bang => try evalBangOperatorExpression(right),
            .Minus => try self.evalMinusOperatorExpression(right),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.FailedAlloc;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s}{s}",
                    .{ operator_string, right.typeName() },
                ) catch break :sw EvaluatorError.FailedAlloc;
                break :sw try self.newError(msg);
            },
        };
    }

    fn evalInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
        return sw: switch (left.*) {
            .integer => switch (right.*) {
                .integer => try self.evalIntegerInfixExpression(operator, left, right),
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.FailedAlloc;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    ) catch break :sw EvaluatorError.FailedAlloc;
                    break :sw try self.newError(msg);
                },
            },
            .string => switch (right.*) {
                .string => switch (operator.*) {
                    .Equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
                    .NotEqual => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
                    else => {
                        const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.FailedAlloc;
                        const msg: []const u8 = std.fmt.allocPrint(
                            self.allocator,
                            "unknown operator: {s} {s} {s}",
                            .{
                                left.typeName(),
                                operator_string,
                                right.typeName(),
                            },
                        ) catch break :sw EvaluatorError.FailedAlloc;
                        break :sw try self.newError(msg);
                    },
                },
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.FailedAlloc;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "type mismatch: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    ) catch break :sw EvaluatorError.FailedAlloc;
                    break :sw try self.newError(msg);
                },
            },
            else => switch (operator.*) {
                .Equal => nativeBoolToBooleanObject(std.meta.eql(left.*, right.*)),
                .NotEqual => nativeBoolToBooleanObject(!std.meta.eql(left.*, right.*)),
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.FailedAlloc;
                    const msg: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "unknown operator: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    ) catch break :sw EvaluatorError.FailedAlloc;
                    break :sw try self.newError(msg);
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
                const division = std.math.divExact(i32, left.integer, right.integer) catch break :sw EvaluatorError.FailedDivision;
                break :sw try self.createIntegerObject(division);
            },
            .GreaterThan => nativeBoolToBooleanObject(left.integer > right.integer),
            .LessThan => nativeBoolToBooleanObject(left.integer < right.integer),
            .Equal => nativeBoolToBooleanObject(left.integer == right.integer),
            .NotEqual => nativeBoolToBooleanObject(left.integer != right.integer),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.FailedAlloc;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                ) catch break :sw EvaluatorError.FailedAlloc;
                break :sw try self.newError(msg);
            },
        };
    }

    fn evalStringInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
        return sw: switch (operator.*) {
            .Equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
            .NotEqual => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch break :sw EvaluatorError.FailedAlloc;
                const msg: []const u8 = std.fmt.allocPrint(
                    self.allocator,
                    "unknown operator: {s} {s} {s}",
                    .{ left.typeName(), operator_string, right.typeName() },
                ) catch break :sw EvaluatorError.FailedAlloc;
                break :sw try self.newError(msg);
            },
        };
    }

    fn evalBangOperatorExpression(right: *Object) !*Object {
        return switch (right.*) {
            .boolean => |boolean| if (boolean) &FALSE_OBJECT else &TRUE_OBJECT,
            else => &FALSE_OBJECT,
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
                ) catch break :sw EvaluatorError.FailedAlloc;
                break :sw try self.newError(msg);
            },
        };
    }

    fn evalIfExpression(self: *Evaluator, if_expr: *const ast.IfExpression, env: *Environment) EvaluatorError!*Object {
        const condition: *Object = self.evalExpression(if_expr.condition, env) catch return EvaluatorError.InvalidCondition;
        return sw: switch (condition.*) {
            .error_ => condition,
            else => {
                if (isTruthy(condition)) {
                    break :sw try self.evalStatement(&.{ .block_statement = if_expr.then_branch.* }, env);
                } else if (if_expr.else_branch) |else_branch| {
                    break :sw try self.evalStatement(&.{ .block_statement = else_branch.* }, env);
                } else {
                    break :sw &NULL_OBJECT;
                }
            },
        };
    }

    fn evalIdentifier(self: *Evaluator, identifier: *const ast.Identifier, env: *Environment) !*Object {
        if (env.get(identifier.value)) |value| {
            return value;
        } else {
            const msg: []const u8 = std.fmt.allocPrint(
                self.allocator,
                "identifier not found: {s}",
                .{identifier.value},
            ) catch return EvaluatorError.FailedAlloc;
            return try self.newError(msg);
        }
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
                        ) catch break :sw EvaluatorError.FailedAlloc;
                        break :sw try self.newError(msg);
                    }
                } else {
                    if (args.len > 0) {
                        const msg: []const u8 = std.fmt.allocPrint(
                            self.allocator,
                            "incorrect number of arguments: expected 0, got {d}",
                            .{args.len},
                        ) catch return EvaluatorError.FailedAlloc;
                        return try self.newError(msg);
                    }
                }

                const extended_env = try self.extendFunctionEnvironment(function, args);
                var evaluated: *Object = &NULL_OBJECT;
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
                ) catch break :sw EvaluatorError.FailedAlloc;
                break :sw try self.newError(msg);
            },
        };
    }

    fn extendFunctionEnvironment(self: *Evaluator, func: *Function, args: []*Object) !*Environment {
        const env_ptr: *Environment = self.allocator.create(Environment) catch return EvaluatorError.FailedAlloc;
        env_ptr.* = .initEnclosed(self.allocator, func.env);

        // Bind arguments of the function call to the function's parameter names.
        if (func.parameters) |parameters| {
            for (parameters.items, args) |param, value| {
                env_ptr.put(param.value, value) catch return EvaluatorError.FailedAlloc;
            }
        }
        return env_ptr;
    }

    fn nativeBoolToBooleanObject(native: bool) *Object {
        return if (native) &TRUE_OBJECT else &FALSE_OBJECT;
    }

    fn isTruthy(object: *Object) bool {
        return switch (object.*) {
            .null_ => false,
            .boolean => |boolean| boolean,
            else => true,
        };
    }

    fn newError(self: *Evaluator, message: []const u8) !*Object {
        const error_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.FailedAlloc;
        error_ptr.* = .{ .error_ = Error{ .message = message } };
        return error_ptr;
    }
};
