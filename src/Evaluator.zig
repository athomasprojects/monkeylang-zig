const std = @import("std");
const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const obj = @import("object.zig");
const Object = obj.Object;
const Function = obj.Function;
const ReturnValue = obj.ReturnValue;
const ErrorObject = obj.Error;
const Lexer = @import("Lexer.zig");
const Token = @import("token.zig").Token;
const Parser = @import("Parser.zig");
const Environment = @import("environment.zig").Environment;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const testing = std.testing;
const expect = testing.expect;

pub const EvaluatorError = error{
    FailedDivision,
    InvalidCondition,
    OutOfMemory,
};

allocator: Allocator,
pub const Evaluator = @This();

/// Returns a newly initialized evaluator.
pub fn init(allocator: Allocator) Evaluator {
    return .{ .allocator = allocator };
}

/// Returns a pointer to the `Object` representation of the program's evaluated AST.
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
                    break :sw &builtins.NULL;
                },
            }
        },
        .block_statement => |*block_statement| {
            var result: *Object = &builtins.NULL;
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
                    var evaled: *Object = &builtins.NULL;
                    var evaled_args = ArrayList(*Object).init(self.allocator);
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
    return if (boolean) &builtins.TRUE else &builtins.FALSE;
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
    switch (operator.*) {
        .Bang => return try evalBangOperatorExpression(right),
        .Minus => return try self.evalMinusOperatorExpression(right),
        else => {
            const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
            return self.createError(
                "unknown operator: {s}{s}",
                .{ operator_string, right.typeName() },
            ) catch return EvaluatorError.OutOfMemory;
        },
    }
}

fn evalInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
    switch (left.*) {
        .integer => switch (right.*) {
            .integer => return try self.evalIntegerInfixExpression(operator, left, right),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                return self.createError(
                    "type mismatch: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                ) catch return EvaluatorError.OutOfMemory;
            },
        },
        .string => switch (right.*) {
            .string => switch (operator.*) {
                .Equal => return nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
                .NotEqual => return nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
                .Plus => {
                    // Concatenate strings
                    const concatenated_string: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "{s}{s}",
                        .{ left.string, right.string },
                    ) catch return EvaluatorError.OutOfMemory;
                    return try self.createStringObject(concatenated_string);
                },
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                    return self.createError(
                        "unknown operator: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    ) catch return EvaluatorError.OutOfMemory;
                },
            },
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                return self.createError(
                    "type mismatch: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                ) catch return EvaluatorError.OutOfMemory;
            },
        },
        else => switch (operator.*) {
            .Equal => return nativeBoolToBooleanObject(std.meta.eql(left.*, right.*)),
            .NotEqual => return nativeBoolToBooleanObject(!std.meta.eql(left.*, right.*)),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                return self.createError(
                    "unknown operator: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                ) catch return EvaluatorError.OutOfMemory;
            },
        },
    }
}

fn evalIntegerInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
    return switch (operator.*) {
        .Plus => try self.createIntegerObject(left.integer + right.integer),
        .Minus => try self.createIntegerObject(left.integer - right.integer),
        .Asterisk => try self.createIntegerObject(left.integer * right.integer),
        .Slash => {
            const division = std.math.divExact(i64, left.integer, right.integer) catch return EvaluatorError.FailedDivision;
            return try self.createIntegerObject(division);
        },
        .GreaterThan => nativeBoolToBooleanObject(left.integer > right.integer),
        .LessThan => nativeBoolToBooleanObject(left.integer < right.integer),
        .Equal => nativeBoolToBooleanObject(left.integer == right.integer),
        .NotEqual => nativeBoolToBooleanObject(left.integer != right.integer),
        else => {
            const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
            return self.createError(
                "unknown operator: {s} {s} {s}",
                .{
                    left.typeName(),
                    operator_string,
                    right.typeName(),
                },
            ) catch return EvaluatorError.OutOfMemory;
        },
    };
}

// fn evalStringInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
//     return switch (operator.*) {
//         .Equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
//         .NotEqual => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
//         else => {
//             const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
//             return self.createError(
//                 "unknown operator: {s} {s} {s}",
//                 .{
//                     left.typeName(),
//                     operator_string,
//                     right.typeName(),
//                 },
//             ) catch return EvaluatorError.OutOfMemory;
//         },
//     };
// }

fn evalBangOperatorExpression(right: *Object) !*Object {
    return switch (right.*) {
        .boolean => |boolean| if (boolean) &builtins.FALSE else &builtins.TRUE,
        else => &builtins.FALSE,
    };
}

fn evalMinusOperatorExpression(self: *Evaluator, right: *Object) !*Object {
    switch (right.*) {
        .integer => |value| return try self.createIntegerObject(-value),
        else => {
            return self.createError(
                "unknown operator: -{s}",
                .{right.typeName()},
            ) catch return EvaluatorError.OutOfMemory;
        },
    }
}

fn evalIfExpression(self: *Evaluator, if_expr: *const ast.IfExpression, scope: *Environment) EvaluatorError!*Object {
    const condition: *Object = self.evalExpression(if_expr.condition, scope) catch return EvaluatorError.InvalidCondition;
    switch (condition.*) {
        .error_ => return condition,
        else => if (isTruthy(condition)) {
            return try self.evalStatement(&.{ .block_statement = if_expr.then_branch.* }, scope);
        } else if (if_expr.else_branch) |else_branch| {
            return try self.evalStatement(&.{ .block_statement = else_branch.* }, scope);
        } else {
            return &builtins.NULL;
        },
    }
}

fn evalIdentifier(self: *Evaluator, identifier: *const ast.Identifier, scope: *Environment) !*Object {
    if (scope.get(identifier.value)) |value| {
        return value;
    }

    return self.createError(
        "identifier not found: {s}",
        .{identifier.value},
    ) catch return EvaluatorError.OutOfMemory;
}

// fn evalBlock(self: *Evaluator, block: *ast.BlockStatement, scope: *Environment) EvaluatorError!*Object {
//     var result: *Object = &builtin.NULL;
//     for (block.statements.items) |*stmt| {
//         result = try self.evalStatement(stmt, scope);
//         switch (result.*) {
//             .error_, .return_ => return result,
//             else => {},
//         }
//     }
//     return result;
// }

fn applyFunction(self: *Evaluator, func: *Object, args: []*Object) EvaluatorError!*Object {
    switch (func.*) {
        .function => |*function| {
            if (function.parameters) |parameters| {
                if (parameters.items.len != args.len) {
                    return self.createError(
                        "incorrect number of arguments: expected {d}, got {d}",
                        .{ parameters.items.len, args.len },
                    ) catch return EvaluatorError.OutOfMemory;
                }
            } else {
                if (args.len > 0) {
                    return self.createError(
                        "incorrect number of arguments: expected 0, got {d}",
                        .{args.len},
                    ) catch return EvaluatorError.OutOfMemory;
                }
            }

            const extended_env = try self.extendFunctionEnvironment(function, args);
            var evaluated: *Object = &builtins.NULL;
            for (function.body.statements.items) |*stmt| {
                evaluated = try self.evalStatement(stmt, extended_env);
                switch (evaluated.*) {
                    .error_ => return evaluated,
                    .return_ => |return_value| return return_value.value,
                    else => {},
                }
            }
            return switch (evaluated.*) {
                .return_ => |return_value| return_value.value,
                else => evaluated,
            };
        },
        else => {
            return self.createError(
                "not a function: {s}",
                .{func.typeName()},
            ) catch return EvaluatorError.OutOfMemory;
        },
    }
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
    return if (native) &builtins.TRUE else &builtins.FALSE;
}

fn isTruthy(object: *Object) bool {
    return switch (object.*) {
        .null_ => false,
        .boolean => |boolean| boolean,
        else => true,
    };
}

fn createError(self: *Evaluator, comptime fmt: []const u8, args: anytype) !*Object {
    const error_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.OutOfMemory;
    error_ptr.* = .{
        .error_ = .{
            .message = try std.fmt.allocPrint(
                self.allocator,
                fmt,
                args,
            ),
        },
    };
    return error_ptr;
}

// Tests
test "null" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 = "null";
    const expected: []const u8 = "identifier not found: null";

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);

    const result = try object.toString(allocator);
    try testing.expectEqualStrings(expected, result);
}

test "boolean literal" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{ "true", "false" };

    for (src) |expected| {
        var lexer: Lexer = .init(expected);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        const result = try object.toString(allocator);
        try testing.expectEqualStrings(expected, result);
    }
}

test "integer literal" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{ "1", "0", "-0", "-5", "-10", "69420" };
    const expected = [_]i64{ 1, 0, 0, -5, -10, 69420 };

    for (src, 0..) |str, i| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        try testing.expect(expected[i] == object.integer);
    }
}

test "prefix operators" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "!true",
        "!false",
        "!5",
        "!!true",
        "!!false",
        "!!5",
        "!!-5",
        "!!!false",
    };
    const bools = [_]bool{
        false,
        true,
        false,
        true,
        false,
        true,
        true,
        true,
    };

    for (src, bools) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        try testing.expect(expected == object.boolean);
    }
}

test "integer expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "5 + 5 + 5 + 5 - 10",
        "2 * 2 * 2 * 2 * 2",
        "-50 + 100 + 50",
        "5 * 2 + 10",
        "5 + 2 * 10",
        "2 * (5 + 10)",
        "3 * 3 * 3 + 10",
        "3 * (3 * 3) + 10",
        "(5 + 10 * 2 + 15 / 3) * 2 + -10",
    };
    const values = [_]i64{
        10,
        32,
        100,
        20,
        25,
        30,
        37,
        37,
        50,
    };

    for (src, values) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        try testing.expect(expected == object.integer);
    }
}

test "boolean expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "true",
        "false",
        "1 < 2",
        "1 > 2",
        "1 < 1",
        "1 > 1",
        "1 == 1",
        "1 != 1",
        "1 == 2",
        "1 != 2",
        "true == true",
        "false == false",
        "true == false",
        "true != false",
        "false != true",
        "(1 < 2) == true",
        "(1 < 2) == false",
        "(1 > 2) == true",
        "(1 > 2) == false",
    };
    const values = [_]bool{
        true,
        false,
        true,
        false,
        false,
        false,
        true,
        false,
        false,
        true,
        true,
        true,
        false,
        true,
        true,
        true,
        false,
        false,
        true,
    };

    for (src, values) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        try testing.expect(expected == object.boolean);
    }
}

test "conditional expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "if (true) { 10 }",
        "if (false) { 10 }",
        "if (1) { 10 }",
        "if (1 < 2) { 10 }",
        "if (1 > 2) { 10 }",
        "if (1 > 2) { 10 } else { 20 }",
        "if (1 < 2) { 10 } else { 20 }",
    };

    const values = [_][]const u8{
        "10",
        "null",
        "10",
        "10",
        "null",
        "20",
        "10",
    };

    for (src, values) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        const result = try object.toString(allocator);
        try testing.expectEqualStrings(expected, result);
    }
}

test "nested conditional expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\ if (2 > 1) {
        \\   if (2 > 1) {
        \\     10
        \\   } else {
        \\     20
        \\   }
        \\ } else {
        \\   30
        \\ }
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);

    try testing.expect(10 == object.integer);
}

test "string concatenation" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let bar = "MONKEY";
        \\"foo" + " "+ bar + " " + "baz!"
    ;

    const expected: []const u8 = "\"foo MONKEY baz!\"";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);
    const result = try object.toString(allocator);
    try testing.expectEqualStrings(expected, result);
}

test "error handling" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const nested: []const u8 =
        \\if (10 > 1) {
        \\    if (10 > 1) {
        \\        return true + false;
        \\    }
        \\    return 1;
        \\}
    ;
    const src = [_][]const u8{
        "5 + true;",
        "5 + true; 5;",
        "-true",
        "true + false",
        "5; true + false; 5",
        "if (10 > 1) { true + false; }",
        nested,
    };
    const error_messages = [_][]const u8{
        "type mismatch: INTEGER + BOOLEAN",
        "type mismatch: INTEGER + BOOLEAN",
        "unknown operator: -BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
        "unknown operator: BOOLEAN + BOOLEAN",
    };

    for (src, error_messages) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        const result = try object.toString(allocator);
        try testing.expectEqualStrings(expected, result);
    }
}

test "let statements" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "let a = 5; a;",
        "let a = 5 * 5; a;",
        "let a = 5; let b = a; b;",
        "let a = 5; let b = a; let c = a + b + 5; c;",
        "let a = 5; let b = a; let c = a + b + 5; let d = if (c > a) { 99 } else { 100 }; d;",
    };
    const error_messages = [_]i64{
        5,
        25,
        5,
        15,
        99,
    };

    for (src, error_messages) |str, expected| {
        var lexer: Lexer = .init(str);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);

        try testing.expect(expected == object.integer);
    }
}

test "function literals" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let add = fn(a,b) { a + b };
        \\let sub = fn(a,b) { a - b };
        \\let applyFunc = fn(a, b, func) { func(a,b) };
        \\applyFunc(2, 2, add)
    ;
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);
    try testing.expect(4 == object.integer);
}

test "call expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\fn(a,b) { a > b }(1, 5-3);
    ;
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);
    try testing.expect(false == object.boolean);
}

test "simple closures" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let newAdder = fn(x) {
        \\  fn(y) { x + y };
        \\};
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);
    try testing.expect(4 == object.integer);
}

test "closures with string concatenation" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let makeGreeter = fn(greeting) { fn(name) { greeting + " " + name + "!" } };
        \\let heyThere = makeGreeter("Hey there");
        \\heyThere("Thorsten");
    ;

    const expected: []const u8 = "\"Hey there Thorsten!\"";
    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);
    const result = try object.toString(allocator);
    try testing.expectEqualStrings(expected, result);
}
