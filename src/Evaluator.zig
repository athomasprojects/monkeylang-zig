const std = @import("std");
const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const obj = @import("object.zig");
const Object = obj.Object;
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
    ArrayLiteral,
    IndexExpression,
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
    return outer: switch (statement.*) {
        .expression_statement => |expression_statement| try self.evalExpression(expression_statement.expression, scope),
        .let_statement => |let_statement| {
            const evaluated_expression = try self.evalExpression(let_statement.value, scope);
            switch (evaluated_expression.*) {
                .error_ => break :outer evaluated_expression,
                else => {
                    // Bind the evaluated expression to the identifier in the current scope.
                    scope.bind(let_statement.name.value, evaluated_expression) catch return EvaluatorError.OutOfMemory;
                    break :outer &builtins.NULL;
                },
            }
        },
        .block_statement => |*block_statement| {
            var result: *Object = &builtins.NULL;
            for (block_statement.statements.items) |*stmt| {
                result = try self.evalStatement(stmt, scope);
                switch (result.*) {
                    .return_, .error_ => break :outer result,
                    else => {},
                }
            }
            break :outer result;
        },
        .return_statement => |return_statement| {
            const value: *Object = try self.evalExpression(return_statement.value, scope);
            const object_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.OutOfMemory;
            object_ptr.* = .{ .return_ = .{ .value = value } };
            break :outer object_ptr;
        },
    };
}

fn evalExpression(self: *Evaluator, expr: *ast.Expression, scope: *Environment) !*Object {
    return outer: switch (expr.*) {
        .integer => |integer| try self.createIntegerObject(integer.value),
        .boolean => |boolean| Evaluator.createBooleanObject(boolean.value),
        .string => |string| try self.createStringObject(string.value),
        .prefix => |prefix_expr| {
            const right = try self.evalExpression(prefix_expr.right, scope);
            break :outer switch (right.*) {
                .error_ => right,
                else => try self.evalPrefixExpression(&prefix_expr.operator, right),
            };
        },
        .infix => |infix_expr| {
            const left = try self.evalExpression(infix_expr.left, scope);
            switch (left.*) {
                .error_ => break :outer left,
                else => {
                    const right = try self.evalExpression(infix_expr.right, scope);
                    break :outer switch (right.*) {
                        .error_ => right,
                        else => try self.evalInfixExpression(&infix_expr.operator, left, right),
                    };
                },
            }
        },
        .if_expression => |if_expr| try self.evalIfExpression(&if_expr, scope),
        .identifier => |identifier| try self.evalIdentifier(&identifier, scope),
        .function => |fn_literal| try self.createFunctionLiteralObject(&fn_literal, scope),
        .call => |call| {
            const fn_val = try self.evalExpression(call.callee, scope);
            switch (fn_val.*) {
                .error_ => break :outer fn_val,
                else => {
                    var evaled: *Object = &builtins.NULL;
                    var evaled_args = ArrayList(*Object).init(self.allocator);
                    if (call.args) |args| {
                        for (args.items) |*arg| {
                            evaled = try self.evalExpression(arg, scope);
                            switch (evaled.*) {
                                .error_ => break :outer evaled,
                                else => evaled_args.append(evaled) catch break :outer EvaluatorError.OutOfMemory,
                            }
                        }
                    }
                    break :outer try self.applyFunction(fn_val, evaled_args.items);
                },
            }
        },
        // TODO: Implement array literal evaluation.
        .array_literal => |array_literal| {
            if (array_literal.elements) |elements| {
                var evaled: *Object = &builtins.NULL;
                var evaled_elems = ArrayList(*Object).init(self.allocator);
                for (elements.items) |*elem| {
                    evaled = try self.evalExpression(elem, scope);
                    switch (evaled.*) {
                        .error_ => break :outer evaled,
                        else => evaled_elems.append(evaled) catch break :outer EvaluatorError.OutOfMemory,
                    }
                }
                const array_literal_ptr: *Object = self.allocator.create(Object) catch return EvaluatorError.OutOfMemory;
                array_literal_ptr.* = .{
                    .array = .{ .elements = evaled_elems },
                };
                break :outer array_literal_ptr;
            } else break :outer &builtins.EMPTY_ARRAY;
        },
        // TODO: Implement index experssion evaluation.
        .index_expression => |index_expression| {
            const left: *Object = try self.evalExpression(index_expression.left, scope);
            switch (left.*) {
                .error_ => break :outer left,
                else => {},
            }
            const index: *Object = try self.evalExpression(index_expression.index, scope);
            switch (index.*) {
                .error_ => break :outer index,
                else => {},
            }
            break :outer try self.evalIndexExpression(left, index);
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
        .bang => return try evalBangOperatorExpression(right),
        .minus => return try self.evalMinusOperatorExpression(right),
        else => {
            const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
            return try self.createError(
                "unknown operator: {s}{s}",
                .{ operator_string, right.typeName() },
            );
        },
    }
}

fn evalInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
    return outer: switch (left.*) {
        .integer => switch (right.*) {
            .integer => break :outer try self.evalIntegerInfixExpression(operator, left, right),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                break :outer try self.createError(
                    "type mismatch: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                );
            },
        },
        .string => switch (right.*) {
            .string => switch (operator.*) {
                .equal => nativeBoolToBooleanObject(std.mem.eql(u8, left.string, right.string)),
                .not_equal => nativeBoolToBooleanObject(!std.mem.eql(u8, left.string, right.string)),
                .plus => {
                    // Concatenate strings
                    const concatenated_string: []const u8 = std.fmt.allocPrint(
                        self.allocator,
                        "{s}{s}",
                        .{ left.string, right.string },
                    ) catch return EvaluatorError.OutOfMemory;
                    break :outer try self.createStringObject(concatenated_string);
                },
                else => {
                    const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                    break :outer try self.createError(
                        "unknown operator: {s} {s} {s}",
                        .{
                            left.typeName(),
                            operator_string,
                            right.typeName(),
                        },
                    );
                },
            },
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                break :outer try self.createError(
                    "type mismatch: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                );
            },
        },
        else => switch (operator.*) {
            .equal => nativeBoolToBooleanObject(std.meta.eql(left.*, right.*)),
            .not_equal => nativeBoolToBooleanObject(!std.meta.eql(left.*, right.*)),
            else => {
                const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
                break :outer try self.createError(
                    "unknown operator: {s} {s} {s}",
                    .{
                        left.typeName(),
                        operator_string,
                        right.typeName(),
                    },
                );
            },
        },
    };
}

fn evalIntegerInfixExpression(self: *Evaluator, operator: *const Token, left: *Object, right: *Object) !*Object {
    return switch (operator.*) {
        .plus => try self.createIntegerObject(left.integer + right.integer),
        .minus => try self.createIntegerObject(left.integer - right.integer),
        .asterisk => try self.createIntegerObject(left.integer * right.integer),
        .slash => {
            const division = std.math.divExact(i64, left.integer, right.integer) catch return EvaluatorError.FailedDivision;
            return try self.createIntegerObject(division);
        },
        .greater_than => nativeBoolToBooleanObject(left.integer > right.integer),
        .less_than => nativeBoolToBooleanObject(left.integer < right.integer),
        .equal => nativeBoolToBooleanObject(left.integer == right.integer),
        .not_equal => nativeBoolToBooleanObject(left.integer != right.integer),
        else => {
            const operator_string: []u8 = operator.toString(self.allocator) catch return EvaluatorError.OutOfMemory;
            return try self.createError(
                "unknown operator: {s} {s} {s}",
                .{
                    left.typeName(),
                    operator_string,
                    right.typeName(),
                },
            );
        },
    };
}

fn evalBangOperatorExpression(right: *Object) !*Object {
    return switch (right.*) {
        .boolean => |boolean| if (boolean) &builtins.FALSE else &builtins.TRUE,
        else => &builtins.FALSE,
    };
}

fn evalMinusOperatorExpression(self: *Evaluator, right: *Object) !*Object {
    return switch (right.*) {
        .integer => |value| try self.createIntegerObject(-value),
        else => try self.createError(
            "unknown operator: -{s}",
            .{right.typeName()},
        ),
    };
}

fn evalIfExpression(self: *Evaluator, if_expr: *const ast.IfExpression, scope: *Environment) EvaluatorError!*Object {
    const condition: *Object = self.evalExpression(if_expr.condition, scope) catch return EvaluatorError.InvalidCondition;
    return state: switch (condition.*) {
        .error_ => condition,
        else => if (isTruthy(condition)) {
            break :state try self.evalStatement(
                &.{ .block_statement = if_expr.then_branch.* },
                scope,
            );
        } else if (if_expr.else_branch) |else_branch| {
            break :state try self.evalStatement(
                &.{ .block_statement = else_branch.* },
                scope,
            );
        } else {
            break :state &builtins.NULL;
        },
    };
}

fn evalIdentifier(self: *Evaluator, identifier: *const ast.Identifier, scope: *Environment) !*Object {
    if (scope.get(identifier.value)) |value| {
        return value;
    }

    if (builtins.getFnObject(identifier.value)) |fn_value| {
        return fn_value;
    }

    return try self.createError(
        "identifier not found: {s}",
        .{identifier.value},
    );
}

fn evalIndexExpression(self: *Evaluator, left: *Object, index: *Object) !*Object {
    return outer: switch (left.*) {
        .array => |array_literal| switch (index.*) {
            .integer => |integer_index| {
                if (array_literal.elements) |elements| {
                    if (integer_index < 0) break :outer &builtins.NULL;
                    const idx: usize = @intCast(integer_index);
                    if (idx > elements.items.len - 1) break :outer &builtins.NULL;
                    break :outer elements.items[idx];
                } else break :outer &builtins.NULL;
            },
            else => break :outer try self.createError("index operator not supported: {s}", .{index.typeName()}),
        },
        else => try self.createError("index operator not supported: {s}", .{left.typeName()}),
    };
}

fn applyFunction(self: *Evaluator, func: *Object, args: []*Object) EvaluatorError!*Object {
    switch (func.*) {
        .function => |*fn_literal| {
            if (fn_literal.parameters) |parameters| {
                if (parameters.items.len != args.len) {
                    return try self.createError(
                        "incorrect number of arguments: expected {d}, got {d}",
                        .{ parameters.items.len, args.len },
                    );
                }
            } else if (args.len > 0) {
                return try self.createError(
                    "incorrect number of arguments: expected 0, got {d}",
                    .{args.len},
                );
            }

            const extended_env = try self.extendFunctionEnvironment(fn_literal, args);
            var evaluated: *Object = &builtins.NULL;
            for (fn_literal.body.statements.items) |*stmt| {
                evaluated = try self.evalStatement(stmt, extended_env);
                switch (evaluated.*) {
                    .error_ => return evaluated,
                    .return_ => |return_value| return return_value.value,
                    else => {},
                }
            }
            return evaluated;
        },
        .builtin => |builtin_fn| {
            return try builtin_fn.call(self.allocator, args);
        },
        else => {
            return try self.createError(
                "not a function: {s}",
                .{func.typeName()},
            );
        },
    }
}

fn extendFunctionEnvironment(self: *Evaluator, func: *obj.Function, args: []*Object) !*Environment {
    var env_ptr: *Environment = self.allocator.create(Environment) catch return EvaluatorError.OutOfMemory;
    env_ptr.* = .initEnclosed(self.allocator, func.env);
    if (func.parameters) |parameters| {
        // Bind arguments of the function call to the function's parameter names.
        for (parameters.items, args) |param, arg| {
            env_ptr.bind(param.value, arg) catch return EvaluatorError.OutOfMemory;
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
            .message = std.fmt.allocPrint(
                self.allocator,
                fmt,
                args,
            ) catch return EvaluatorError.OutOfMemory,
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
    try testing.expectEqualStrings(expected, try object.toString(allocator));
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

        try testing.expectEqualStrings(expected, try object.toString(allocator));
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

        try testing.expectEqualStrings(expected, try object.toString(allocator));
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
    try testing.expectEqualStrings(expected, try object.toString(allocator));
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

        try testing.expectEqualStrings(expected, try object.toString(allocator));
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

test "function literal, zero arguments" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src: []const u8 =
        \\let foo = fn(){};
        \\foo()
    ;

    var lexer: Lexer = .init(src);
    var parser: Parser = .init(&lexer, allocator);
    var program = try parser.parse();
    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);
    const object: *Object = try evaluator.evalProgram(&program, &env);
    try testing.expectEqualStrings(@tagName(builtins.NULL), @tagName(object.*));
    try testing.expectEqualStrings(try object.toString(allocator), "null");
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
    try testing.expectEqualStrings(expected, try object.toString(allocator));
}

test "builtin len" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "len(\"foo\")",
        "len(\"monkey lang\")",
        "len(\"foo\" + \"bar\")",
        "fn(x) { len(x) + 5 }(\"hello\")",
    };

    const lengths = [_]i64{ 3, 11, 6, 10 };

    for (src, lengths) |input, expected| {
        var lexer: Lexer = .init(input);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);
        try testing.expect(expected == object.integer);
    }
}

test "builtin len unsupported arguments and wrong number of arguments" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "len(\"one\", \"two\")",
        "len(1)",
        "len([1,fn(){}, \"foo\"])",
        "len(fn(){})",
        "let foo = fn(x) { x * 2 }; len(foo)",
        "let bar = fn(x) { x / 2 - x }; len(bar(2))",
        "len(true)",
        "len(fn(){}())",
    };

    const lengths = [_][]const u8{
        "incorrect number of arguments: expected 1, got 2",
        "argument to `len` not supported: got INTEGER",
        "argument to `len` not supported: got ARRAY",
        "argument to `len` not supported: got FUNCTION",
        "argument to `len` not supported: got FUNCTION",
        "argument to `len` not supported: got INTEGER",
        "argument to `len` not supported: got BOOLEAN",
        "argument to `len` not supported: got NULL",
    };

    for (src, lengths) |input, expected| {
        var lexer: Lexer = .init(input);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);
        try testing.expectEqualStrings(expected, try object.toString(allocator));
    }
}

test "array literals" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "[]",
        "[1, 2, 3]",
        "[1, 2 * 2, 3 + 3]",
        "let double = fn(x) { x * 2 }; [1, double(2), 3 * 3, 4 - 3]",
        "[fn(x,y,z){ if (x > y) { x } else { z }}(1,2,3), \"foo\", 5 + (4*2) > 3, -10]",
    };
    const expected_array_literals = [_][]const u8{
        "[]",
        "[1, 2, 3]",
        "[1, 4, 6]",
        "[1, 4, 9, 1]",
        "[3, \"foo\", true, -10]",
    };

    for (src, expected_array_literals) |input, expected| {
        var lexer: Lexer = .init(input);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);
        try testing.expectEqualStrings(expected, try object.toString(allocator));
    }
}

test "index expressions" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "[][0]",
        "[1, 2, 3][-458]",
        "[1, 2, 3][0]",
        "[1, 2, 3][1]",
        "[1, 2, 3][2]",
        "[1, 2, 3][3]",
        "let i = 0; [1][i]",
        "[1, 2, 5][1+1]",
        "let myArray = [fn(){}, 2, \"foo\"]; myArray[2];",
        "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
        "let double = fn(x) { x * 2 }; let i = 1; [1, double(2), 3 * 3, 4 - 3, 8/2][i]",
        "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
    };
    const expected_array_literals = [_][]const u8{
        "null",
        "null",
        "1",
        "2",
        "3",
        "null",
        "1",
        "5",
        "\"foo\"",
        "6",
        "4",
        "2",
    };

    for (src, expected_array_literals) |input, expected| {
        var lexer: Lexer = .init(input);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);
        try testing.expectEqualStrings(expected, try object.toString(allocator));
    }
}

test "builtin first" {
    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const src = [_][]const u8{
        "first([])",
        "first([\"foo\", \"bar\"])",
        "let a = [\"first\", fn(){}, 5 - 3, \"foo\", \"bar\"]; first(a)",
        "let b = [1,2,3,4]; first(b)",
        "let foo = fn() { true }; let c = [foo]; first(c)() == true",
    };

    const expected_array_literals = [_][]const u8{
        "null",
        "\"foo\"",
        "\"first\"",
        "1",
        "true",
    };

    for (src, expected_array_literals) |input, expected| {
        var lexer: Lexer = .init(input);
        var parser: Parser = .init(&lexer, allocator);
        var program = try parser.parse();

        var evaluator: Evaluator = .init(allocator);
        var env: Environment = .init(allocator);
        const object: *Object = try evaluator.evalProgram(&program, &env);
        try testing.expectEqualStrings(expected, try object.toString(allocator));
    }
}
