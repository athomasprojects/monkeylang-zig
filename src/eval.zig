const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const Allocator = std.mem.Allocator;

pub const EvaluatorError = error{
    FailedAlloc,
};

const BUILTIN_TRUE: object.Object = .{ .Boolean = true };
const BUILTIN_FALSE: object.Object = .{ .Boolean = false };
const BUILTIN_NULL: object.Object = .Null;

var TRUE_OBJECT: object.Object = BUILTIN_TRUE;
var FALSE_OBJECT: object.Object = BUILTIN_FALSE;
var NULL_OBJECT: object.Object = BUILTIN_NULL;

pub const Evaluator = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Evaluator {
        return .{ .allocator = allocator };
    }

    pub fn evalProgram(self: *Evaluator, program: *ast.Program) !*object.Object {
        var result: *object.Object = undefined;
        for (program.statements.items) |statement| {
            result = try self.evalStatement(&statement);
        }
        return result;
    }

    fn evalStatement(self: *Evaluator, statement: *const ast.Statement) !*object.Object {
        return switch (statement.*) {
            .expression_statement => |expression_statement| try self.evalExpression(expression_statement.expression),
            // .let_statement => |let_statement| try self.evalExpression(let_statement),
            // .return_statement => |return_statement| try self.evalExpression(return_statement),
            // .block_statement => |block_statement| try self.evalExpression(block_statement),
            else => &NULL_OBJECT,
        };
    }

    fn evalExpression(self: *Evaluator, expr: *ast.Expression) !*object.Object {
        return switch (expr.*) {
            .integer => |integer| try self.createInteger(integer.value),
            .boolean => |boolean| Evaluator.createBoolean(boolean.value),
            else => &NULL_OBJECT,
        };
    }

    fn createInteger(self: *Evaluator, integer: i32) !*object.Object {
        const integer_ptr: *object.Object = self.allocator.create(object.Object) catch return EvaluatorError.FailedAlloc;
        integer_ptr.* = object.Object{ .Integer = integer };
        return integer_ptr;
    }

    fn createBoolean(boolean: bool) *object.Object {
        // const boolean_ptr: *object.Object = self.allocator.create(object.Object) catch return EvaluatorError.FailedAlloc;
        if (boolean) {
            return &TRUE_OBJECT;
        } else {
            return &FALSE_OBJECT;
        }
    }

    // fn createObjectFromExpression(self: *Evaluator, expression: *ast.Expression, object_tag: object.ObjectTag) !object.Object {
    //     const object_ptr = self.allocator.create(object.Object) catch return EvaluatorError.FailedAlloc;
    //     object_ptr.* = switch (object_tag) {
    //         .Integer => object.Object{ .Integer = expression.value },
    //         .Boolean => object.Object{ .Boolean = expression.value },
    //         .Null => object.NULL_OBJECT,
    //     };
    //     return object_ptr;
    // }
};
