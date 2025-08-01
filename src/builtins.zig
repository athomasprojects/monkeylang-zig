const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("object.zig").Object;
const EvaluatorError = @import("Evaluator.zig").EvaluatorError;

const true_object: Object = .{ .boolean = true };
const false_object: Object = .{ .boolean = false };
const null_object: Object = .null_;

pub var TRUE: Object = true_object;
pub var FALSE: Object = false_object;
pub var NULL: Object = null_object;

pub const BuiltinFns = enum {
    len,
};

pub fn call(self: BuiltinFns, allocator: Allocator) !void {
    _ = self;
    _ = allocator;
}
