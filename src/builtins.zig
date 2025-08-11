const std = @import("std");
const Allocator = std.mem.Allocator;
const obj = @import("object.zig");
const Object = obj.Object;
const EvaluatorError = @import("Evaluator.zig").EvaluatorError;

const true_object: Object = .{ .boolean = true };
const false_object: Object = .{ .boolean = false };
const null_object: Object = .null_;

pub var TRUE: Object = true_object;
pub var FALSE: Object = false_object;
pub var NULL: Object = null_object;
pub var EMPTY_ARRAY: Object = .{ .array = obj.ArrayLiteral.empty };

pub var len_object: Object = .{
    .builtin = .{
        .func = len_fn,
        .tag = .len,
    },
};

pub var first_object: Object = .{
    .builtin = .{
        .func = first_fn,
        .tag = .first,
    },
};

pub var last_object: Object = .{
    .builtin = .{
        .func = last_fn,
        .tag = .last,
    },
};

const keywords = std.StaticStringMap(TagType).initComptime(.{
    .{ "len", .len },
    .{ "first", .first },
    .{ "last", .last },
});

pub const TagType = enum {
    len,
    first,
    last,
};

pub fn getFnObject(bytes: []const u8) ?*Object {
    if (keywords.get(bytes)) |tag| {
        return switch (tag) {
            .len => &len_object,
            .first => &first_object,
            .last => &last_object,
        };
    }
    return null;
}

fn len_fn(allocator: Allocator, args: []*Object) !*Object {
    if (try expectArgNumber(allocator, 1, args)) |err_object| {
        return err_object;
    }

    switch (args[0].*) {
        .string => |string| {
            const integer_ptr: *Object = allocator.create(Object) catch return EvaluatorError.OutOfMemory;
            integer_ptr.* = .{ .integer = @intCast(string.len) };
            return integer_ptr;
        },
        else => return try createError(
            allocator,
            "argument to `len` not supported: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn first_fn(allocator: Allocator, args: []*Object) !*Object {
    if (try expectArgNumber(allocator, 1, args)) |err_object| {
        return err_object;
    }

    switch (args[0].*) {
        .array => |array_literal| return if (array_literal.elements) |elements| elements.items[0] else &NULL,
        else => return try createError(
            allocator,
            "argument to `first` must be ARRAY: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn last_fn(allocator: Allocator, args: []*Object) !*Object {
    if (try expectArgNumber(allocator, 1, args)) |err_object| {
        return err_object;
    }

    switch (args[0].*) {
        .array => |array_literal| {
            if (array_literal.elements) |elements| {
                return elements.items[elements.items.len - 1];
            } else {
                return &NULL;
            }
        },
        else => return try createError(
            allocator,
            "argument to `last` must be ARRAY: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn expectArgNumber(allocator: Allocator, expected: usize, args: []*Object) !?*Object {
    if (args.len != expected) {
        return try createError(
            allocator,
            "incorrect number of arguments: expected {d}, got {d}",
            .{ expected, args.len },
        );
    } else {
        return null;
    }
}

fn createError(allocator: Allocator, comptime fmt: []const u8, args: anytype) !*Object {
    const error_ptr: *Object = allocator.create(Object) catch return EvaluatorError.OutOfMemory;
    error_ptr.* = .{
        .error_ = .{
            .message = std.fmt.allocPrint(
                allocator,
                fmt,
                args,
            ) catch return EvaluatorError.OutOfMemory,
        },
    };
    return error_ptr;
}
