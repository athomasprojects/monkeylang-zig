const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const obj = @import("object.zig");
const Object = obj.Object;
const EvaluatorError = @import("Evaluator.zig").EvaluatorError;

const true_object: Object = .{ .boolean = true };
const false_object: Object = .{ .boolean = false };
const null_object: Object = .null_;
const let_object: Object = .let;

pub var TRUE: Object = true_object;
pub var FALSE: Object = false_object;
pub var NULL: Object = null_object;
pub var LET: Object = let_object;
pub var EMPTY_ARRAY: Object = .{ .array = obj.ArrayLiteral.empty };
pub var EMPTY_HASH: Object = .{ .hash = obj.Hash.empty };

pub var len_object: Object = .{
    .builtin = .{
        .func = lenFn,
        .tag = .len,
    },
};

pub var first_object: Object = .{
    .builtin = .{
        .func = firstFn,
        .tag = .first,
    },
};

pub var last_object: Object = .{
    .builtin = .{
        .func = lastFn,
        .tag = .last,
    },
};

pub var rest_object: Object = .{
    .builtin = .{
        .func = restFn,
        .tag = .rest,
    },
};

pub var push_object: Object = .{
    .builtin = .{
        .func = pushFn,
        .tag = .push,
    },
};

pub var puts_object: Object = .{
    .builtin = .{
        .func = putsFn,
        .tag = .puts,
    },
};

const keywords = std.StaticStringMap(TagType).initComptime(.{
    .{ "len", .len },
    .{ "first", .first },
    .{ "last", .last },
    .{ "rest", .rest },
    .{ "push", .push },
    .{ "puts", .puts },
});

pub const TagType = enum {
    len,
    first,
    last,
    rest,
    push,
    puts,
};

pub fn getFnObject(bytes: []const u8) ?*Object {
    if (keywords.get(bytes)) |tag| {
        return switch (tag) {
            .len => &len_object,
            .first => &first_object,
            .last => &last_object,
            .rest => &rest_object,
            .push => &push_object,
            .puts => &puts_object,
        };
    }
    return null;
}

fn lenFn(allocator: Allocator, args: []*Object) !*Object {
    if (try expectArgNumber(allocator, 1, args)) |err_object| {
        return err_object;
    }

    switch (args[0].*) {
        .string => |string| {
            const integer_ptr: *Object = allocator.create(Object) catch return EvaluatorError.OutOfMemory;
            integer_ptr.* = .{ .integer = @intCast(string.len) };
            return integer_ptr;
        },
        .array => |array_literal| {
            const integer_ptr: *Object = allocator.create(Object) catch return EvaluatorError.OutOfMemory;
            const length: i64 = blk: {
                if (array_literal.elements) |elements| {
                    break :blk @intCast(elements.items.len);
                } else {
                    break :blk 0;
                }
            };
            integer_ptr.* = .{ .integer = length };
            return integer_ptr;
        },
        else => return createError(
            allocator,
            "argument to `len` not supported: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn firstFn(allocator: Allocator, args: []*Object) !*Object {
    if (try expectArgNumber(allocator, 1, args)) |err_object| {
        return err_object;
    }

    switch (args[0].*) {
        .array => |array_literal| return if (array_literal.elements) |elements| elements.items[0] else &NULL,
        else => return createError(
            allocator,
            "argument to `first` must be ARRAY: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn lastFn(allocator: Allocator, args: []*Object) !*Object {
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
        else => return createError(
            allocator,
            "argument to `last` must be ARRAY: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn restFn(allocator: Allocator, args: []*Object) !*Object {
    if (try expectArgNumber(allocator, 1, args)) |err_object| {
        return err_object;
    }

    switch (args[0].*) {
        .array => |array_literal| {
            if (array_literal.elements) |elements| {
                const array_ptr: *Object = allocator.create(Object) catch return EvaluatorError.OutOfMemory;
                array_ptr.* = state: switch (elements.items.len) {
                    1 => .{ .array = .empty },
                    else => {
                        var remaining_elements: ArrayList(*Object) = .init(allocator);
                        remaining_elements.insertSlice(0, elements.items[1..elements.items.len]) catch return EvaluatorError.OutOfMemory;
                        break :state .{ .array = .{ .elements = remaining_elements } };
                    },
                };
                return array_ptr;
            } else {
                return &NULL;
            }
        },
        else => return createError(
            allocator,
            "argument to `rest` must be ARRAY: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn pushFn(allocator: Allocator, args: []*Object) !*Object {
    if (try expectArgNumber(allocator, 2, args)) |err_object| {
        return err_object;
    }

    switch (args[0].*) {
        .array => |array_literal| {
            const array_ptr: *Object = allocator.create(Object) catch return EvaluatorError.OutOfMemory;
            var new_array: ArrayList(*Object) = blk: {
                if (array_literal.elements) |elements| {
                    break :blk elements.clone() catch return EvaluatorError.OutOfMemory;
                } else {
                    break :blk .init(allocator);
                }
            };
            new_array.append(args[args.len - 1]) catch return EvaluatorError.OutOfMemory;
            array_ptr.* = .{ .array = .{ .elements = new_array } };
            return array_ptr;
        },
        else => return createError(
            allocator,
            "argument to `push` must be ARRAY: got {s}",
            .{args[0].typeName()},
        ),
    }
}

fn putsFn(_: Allocator, args: []*Object) !*Object {
    for (args) |arg| {
        arg.print();
        std.debug.print("\n", .{});
    }
    return &NULL;
}

fn expectArgNumber(allocator: Allocator, expected: usize, args: []*Object) !?*Object {
    if (args.len != expected) {
        return createError(
            allocator,
            "wrong number of arguments: expected {d}, got {d}",
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
