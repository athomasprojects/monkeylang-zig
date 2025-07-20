const std = @import("std");
const ast = @import("ast.zig");

pub const ObjectTag = enum {
    null_,
    integer,
    boolean,
    string,
    return_,
    error_,
};

pub const Object = union(ObjectTag) {
    null_,
    integer: i32,
    boolean: bool,
    string: []const u8,
    return_: ReturnValue,
    error_: Error,

    pub fn print(self: Object) void {
        switch (self) {
            .null_ => std.debug.print("null", .{}),
            .integer => |integer| std.debug.print("{d}", .{integer}),
            .boolean => |boolean| std.debug.print("{}", .{boolean}),
            .string => |string| std.debug.print("\"{s}\"", .{string}),
            .return_ => |return_value| return_value.value.print(),
            .error_ => |error_| error_.print(),
        }
        std.debug.print("\n", .{});
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
            .return_ => |return_value| return_value.value.toString(allocator),
            .error_ => |error_| try error_.toString(allocator),
        };
    }

    pub fn typeName(self: Object) []const u8 {
        return switch (self) {
            .null_ => "NULL",
            .integer => "INTEGER",
            .boolean => "BOOLEAN",
            .string => "STRING",
            // Note: In release mode the compiler uses optimizers that assume this prong will never be hit in order to perform optimizations. It will not panic if the prong is hit!
            // Todo: Handle error message for _unreachable_  prong.
            .return_ => unreachable,
            .error_ => "ERROR",
        };
    }
};

pub const ReturnValue = struct {
    value: *Object,
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
