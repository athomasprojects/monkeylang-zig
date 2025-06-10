const std = @import("std");
const ast = @import("ast.zig");

pub const ObjectTag = enum {
    null_,
    integer,
    boolean,
    string,
};

pub const Object = union(ObjectTag) {
    null_,
    integer: i32,
    boolean: bool,
    string: []const u8,

    pub fn print(self: Object) void {
        switch (self) {
            .null_ => std.debug.print("null", .{}),
            .integer => |integer| std.debug.print("{d}", .{integer}),
            .boolean => |boolean| std.debug.print("{}", .{boolean}),
            .string => |string| std.debug.print("\"{s}\"", .{string}),
        }
        std.debug.print("\n", .{});
    }

    pub fn toString(self: Object, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .null_ => try std.fmt.allocPrint(allocator, "null", .{}),
            .integer => |integer| try std.fmt.allocPrint(allocator, "{d}", .{integer}),
            .boolean => |boolean| try std.fmt.allocPrint(allocator, "{}", .{boolean}),
            .string => |string| try std.fmt.allocPrint(allocator, "\"{s}\"", .{string}),
        };
    }
};
