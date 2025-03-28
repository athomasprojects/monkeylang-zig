const std = @import("std");
const ast = @import("ast.zig");

pub const ObjectTag = enum {
    Null,
    Integer,
    Boolean,
};

pub const Object = union(ObjectTag) {
    Null,
    Integer: i32,
    Boolean: bool,

    pub fn print(self: Object) void {
        switch (self) {
            .Null => std.debug.print("null", .{}),
            .Integer => |integer| std.debug.print("{d}", .{integer}),
            .Boolean => |boolean| std.debug.print("{}", .{boolean}),
        }
        std.debug.print("\n", .{});
    }

    pub fn toString(self: Object, allocator: std.mem.Allocator) ![]u8 {
        return switch (self) {
            .Null => try std.fmt.allocPrint(allocator, "null", .{}),
            .Integer => |integer| try std.fmt.allocPrint(allocator, "{d}", .{integer}),
            .Boolean => |boolean| try std.fmt.allocPrint(allocator, "{}", .{boolean}),
        };
    }
};
