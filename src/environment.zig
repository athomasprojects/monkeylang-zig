const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Object = @import("object.zig").Object;

pub const Environment = struct {
    outer: ?*Environment = null,
    allocator: Allocator,
    store: StringHashMap(*Object),

    pub fn init(allocator: Allocator) Environment {
        return .{
            .allocator = allocator,
            .store = StringHashMap(*Object).init(allocator),
        };
    }

    pub fn initEnclosed(allocator: Allocator, outer: *Environment) Environment {
        return .{
            .outer = outer,
            .allocator = allocator,
            .store = StringHashMap(*Object).init(allocator),
        };
    }

    pub fn get(self: *Environment, key: []const u8) ?*Object {
        if (self.store.get(key)) |name| return name;
        if (self.outer) |outer| return outer.get(key);
        return null;
    }

    pub fn put(self: *Environment, key: []const u8, value: *Object) !void {
        try self.store.put(key, value);
    }
};
