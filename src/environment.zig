const std = @import("std");
const Allocator = std.mem.Allocator;
const StringHashMap = std.StringHashMap;
const Object = @import("object.zig").Object;
const EvaluatorError = @import("evaluator.zig").EvaluatorError;

pub const Environment = struct {
    allocator: Allocator,
    store: StringHashMap(*Object),

    pub fn init(allocator: Allocator) Environment {
        return .{
            .allocator = allocator,
            .store = StringHashMap(*Object).init(allocator),
        };
    }

    pub fn get(self: *Environment, key: []const u8) ?*Object {
        return self.store.get(key);
    }

    pub fn put(self: *Environment, key: []const u8, value: *Object) !void {
        self.store.put(key, value) catch return EvaluatorError.FailedAlloc;
    }
};
