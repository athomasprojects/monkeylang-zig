// The Lexer generates tokens from an input stream of bytes

const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    ch: ?u8 = null,
    length: usize = undefined,

    pub fn init(input: []const u8) Lexer {
        return switch (input.len) {
            0 => Lexer{ .input = input, .length = input.len },
            else => Lexer{ .input = input, .ch = input[0], .length = input.len },
        };
    }

    /// Writes the string representation of the `Lexer` to the `writer`.
    pub fn debugPrint(self: Lexer) void {
        std.debug.print("Lexer{{\n", .{});
        std.debug.print("    input: \"{s}\",\n", .{self.input});
        std.debug.print("    position: {},\n", .{self.position});
        if (self.ch) |ch| {
            std.debug.print("    ch: {c},\n", .{ch});
        } else {
            std.debug.print("    ch: null,\n", .{});
        }
        std.debug.print("    length: {},\n", .{self.length});
        std.debug.print("}}\n", .{});
    }
};
