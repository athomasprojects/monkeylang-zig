// The Lexer generates tokens from an input stream of bytes

const std = @import("std");
const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    ch: ?u8 = null,
    length: usize = 0,

    pub fn init(input: []const u8) Lexer {
        return switch (input.len) {
            0 => Lexer{
                .input = input,
            },
            else => Lexer{ .input = input, .ch = input[0], .length = input.len },
        };
    }

    /// Writes the string representation of the `Lexer` to the `writer`.
    pub fn debugPrint(self: Lexer) void {
        std.debug.print("Lexer{{\n", .{});
        std.debug.print("    input: \"{s}\",\n", .{self.input});
        std.debug.print("    position: {},\n", .{self.position});
        if (self.ch) |ch| {
            std.debug.print("    ch: '{c}',\n", .{ch});
        } else {
            std.debug.print("    ch: null,\n", .{});
        }
        std.debug.print("    length: {},\n", .{self.length});
        std.debug.print("}}\n", .{});
    }

    pub fn advance(self: *Lexer) void {
        if (self.length > 0) {
            if (self.position >= self.length - 1) {
                self.ch = null;
            } else {
                self.position += 1;
                self.ch = self.input[self.position];
            }
        }
    }

    fn is_string(ch: u8) bool {
        return ch == '"';
    }

    fn is_letter(ch: u8) bool {
        return std.ascii.isAlphabetic(ch) or ch == '_';
    }

    fn scan_until(self: *Lexer, condition: fn (ch: u8) bool) void {
        while (self.ch) |ch| {
            if (condition(ch)) {
                self.advance();
            } else {
                break;
            }
        }
    }

    pub fn skip_whitespace(self: *Lexer) void {
        self.scan_until(std.ascii.isWhitespace);
    }
};
