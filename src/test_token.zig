const std = @import("std");
const expect = std.testing.expect;
const token = @import("token.zig");
const Token = token.Token;

test "lookup identifiers" {
    const keys = [_][]const u8{ "let", "fn", "if", "else", "true", "false", "return", "does not exist!" };
    for (keys) |key| {
        switch (token.lookupIdent(key)) {
            .Let, .Function, .If, .Else, .True, .False, .Return => |_| try std.testing.expect(true),
            .Ident => |ident| try std.testing.expect(std.mem.eql(u8, ident, key)),
            else => unreachable,
        }
    }
}
