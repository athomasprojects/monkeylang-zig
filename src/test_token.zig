const std = @import("std");
const expect = std.testing.expect;
const token = @import("token.zig");
const Token = token.Token;

test "lookup identifiers" {
    const strings = [_][]const u8{ "let", "fn", "if", "else", "true", "false", "return", "does not exist!", "15" };
    for (strings) |str| {
        const result = switch (token.lookupIdent(str)) {
            .Let, .Function, .If, .Else, .True, .False, .Return => true,
            .Ident => |ident| std.mem.eql(u8, ident, str),
            else => false,
        };
        // std.debug.print("{s}\n", .{str});
        try expect(result);
    }
}
