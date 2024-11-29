// Monkeylang REPL

const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn start() void {
    stdout.print("Hello! This is the monkey programming language!\nFeel free to type in commands.\n", .{}) catch {};

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    loop(arena.allocator());
    return;
}

fn loop(allocator: std.mem.Allocator) void {
    var buf: [2048]u8 = undefined;

    stdout.print("{s}", .{">> "}) catch {};

    if (stdin.readUntilDelimiterOrEof(&buf, '\n')) |input| {
        if (input) |str| {
            if (condition(str)) {
                var lexer: Lexer = Lexer.init(str);

                var parser: Parser = Parser.init(&lexer, allocator);
                _ = parser.parse() catch {};

                // for (program.statements.items) |s| {
                //     s.debugPrint();
                // }

                // while (lexer.nextToken()) |next_tok| {
                //     next_tok.debugPrint();
                //     std.debug.print("\n", .{});
                // }
                loop(allocator);
            }
        }
    } else |_| {}
}

fn condition(input: []const u8) bool {
    // exit REPL if 'quit' command is entered
    // else, keep accepting user input
    if (std.mem.eql(u8, input, "quit")) {
        return false;
    } else {
        return input.len >= 0;
    }
}
