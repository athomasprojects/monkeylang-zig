// Monkeylang REPL

const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn start() void {
    stdout.print("Hello! This is the monkey programming language!\nFeel free to type in commands.\n", .{}) catch {};
    loop();
    return;
}

fn loop() void {
    var buf: [2048]u8 = undefined;

    stdout.print("{s}", .{">> "}) catch {};

    if (stdin.readUntilDelimiterOrEof(&buf, '\n')) |input| {
        if (input) |str| {
            if (condition(str)) {
                var lexer = Lexer.init(str);
                while (lexer.nextToken()) |next_tok| {
                    next_tok.debugPrint();
                }
                loop();
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
