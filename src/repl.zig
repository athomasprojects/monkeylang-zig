// Monkeylang REPL

const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const printParserError = @import("parser.zig").printParserError;
const ast = @import("ast.zig");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn start() !void {
    try stdout.print("Hello! This is the monkey programming language!\nFeel free to type in commands.\n", .{});

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [32768]u8 = undefined;
    var index: usize = 0;

    while (true) {
        stdout.print(">> ", .{}) catch {};
        if (stdin.readUntilDelimiterOrEof(buf[index..], '\n')) |input| {
            if (input) |str| {
                index += str.len;
                if (std.mem.eql(u8, str, "quit")) {
                    break;
                } else {
                    var lexer: Lexer = Lexer.init(str);
                    var parser: Parser = Parser.init(&lexer, allocator);
                    // Print parser statements for now. Eventually we will only print the output of the evaulated source code.
                    // It's ok if we error for input that cannot be parsed for now, as it helps with debugging. Eventually we'll provide an error message and properly handle the error.
                    var program = try parser.parse();
                    program.printStatements();
                    // if (parser.parse()) |program| {
                    //     program.printStatements();
                    // } else |err| printParserError(err);
                }
            }
        } else |_| {}
    }
}
