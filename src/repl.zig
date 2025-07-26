// Monkeylang REPL

const std = @import("std");
const ast = @import("ast.zig");
const ArenaAllocator = std.heap.ArenaAllocator;
const Environment = @import("environment.zig").Environment;
const Evaluator = @import("evaluator.zig").Evaluator;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Object = @import("object.zig").Object;
const printParserError = @import("parser.zig").printParserError;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

pub fn start() !void {
    try stdout.print("Hello! This is the monkey programming language!\nFeel free to type in commands.\n", .{});

    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [32768]u8 = undefined;
    var index: usize = 0;

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);

    while (true) {
        stdout.print(">> ", .{}) catch {};
        if (stdin.readUntilDelimiterOrEof(buf[index..], '\n')) |input| {
            if (input) |str| {
                index += str.len;
                if (str.len == 0) {
                    stdout.print("", .{}) catch {};
                } else if (std.mem.eql(u8, str, "quit")) {
                    break;
                } else {
                    var lexer: Lexer = .init(str);
                    var parser: Parser = .init(&lexer, allocator);

                    // It's ok if we error for input that cannot be parsed for now, as it helps with debugging.
                    // Eventually we'll provide an error message and properly handle the error.
                    var program = try parser.parse();
                    // program.printStatements();

                    const obj: *Object = try evaluator.evalProgram(&program, &env);
                    obj.print();

                    // if (parser.parse()) |program| {
                    //     program.printStatements();
                    // } else |err| printParserError(err);
                }
            }
        } else |_| {}
    }
}
