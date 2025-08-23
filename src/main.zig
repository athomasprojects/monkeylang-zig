const std = @import("std");
const builtin = @import("builtin");
const ast = @import("ast.zig");
const ArenaAllocator = std.heap.ArenaAllocator;
const Environment = @import("environment.zig").Environment;
const Evaluator = @import("Evaluator.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Object = @import("object.zig").Object;

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

fn nextLine(buffer: []u8) !?[]const u8 {
    const line = try stdin.readUntilDelimiterOrEof(buffer, '\n');
    if (builtin.os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    }
    return line;
}

pub fn main() !void {
    const greeting = "This is the monkey programming language!\nFeel free to type in commands.";
    if (std.posix.getenv("USER")) |uname| {
        try stdout.print(
            "Hello {s}! {s}\n",
            .{ uname, greeting },
        );
    } else if (std.posix.getenv("USERNAME")) |uname| {
        try stdout.print(
            "Hello {s}! {s}\n",
            .{ uname, greeting },
        );
    } else {
        try stdout.print(
            "Hello! {s}\n",
            .{greeting},
        );
    }

    var arena = ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var buf: [32768]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    var index: usize = 0;

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);

    while (true) : (try fbs.seekTo(index)) {
        try stdout.print(">> ", .{});

        stdin.streamUntilDelimiter(fbs.writer(), '\n', fbs.buffer.len) catch |err| {
            switch (err) {
                error.NoSpaceLeft, error.StreamTooLong => {
                    // Flush the rest of the line in the buffer.
                    var discard_buf: [256]u8 = undefined;
                    while (true) {
                        const read_bytes = try stdin.read(&discard_buf);
                        if (read_bytes == 0) break;
                        if (std.mem.indexOfScalar(u8, discard_buf[0..read_bytes], '\n')) |_| {
                            break;
                        }
                    }
                    break;
                },
                else => {
                    try stdout.print("Error reading input: {any}\n", .{err});
                    break;
                },
            }
        };

        const source: []const u8 = fbs.buffer[index..fbs.pos];
        if (source.len == 0) {
            try stdout.print("", .{});
        } else if (std.mem.eql(u8, source, "quit")) {
            break;
        } else {
            index += source.len;
            var lexer: Lexer = .init(source);
            var parser: Parser = .init(&lexer, allocator);

            // It's ok if we error for input that cannot be parsed for now, as it helps with debugging.
            // Eventually we'll provide an error message and properly handle the error.
            var program = try parser.parse();
            // program.printStatements();

            const obj: *Object = try evaluator.evalProgram(&program, &env);
            obj.print();
            try stdout.print("\n", .{});

            // if (parser.parse()) |program| {
            //     program.printStatements();
            // } else |err| Parser.printParserError(err);
        }
    }
}
