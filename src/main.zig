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

    var buf: [65536]u8 = @splat(0);
    var fixed_buf_stream = std.io.fixedBufferStream(&buf);
    var offset: usize = 0;

    var evaluator: Evaluator = .init(allocator);
    var env: Environment = .init(allocator);

    outer: while (true) : (try fixed_buf_stream.seekTo(offset)) {
        try stdout.print(">> ", .{});
        stdin.streamUntilDelimiter(fixed_buf_stream.writer(), '\n', fixed_buf_stream.buffer.len) catch |err| {
            switch (err) {
                error.NoSpaceLeft, error.StreamTooLong => {
                    // Flush the rest of the line in the buffer.
                    var discard_buf: [256]u8 = @splat(0);
                    while (true) {
                        const num_bytes_read = try stdin.read(&discard_buf);
                        if (num_bytes_read == 0) break;
                        if (std.mem.indexOfScalar(u8, discard_buf[0..num_bytes_read], '\n') != null) {
                            break :outer;
                        }
                    }
                },
                else => {
                    try stdout.print("Error reading input: {s}\n", .{@errorName(err)});
                    break;
                },
            }
        };

        const source: []const u8 = fixed_buf_stream.buffer[offset..fixed_buf_stream.pos];
        if (source.len == 0) {
            try stdout.print("", .{});
        } else if (std.mem.eql(u8, source, "quit")) {
            break;
        } else {
            offset += source.len;
            var lexer: Lexer = .init(source);
            var parser: Parser = .init(&lexer, allocator);

            var program = parser.parse() catch |err| {
                try stdout.print("unable parse input: {s}\n", .{@errorName(err)});
                continue;
            };

            const obj: *Object = evaluator.evalProgram(&program, &env) catch |err| {
                try stdout.print("unable to evaluate input: {s}\n", .{@errorName(err)});
                continue;
            };
            switch (obj.*) {
                .let => continue,
                else => {
                    obj.print();
                    try stdout.print("\n", .{});
                },
            }
        }
    }
}
