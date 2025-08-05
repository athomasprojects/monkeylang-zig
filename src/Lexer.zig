/// The Lexer generates tokens from an input stream of bytes
const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const ascii = std.ascii;
const testing = std.testing;
const expect = testing.expect;
const ArenaAllocator = std.heap.ArenaAllocator;

input: []const u8 = undefined,
pos: usize = 0,
length: usize = 0,
ch: ?u8 = null,
pub const Lexer = @This();

/// Creates a new lexer from the `input`.
pub fn init(input: []const u8) Lexer {
    return switch (input.len) {
        0 => .{
            .input = input,
        },
        else => .{
            .input = input,
            .length = input.len,
            .ch = input[0],
        },
    };
}

/// Updates the lexer state and returns the next token in the stream.
pub fn nextToken(self: *Lexer) Token {
    self.skipWhitespace();
    if (self.ch) |ch| {
        return state: switch (ch) {
            '=' => self.twoCharToken('=', .Assign, .Equal),
            '!' => self.twoCharToken('=', .Bang, .NotEqual),
            '-' => {
                self.advance();
                break :state .Minus;
            },
            '+' => {
                self.advance();
                break :state .Plus;
            },
            '*' => {
                self.advance();
                break :state .Asterisk;
            },
            '/' => {
                self.advance();
                break :state .Slash;
            },
            '(' => {
                self.advance();
                break :state .LeftParen;
            },
            ')' => {
                self.advance();
                break :state .RightParen;
            },
            '{' => {
                self.advance();
                break :state .LeftBrace;
            },
            '}' => {
                self.advance();
                break :state .RightBrace;
            },
            '[' => {
                self.advance();
                break :state .LeftBracket;
            },
            ']' => {
                self.advance();
                break :state .RightBracket;
            },
            '<' => {
                self.advance();
                break :state .LessThan;
            },
            '>' => {
                self.advance();
                break :state .GreaterThan;
            },
            ',' => {
                self.advance();
                break :state .Comma;
            },
            ';' => {
                self.advance();
                break :state .Semicolon;
            },
            '"' => self.readString(),
            else => {
                if (isLetter(ch)) break :state self.readIdentifier();
                if (ascii.isDigit(ch)) break :state self.readNumber();
                self.advance();
                break :state .Illegal;
            },
        };
    } else {
        return .Eof;
    }
}

fn skipWhitespace(self: *Lexer) void {
    while (self.ch) |ch| {
        if (ascii.isWhitespace(ch)) self.advance() else break;
    }
}

fn advance(self: *Lexer) void {
    if (self.length > 0) {
        if (self.pos >= self.length - 1) {
            self.ch = null;
        } else {
            self.pos += 1;
            self.ch = self.input[self.pos];
        }
    }
}

fn twoCharToken(self: *Lexer, match: u8, default_token: Token, two_char_token: Token) Token {
    if (self.length - self.pos - 1 >= 1 and self.input[self.pos + 1] == match) {
        self.advance();
        self.advance();
        return two_char_token;
    }
    self.advance();
    return default_token;
}

fn takeWhile(self: *Lexer, condition: *const fn (u8) bool) []const u8 {
    var count: usize = 0;
    for (self.input[self.pos..self.length]) |ch| {
        if (condition(ch)) count += 1 else break;
    }
    return self.input[self.pos .. self.pos + count];
}

fn readIdentifier(self: *Lexer) Token {
    const ident = self.takeWhile(isLetter);
    self.pos += ident.len - 1;
    self.advance();
    return token.keywordToIdentifier(ident);
}

fn readString(self: *Lexer) Token {
    self.advance();
    const str = self.takeWhile(struct {
        pub fn call(ch: u8) bool {
            return !(ch == '"');
        }
    }.call);
    self.pos += str.len;
    self.advance();
    return if (self.pos >= self.length) .Illegal else .{ .String = str };
}

fn readNumber(self: *Lexer) Token {
    const literal = self.takeWhile(ascii.isDigit);
    self.pos += literal.len - 1;
    self.advance();
    if (std.fmt.parseInt(i64, literal, 10)) |int| {
        return .{ .Integer = int };
    } else |_| {
        return .Illegal;
    }
}

fn isLetter(ch: u8) bool {
    return ch == '_' or ascii.isAlphabetic(ch);
}

/// Pretty printing (intended for print debugging).
/// Writes the string representation of the `Lexer` to the stderr.
pub fn print(self: Lexer) void {
    std.debug.print("Lexer{{\n", .{});
    std.debug.print("    input: \"{s}\",\n", .{self.input});
    std.debug.print("    pos: {},\n", .{self.pos});
    if (self.ch) |ch| {
        std.debug.print("    ch: '{c}',\n", .{ch});
    } else {
        std.debug.print("    ch: null,\n", .{});
    }
    std.debug.print("    length: {},\n", .{self.length});
    std.debug.print("}}", .{});
}

// Tests
test "Lexer - init lexer" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y){
        \\x + y;
        \\};
        \\let result = add(five, ten);
        \\if x != y {
        \\let foo_bar = y / x * 69;
        \\return true;
        \\} else {
        \\return false;
        \\}
        \\10 == 10;
        \\10 != 9;
        \\baz =!= 420
        \\"string"
    ;
    const lexers = [_]Lexer{ .init(""), .init(input) };
    const vals = [_]Lexer{
        .{
            .input = "",
            .pos = 0,
            .ch = null,
        },
        .{
            .input = input,
            .pos = 0,
            .ch = input[0],
            .length = input.len,
        },
    };
    for (lexers, vals) |lexer, val| {
        try expect(std.meta.eql(lexer, val));
    }
}

// Note: `std.meta.eql` does not work for structs containing fields that are
// slices. This is because slices are 'fat pointers'.
//
// So, for example, if we are comparing two fields whose contents are slices,
// then `std.meta.eql` will compare their their lengths and _pointers_, NOT
// their contents!
//
// The pointers might not point to the same locations in
// memory, even if they happen to have the same contents. Since meaning of
// pointer comparison can be somewhat ambiguous and therefore is not handled by
// `std.meta.eql`. Instead the user has to implement the comparison.
//
// The `std.meta.eql` docs explicitly states that pointers are NOT followed!
// See: https://ziglang.org/documentation/master/std/#std.meta.eql
// See also: https://www.reddit.com/r/Zig/comments/17ug7l7/zigs_stdmetaeql_fails_to_find_tagged_union/
test "Lexer - next token" {
    const str =
        \\=      + -/    ,;{<][>)(   !-512.79 let    if else fn
        \\ true false
        \\  _foo_! !bar_baz__
        \\"string!%"
        \\ != == === >=
        \\return * =!=
    ;
    const expected_tokens = [_]Token{
        .Assign,
        .Plus,
        .Minus,
        .Slash,
        .Comma,
        .Semicolon,
        .LeftBrace,
        .LessThan,
        .RightBracket,
        .LeftBracket,
        .GreaterThan,
        .RightParen,
        .LeftParen,
        .Bang,
        .Minus,
        .{ .Integer = 512 },
        .Illegal,
        .{ .Integer = 79 },
        .Let,
        .If,
        .Else,
        .Function,
        .True,
        .False,
        .{ .Ident = "_foo_" },
        .Bang,
        .Bang,
        .{ .Ident = "bar_baz__" },
        .{ .String = "string!%" },
        .NotEqual,
        .Equal,
        .Equal,
        .Assign,
        .GreaterThan,
        .Assign,
        .Return,
        .Asterisk,
        .Assign,
        .NotEqual,
    };
    var lexer: Lexer = .init(str);
    var idx: usize = 0;
    var next_tok: Token = lexer.nextToken();
    state: switch (next_tok) {
        .Eof => {},
        else => {
            try expect(expected_tokens[idx].isEqual(next_tok));
            next_tok = lexer.nextToken();
            idx += 1;
            continue :state next_tok;
        },
    }
}
