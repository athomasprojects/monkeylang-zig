/// The Lexer generates tokens from an input stream of bytes
const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const ascii = std.ascii;
const testing = std.testing;
const expect = testing.expect;

input: []const u8 = undefined,
pos: usize,
length: usize,
ch: ?u8,
pub const Lexer = @This();

/// Creates a new lexer from the `input`.
pub fn init(input: []const u8) Lexer {
    return switch (input.len) {
        0 => .{
            .input = input,
            .pos = 0,
            .length = 0,
            .ch = null,
        },
        else => .{
            .input = input,
            .pos = 0,
            .length = input.len,
            .ch = input[0],
        },
    };
}

/// Updates the lexer state and returns the next token in the stream.
pub fn nextToken(self: *Lexer) Token {
    // Skip whitespace.
    while (self.ch) |ch| {
        if (ascii.isWhitespace(ch)) self.advance() else break;
    }

    if (self.ch) |ch| {
        return state: switch (ch) {
            '=' => self.twoCharToken('=', .assign, .equal),
            '!' => self.twoCharToken('=', .bang, .not_equal),
            '-' => {
                self.advance();
                break :state .minus;
            },
            '+' => {
                self.advance();
                break :state .plus;
            },
            '*' => {
                self.advance();
                break :state .asterisk;
            },
            '/' => {
                self.advance();
                break :state .slash;
            },
            '(' => {
                self.advance();
                break :state .l_paren;
            },
            ')' => {
                self.advance();
                break :state .r_paren;
            },
            '{' => {
                self.advance();
                break :state .l_brace;
            },
            '}' => {
                self.advance();
                break :state .r_brace;
            },
            '[' => {
                self.advance();
                break :state .l_bracket;
            },
            ']' => {
                self.advance();
                break :state .r_bracket;
            },
            '<' => {
                self.advance();
                break :state .less_than;
            },
            '>' => {
                self.advance();
                break :state .greater_than;
            },
            ',' => {
                self.advance();
                break :state .comma;
            },
            ';' => {
                self.advance();
                break :state .semicolon;
            },
            ':' => {
                self.advance();
                break :state .colon;
            },
            '"' => self.readString(),
            'A'...'Z', 'a'...'z', '_' => self.readIdentifier(),
            '0'...'9' => self.readNumber(),
            else => {
                self.advance();
                break :state .illegal;
            },
        };
    }
    return .eof;
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
    var offset: usize = self.pos;
    while (offset < self.input.len and condition(self.input[offset])) {
        offset += 1;
    }
    return self.input[self.pos..offset];
}

fn readIdentifier(self: *Lexer) Token {
    const ident = self.takeWhile(isLetterOrNumberOrUnderscore);
    self.pos += ident.len - 1;
    self.advance();
    return token.fromIdentifierOrKeyword(ident);
}

fn readString(self: *Lexer) Token {
    self.advance();
    const str = self.takeWhile(notDoubleQuote);
    self.pos += str.len;
    self.advance();
    return if (self.pos >= self.length) .illegal else .{ .string_literal = str };
}

fn readNumber(self: *Lexer) Token {
    const number_literal = self.takeWhile(ascii.isDigit);
    self.pos += number_literal.len - 1;
    self.advance();
    if (std.fmt.parseInt(i64, number_literal, 10)) |int| {
        return .{ .integer_literal = int };
    } else |_| {
        return .illegal;
    }
}

fn isLetterOrNumberOrUnderscore(ch: u8) bool {
    return switch (ch) {
        'A'...'Z',
        'a'...'z',
        '0'...'9',
        '_',
        => true,
        else => false,
    };
}

fn notDoubleQuote(ch: u8) bool {
    return ch != '"';
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
    const sources = [_][]const u8{ "", input };
    const results = [_]Lexer{
        .{
            .input = "",
            .pos = 0,
            .length = 0,
            .ch = null,
        },
        .{
            .input = input,
            .pos = 0,
            .length = input.len,
            .ch = input[0],
        },
    };
    for (sources, results) |source, expected| {
        const lexer: Lexer = .init(source);
        try expect(std.meta.eql(expected, lexer));
        try testing.expectEqualStrings(source, lexer.input);
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
        \\return * =!=::
    ;
    const expected_tokens = [_]Token{
        .assign,
        .plus,
        .minus,
        .slash,
        .comma,
        .semicolon,
        .l_brace,
        .less_than,
        .r_bracket,
        .l_bracket,
        .greater_than,
        .r_paren,
        .l_paren,
        .bang,
        .minus,
        .{ .integer_literal = 512 },
        .illegal,
        .{ .integer_literal = 79 },
        .keyword_let,
        .keyword_if,
        .keyword_else,
        .keyword_function,
        .keyword_true,
        .keyword_false,
        .{ .identifier = "_foo_" },
        .bang,
        .bang,
        .{ .identifier = "bar_baz__" },
        .{ .string_literal = "string!%" },
        .not_equal,
        .equal,
        .equal,
        .assign,
        .greater_than,
        .assign,
        .keyword_return,
        .asterisk,
        .assign,
        .not_equal,
        .colon,
        .colon,
    };
    var lexer: Lexer = .init(str);
    var idx: usize = 0;
    var next_tok: Token = lexer.nextToken();
    state: switch (next_tok) {
        .eof => {},
        else => {
            try expect(expected_tokens[idx].eql(next_tok));
            next_tok = lexer.nextToken();
            idx += 1;
            continue :state next_tok;
        },
    }
}
