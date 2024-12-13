// The Lexer generates tokens from an input stream of bytes

const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;

pub const Lexer = struct {
    input: []const u8,
    pos: usize = 0,
    ch: ?u8 = null,
    length: usize = 0,

    /// Creates a new lexer from the `input`.
    pub fn init(input: []const u8) Lexer {
        return switch (input.len) {
            0 => Lexer{
                .input = input,
            },
            else => Lexer{ .input = input, .ch = input[0], .length = input.len },
        };
    }

    /// Updates the lexer state and returns the next token in the stream.
    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();
        if (self.ch) |ch| {
            switch (ch) {
                '=' => return self.twoCharToken('=', .Assign, .Equal),
                '!' => return self.twoCharToken('=', .Bang, .NotEqual),
                '-' => {
                    self.advance();
                    return .Minus;
                },
                '+' => {
                    self.advance();
                    return .Plus;
                },
                '*' => {
                    self.advance();
                    return .Asterisk;
                },
                '/' => {
                    self.advance();
                    return .Slash;
                },
                '(' => {
                    self.advance();
                    return .LeftParen;
                },
                ')' => {
                    self.advance();
                    return .RightParen;
                },
                '{' => {
                    self.advance();
                    return .LeftBrace;
                },
                '}' => {
                    self.advance();
                    return .RightBrace;
                },
                '[' => {
                    self.advance();
                    return .LeftBracket;
                },
                ']' => {
                    self.advance();
                    return .RightBracket;
                },
                '<' => {
                    self.advance();
                    return .LessThan;
                },
                '>' => {
                    self.advance();
                    return .GreaterThan;
                },
                ',' => {
                    self.advance();
                    return .Comma;
                },
                ';' => {
                    self.advance();
                    return .Semicolon;
                },
                '"' => {
                    return self.readString();
                },
                else => {
                    if (isLetter(ch)) {
                        return self.readIdentifier();
                    }
                    if (std.ascii.isDigit(ch)) {
                        return self.readNumber();
                    }
                    self.advance();
                    return .Illegal;
                },
            }
        } else {
            return .Eof;
        }
    }

    fn skipWhitespace(self: *Lexer) void {
        self.scanUntil(std.ascii.isWhitespace);
    }

    fn scanUntil(self: *Lexer, condition: fn (ch: u8) bool) void {
        while (self.ch) |ch| {
            if (condition(ch)) {
                self.advance();
            } else {
                break;
            }
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
        if (self.peekChar()) |peeked| {
            if (peeked == match) {
                self.advance();
                self.advance();
                return two_char_token;
            }
        }
        self.advance();
        return default_token;
    }

    fn peekChar(self: *Lexer) ?u8 {
        const remaining_chars = self.length - self.pos - 1;
        if (remaining_chars >= 1) {
            return self.input[self.pos + 1];
        } else {
            return null;
        }
    }

    fn takeWhile(self: Lexer, condition: fn (u8) bool) []const u8 {
        var count: usize = 0;
        for (self.input[self.pos..self.length]) |ch| {
            // std.debug.print("{c}\n", .{ch});
            if (condition(ch)) {
                count += 1;
            } else {
                break;
            }
        }
        return self.input[self.pos .. self.pos + count];
    }

    fn readIdentifier(self: *Lexer) Token {
        const ident = self.takeWhile(isLetter);
        self.pos += ident.len - 1;
        self.advance();
        return token.lookupIdent(ident);
    }

    fn readString(self: *Lexer) Token {
        self.advance();
        const str = self.takeWhile(struct {
            pub fn call(ch: u8) bool {
                return !isString(ch);
            }
        }.call);
        self.pos += str.len;
        self.advance();
        if (self.pos >= self.length) {
            return .Illegal;
        } else {
            return .{ .String = str };
        }
    }

    fn readNumber(self: *Lexer) Token {
        const literal = self.takeWhile(std.ascii.isDigit);
        self.pos += literal.len - 1;
        self.advance();
        if (std.fmt.parseInt(i32, literal, 10)) |int| {
            return .{ .Integer = int };
        } else |_| {
            return .Illegal;
        }
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
};

fn isString(ch: u8) bool {
    return ch == '"';
}

fn isLetter(ch: u8) bool {
    return ch == '_' or std.ascii.isAlphabetic(ch);
}
