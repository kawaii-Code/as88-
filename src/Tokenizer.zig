const std = @import("std");
const as88 = @import("as88.zig");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");

const print = std.debug.print;

const Self = @This();

pub const ProgramSourceCode = struct {
    filepath: ?[]const u8,
    contents: []const u8,
};

pub const TokenWithLocation = struct {
    token: Token,
    location: SourceLocation,
};

pub const Token = union(enum) {
    pub const Tag = std.meta.Tag(@This());

    none,
    comma,
    semicolon,
    newline,

    left_paren,
    right_paren,
    equals_sign,

    plus,
    minus,
    star,
    forward_slash,

    comment,
    label: []const u8,
    string: []const u8,
    identifier: []const u8,
    number: i16,
    directive: intel8088.asm_syntax.Directive,
    register: intel8088.Register,
    instruction_mnemonic: intel8088.InstructionMnemonic,

    pub fn is(self: @This(), tag: Tag) bool {
        return self == tag;
    }

    pub fn isOperator(self: @This()) bool {
        return switch (self) {
            .plus, .minus, .star, .forward_slash => true,
            else => false,
        };
    }

    pub fn toBinaryOperator(self: @This()) intel8088.asm_syntax.BinaryOperator {
        return switch (self) {
            .plus => .plus,
            .minus => .minus,
            .star => .multiply,
            .forward_slash => .divide,
            else => unreachable,
        };
    }

    pub fn isWhitespace(self: @This()) bool {
        return switch (self) {
            .newline, .comment => true,
            else => false,
        };
    }
};

pub const SourceLocation = struct {
    line: usize,
    column: usize,
    start_byte: usize,
    end_byte: usize,

    pub fn combine(from: @This(), to: @This()) @This() {
        return .{
            .line = from.line,
            .column = from.column,
            .start_byte = from.start_byte,
            .end_byte = to.end_byte,
        };
    }

    pub fn length(self: @This()) usize {
        std.debug.assert(self.end_byte >= self.start_byte);
        return self.end_byte - self.start_byte;
    }
};

file: *as88.File,
line: []const u8,
location: SourceLocation,

pub fn tokenize(file: *as88.File) !void {
    var self = Self{
        .file = file,
        .line = undefined,
        .location = .{
            .column = 1,
            .line = 1,
            .start_byte = 0,
            .end_byte = 0,
        },
    };
    var tokens = &file.tokens;
    const allocator = file.allocator;

    var line_it = std.mem.splitScalar(u8, file.text, '\n');
    while (line_it.next()) |line| : ({
        self.location.line += 1;
        self.location.column = 1;
    }) {
        self.line = line;

        while (self.peek()) |c| : ({}) {
            if (self.nextToken(c)) |token| {
                try tokens.append(allocator, .{
                    .token = token,
                    .location = self.location,
                });

                if (token == .newline) {
                    self.location.column += self.location.length();
                    self.location.start_byte = self.location.end_byte;
                    break;
                }
            }

            self.location.column += self.location.length();
            self.location.start_byte = self.location.end_byte;
        }
    }
}

fn nextToken(self: *Self, c: u8) ?Token {
    switch (c) {
        ',' => {
            _ = self.next();
            return .comma;
        },
        '(' => {
            _ = self.next();
            return .left_paren;
        },
        ')' => {
            _ = self.next();
            return .right_paren;
        },
        ';' => {
            _ = self.next();
            return .semicolon;
        },
        '+' => {
            _ = self.next();
            return .plus;
        },
        '*' => {
            _ = self.next();
            return .star;
        },
        '=' => {
            _ = self.next();
            return .equals_sign;
        },
        '/' => {
            _ = self.next();
            return .forward_slash;
        },
        '\n' => {
            _ = self.next();
            return .newline;
        },
        '!' => {
            self.skipUntil('\n');
            return .comment;
        },
        ':' => {
            self.file.errors.addError(self.location, "Unexpected ':'", .{});
            self.file.errors.addNote("':' can only follow an identifier, e.g. `L1:`", .{});
            self.file.errors.commit();

            _ = self.next();
            return null;
        },
        '-' => {
            _ = self.next();
            return .minus;
        },
        '0'...'9' => {
            // Treat numbers as identifiers to report errors for '0hello' for example
            const unparsed_number = self.skipIdentifier();
            if (std.fmt.parseInt(i16, unparsed_number, 0)) |number| {
                return .{ .number = number };
            } else |err| switch (err) {
                error.InvalidCharacter => {
                    self.file.errors.addError(self.location, "bad character in integer '{s}'", .{unparsed_number});
                    self.file.errors.commit();
                },
                error.Overflow => {
                    self.file.errors.addError(self.location, "integer '{s}' is too big", .{unparsed_number});
                    self.file.errors.addNote("integers must be in range [-32768 .. 32767]", .{});
                    self.file.errors.commit();
                },
            }

            return null;
        },
        'a'...'z', 'A'...'Z', '_' => {
            const identifier = self.skipIdentifier();
            if (intel8088.Register.Names.find(identifier)) |register| {
                return .{ .register = register };
            } else if (intel8088.InstructionMnemonic.Names.find(identifier)) |instruction_mnemonic| {
                return .{ .instruction_mnemonic = instruction_mnemonic };
            } else {
                if (self.peek()) |char_after_identifier| {
                    if (char_after_identifier == ':') {
                        // skip ':'
                        _ = self.next();
                        return .{ .label = identifier };
                    }
                }
                return .{ .identifier = identifier };
            }
            return null;
        },
        '.' => {
            _ = self.next();
            if (self.peek()) |first_directive_char| {
                if (isIdentifierChar(first_directive_char)) {
                    const directive_identifier = self.skipIdentifier();
                    if (intel8088.asm_syntax.Directive.Names.find(directive_identifier)) |directive| {
                        return .{ .directive = directive };
                    } else {
                        self.file.errors.addError(self.location, "no directive '{s}' exists", .{directive_identifier});
                        self.file.errors.addNote("'{s}' was interpreted as a directive because it starts with a '.'", .{directive_identifier});
                        self.file.errors.commit();
                    }
                } else {
                    self.file.errors.addError(self.location, "expected a directive after '.'", .{});
                    self.file.errors.commit();
                }
            } else {
                self.file.errors.addError(self.location, "expected a directive after '.'", .{});
                self.file.errors.commit();
            }
            return null;
        },
        '"' => {
            if (self.skipStringLiteral()) |unparsed_string| {
                // This allocation will be freed along with the arena
                if (std.zig.string_literal.parseAlloc(self.file.allocator, unparsed_string)) |string| {
                    return .{ .string = string };
                } else |_| {
                    self.file.errors.addError(self.location, "invalid string literal", .{});
                    self.file.errors.commit();
                }
            } else {
                self.file.errors.addError(self.location, "unclosed '\"'", .{});
                self.file.errors.commit();
            }
            return null;
        },
        ' ', '\t', '\r' => {
            _ = self.next();
            return null;
        },
        else => {
            self.file.errors.addError(self.location, "unexpected char '{c}'", .{c});
            self.file.errors.commit();
            _ = self.next();
            return null;
        },
    }
}

fn isIdentifierChar(c: u8) bool {
    return switch (std.ascii.toLower(c)) {
        'a'...'z', '_', '0'...'9' => true,
        else => false,
    };
}

fn skipUntil(self: *Self, sentinel: u8) void {
    while (self.peek()) |c| {
        if (c != sentinel) {
            self.location.end_byte += 1;
        } else {
            break;
        }
    }
}

// Includes quotes, so, returns "abc" instead of abc
fn skipStringLiteral(self: *Self) ?[]const u8 {
    const start = self.location.end_byte;
    _ = self.next(); // skip opening '"'
    while (self.next()) |c| {
        if (c == '\\') {
            _ = self.next();
        } else if (c == '"') {
            // self.location.end_byte points to a character past the closing '"'
            return self.file.text[start..self.location.end_byte];
        }
    }
    return null;
}

fn skipIdentifier(self: *Self) []const u8 {
    const start = self.location.end_byte;
    while (self.peek()) |c| {
        if (isIdentifierChar(c)) {
            _ = self.next();
        } else {
            break;
        }
    }
    return self.file.text[start..self.location.end_byte];
}

fn next(self: *Self) ?u8 {
    const current = self.peek();
    self.location.end_byte = @min(self.location.end_byte + 1, self.file.text.len);
    return current;
}

fn peek(self: *const Self) ?u8 {
    return self.peekN(0);
}

fn peekN(self: *const Self, n: usize) ?u8 {
    return common.getOrNull(u8, self.file.text, self.location.end_byte + n);
}

const testing = std.testing;

test "tokenize labels" {
    const allocator = std.testing.allocator;
    {
        const source = ProgramSourceCode{ .filepath = null, .contents = 
        \\L1:
        \\L2: long_label_identifier:
        };

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const actual_tokens = (try tokenize(source, &arena)).items(.token);

        try expectLabelToken("L1", actual_tokens[0]);
        try testing.expectEqual(.newline, actual_tokens[1]);
        try expectLabelToken("L2", actual_tokens[2]);
        try expectLabelToken("long_label_identifier", actual_tokens[3]);
        try testing.expectEqual(.newline, actual_tokens[4]);
    }
}

test "tokenize string literals" {
    const allocator = std.testing.allocator;
    {
        const source = ProgramSourceCode{ .filepath = null, .contents = 
        \\"Hello, World!\n"
        };

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var actual_tokens = try tokenize(source, &arena);

        const token = actual_tokens.items(.token)[0];
        try expectStringToken("Hello, World!\n", token);
    }

    {
        const source = ProgramSourceCode{ .filepath = null, .contents = 
        \\".SECT .TEXT MOV AX, BX;"
        \\.SECT ".SECT"
        };

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var actual_tokens = try tokenize(source, &arena);

        try expectStringToken(".SECT .TEXT MOV AX, BX;", actual_tokens.items(.token)[0]);
        try testing.expectEqual(Token.newline, actual_tokens.items(.token)[1]);
        try testing.expectEqual(Token{ .directive = .sect }, actual_tokens.items(.token)[2]);
        try expectStringToken(".SECT", actual_tokens.items(.token)[3]);
    }

    {
        const source = ProgramSourceCode{ .filepath = null, .contents = 
        \\"\""
        };

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var actual_tokens = try tokenize(source, &arena);

        try expectStringToken("\"", actual_tokens.items(.token)[0]);
    }

    {
        const source = ProgramSourceCode{ .filepath = null, .contents = 
        \\"newline - \n, tab - \t, backslash - \\, quote - \", escaped quote - \\\", single quote - '"
        };

        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var actual_tokens = try tokenize(source, &arena);

        try expectStringToken("newline - \n, tab - \t, backslash - \\, quote - \", escaped quote - \\\", single quote - '", actual_tokens.items(.token)[0]);
    }
}

test "tokenize simple source file" {
    const allocator = std.testing.allocator;
    const source = ProgramSourceCode{ .filepath = null, .contents = 
    \\! Comment
    \\.SECT .TEXT
    \\  MOV AX, 3
    \\  ADD BX, 4
    };

    const expected_tokens = [_]Token{
        .comment,
        .newline,

        .{ .directive = .sect },
        .{ .directive = .text },
        .newline,

        .{ .instruction_mnemonic = .mov },
        .{ .register = .ax },
        .comma,
        .{ .number = 3 },
        .newline,

        .{ .instruction_mnemonic = .add },
        .{ .register = .bx },
        .comma,
        .{ .number = 4 },
        .newline,
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var actual_tokens = try tokenize(source, &arena);

    try testing.expectEqualSlices(Token, &expected_tokens, actual_tokens.items(.token));
}

test "tokenizes a compound expression" {
    const allocator = std.testing.allocator;
    const source = ProgramSourceCode{
        .filepath = null,
        .contents =
        // This won't compile, but makes a good tokenizer test
        \\MOV AX - 2 / 2, 3 * 5 + (my_label)
    };
    const expected_tokens = [_]Token{
        .{ .instruction_mnemonic = .mov },
        .{ .register = .ax },
        .minus,
        .{ .number = 2 },
        .forward_slash,
        .{ .number = 2 },
        .comma,

        .{ .number = 3 },
        .star,
        .{ .number = 5 },
        .plus,
        .left_paren,
        .{ .identifier = "my_label" },
        .right_paren,

        .newline,
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var actual_tokens = try tokenize(source, &arena);

    try testing.expectEqualDeep(&expected_tokens, actual_tokens.items(.token));
}

test "tokenizes a constant declaration and a label" {
    const allocator = std.testing.allocator;
    const source = ProgramSourceCode{ .filepath = null, .contents = 
    \\  _MY_VAR = 2
    \\MOV   AX, _MY_VAR
    };
    const expected_tokens = [_]Token{
        .{ .identifier = "_MY_VAR" },
        .equals_sign,
        .{ .number = 2 },
        .newline,

        .{ .instruction_mnemonic = .mov },
        .{ .register = .ax },
        .comma,
        .{ .identifier = "_MY_VAR" },
        .newline,
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var actual_tokens = try tokenize(source, &arena);

    try testing.expectEqualDeep(&expected_tokens, actual_tokens.items(.token));
}

fn expectStringToken(expected: []const u8, token: Token) anyerror!void {
    try testing.expect(token.is(.string));
    try testing.expectEqualStrings(expected, token.string);
}

fn expectLabelToken(expected: []const u8, token: Token) anyerror!void {
    try testing.expect(token.is(.label));
    try testing.expectEqualStrings(expected, token.label);
}
