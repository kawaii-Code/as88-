const std = @import("std");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");

const print = std.debug.print;


const Self = @This();

pub const ProgramSourceCode = struct {
    filepath: ?[]const u8,
    contents: []const u8,
};

pub const TokenizationResult = union(enum) {
    tokens: std.MultiArrayList(TokenWithLocation),
    errors: std.ArrayList([]const u8),
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
        return switch(self) {
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
    length: usize,
};

filepath: ?[]const u8,
line: []const u8,
location: SourceLocation,
position: usize,
allocator: std.mem.Allocator,
last_error: std.ArrayList(u8),
errors: std.ArrayList([]const u8),
tokens: std.MultiArrayList(TokenWithLocation),

pub fn init(
    arena: *std.heap.ArenaAllocator,
) Self {
    return Self{
        .filepath = undefined,
        .line = undefined,
        .position = 0,
        .location = .{
            .line = 1,
            .column = 1,
            .length = 1,
        },
        .last_error = std.ArrayList(u8).init(arena.allocator()),
        .allocator = arena.allocator(),
        .errors = std.ArrayList([]const u8).init(arena.allocator()),
        .tokens = std.MultiArrayList(TokenWithLocation) {},
    };
}

pub fn tokenize(
    self: *Self,
    source: ProgramSourceCode,
) !void {
    self.filepath = source.filepath;
    var line_it = std.mem.splitScalar(u8, source.contents, '\n');
    while (line_it.next()) |line| : (self.location.line += 1) {
        self.line = line;
        self.location.column = 1;
        self.location.length = 1;
        self.position = 0;

        while (self.peek()) |c| : (self.location.column = self.position + 1) {
            const token_start = self.position;
            const maybe_token = self.tokenFromChar(c);
            const token_length = self.position - token_start;

            if (maybe_token) |token| {
                try self.tokens.append(self.allocator, .{
                    .token = token,
                    .location = .{
                        .line = self.location.line,
                        .column = self.position + 1,
                        .length = token_length,
                    },
                });
            }
        }

        try self.tokens.append(self.allocator, .{
            .token = .newline,
            .location = .{
                .line = self.location.line,
                .column = self.position,
                .length = 1,
            },
        });
    }
}

fn tokenFromChar(self: *Self, c: u8) ?Token {
    switch (c) {
        ',' => { _ = self.next(); return .comma; },
        '(' => { _ = self.next(); return .left_paren; },
        ')' => { _ = self.next(); return .right_paren; },
        ';' => { _ = self.next(); return .semicolon; },
        '+' => { _ = self.next(); return .plus; },
        '*' => { _ = self.next(); return .star; },
        '=' => { _ = self.next(); return .equals_sign; },
        '/' => { _ = self.next(); return .forward_slash; },
        '!' => { self.skipUntil('\n'); return .comment; },
        ':' => {
            self.pushError("Unexpected ':'", .{});
            self.pushNote("':' can only follow an identifier, e.g. `L1:`", .{});
            self.finishError();

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
                    self.pushError("bad character in integer '{s}'", .{unparsed_number});
                    self.finishError();
                },
                error.Overflow => {
                    self.pushError("integer '{s}' is too big", .{unparsed_number});
                    self.pushNote("integers must be in range [-32768 .. 32767]", .{});
                    self.finishError();
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
                        self.pushError("no directive '{s}' exists", .{directive_identifier});
                        self.pushNote("'{s}' was interpreted as a directive because it starts with a '.'", .{directive_identifier});
                        self.finishError();
                    }
                } else {
                    self.pushError("expected a directive after '.'", .{});
                    self.finishError();
                }
            } else {
                self.pushError("expected a directive after '.'", .{});
                self.finishError();
            }
            return null;
        },
        '"' => {
            if (self.skipStringLiteral()) |unparsed_string| {
                // This allocation will be freed along with the arena
                if (std.zig.string_literal.parseAlloc(self.allocator, unparsed_string)) |string| {
                    return .{ .string = string };
                } else |_| {
                    self.pushError("invalid string literal", .{});
                    self.finishError();
                }
            } else {
                self.pushError("unclosed '\"'", .{});
                self.finishError();
            }
            return null;
        },
        ' ', '\t', '\r' => {
            _ = self.next();
            return null;
        },
        else => {
            self.pushError("unextpected char '{c}'", .{c});
            self.finishError();
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
            self.position += 1;
        } else {
            break;
        }
    }
}

// Includes quotes, so, returns "abc" instead of abc
fn skipStringLiteral(self: *Self) ?[]const u8 {
    const start = self.position;
    _ = self.next(); // skip opening '"'
    while (self.next()) |c| {
        if (c == '\\') {
            _ = self.next();
        } else if (c == '"') {
            // self.position points to a character past the closing '"'
            return self.line[start .. self.position];
        }
    }
    return null;
}

fn skipIdentifier(self: *Self) []const u8 {
    const start = self.position;
    while (self.peek()) |c| {
        if (isIdentifierChar(c)) {
            self.position += 1;
        } else {
            break;
        }
    }
    return self.line[start .. self.position];
}

fn next(self: *Self) ?u8 {
    const current = self.peek();
    self.position = @min(self.position + 1, self.line.len);
    return current;
}

fn peek(self: *const Self) ?u8 {
    return self.peekN(0);
}

fn peekN(self: *const Self, n: usize) ?u8 {
    return common.getOrNull(u8, self.line, self.position + n);
}

fn pushNote(self: *Self, comptime fmt: []const u8, args: anytype) void {
    var buf = &self.last_error;
    std.fmt.format(buf.writer(), "note: ", .{}) catch unreachable;
    std.fmt.format(buf.writer(), fmt, args) catch unreachable;
    buf.append('\n') catch unreachable;
}

fn pushError(
    self: *Self,
    comptime error_fmt: []const u8,
    error_args: anytype,
) void {
    var buf = &self.last_error;
    if (self.filepath) |filepath| {
        std.fmt.format(buf.writer(), "{s}:{}:{}: error: ", .{
            std.fs.path.basename(filepath), // If we have files named the same in two different directories, this could be confusing.
                                            // But. This is an old assembler for a processor that no one uses. I won't do it.
            self.location.line,
            self.location.column,
        }) catch unreachable;
    } else {
        std.fmt.format(buf.writer(), "{}:{}: error: ", .{
            self.location.line,
            self.location.column,
        }) catch unreachable;
    }
    std.fmt.format(buf.writer(), error_fmt, error_args) catch unreachable;
    buf.append('\n') catch unreachable;
    std.fmt.format(buf.writer(), "\t{s}\n\t", .{self.line}) catch unreachable;
    for (0 .. self.location.column - 1) |_| {
        buf.append(' ') catch unreachable;
    }
    buf.append('^') catch unreachable;
    buf.append('\n') catch unreachable;
}

pub fn finishError(self: *Self) void {
    const error_copy = self.last_error.toOwnedSlice() catch unreachable;
    self.errors.append(error_copy) catch unreachable;
}

const testing = std.testing;

test "tokenize labels" {
    const allocator = std.testing.allocator;
    {
        const source = ProgramSourceCode{
            .filepath = null,
            .contents =
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
        const source = ProgramSourceCode{
            .filepath = null,
            .contents =
                \\"Hello, World!\n"
        };
        
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var actual_tokens = try tokenize(source, &arena);

        const token = actual_tokens.items(.token)[0];
        try expectStringToken("Hello, World!\n", token);
    }
    
    {
        const source = ProgramSourceCode{
            .filepath = null,
            .contents =
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
        const source = ProgramSourceCode{
            .filepath = null,
            .contents =
            \\"\""
        };
        
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var actual_tokens = try tokenize(source, &arena);
        
        try expectStringToken("\"", actual_tokens.items(.token)[0]);
    }
    
    {
        const source = ProgramSourceCode{
            .filepath = null,
            .contents =
                \\"newline - \n, tab - \t, backslash - \\, quote - \", escaped quote - \\\", single quote - '"
        };
        
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        var actual_tokens = try tokenize(source, &arena);

        try expectStringToken(
            "newline - \n, tab - \t, backslash - \\, quote - \", escaped quote - \\\", single quote - '",
            actual_tokens.items(.token)[0]
        );
    }

}


test "tokenize simple source file" {
    const allocator = std.testing.allocator;
    const source = ProgramSourceCode{
        .filepath = null,
        .contents = 
            \\! Comment
            \\.SECT .TEXT
            \\  MOV AX, 3
            \\  ADD BX, 4
    };

    const expected_tokens = [_]Token {
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
    const expected_tokens = [_]Token {
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
    const source = ProgramSourceCode{
        .filepath = null,
        .contents = 
            \\  _MY_VAR = 2
            \\MOV   AX, _MY_VAR
    };
    const expected_tokens = [_]Token {
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