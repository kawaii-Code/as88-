const std = @import("std");

const print = std.debug.print;


const Self = @This();

pub const ProgramSource = struct {
    filepath: ?[]const u8,
    contents: []const u8,
};

pub const SourceLocation = struct {
    line: usize,
    column: usize,
};

pub const TokenWithLocation = struct {
    token: Token,
    location: SourceLocation,
};

pub const Token = union(enum) {
    pub const Tag = std.meta.Tag(@This());

    none,
    comma,
    left_paren,
    right_paren,
    newline,
    semicolon,
    comment,
    label: []const u8,
    string: []const u8,
    identifier: []const u8,
    number: i16,
    directive: Directive,
    register: Register,
    instruction: InstructionKind,

    pub fn is(self: @This(), tag: Tag) bool {
        return self == tag;
    }
};

pub const InstructionKind = enum {
    mov,
    add,
};

pub const Register = enum {
    ax,
    bx,
    cx,
    dx,
};

pub const Directive = enum {
    sect,
    word,
    space,
    text,
    data,
    bss,

    pub fn isOneOf(self: @This(), expected: []const @This()) bool {
        return std.mem.indexOfScalar(@This(), expected, self) != null;
    }

    pub fn isSectionType(self: @This()) bool {
        return switch (self) {
            .text, .data, .bss => true,
            else => false,
        };
    }

    pub fn isMemory(self: @This()) bool {
        return switch (self) {
            .word, .space => true,
            else => false,
        };
    }
};


const instruction_names = EnumNamesToMembers(InstructionKind).init();
const register_names = EnumNamesToMembers(Register).init();
const directive_names = EnumNamesToMembers(Directive).init();


filepath: ?[]const u8,
line: []const u8,
location: SourceLocation,
position: usize,
allocator: std.mem.Allocator,
tokens: std.MultiArrayList(TokenWithLocation),


pub fn tokenize(
    source: ProgramSource,
    arena: *std.heap.ArenaAllocator
) !std.MultiArrayList(TokenWithLocation) {
    var tokenizer = Self{
        .filepath = source.filepath,
        .line = undefined,
        .position = 0,
        .location = .{
            .line = 1,
            .column = 1,
        },
        .allocator = arena.allocator(),
        .tokens = std.MultiArrayList(TokenWithLocation) {},
    };

    var line_it = std.mem.tokenizeAny(u8, source.contents, "\n");
    while (line_it.next()) |line| : (tokenizer.location.line += 1) {
        tokenizer.line = line;
        tokenizer.location.column = 1;
        tokenizer.position = 0;
        
        while (tokenizer.peek()) |c| : (tokenizer.location.column = tokenizer.position + 1) {
            switch (c) {
                '!' => {
                    try tokenizer.addToken(.comment);
                    tokenizer.skipUntil('\n');
                },
                ',' => try tokenizer.addTokenAndNext(.comma),
                '(' => try tokenizer.addTokenAndNext(.left_paren),
                ')' => try tokenizer.addTokenAndNext(.right_paren),
                ';' => try tokenizer.addTokenAndNext(.semicolon),
                '-', '0'...'9' => {
                    const unparsed_number = tokenizer.skipWhile(isNumberChar);
                    if (std.fmt.parseInt(i16, unparsed_number, 0)) |number| {
                        try tokenizer.addToken(.{ .number = number });
                    } else |err| switch (err) {
                        error.InvalidCharacter => tokenizer.reportError("bad character in integer '{s}'\n", .{unparsed_number}),
                        error.Overflow => {
                            tokenizer.reportError("integer '{s}' is too big\n", .{unparsed_number});
                            tokenizer.reportNote("in intel8088 assembly, integers must be in range [-32768 .. 32767]\n", .{});
                        },
                    }
                },
                'a'...'z', 'A'...'Z', '_' => {
                    const identifier = tokenizer.skipWhile(isIdentifierChar);
                    if (register_names.find(identifier)) |register| {
                        try tokenizer.addToken(.{ .register = register });
                    } else if (instruction_names.find(identifier)) |instruction| {
                        try tokenizer.addToken(.{ .instruction = instruction });
                    } else {
                        const token = init: {
                            if (tokenizer.peek()) |char_after_identifier| {
                                if (char_after_identifier == ':') { 
                                    // skip ':'
                                    _ = tokenizer.next();
                                    break :init Token{ .label = identifier };
                                }
                            }
                            break :init Token{ .identifier = identifier };
                        };
                        try tokenizer.addToken(token);
                    }
                },
                ':' => {
                    tokenizer.reportError("':' can only follow an identifier, in which case it means a label\n", .{});
                    _ = tokenizer.next();
                },
                '.' => {
                    _ = tokenizer.next();
                    if (tokenizer.peek()) |first_directive_char| {
                        if (isIdentifierChar(first_directive_char)) {
                            const directive_identifier = tokenizer.skipWhile(isIdentifierChar);
                            if (directive_names.find(directive_identifier)) |directive| {
                                try tokenizer.addToken(.{ .directive = directive });
                            } else {
                                tokenizer.reportError("no directive '{s}' exists\n", .{directive_identifier});
                                tokenizer.reportNote("'{s}' was interpreted as a directive because it starts with a '.'\n", .{directive_identifier});
                            }
                        } else {
                            tokenizer.reportError("expected a directive after '.'\n", .{});
                        }
                    } else {
                        tokenizer.reportError("expected a directive after '.'\n", .{});
                    }
                },
                '"' => {
                    if (tokenizer.skipStringLiteral()) |unparsed_string| {
                        // This allocation will be freed along with the arena
                        if (std.zig.string_literal.parseAlloc(tokenizer.allocator, unparsed_string)) |string| {
                            try tokenizer.addToken(.{ .string = string });
                        } else |_| {
                            tokenizer.reportError("invalid string literal\n", .{});
                        }
                    } else {
                        tokenizer.reportError("unclosed '\"'\n", .{});
                    }
                },
                ' ', '\t', '\r' => _ = tokenizer.next(),
                else => {
                    _ = tokenizer.next();
                    print("Unhandled char '{c}'\n", .{c});
                },
            }
        }
        
        try tokenizer.addToken(.newline);
    }


    return tokenizer.tokens;
}

fn addTokenAndNext(self: *Self, token: Token) !void {
    try self.addToken(token);
    _ = self.next();
}

fn addToken(self: *Self, token: Token) !void {
    try self.tokens.append(self.allocator, .{ .token = token, .location = self.location });
}

fn isIdentifierChar(c: u8) bool {
    return switch (std.ascii.toLower(c)) {
        'a'...'z', '_', '0'...'9' => true,
        else => false,
    };
}

fn isNumberChar(c: u8) bool {
    // Accept letters like 'a'..'z' in numbers for better error reporting
    return isIdentifierChar(c) or c == '-';
}

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

fn skipUntil(self: *Self, sentinel: u8) void {
    while (self.peek()) |c| {
        if (c != sentinel) {
            self.position += 1;
        } else {
            break;
        }
    }
}

fn skipWhile(self: *Self, predicate: *const fn (u8) bool) []const u8 {
    const start = self.position;
    while (self.peek()) |c| {
        if (predicate(c)) {
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
    if (self.position < self.line.len) {
        return self.line[self.position];
    }
    return null;
}

fn reportNote(self: *Self, comptime fmt: []const u8, args: anytype) void {
    _ = self;
    print("note: ", .{});
    print(fmt, args);
}

fn reportError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    print("{s}:{}:{}: error: ", .{ self.filepath orelse "no file", self.location.line, self.location.column });
    print(fmt, args);
    print("\t{s}\n\t", .{self.line});
    for (0 .. self.location.column - 1) |_| {
        print(" ", .{});
    }
    print("^\n", .{});
}


fn EnumNamesToMembers(comptime T: type) type {
    return struct {
        names: @TypeOf(std.meta.fieldNames(T)),
        members: []const T,

        pub fn init() @This() {
            return @This(){
                .names = std.meta.fieldNames(T),
                .members = std.enums.values(T),
            };
        }

        pub fn find(self: *const @This(), name: []const u8) ?T {
            for (0..self.names.len) |i| {
                if (std.ascii.eqlIgnoreCase(self.names[i], name)) {
                    return self.members[i];
                }
            }
            return null;
        }
    };
}


const testing = std.testing;

test "tokenize labels" {
    const allocator = std.testing.allocator;
    {
        const source = ProgramSource{
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
        const source = ProgramSource{
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
        const source = ProgramSource{
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
        const source = ProgramSource{
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
        const source = ProgramSource{
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
    const source = ProgramSource{
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
        
        .{ .instruction = .mov },
        .{ .register = .ax },
        .comma,
        .{ .number = 3 },
        .newline,
        
        .{ .instruction = .add },
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

fn expectStringToken(expected: []const u8, token: Token) anyerror!void {
    try testing.expect(token.is(.string));
    try testing.expectEqualStrings(expected, token.string);
}

fn expectLabelToken(expected: []const u8, token: Token) anyerror!void {
    try testing.expect(token.is(.label));
    try testing.expectEqualStrings(expected, token.label);
}