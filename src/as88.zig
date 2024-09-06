const std = @import("std");

const print = std.debug.print;

// In assembly, ast is flat
const AstNode = union(enum) {
    instruction: Instruction,
    register_name: Register,
    section: Section,
};

const Instruction = struct {
    kind: InstructionKind,
    args: void, // unimplemented
};

const Register = enum {
    ax,
    bx,
    cx,
    dx,
};

const Section = enum {
    text,
    data,
    bss,
};

const Keyword = enum {
    sect,
    word,
    space,
};

const instruction_names = EnumNamesToMembers(InstructionKind).init();
const register_names = EnumNamesToMembers(Register).init();
const section_names = EnumNamesToMembers(Section).init();
const keyword_names = EnumNamesToMembers(Keyword).init();

const TokenWithLocation = struct {
    token: Token,
    location: SourceLocation,
};

// TODO: add filename
const SourceLocation = struct {
    line: usize,
    column: usize,
};

const Token = union(enum) {
    none,
    comma,
    left_paren,
    right_paren,
    newline,
    colon,
    semicolon,
    comment,
    identifier: []const u8,
    keyword: Keyword,
    section: Section,
    number: i16,
    register: Register,
    instruction: InstructionKind,
};

const InstructionKind = enum {
    mov,
    add,
};

const Tokenizer = struct {
    const Self = @This();

    line: []const u8,
    location: SourceLocation,
    position: usize,
    tokens: std.ArrayList(TokenWithLocation),

    pub fn tokenize(source: []const u8, allocator: std.mem.Allocator) !std.ArrayList(TokenWithLocation) {
        var tokenizer = Self{
            .line = undefined,
            .position = 0,
            .location = .{
                .line = 1,
                .column = 1,
            },
            .tokens = std.ArrayList(TokenWithLocation).init(allocator),
        };

        var line_it = std.mem.tokenizeAny(u8, source, "\n");
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
                    ':' => try tokenizer.addTokenAndNext(.colon),
                    ';' => try tokenizer.addTokenAndNext(.semicolon),
                    '-', '0'...'9' => {
                        const number_bytes = tokenizer.skipWhile(isNumberChar);
                        if (std.fmt.parseInt(i16, number_bytes, 0)) |number| {
                            try tokenizer.addToken(.{ .number = number });
                        } else |err| switch (err) {
                            error.InvalidCharacter => tokenizer.reportError("bad character in integer '{s}'\n", .{number_bytes}),
                            error.Overflow => {
                                tokenizer.reportError("integer '{s}' is too big\n", .{number_bytes});
                                tokenizer.reportNote("in intel8088 assembly, integers must be in range [-32768 .. 32767]\n", .{});
                            },
                        }
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        const word = tokenizer.skipWhile(isIdentifierChar);
                        if (register_names.find(word)) |register| {
                            try tokenizer.addToken(.{ .register = register });
                        } else if (section_names.find(word)) |section| {
                            try tokenizer.addToken(.{ .section = section });
                        } else if (instruction_names.find(word)) |instruction| {
                            try tokenizer.addToken(.{ .instruction = instruction });
                        } else {
                            try tokenizer.addToken(.{ .identifier = word });
                        }
                    },
                    '.' => {
                        _ = tokenizer.next();
                        if (tokenizer.peek()) |first_keyword_char| {
                            if (isIdentifierChar(first_keyword_char)) {
                                const keyword_text = tokenizer.skipWhile(isIdentifierChar);
                                if (keyword_names.find(keyword_text)) |keyword| {
                                    try tokenizer.addToken(.{ .keyword = keyword });
                                } else {
                                    tokenizer.reportError("no keyword '{s}' exists\n", .{keyword_text});
                                    tokenizer.reportNote("'{s}' was interpreted as a keyword because it starts with a '.'\n", .{keyword_text});
                                }
                            } else {
                                tokenizer.reportError("expected a keyword after '.'\n", .{});
                            }
                        } else {
                            tokenizer.reportError("expected a keyword after '.'\n", .{});
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
        try self.tokens.append(.{ .token = token, .location = self.location });
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
        print("error({}:{}): ", .{ self.location.line, self.location.column });
        print(fmt, args);
        print("\t{s}\n\t", .{self.line});
        for (0 .. self.location.column - 1) |_| {
            print(" ", .{});
        }
        print("^\n", .{});
    }
};


pub fn assemble(source: []const u8, allocator: std.mem.Allocator) !std.ArrayList(AstNode) {
    const tokens = try Tokenizer.tokenize(source, allocator);
    defer tokens.deinit();

    for (tokens.items) |token| {
        print("({}:{}): {}\n", .{ token.location.line, token.location.column, token.token });
    }

    const ast = std.ArrayList(AstNode).init(allocator);
    return ast;
}

pub fn run(ast: std.ArrayList(AstNode), allocator: std.mem.Allocator) !void {
    _ = ast;
    _ = allocator;
}

fn EnumNamesToMembers(comptime T: type) type {
    return struct {
        const Self = @This();

        names: @TypeOf(std.meta.fieldNames(T)),
        members: []const T,

        pub fn init() Self {
            return Self{
                .names = std.meta.fieldNames(T),
                .members = std.enums.values(T),
            };
        }

        pub fn find(self: *const Self, name: []const u8) ?T {
            for (0..self.names.len) |i| {
                if (std.ascii.eqlIgnoreCase(self.names[i], name)) {
                    return self.members[i];
                }
            }
            return null;
        }
    };
}
