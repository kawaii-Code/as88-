const std = @import("std");

const print = std.debug.print;

// In assembly, ast is flat
const AstNode = union(enum) {
    instruction: Instruction,
};

const Instruction = struct {
    kind: InstructionKind,
    operand1: Operand,
    operand2: Operand,
};

const Operand = union(enum) {
    immediate: i16,
    register: Register,
    memory: []const u8,
};

const InstructionKind = enum {
    mov,
    add,
};

const Register = enum {
    ax,
    bx,
    cx,
    dx,
};

const Directive = enum {
    sect,
    word,
    space,
    text,
    data,
    bss,
};

const instruction_names = EnumNamesToMembers(InstructionKind).init();
const register_names = EnumNamesToMembers(Register).init();
const directive_names = EnumNamesToMembers(Directive).init();

const TokenWithLocation = struct {
    token: Token,
    location: SourceLocation,
};

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
    number: i16,
    directive: Directive,
    register: Register,
    instruction: InstructionKind,
};


const Tokenizer = struct {
    const Self = @This();

    filepath: []const u8,
    line: []const u8,
    location: SourceLocation,
    position: usize,
    tokens: std.ArrayList(TokenWithLocation),

    pub fn tokenize(
        source: SourceFile,
        allocator: std.mem.Allocator
    ) !std.ArrayList(TokenWithLocation) {
        var tokenizer = Self{
            .filepath = source.filepath,
            .line = undefined,
            .position = 0,
            .location = .{
                .line = 1,
                .column = 1,
            },
            .tokens = std.ArrayList(TokenWithLocation).init(allocator),
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
                        } else if (instruction_names.find(word)) |instruction| {
                            try tokenizer.addToken(.{ .instruction = instruction });
                        } else {
                            try tokenizer.addToken(.{ .identifier = word });
                        }
                    },
                    '.' => {
                        _ = tokenizer.next();
                        if (tokenizer.peek()) |first_directive_char| {
                            if (isIdentifierChar(first_directive_char)) {
                                const directive_text = tokenizer.skipWhile(isIdentifierChar);
                                if (directive_names.find(directive_text)) |directive| {
                                    try tokenizer.addToken(.{ .directive = directive });
                                } else {
                                    tokenizer.reportError("no directive '{s}' exists\n", .{directive_text});
                                    tokenizer.reportNote("'{s}' was interpreted as a directive because it starts with a '.'\n", .{directive_text});
                                }
                            } else {
                                tokenizer.reportError("expected a directive after '.'\n", .{});
                            }
                        } else {
                            tokenizer.reportError("expected a directive after '.'\n", .{});
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
        print("{s}:{}:{}: error: ", .{ self.filepath, self.location.line, self.location.column });
        print(fmt, args);
        print("\t{s}\n\t", .{self.line});
        for (0 .. self.location.column - 1) |_| {
            print(" ", .{});
        }
        print("^\n", .{});
    }
};


const Parser = struct {
    const Self = @This();

    ast: std.ArrayList(AstNode),
    labels: std.ArrayList([]const u8),
    tokens: std.ArrayList(TokenWithLocation),
    position: usize,
    next_has_label: bool,

    pub fn parse(
        tokens: std.ArrayList(TokenWithLocation),
        allocator: std.mem.Allocator
    ) !std.ArrayList(AstNode) {
        var parser = Self{
            .ast = std.ArrayList(AstNode).init(allocator),
            .labels = std.ArrayList([]const u8).init(allocator),
            .tokens = tokens,
            .position = 0,
            .next_has_label = false,
        };
        defer parser.labels.deinit();

        while (parser.peek()) |token_with_location| {
            const token = token_with_location.token;

            switch (token) {
                .directive => |directive| {
                    if (directive == .sect) {
                        try parser.parseSection();
                    } else {
                        print("didn't expect directive '{}'\n", .{directive});
                    }
                },
                .instruction => |_| {
                    try parser.parseInstruction();
                },
                .identifier => |_| {
                    // TODO: Typos, such as 'MUV', would
                    // get here and result in a confusing error.
                    try parser.parseLabel();
                },
                .comment => {
                    _ = parser.next();
                    _ = parser.expect(.newline);
                },
                .newline => {
                    _ = parser.next();
                },
                else => {
                    print("didn't expect token: {}\n", .{token});
                    _ = parser.next();
                },
            }
        }

        return parser.ast;
    }

    fn parseLabel(self: *Self) !void {
        const label_identifier = self.next() orelse return;
        _ = self.expect(.colon) orelse return;
        try self.addLabel(label_identifier.token.identifier);
    }

    fn parseSection(self: *Self) !void {
        _ = self.next() orelse return;
        const section = self.expectOneOfDirectives(&[_]Directive{ .text, .bss, .data }) orelse return;
        _ = section;
        // try self.addNode(.{ .section = section.token.section });
    }

    fn parseInstruction(self: *Self) !void {
        const instruction_kind = self.next() orelse return;
        const operand1 = self.expectInstructionOperand() orelse return;
        _ = self.expect(.comma) orelse return;
        const operand2 = self.expectInstructionOperand() orelse return;
        try self.addNode(.{ .instruction = .{
            .kind = instruction_kind.token.instruction,
            .operand1 = operand1,
            .operand2 = operand2,
        } });
    }

    fn addLabel(self: *Self, label: []const u8) !void {
        self.next_has_label = true;
        try self.labels.append(label);
    }

    fn addNode(self: *Self, ast_node: AstNode) !void {
        try self.ast.append(ast_node);
    }

    fn expectInstructionOperand(self: *Self) ?Operand {
        if (self.next()) |token_with_location| {
            const token = token_with_location.token;
            switch (token) {
                .number => {
                    return Operand { .immediate = token.number };
                },
                .register => {
                    return Operand { .register = token.register };
                },
                .left_paren => {
                    const label = self.expect(.identifier) orelse return null;
                    _ = self.expect(.right_paren);
                    // TODO: Some indexing scheme
                    return Operand { .memory = label.token.identifier };
                },
                else => {
                    // TODO: Report error
                    return null;
                }
            }
        }
        // TODO: Report error
        return null;
    }

    fn isInstructionArgument(token: Token) bool {
        return switch (token) {
            .instruction => |_| true,
            .register => |_| true,
            .number => |_| true,
            else => false,
        };
    }

    fn expect(self: *Self, expected_token: std.meta.Tag(Token)) ?TokenWithLocation {
        if (self.next()) |token| {
            if (token.token == expected_token) {
                return token;
            } else {
                self.reportError("expected '{}', found '{}'\n", .{expected_token, token.token});
                return null;
            }
        }
        // TODO: Report error
        return null;
    }
    
    fn expectOneOfDirectives(self: *Self, comptime expected_directives: []const Directive) ?TokenWithLocation {
        if (self.next()) |token_with_location| {
            switch (token_with_location.token) {
                .directive => |directive| {
                    if (std.mem.indexOfScalar(Directive, expected_directives, directive)) |_| {
                         return token_with_location;
                    }
                },
                else => {
                    // TODO: Report error
                    return null;
                },
            }
        }
        // TODO: Report error
        return null;
    }

    fn expectFunc(self: *Self, predicate: *const fn (Token) bool) ?TokenWithLocation {
        if (self.next()) |token| {
            if (predicate(token.token)) {
                return token;
            } else {
                // TODO: Report error
                return null;
            }
        }
        // TODO: Guaranteed error
        return null;
    }

    fn next(self: *Self) ?TokenWithLocation {
        const result = self.peek();
        self.position = @min(self.position + 1, self.tokens.items.len);
        return result;
    }

    fn peekN(self: *const Self, n: usize) ?TokenWithLocation {
        if (self.position + n < self.tokens.items.len) {
            return self.tokens.items[self.position + n];
        }
        return null;
    }

    fn peek(self: *const Self) ?TokenWithLocation {
        return self.peekN(0);
    }

    fn reportError(self: *Self, comptime fmt: []const u8, args: anytype) void {
        _ = self;
        print("error): ", .{});
        print(fmt, args);
    }
};


const SourceFile = struct {
    filepath: []const u8,
    contents: []const u8,
};


pub fn assemble(source: SourceFile, allocator: std.mem.Allocator) !std.ArrayList(AstNode) {
    const tokens = try Tokenizer.tokenize(source, allocator);
    defer tokens.deinit();

    for (tokens.items) |token| {
        print("({}:{}): {}\n", .{ token.location.line, token.location.column, token.token });
    }

    const ast = try Parser.parse(tokens, allocator);
    for (ast.items) |node| {
        print("{}\n", .{node});
    }
    return ast;
}

pub fn run(ast: std.ArrayList(AstNode), allocator: std.mem.Allocator) !void {
    print("---------------------------\n", .{});
    var cpu = CPU {
        .registers = undefined,
    };
    for (0 .. cpu.registers.len) |i| {
        cpu.registers[i] = 0;
    }
    
    print("registers = {any}\n", .{cpu.registers});
    for (ast.items) |node| {
        switch (node) {
            .instruction => |instruction| {
                step(&cpu, instruction);
            },
        }
        print("registers = {any}\n", .{cpu.registers});
    }
    _ = allocator;
}

const CPU = struct {
    registers: [std.enums.values(Register).len]i16,
};

fn step(cpu: *CPU, instruction: Instruction) void {
    switch (instruction.kind) {
        .mov => {
            const lhs = instruction.operand1;
            const rhs = instruction.operand2;
            var destination: *i16 = undefined;
            
            switch (lhs) {
                .register => destination = &cpu.registers[@as(u32, @intFromEnum(lhs.register))],
                .immediate => todo(), // Can't write into immediate
                else => todo(),
            }
            
            switch (rhs) {
                .register => destination.* = cpu.registers[@as(u32, @intFromEnum(rhs.register))],
                .immediate => destination.* = rhs.immediate,
                else => todo(),
            }
        },
        .add => {
            const lhs = instruction.operand1;
            const rhs = instruction.operand2;
            var destination: *i16 = undefined;
            
            switch (lhs) {
                .register => destination = &cpu.registers[@as(u32, @intFromEnum(lhs.register))],
                .immediate => todo(), // Can't write into immediate
                else => todo(),
            }
            
            switch (rhs) {
                .register => destination.* += cpu.registers[@as(u32, @intFromEnum(rhs.register))],
                .immediate => destination.* += rhs.immediate,
                else => todo(),
            }
        },
    }
}

fn todo() noreturn {
    std.debug.panic("this was not yet implemented\n", .{});
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


test "tokenize simple instructions" {
    const allocator = std.testing.allocator;

    const source = SourceFile{
        .filepath = "*fake file*",
        .contents = "MOV AX, 3; ADD BX, 4\n",
    };

    const expected_tokens = [_]Token {
        .{ .instruction = .mov },
        .{ .register = .ax },
        .comma,
        .{ .number = 3 },
        .semicolon,
        .{ .instruction = .add },
        .{ .register = .bx },
        .comma,
        .{ .number = 4 },
        .newline,
    };

    const actual_tokens = try Tokenizer.tokenize(source, allocator);
    defer actual_tokens.deinit();
    try std.testing.expectEqual(expected_tokens.len, actual_tokens.items.len);
    for (actual_tokens.items, 0..) |token_with_location, i| {
        const actual = token_with_location.token;
        const expected = expected_tokens[i];
        try std.testing.expectEqual(expected, actual);
    }
}