const std = @import("std");


const Tokenizer = @import("Tokenizer.zig");
pub const ProgramSource = Tokenizer.ProgramSource;

const print = std.debug.print;

// In assembly, ast is flat
const AstNode = union(enum) {
    instruction: Instruction,
};

const Instruction = struct {
    kind: Tokenizer.InstructionKind,
    operand1: Operand,
    operand2: Operand,
};

const Operand = union(enum) {
    immediate: i16,
    register: Tokenizer.Register,
    memory: []const u8,
};

const Parser = struct {
    const Self = @This();

    const Token = Tokenizer.Token;

    ast: std.ArrayList(AstNode),
    labels: std.ArrayList([]const u8),
    tokens: []Token,
    position: usize,

    pub fn parse(
        tokens: std.MultiArrayList(Tokenizer.TokenWithLocation),
        allocator: std.mem.Allocator
    ) !std.ArrayList(AstNode) {
        var parser = Self{
            .ast = std.ArrayList(AstNode).init(allocator),
            .labels = std.ArrayList([]const u8).init(allocator),
            .tokens = tokens.items(.token),
            .position = 0,
        };

        while (parser.peek()) |token| {
            switch (token) {
                .directive => |directive| {
                    if (directive == .sect) {
                        try parser.parseSection();
                    } else {
                        print("didn't expect directive '{}'\n", .{directive});
                        _ = parser.next();
                    }
                },
                .instruction => |_| {
                    try parser.parseInstruction();
                },
                .identifier => |_| {
                    // TODO: parse a constant definition
                    _ = parser.next();
                },
                .label => |label| {
                    try parser.labels.append(label);
                    try parser.parseDataInitializer();
                    _ = parser.next();
                },
                .comment => {
                    _ = parser.next();
                    _ = parser.match(.newline);
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

    fn parseSection(self: *Self) !void {
        _ = self.next() orelse return;
        const directive_after_sect = self.match(.directive) orelse return;
        if (!directive_after_sect.directive.isOneOf(&[_]Tokenizer.Directive{ .text, .data, .bss })) {
            // TODO: report error
            return;
        }
        // try self.addNode(.{ .section = section.token.section });
    }

    fn parseInstruction(self: *Self) !void {
        const instruction_token = self.next() orelse return;
        const operand1 = self.parseInstructionOperand() orelse return;
        _ = self.match(.comma) orelse return;
        const operand2 = self.parseInstructionOperand() orelse return;
        try self.addNode(.{ .instruction = .{
            .kind = instruction_token.instruction,
            .operand1 = operand1,
            .operand2 = operand2,
        } });
    }

    fn addNode(self: *Self, ast_node: AstNode) !void {
        try self.ast.append(ast_node);
    }

    fn parseInstructionOperand(self: *Self) ?Operand {
        if (self.next()) |token| {
            switch (token) {
                .number => {
                    return Operand { .immediate = token.number };
                },
                .register => {
                    return Operand { .register = token.register };
                },
                .left_paren => {
                    const label = self.match(.identifier) orelse return null;
                    _ = self.match(.right_paren) orelse return null;
                    // TODO: Some indexing scheme
                    return Operand { .memory = label.identifier };
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

    fn match(self: *Self, expected: Token.Tag) ?Token {
        if (self.next()) |token| {
            if (token.is(expected)) {
                return token;
            } else {
                self.reportError("expected '{}', but found '{}'\n", .{expected, token});
                return null;
            }
        }
        self.reportError("expected '{}'\n", .{expected});
        return null;
    }
    
    fn next(self: *Self) ?Token {
        const result = self.peek();
        self.position = @min(self.position + 1, self.tokens.len);
        return result;
    }

    fn peek(self: *const Self) ?Token {
        return self.peekN(0);
    }

    fn peekN(self: *const Self, n: usize) ?Token {
        if (self.position + n < self.tokens.len) {
            return self.tokens[self.position + n];
        }
        return null;
    }

    fn reportError(self: *Self, comptime fmt: []const u8, args: anytype) void {
        _ = self;
        print("error): ", .{});
        print(fmt, args);
    }
};



pub fn assemble(source: ProgramSource, arena: *std.heap.ArenaAllocator) !std.ArrayList(AstNode) {
    const tokens = try Tokenizer.tokenize(source, arena);

    for (0 .. tokens.slice().len) |i| {
        const token = tokens.get(i);
        print("({}:{}): {}\n", .{ token.location.line, token.location.column, token.token });
    }

    const ast = try Parser.parse(tokens, arena.allocator());
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
    registers: [std.enums.values(Tokenizer.Register).len]i16,
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
