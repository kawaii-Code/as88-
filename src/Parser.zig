const std = @import("std");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");
const Tokenizer = @import("Tokenizer.zig");

const Token = Tokenizer.Token;
const print = std.debug.print;


const Self = @This();

pub const ParseResult = struct {
    instructions: std.ArrayList(Instruction),
    memory: std.ArrayList(Memory),
    labels: std.StringHashMap(Label),
};

pub const Instruction = struct {
    mnemonic: intel8088.InstructionMnemonic,
    operands: []const intel8088.Operand,
};

// A label can address either memory or code, e.g.
// L1: MOV AX, BX
// or
// L1:  .WORD   2
pub const Label = union(enum) {
    instruction: u16,
    memory_field: u16,
};

pub const Memory = struct {
    size:  u16,
    value: i16, // This is incorrect. Memory can be of any size.
};


const ParseState = enum {
    no_section,
    text_section,
    data_section,
    bss_section,
};

const PreprocessResult = struct {
    labels: std.StringHashMap(Label),
    memory: std.ArrayList(Memory),
};


allocator: std.mem.Allocator,
result: ParseResult,
tokens: []Token,
position: usize,


pub fn parse(
    tokens: std.MultiArrayList(Tokenizer.TokenWithLocation),
    allocator: std.mem.Allocator
) !ParseResult {
    var parser = Self{
        .allocator = allocator,
        .result = .{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .memory = undefined,
            .labels = undefined,
        },
        .tokens = tokens.items(.token),
        .position = 0,
    };

    const preprocess_result = try parser.preprocess(allocator);
    parser.result.memory = preprocess_result.memory;
    parser.result.labels = preprocess_result.labels;

    // We should only parse variable declarations
    // and instructions. Simple enough?
    var parser_state: ParseState = .no_section;
    while (parser.peek()) |token| {
        switch (token) {
            .directive => |directive| {
                if (directive == .sect) {
                    if (parser.match(.directive)) |next_directive| {
                        if (next_directive.directive.isSectionType()) {
                            parser_state = switch(next_directive.directive) {
                                .text => .text_section,
                                .data => .data_section,
                                .bss => .bss_section,
                                else => unreachable,
                           };
                        } else {
                            // TODO: Report error
                        }
                    } else {
                        // TODO: Report error
                    }
                    _ = parser.next();
                } else {
                    if (!directive.isMemory()) {
                        print("didn't expect directive '{}'\n", .{directive});
                    }
                    _ = parser.next();
                }
            },
            .instruction_mnemonic => |_| {
                try parser.parseInstruction();
            },
            .identifier => |_| {
                // TODO: parse a constant definition
                _ = parser.next();
            },
            .comment => {
                _ = parser.next();
                _ = parser.match(.newline);
            },
            .newline, .label => {
                _ = parser.next();
            },
            else => {
                //print("didn't expect token: {}\n", .{token});
                _ = parser.next();
            },
        }
    }

    return parser.result;
}

// Tokens are preprocessed before being parsed.
// This step finds and sets up labels and data fields.
// It also computes how much memory the DATA and BSS sections occupy.
fn preprocess(
    self: *Self,
    allocator: std.mem.Allocator
) !PreprocessResult {
    // After preprocessing the token stream,
    // go back to the beginning.
    defer self.position = 0;

    var labels = std.StringHashMap(Label).init(allocator);
    var memory = std.ArrayList(Memory).init(allocator);
    
    var next_token_is_under_label: ?[]const u8 = null;
    var instruction_pointer: u16 = 0;
    var memory_pointer: u16 = 0;
    while (self.next()) |token| : (next_token_is_under_label = if (token.is(.label)) token.label else null) {
        switch (token) {
            .instruction_mnemonic => |_| {
                if (next_token_is_under_label) |label_identifier| {
                    try labels.put(label_identifier, Label{ .memory_field = memory_pointer });
                }
                instruction_pointer += 1;
            },
            .directive => |directive| {
                if (!directive.isOneOf(&[_]intel8088.asm_syntax.Directive{ .word, .space })) {
                    continue;
                }

                var memory_field: Memory = undefined;
                if (directive == .space) {
                    if (self.next()) |next_token| {
                        if (next_token.is(.number)) {
                            // TODO: Check that we are in bss section
                            if (next_token.number >= 0) {
                                memory_field.size = @as(u16, @intCast(next_token.number));
                                memory_field.value = 0;
                            } else {
                                // TODO: Report error
                            }
                        } else {
                            // TODO: Report error
                        }
                    } else {
                        // TODO: Report error
                    }
                } else if (directive == .word) {
                    memory_field.size = 2;
                    if (self.match(.number)) |number_token| {
                        memory_field.value = number_token.number;
                    } else {
                        // TODO: Report error
                    }
                } else {
                    unreachable;
                }
                try memory.append(memory_field);

                if (next_token_is_under_label) |label_identifier| {
                    try labels.put(label_identifier, Label{ .memory_field = memory_pointer });
                }
                memory_pointer += memory_field.size;
            },
            else => {
                if (next_token_is_under_label) |_| {
                    // TODO: Report error
                }
            }
        }
    }
    
    return PreprocessResult{
        .labels = labels,
        .memory = memory,
    };
}

// TODO: Check the operands according to description
fn parseInstruction(self: *Self) !void {
    const instruction_token = self.next() orelse return;

    const description = intel8088.isa.get(instruction_token.instruction_mnemonic);

    // I could do the [8]Operand approach here as well, but it wastes
    // too much memory. I know that I am using an arena to allocate,
    // so this is fast anyway.
    var operands = try self.allocator.alloc(intel8088.Operand, description.operand_count);
    for (0 .. description.operand_count) |i| {
        operands[i] = self.parseInstructionOperand() orelse return;
        if (i != description.operand_count - 1) {
            _ = self.match(.comma) orelse return;
        }
    }
    try self.result.instructions.append(.{
        .mnemonic = instruction_token.instruction_mnemonic,
        .operands = operands,
    });
}

fn parseInstructionOperand(self: *Self) ?intel8088.Operand {
    if (self.next()) |token| {
        switch (token) {
            .number => {
                return .{ .immediate = token.number };
            },
            .register => {
                return .{ .register = token.register };
            },
            .left_paren => {
                const label_token = self.match(.identifier) orelse return null;
                _ = self.match(.right_paren) orelse return null;
                if (self.result.labels.get(label_token.identifier)) |label| {
                    // TODO: Accept labels to code
                    return .{ .memory = label.memory_field };
                } else {
                    // Also could be a register or an immediate
                    unreachable;
                }
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
    return common.getOrNull(Token, self.tokens, self.position + n);
}

fn reportError(self: *Self, comptime fmt: []const u8, args: anytype) void {
    _ = self;
    print("error: ", .{});
    print(fmt, args);
}



// The tokenizer is used in the tests instead of hardcoding
// the tokens. This means that if there is an error in the tokenizer,
// the parser tests would fail, which is confusing.
//
// But the tests get more readable, so this tradeoff was made.
const testing = std.testing;

test "parses text section" {
    const shared_beginning = ".SECT .TEXT\n";
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    {
        const source = Tokenizer.ProgramSource{
            .filepath = null,
            .contents = shared_beginning ++
                        \\  MOV AX, BX
        };

        const expected_instructions: []const Instruction = &.{
            .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .register = .bx } } },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const compilation_result = try parse(tokens, arena.allocator());

        try testing.expectEqualDeep(expected_instructions, compilation_result.instructions.items);
    }
}
 
test "parser compound expression" {
    const shared_beginning = ".SECT .TEXT\n";
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

   {
        const source = Tokenizer.ProgramSource{
            .filepath = null,
            .contents = shared_beginning ++
                        \\  MOV AX, 1 + 2
        };

        const expected_instructions: []const Instruction = &.{
            .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .immediate = 3 } } },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const compilation_result = try parse(tokens, arena.allocator());

        try testing.expectEqualDeep(expected_instructions, compilation_result.instructions.items);
    }
}

test "parses data section" {
    const shared_beginning = ".SECT .DATA\n";
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    {
        const source = Tokenizer.ProgramSource{
            .filepath = null,
            .contents = shared_beginning ++
                        \\x:    .WORD   3
                        \\y:    .WORD   5
        };

        const expected_labels = &[_]Label {
            .{ .memory_field = 0 },
            .{ .memory_field = 2 },
        };

        const expected_memory = &[_]Memory{
            .{ .size = 2, .value = 3 },
            .{ .size = 2, .value = 5 },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const compilation_result = try parse(tokens, arena.allocator());

        const actual_labels = collectLabelsToOwnedSlice(&compilation_result.labels, allocator);
        defer allocator.free(actual_labels);

        try testing.expectEqualSlices(Label, expected_labels, actual_labels);
        try testing.expectEqualSlices(Memory, expected_memory, compilation_result.memory.items);
    }

    // Need to change memory struct for this test to work
    //{
    //    const source = Tokenizer.ProgramSource{
    //        .filepath = null,
    //        .contents = shared_beginning ++
    //                    \\hello_world:  .ASCII "Hello, World!\n"
    //                    \\hello_world:  .ASCIZ "Hello, World!\n"
    //    };

    //    const tokens = try Tokenizer.tokenize(source, &arena);
    //    const compilation_result = try parse(tokens, arena.allocator());

    //    try testing.expectEqualSlices(Label, expected_labels, compilation_result.labels.items);
    //    try testing.expectEqualSlices(Memory, expected_memory, compilation_result.memory.items);
    //}
}

test "parses bss section" {
    const shared_beginning = ".SECT .BSS\n";
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    {
        const source = Tokenizer.ProgramSource{
            .filepath = null,
            .contents = shared_beginning ++
                        \\x:    .SPACE   2
        };

        const expected_labels = &[_]Label{
            .{ .memory_field = 0 },
        };

        const expected_memory = &[_]Memory{
            .{ .size = 2, .value = 0 },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const compilation_result = try parse(tokens, arena.allocator());

        const actual_labels = collectLabelsToOwnedSlice(&compilation_result.labels, allocator);
        defer allocator.free(actual_labels);

        try testing.expectEqualSlices(Label, expected_labels, actual_labels);
        try testing.expectEqualSlices(Memory, expected_memory, compilation_result.memory.items);
    }
}

fn collectLabelsToOwnedSlice(hashmap: *const std.StringHashMap(Label), allocator: std.mem.Allocator) []Label {
    var result = allocator.alloc(Label, hashmap.count()) catch unreachable;
    
    var i: usize = 0;
    var value_it = hashmap.valueIterator();
    while (value_it.next()) |value| {
        result[i] = value.*;
        i += 1;
    }
    return result;
}