const std = @import("std");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");
const Tokenizer = @import("Tokenizer.zig");

const Token = Tokenizer.Token;
const print = std.debug.print;
const SourceLocation = Tokenizer.SourceLocation;


const Self = @This();

pub const AssembledCode = struct {
    instructions: std.ArrayList(Instruction),
    memory: std.ArrayList([]const u8),
    labels: std.StringHashMap(Label),
};

pub const Instruction = struct {
    mnemonic: intel8088.InstructionMnemonic,
    operands: []const intel8088.InstructionOperand,
    location: SourceLocation,
};

// A label can address either memory or code, e.g.
// L1: MOV AX, BX
// or
// L1:  .WORD   2
pub const Label = union(enum) {
    instruction: u16,
    memory_field: u16,
};

const ParseState = enum {
    no_section,
    text_section,
    data_section,
    bss_section,
};

const PreprocessResult = struct {
    labels: std.StringHashMap(Label),
    memory: std.ArrayList([]const u8),
};


allocator: std.mem.Allocator,
result: AssembledCode,
tokens: []Token,
locations: []SourceLocation,
position: usize,


pub fn parse(
    tokens: std.MultiArrayList(Tokenizer.TokenWithLocation),
    allocator: std.mem.Allocator
) !AssembledCode {
    var parser = Self{
        .allocator = allocator,
        .result = .{
            .instructions = std.ArrayList(Instruction).init(allocator),
            .memory = undefined,
            .labels = undefined,
        },
        .tokens = tokens.items(.token),
        .locations = tokens.items(.location),
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
                    if (!directive.isMemoryDataType()) {
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
    var memory = std.ArrayList([]const u8).init(allocator);
    
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
                if (!directive.isMemoryDataType()) {
                    continue;
                }

                var maybe_memory_field: ?[]u8 = null;
                if (directive == .space) {
                    if (self.next()) |next_token| {
                        if (next_token.is(.number)) {
                            // TODO: Check that we are in bss section
                            if (next_token.number >= 0) {
                                // TODO: Check that number is not negatvie
                                maybe_memory_field = try allocator.alloc(u8, @as(usize, @intCast(next_token.number)));
                                for (0 .. maybe_memory_field.?.len) |i| {
                                    // How to zero init?
                                    maybe_memory_field.?[i] = 0;
                                }
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
                    if (self.match(.number)) |number_token| {
                        maybe_memory_field = try allocator.alloc(u8, 2);
                        std.mem.writeInt(i16, maybe_memory_field.?[0 .. 2], number_token.number, .little);
                    } else {
                        // TODO: Report error
                    }
                } else if (directive == .ascii or directive == .asciz) {
                    if (self.match(.string)) |string_token| {
                        if (directive == .asciz) {
                            const string0 = try allocator.dupeZ(u8, string_token.string);
                            // Interestingly, in zig, an implicit cast is allowed here:
                            // maybe_memory_field = string0; // [:0]u8 -> []u8
                            //
                            // But it doesn't include the sentinel element. I don't know if this is
                            // good or bad, but it got me a little.
                            maybe_memory_field = string0[0 .. string0.len + 1];
                        } else {
                            maybe_memory_field = try allocator.dupe(u8, string_token.string);
                        }
                    } else {
                        // TODO: Report error
                    }
                } else {
                    unreachable;
                }
                
                if (next_token_is_under_label) |label_identifier| {
                    const bucket = try labels.getOrPut(label_identifier);
                    if (bucket.found_existing) {
                        // TODO: Report duplicate label error
                        unreachable;
                    } else {
                        bucket.value_ptr.* = Label{ .memory_field = memory_pointer };
                    }
                }

                if (maybe_memory_field) |memory_field| {
                    try memory.append(memory_field);
                    memory_pointer += @as(u16, @intCast(memory_field.len));
                }
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
    const instruction_location = self.peekLocation();
    const instruction_token = self.next() orelse return;

    const description = intel8088.isa.get(instruction_token.instruction_mnemonic);

    // I know that I am using an arena, so this is fast.
    var operands = try self.allocator.alloc(intel8088.InstructionOperand, description.allowed_operands.len);
    for (0 .. description.allowed_operands.len) |i| {
        operands[i] = (try self.parseInstructionOperand()) orelse return;
        if (i != description.allowed_operands.len - 1) {
            _ = self.match(.comma) orelse return;
        }
    }
    try self.result.instructions.append(.{
        .mnemonic = instruction_token.instruction_mnemonic,
        .operands = operands,
        .location = instruction_location,
    });
}

fn parseInstructionOperand(self: *Self) !?intel8088.InstructionOperand {
    return try self.parseExpression();
}

fn parseExpression(self: *Self) !?intel8088.InstructionOperand {
    const ExpressionNode = union(enum) {
        add,
        sub,
        mul,
        div,
        operand: intel8088.InstructionOperand,

        pub fn isOperator(node: @This()) bool {
            return switch (node) {
                .add, .sub, .mul, .div => true,
                .operand => false,
            };
        }

        pub fn priority(node: @This()) u16 {
            return switch (node) {
                .add, .sub => 0,
                .mul, .div => 1,
                else => unreachable,
            };
        }
    };
    
    const ReversePolishEvaluator = struct {
        operands: std.ArrayList(ExpressionNode),
        operators: std.ArrayList(ExpressionNode),
        rewritten: std.ArrayList(ExpressionNode),

        pub fn init(allocator: std.mem.Allocator) !@This() {
            return @This() {
                .operands = std.ArrayList(ExpressionNode).init(allocator),
                .operators = std.ArrayList(ExpressionNode).init(allocator),
                .rewritten = std.ArrayList(ExpressionNode).init(allocator),
            };
        }
        
        pub fn deinit(p: *@This()) void {
            p.operands.deinit();
            p.operators.deinit();
            p.rewritten.deinit();
        }

        pub fn addOperand(p: *@This(), operand: intel8088.InstructionOperand) !void {
            try p.operands.append(.{ .operand = operand });
        }
        
        pub fn addOperator(p: *@This(), operator: ExpressionNode) !void {
            while (p.operators.getLastOrNull()) |last_operator| {
                if (last_operator.priority() >= operator.priority()) {
                    try p.addRewrittenBinaryExpression(p.operators.pop());
                } else {
                    break;
                }
            }
            try p.operators.append(operator);
        }

        pub fn evaluate(p: *@This()) !?intel8088.InstructionOperand {
            if (p.operands.items.len == 1 and p.operators.items.len == 0) {
                return p.operands.items[0].operand;
            }
    
            while (p.operators.popOrNull()) |last_operator| {
                try p.addRewrittenBinaryExpression(last_operator);
            }
            if (p.operands.items.len > 0) {
                // TODO: Report error
                return null;
            }
            if (p.operands.items.len > 0) {
                return null;
            }
            
            var arg1: ?intel8088.InstructionOperand = null;
            var arg2: ?intel8088.InstructionOperand = null;
            for (p.rewritten.items) |node| {
                if (node.isOperator()) {
                    if (arg1 != null and arg2 != null) {
                        switch (node) {
                            // TODO: Overflow and other error checking
                            .add => arg1.?.immediate += arg2.?.immediate,
                            .sub => arg1.?.immediate -= arg2.?.immediate,
                            .mul => arg1.?.immediate *= arg2.?.immediate,
                            .div => arg1.?.immediate = @divFloor(arg1.?.immediate, arg2.?.immediate),
                            else => unreachable,
                        }
                        arg2 = null;
                    } else {
                        // TODO: Report error
                    }
                } else switch (node.operand) {
                    .memory => {
                        // TODO: Report error
                    },
                    .register => {
                        // TODO: Report error
                    },
                    .immediate => {
                        if (arg1 == null) {
                            arg1 = node.operand;
                        } else if (arg2 == null) {
                            arg2 = node.operand;
                        } else {
                            // TODO: Report error
                        }
                    }
                }
            }
            
            if (arg2 != null) {
                // TODO: Report error
            }
            return arg1;
        }
        
        fn addRewrittenBinaryExpression(p: *@This(), operator: ExpressionNode) !void {
            const arg1 = p.operands.popOrNull() orelse {
                // TODO: Report error
                return;
            };
            const arg2 = p.operands.popOrNull();
            if (arg2 != null) {
                try p.rewritten.append(arg2.?);
            }
            try p.rewritten.append(arg1);
            try p.rewritten.append(operator);
        }
    };
    
    var polish = try ReversePolishEvaluator.init(self.allocator);
    defer polish.deinit();
    while (self.next()) |token| {
        switch (token) {
            .number => |number| try polish.addOperand(.{ .immediate = number }),
            .plus => try polish.addOperator(.add),
            .minus => try polish.addOperator(.sub),
            .star => try polish.addOperator(.mul),
            .forward_slash => try polish.addOperator(.div),
            .register => |register| try polish.addOperand(.{ .register = register }),
            .identifier => |identifier| {
                if (self.result.labels.get(identifier)) |label| {
                    // TODO: Accept labels to code
                    // TODO: Immediates should be evaluated in i32, for better error reporting
                    try polish.addOperand(.{ .immediate = @as(i16, @intCast(label.memory_field)) });
                } else {
                    // TODO: Report error
                }
            },
            .left_paren => {
                const nested_memory_operand = try self.parseExpression() orelse return null;
                if (self.match(.right_paren)) |_| {
                    switch (nested_memory_operand) {
                        .immediate => |address| try polish.addOperand(.{ .memory = @as(u16, @intCast(address)) }),
                        else => {
                            // TODO: Report unsupported type
                            print("Memory must be an immediate, found {}", .{nested_memory_operand});
                            return null;
                        }
                    }
                } else {
                    // TODO: Report unclosed parentheses
                }
            },
            // On any other token, we assume that the expression ended
            // and it's time to evaluate it.
            else => {
                // Go 1 token back, since we skipped one token right after the expression end.
                _ = self.prev();

                if (try polish.evaluate()) |result| {
                    return result;
                }
                return null;
            },
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

fn prev(self: *Self) ?Token {
    const result = self.peek();
    self.position = @max(self.position - 1, 0);
    return result;
}

fn peek(self: *const Self) ?Token {
    return self.peekN(0);
}

fn peekLocation(self: *const Self) SourceLocation {
    return self.locations[self.position];
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
        const source = Tokenizer.ProgramSourceCode{
            .filepath = null,
            .contents = shared_beginning ++
                        \\  MOV AX, BX
        };

        const expected_instructions: []const Instruction = &.{
            .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .register = .bx } }, .location = .{ .line = 2, .column = 3 } },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const assembled_program = try parse(tokens, arena.allocator());

        try testing.expectEqualDeep(expected_instructions, assembled_program.instructions.items);
    }
}
 
test "parses compound expression" {
    const shared_beginning = ".SECT .TEXT\n";
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

   {
        const source = Tokenizer.ProgramSourceCode{
            .filepath = null,
            .contents = shared_beginning ++
                        \\  MOV AX, 1 + 2
        };

        const expected_instructions: []const Instruction = &.{
            .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .immediate = 3 } }, .location = .{ .line = 2, .column = 3 } },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const assembled_program = try parse(tokens, arena.allocator());

        try testing.expectEqualDeep(expected_instructions, assembled_program.instructions.items);
    }
    
    {
        const source = Tokenizer.ProgramSourceCode{
            .filepath = null,
            .contents = shared_beginning ++
                \\! Calculates length, excluding the zero terminator of the string "Hello"
                \\MOV AX, after_hello - hello - 1
                \\.SECT .DATA
                \\hello: .ASCIZ "Hello"
                \\after_hello: .WORD 0
        };
        
        const expected_labels = &[_]Label {
            .{ .memory_field = 0 },
            .{ .memory_field = 6 },
        };
        const expected_instructions: []const Instruction = &.{
            .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .immediate = 5 } }, .location = .{ .line = 3, .column = 1 } },
            //                                                                                ^^^^^^^^ :( I don't want this
        };
        
        const tokens = try Tokenizer.tokenize(source, &arena);
        const assembled_program = try parse(tokens, arena.allocator());
        
        const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
        defer allocator.free(actual_labels);

        try testing.expectEqualSlices(Label, expected_labels, actual_labels);
        try testing.expectEqualDeep(expected_instructions[0], assembled_program.instructions.items[0]);
    }
}

test "parses data section" {
    const shared_beginning = ".SECT .DATA\n";
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    {
        const source = Tokenizer.ProgramSourceCode{
            .filepath = null,
            .contents = shared_beginning ++
                        \\x:    .WORD   3
                        \\y:    .WORD   5
        };

        const expected_labels = &[_]Label {
            .{ .memory_field = 0 },
            .{ .memory_field = 2 },
        };

        const expected_memory = &[_][]const u8{
            &.{ 3, 0 },
            &.{ 5, 0 },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const assembled_program = try parse(tokens, arena.allocator());

        const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
        defer allocator.free(actual_labels);

        try testing.expectEqualSlices(Label, expected_labels, actual_labels);
        try testing.expectEqualDeep(expected_memory, assembled_program.memory.items);
    }

    {
        const source = Tokenizer.ProgramSourceCode{
            .filepath = null,
            .contents = shared_beginning ++
                        \\hello_world:  .ASCII "Hello, World!\n"
                        \\hello_world0:  .ASCIZ "Hello, World!\n"
        };

        // Label order is messed up because HashMap does not guarantee order:
        // https://ziglang.org/documentation/master/std/#std.hash_map.HashMap
        // I don't really care about order though, it's enough that the memory
        // pointers are correct. Order can be hardcoded, like here.
        const expected_labels = &[_]Label {
            .{ .memory_field = 14 },
            .{ .memory_field = 0 },
        };

        const expected_memory = &[_][]const u8{
            // I didn't do this by hand
            &.{72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 10 },
            &.{72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 10, 0},
        };
        
        const tokens = try Tokenizer.tokenize(source, &arena);
        const assembled_program = try parse(tokens, arena.allocator());

        const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
        defer allocator.free(actual_labels);

        try testing.expectEqualSlices(Label, expected_labels, actual_labels);
        try testing.expectEqualDeep(expected_memory, assembled_program.memory.items);
    }
}

test "parses bss section" {
    const shared_beginning = ".SECT .BSS\n";
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    {
        const source = Tokenizer.ProgramSourceCode{
            .filepath = null,
            .contents = shared_beginning ++
                        \\x:    .SPACE   7
        };

        const expected_labels = &[_]Label{
            .{ .memory_field = 0 },
        };

        const expected_memory = &[_][]const u8{
            &.{ 0, 0, 0, 0, 0, 0, 0 },
        };

        const tokens = try Tokenizer.tokenize(source, &arena);
        const assembled_program = try parse(tokens, arena.allocator());

        const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
        defer allocator.free(actual_labels);

        try testing.expectEqualSlices(Label, expected_labels, actual_labels);
        try testing.expectEqualDeep(expected_memory, assembled_program.memory.items);
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