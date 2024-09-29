const std = @import("std");
const as88 = @import("as88.zig");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");
const Tokenizer = @import("Tokenizer.zig");

const Token = Tokenizer.Token;
const print = std.debug.print;
const SourceLocation = Tokenizer.SourceLocation;

const Self = @This();

pub const UncheckedAst = struct {
    outside_any_section: std.ArrayList(AstNode),
    text_section: std.ArrayList(AstNode),
    data_section: std.ArrayList(AstNode),
    bss_section: std.ArrayList(AstNode),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return @This() {
            .outside_any_section = std.ArrayList(AstNode).init(allocator),
            .text_section = std.ArrayList(AstNode).init(allocator),
            .data_section = std.ArrayList(AstNode).init(allocator),
            .bss_section = std.ArrayList(AstNode).init(allocator),
        };
    }
};

pub const AstNode = union(enum) {
    instruction: InstructionAstNode,
    label: LabelAstNode,
    constant: ConstantDefinitionAstNode,
    data_field: DataFieldAstNode,
};

pub const DataFieldAstNode = struct {
    // Not strict
    data_type: intel8088.asm_syntax.Directive,
    size: u16,
    initializer: *ExpressionAstNode,
};

pub const ConstantDefinitionAstNode = struct {
    name: []const u8,
    initializer: *ExpressionAstNode,
};

pub const ExpressionAstNode = union(enum) {
    binary: BinaryExpressionAstNode,
    unary: UnaryExpressionAstNode,
    value: ValueAstNode,
};

pub const BinaryExpressionAstNode = struct {
    left: *ExpressionAstNode,
    right: *ExpressionAstNode,
    operator: intel8088.asm_syntax.BinaryOperator,
};

pub const UnaryExpressionAstNode = struct {
    operand: *ExpressionAstNode,
    operator: intel8088.asm_syntax.UnaryOperator,
};

pub const ValueAstNode = union(enum) {
    string: []const u8, // this should only be allowed in data fields
    identifier: []const u8,
    register: intel8088.Register,
    word: i16, // There are more variants, like unsigned word,
               // byte and unsigned byte
};

pub const InstructionAstNode = struct {
    mnemonic: intel8088.InstructionMnemonic,
    operands: []const *ExpressionAstNode,
    location: SourceLocation,
};

pub const LabelAstNode = struct {
    name: []const u8,
};

const ParseState = enum {
    no_section,
    text_section,
    data_section,
    bss_section,
};

allocator: std.mem.Allocator,
file: *as88.File,
tokens: []Token,
locations: []SourceLocation,
position: usize,

pub fn parse(file: *as88.File) !void {
    var self = Self{
        .file = file,
        .allocator = file.allocator,
        .tokens = file.tokens.items(.token),
        .locations = file.tokens.items(.location),
        .position = 0,
    };

    var current_section: intel8088.asm_syntax.Section = .no_section;
    while (self.peek()) |token| {
        var parsed_node: ?AstNode = null;
        switch (token) {
            .directive => |directive| {
                if (directive == .sect) {
                    _ = self.next();
                    if (self.match(.directive)) |next_directive| {
                        if (next_directive.directive.isSectionType()) {
                            current_section = switch(next_directive.directive) {
                                .text => .text_section,
                                .data => .data_section,
                                .bss => .bss_section,
                                else => unreachable,
                           };
                        } else {
                            // TODO: Report error
                            unreachable;
                        }
                    } else {
                        // TODO: Report error
                        unreachable;
                    }
                    _ = self.next();
                } else if (directive.isMemoryDataType()) {
                    parsed_node = try self.parseDataField();
                    print("emit data field {}\n\n", .{parsed_node.?});
                } else {
                    print("didn't expect directive '{}'\n", .{directive});
                    _ = self.next();
                    unreachable;
                }
            },
            .instruction_mnemonic => |_| {
                parsed_node = try self.parseInstruction() orelse {
                    // TODO: Report error
                    unreachable;
                };
            },
            .identifier => {
                // TODO: This could lead to confusing errors when
                // having typos, like MOB instead of MOV. Maybe check
                // for an equal sign?
                parsed_node = try self.parseConstant();
            },
            .newline => {
                _ = self.next();
            },
            .label => {
                parsed_node = try self.parseLabel();
            },
            .comment => {
                _ = self.next();
                _ = self.match(.newline);
            },
            else => {
                _ = self.next();
            },
        }
        
        if (parsed_node) |node| {
            switch (current_section) {
                .no_section => try file.ast.outside_any_section.append(node),
                .text_section => try file.ast.text_section.append(node),
                .data_section => try file.ast.data_section.append(node),
                .bss_section => try file.ast.bss_section.append(node),
            }
        }
    }
}

fn parseConstant(self: *Self) !?AstNode {
    const constant_name_token = self.next() orelse unreachable;
    if (self.next()) |equals_token| {
        if (equals_token != .equals_sign) {
            // TODO: Report error
            return null;
        }

        const initializer = (try self.parseExpression()) orelse return null;
        return .{ .constant = .{
            .name = constant_name_token.identifier,
            .initializer = initializer,
        } };
    } else {
        // TODO: Report error
        unreachable;
    }
}

fn parseLabel(self: *Self) !AstNode {
    const label_name_token = self.next() orelse unreachable;
    std.debug.assert(label_name_token.is(.label));
    return .{ .label = .{
        .name = label_name_token.label,
    } };
}

fn parseDataField(self: *Self) !AstNode {
    const data_type_token = self.next() orelse unreachable;
    std.debug.assert(data_type_token.is(.directive));

    const data_type_directive = data_type_token.directive;
    if (data_type_directive.isMemoryDataType()) {
        var size: u16 = 0;
        var initializer: ?*ExpressionAstNode = null;
        if (data_type_directive == .space) {
            if (self.next()) |space_size| {
                if (space_size.is(.number)) {
                    const number = space_size.number;
                    if (number < 0) {
                        // TODO: Report error
                        unreachable;
                    }
                    size = @as(u16, @intCast(space_size.number));
                    // There shouldn't be an initializer, actually.
                    // Or rather, it's space-sized zero filled string.
                    initializer = try self.parseExpression();
                } else {
                    // TODO: Report error
                    unreachable;
                }
            } else {
                // TODO: Report error
                unreachable;
            }
        } else if (data_type_directive.isStringDataType()) {
            initializer = try self.parseExpression();
            size = 0; // ???
        } else {
            size = data_type_directive.size();
            initializer = try self.parseExpression();
        }

        if (initializer) |notnull_initializer| {
            return .{ .data_field = .{
                .data_type = data_type_directive,
                .size = size,
                .initializer = notnull_initializer,
            } };
        } else {
            // TODO: Report error
            unreachable;
        }
    } else {
        // TODO: Report error
        unreachable;
    }
}

fn parseInstruction(self: *Self) !?AstNode {
    const instruction_location = self.peekLocation();
    const instruction_token = self.next() orelse unreachable;
    const description = intel8088.isa.get(instruction_token.instruction_mnemonic);

    var operands = try self.allocator.alloc(*ExpressionAstNode, description.allowed_operands.len);
    for (0 .. description.allowed_operands.len) |i| {
        operands[i] = try self.parseExpression() orelse return null;
        if (i != description.allowed_operands.len - 1) {
            _ = self.match(.comma) orelse {
                print("error({}:{}): expected a comma\n", .{
                    instruction_location.line,
                    instruction_location.column,
                });
                // TODO: Report error
                unreachable;
            };
        }
    }
    return .{ .instruction = .{
        .mnemonic = instruction_token.instruction_mnemonic,
        .operands = operands,
        .location = instruction_location,
    } };
}

fn parseExpression(self: *Self) !?*ExpressionAstNode {
    return self.parseBinaryExpression(0);
}

fn parseBinaryExpression(self: *Self, last_precedence: i32) !?*ExpressionAstNode {
    const expression : *ExpressionAstNode = try self.parseUnaryExpression() orelse return null;
    while (self.peek()) |token| {
        if (!token.isOperator()) {
            break;
        }

        const operator = token.toBinaryOperator();

        const precedence = precedenceOf(operator);
        if (precedence < last_precedence) {
            break;
        }

        _ = self.next();
        const maybe_right = try self.parseBinaryExpression(precedence + 1);
        if (maybe_right) |right| {
            const left = try self.allocator.create(ExpressionAstNode);
            left.* = expression.*;
            expression.* = .{ .binary = .{
                .left = left,
                .right = right,
                .operator = operator,
            } };
        } else {
            // TODO: Report error
            unreachable;
        }
    }

    return expression;
}

fn parseUnaryExpression(self: *Self) !?*ExpressionAstNode {
    const current = self.peek() orelse unreachable;

    switch (current) {
        .plus, .minus => {
            const operator: intel8088.asm_syntax.UnaryOperator = switch (current) {
                .plus => .plus,
                .minus => .minus,
                else => unreachable,
            };
            const operand = try self.parseUnaryExpression() orelse return null;
            const result = try self.allocator.create(ExpressionAstNode);
            result.* = .{ .unary = .{
                .operand = operand,
                .operator = operator,
            } };
            return result;
        },
        else => {},
    }

    return self.parseAtom();
}

fn parseAtom(self: *Self) !?*ExpressionAstNode {
    const current = self.next() orelse unreachable;
    const result = try self.allocator.create(ExpressionAstNode);
    result.* = switch (current) {
        .identifier => |identifier| .{ .value = .{ .identifier = identifier } },
        .register => |register| .{ .value = .{ .register = register } },
        .number => |number| .{ .value = .{ .word = number } },
        // TODO: Zero out the ascii string. Not here though?
        .string => |string| .{ .value = .{ .string = string } },
        else => {
            // TODO: Report error
            unreachable;
        },
    };
    return result;
}

fn precedenceOf(operator: intel8088.asm_syntax.BinaryOperator) i32 {
    return switch (operator) {
        .plus => 1,
        .minus => 1,
        .multiply => 2,
        .divide => 2,
    };
}

fn match(self: *Self, expected: Token.Tag) ?Token {
    if (self.next()) |token| {
        if (token.is(expected)) {
            return token;
        } else {
            const previous_token_location = self.peekLocationN(-1);
            self.file.errors.append(previous_token_location, "expected '{}', but found '{}'\n", .{expected, token});
            return null;
        }
    }
    self.file.errors.append(self.peekLocationN(-1), "expected '{}', but found EOF\n", .{expected});
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

fn peekN(self: *const Self, n: usize) ?Token {
    return common.getOrNull(Token, self.tokens, self.position + n);
}

fn peekLocation(self: *const Self) SourceLocation {
    return common.getOrNull(SourceLocation, self.locations, self.position) orelse std.debug.panic("out of bounds", .{});
}

fn peekLocationN(self: *const Self, n: isize) SourceLocation {
    // This is really annoying.
    const position_as_isize = @as(isize, @intCast(self.position));
    const peek_position = @as(usize, @intCast(position_as_isize + n));
    return common.getOrNull(SourceLocation, self.locations, peek_position) orelse std.debug.panic("out of bounds", .{});
}


// The tokenizer is used in the tests instead of hardcoding
// the tokens. This means that if there is an error in the tokenizer,
// the parser tests would fail, which is confusing.
//
// But the tests get more readable, so this tradeoff was made.
const testing = std.testing;

// test "parses text section" {
//     const shared_beginning = ".SECT .TEXT\n";
//     const allocator = std.testing.allocator;
//     var arena = std.heap.ArenaAllocator.init(allocator);
//     defer arena.deinit();
// 
//     {
//         const source = Tokenizer.ProgramSourceCode{
//             .filepath = null,
//             .contents = shared_beginning ++
//                         \\  MOV AX, BX
//         };
// 
//         const expected_instructions: []const Instruction = &.{
//             .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .register = .bx } }, .location = .{ .line = 2, .column = 3 } },
//         };
// 
//         const tokens = try Tokenizer.tokenize(source, &arena);
//         const assembled_program = try parse(tokens, arena.allocator());
// 
//         try testing.expectEqualDeep(expected_instructions, assembled_program.instructions.items);
//     }
// }
//  
// test "parses compound expression" {
//     const shared_beginning = ".SECT .TEXT\n";
//     const allocator = std.testing.allocator;
//     var arena = std.heap.ArenaAllocator.init(allocator);
//     defer arena.deinit();
// 
//    {
//         const source = Tokenizer.ProgramSourceCode{
//             .filepath = null,
//             .contents = shared_beginning ++
//                         \\  MOV AX, 1 + 2
//         };
// 
//         const expected_instructions: []const Instruction = &.{
//             .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .immediate = 3 } }, .location = .{ .line = 2, .column = 3 } },
//         };
// 
//         const tokens = try Tokenizer.tokenize(source, &arena);
//         const assembled_program = try parse(tokens, arena.allocator());
// 
//         try testing.expectEqualDeep(expected_instructions, assembled_program.instructions.items);
//     }
//     
//     {
//         const source = Tokenizer.ProgramSourceCode{
//             .filepath = null,
//             .contents = shared_beginning ++
//                 \\! Calculates length, excluding the zero terminator of the string "Hello"
//                 \\MOV AX, after_hello - hello - 1
//                 \\.SECT .DATA
//                 \\hello: .ASCIZ "Hello"
//                 \\after_hello: .WORD 0
//         };
//         
//         const expected_labels = &[_]Label {
//             .{ .memory_field = 0 },
//             .{ .memory_field = 6 },
//         };
//         const expected_instructions: []const Instruction = &.{
//             .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .immediate = 5 } }, .location = .{ .line = 3, .column = 1 } },
//             //                                                                                ^^^^^^^^ :( I don't want this
//         };
//         
//         const tokens = try Tokenizer.tokenize(source, &arena);
//         const assembled_program = try parse(tokens, arena.allocator());
//         
//         const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
//         defer allocator.free(actual_labels);
// 
//         try testing.expectEqualSlices(Label, expected_labels, actual_labels);
//         try testing.expectEqualDeep(expected_instructions[0], assembled_program.instructions.items[0]);
//     }
// }
// 
// test "parses data section" {
//     const shared_beginning = ".SECT .DATA\n";
//     const allocator = std.testing.allocator;
//     var arena = std.heap.ArenaAllocator.init(allocator);
//     defer arena.deinit();
// 
//     {
//         const source = Tokenizer.ProgramSourceCode{
//             .filepath = null,
//             .contents = shared_beginning ++
//                         \\x:    .WORD   3
//                         \\y:    .WORD   5
//         };
// 
//         const expected_labels = &[_]Label {
//             .{ .memory_field = 0 },
//             .{ .memory_field = 2 },
//         };
// 
//         const expected_memory = &[_][]const u8{
//             &.{ 3, 0 },
//             &.{ 5, 0 },
//         };
// 
//         const tokens = try Tokenizer.tokenize(source, &arena);
//         const assembled_program = try parse(tokens, arena.allocator());
// 
//         const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
//         defer allocator.free(actual_labels);
// 
//         try testing.expectEqualSlices(Label, expected_labels, actual_labels);
//         try testing.expectEqualDeep(expected_memory, assembled_program.memory.items);
//     }
// 
//     {
//         const source = Tokenizer.ProgramSourceCode{
//             .filepath = null,
//             .contents = shared_beginning ++
//                         \\hello_world:  .ASCII "Hello, World!\n"
//                         \\hello_world0:  .ASCIZ "Hello, World!\n"
//         };
// 
//         // Label order is messed up because HashMap does not guarantee order:
//         // https://ziglang.org/documentation/master/std/#std.hash_map.HashMap
//         // I don't really care about order though, it's enough that the memory
//         // pointers are correct. Order can be hardcoded, like here.
//         const expected_labels = &[_]Label {
//             .{ .memory_field = 14 },
//             .{ .memory_field = 0 },
//         };
// 
//         const expected_memory = &[_][]const u8{
//             // I didn't do this by hand
//             &.{72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 10 },
//             &.{72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 10, 0},
//         };
//         
//         const tokens = try Tokenizer.tokenize(source, &arena);
//         const assembled_program = try parse(tokens, arena.allocator());
// 
//         const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
//         defer allocator.free(actual_labels);
// 
//         try testing.expectEqualSlices(Label, expected_labels, actual_labels);
//         try testing.expectEqualDeep(expected_memory, assembled_program.memory.items);
//     }
// }
// 
// test "parses bss section" {
//     const shared_beginning = ".SECT .BSS\n";
//     const allocator = std.testing.allocator;
//     var arena = std.heap.ArenaAllocator.init(allocator);
//     defer arena.deinit();
// 
//     {
//         const source = Tokenizer.ProgramSourceCode{
//             .filepath = null,
//             .contents = shared_beginning ++
//                         \\x:    .SPACE   7
//         };
// 
//         const expected_labels = &[_]Label{
//             .{ .memory_field = 0 },
//         };
// 
//         const expected_memory = &[_][]const u8{
//             &.{ 0, 0, 0, 0, 0, 0, 0 },
//         };
// 
//         const tokens = try Tokenizer.tokenize(source, &arena);
//         const assembled_program = try parse(tokens, arena.allocator());
// 
//         const actual_labels = collectLabelsToOwnedSlice(&assembled_program.labels, allocator);
//         defer allocator.free(actual_labels);
// 
//         try testing.expectEqualSlices(Label, expected_labels, actual_labels);
//         try testing.expectEqualDeep(expected_memory, assembled_program.memory.items);
//     }
// }
// 
// test "constant declarations work" {
//     const allocator = std.testing.allocator;
//     var arena = std.heap.ArenaAllocator.init(allocator);
//     defer arena.deinit();
// 
//     {
//         const source = Tokenizer.ProgramSourceCode{
//             .filepath = null,
//             .contents =
//                 \\  _MY_VAR = 2
//                 \\MOV   AX, _MY_VAR
//         };
// 
//         const expected_instructions = &[_]Instruction{
//             .{ .mnemonic = .mov, .operands = &.{ .{ .register = .ax }, .{ .immediate = 2 } }, .location = .{ .line = 2, .column = 1 } },
//         };
// 
//         const tokens = try Tokenizer.tokenize(source, &arena);
//         const assembled_program = try parse(tokens, arena.allocator());
// 
//         try testing.expectEqualDeep(expected_instructions, assembled_program.instructions.items);
//     }
// }
// 
// fn collectLabelsToOwnedSlice(hashmap: *const std.StringHashMap(Label), allocator: std.mem.Allocator) []Label {
//     var result = allocator.alloc(Label, hashmap.count()) catch unreachable;
//     
//     var i: usize = 0;
//     var value_it = hashmap.valueIterator();
//     while (value_it.next()) |value| {
//         result[i] = value.*;
//         i += 1;
//     }
//     return result;
// }