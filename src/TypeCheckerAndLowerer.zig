const std = @import("std");
const intel8088 = @import("intel8088_cpu_description.zig");
const Parser = @import("Parser.zig");
const Tokenizer = @import("Tokenizer.zig");

const SourceLocation = Tokenizer.SourceLocation;
const print = std.debug.print;

const Self = @This();

pub const AssembledProgram = struct {
    instructions: std.ArrayList(Instruction),
    memory: std.ArrayList([]const u8),
    labels: std.StringHashMap(Label),
    constants: std.StringHashMap(i16),
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

allocator: std.mem.Allocator,
instructions: std.ArrayList(Instruction),
memory: std.ArrayList([]const u8),
constants: std.StringHashMap(i16),
labels: std.StringHashMap(Label),

pub fn typeCheckAndFinalize(parse_result: *const Parser.UncheckedAst, allocator: std.mem.Allocator) !AssembledProgram {
    var type_checker = Self{
        .allocator = allocator,
        .labels = std.StringHashMap(Label).init(allocator),
        .constants = std.StringHashMap(i16).init(allocator),
        .memory = std.ArrayList([]const u8).init(allocator),
        .instructions = std.ArrayList(Instruction).init(allocator), 
    };
    
    try type_checker.selfTypeCheckAndFinalize(parse_result);
        
    return AssembledProgram{
        .instructions = type_checker.instructions,
        .memory = type_checker.memory,
        .constants = type_checker.constants,
        .labels = type_checker.labels,
    };
}

pub fn selfTypeCheckAndFinalize(self: *Self, parse_result: *const Parser.UncheckedAst) !void {
    // This is a temporary hack
    var ast = std.ArrayList(Parser.AstNode).init(self.allocator);
    for (parse_result.text_section.items) |node| {
        try ast.append(node);
    }
    for (parse_result.data_section.items) |node| {
        try ast.append(node);
    }
    for (parse_result.bss_section.items) |node| {
        try ast.append(node);
    }
    for (parse_result.outside_any_section.items) |node| {
        try ast.append(node);
    }

    // Label + constant pass
    var instruction_address: u16 = 0;
    var memory_address: u16 = 0;
    var last_label_name: ?[]const u8 = null;
    for (ast.items) |node| {
        switch (node) {
            .instruction => |_| {
                if (last_label_name) |label_name| {
                    const result = try self.labels.getOrPut(label_name);
                    if (result.found_existing) {
                        // TODO: Report error
                        unreachable;
                    } else {
                        result.value_ptr.* = .{ .instruction = instruction_address };
                    }
                }
                instruction_address += 1;
            },
            .label => |_| {
                // TODO: Actually, two or more labels could point
                // at the same data, which is a little annoying.
                if (last_label_name != null) {
                    // TODO: Report error
                    unreachable;
                }
            },
            .data_field => |data_field| {
                if (last_label_name) |label_name| {
                    const result = try self.labels.getOrPut(label_name);
                    if (result.found_existing) {
                        // TODO: Report error
                        unreachable;
                    } else {
                        result.value_ptr.* = .{ .memory_field = memory_address };
                    }
                }
                const value = try self.evaluateExpression(data_field.initializer);
                const size = switch (data_field.data_type) {
                    .word => 2,
                    .byte => 1,
                    .ascii => value.string.len,
                    .asciz => value.string.len + 1,
                    .space => blk: {
                        const immediate = value.immediate;
                        if (immediate < 0) {
                            // TODO: Report error
                            unreachable;
                        }
                        break :blk @as(usize, @intCast(value.immediate));
                    },
                    else => unreachable,
                };
                memory_address += @as(u16, @intCast(size));
                try self.memory.append(switch (value) {
                    .register => unreachable, // TODO: Report error
                    .string => |string| string,
                    // memory_address Could be an error?
                    .memory_address => |address| try self.allocator.dupe(u8, std.mem.asBytes(&address)),
                    .immediate => |immediate| try self.allocator.dupe(u8, std.mem.asBytes(&immediate)),
                });
            },
            .constant => |constant| {
                if (last_label_name != null) {
                    // TODO: Report error
                    unreachable;
                }
            
                const value = try self.evaluateExpression(constant.initializer);
                const bucket = try self.constants.getOrPut(constant.name);
                if (bucket.found_existing) {
                    // TODO: Report constant already defined
                    unreachable;
                } else {
                    bucket.value_ptr.* = switch (value) {
                        .register => unreachable, // TODO: Report error
                        .string => unreachable, // TODO: Report error
                        .immediate => |immediate| immediate,
                        .memory_address => |address| @as(i16, @bitCast(address)),
                    };
                }
            },
        }

        if (node == .label) {
            last_label_name = node.label.name;
        } else {
            last_label_name = null;
        }
    }

    for (ast.items) |node| {
        switch (node) {
            .instruction => |instruction| {
                const type_checked_instruction = try self.typeCheckInstruction(instruction);
                try self.instructions.append(type_checked_instruction);
            },
            .data_field => |_| {
            },
            else => {},
        }
    }
}

fn typeCheckInstruction(self: *Self, instruction: Parser.InstructionAstNode) !Instruction {
    const description = intel8088.isa.get(instruction.mnemonic);
    if (description.allowed_operands.len != instruction.operands.len) {
        // TODO: Report error
        unreachable;
    }

    var operands = try self.allocator.alloc(intel8088.InstructionOperand, instruction.operands.len);

    for (instruction.operands, 0..) |operand, i| {
        const evaluated_operand = try self.evaluateExpression(operand);

        const evaluated_operand_type = evaluated_operand.asOperandType();
        const allowed_operand_types = description.allowed_operands[i];
        if (!allowed_operand_types.contains(evaluated_operand_type)) {
            print("instruction {} expected one of {}, but got {}", .{
                instruction.mnemonic,
                allowed_operand_types,
                evaluated_operand_type,
            });
            // TODO: Report error and cleanup `operands`
            unreachable;
        }

        operands[i] = switch (evaluated_operand) {
            .register => |register| .{ .register = register },
            .memory_address => |address| .{ .memory = address },
            .immediate => |immediate| .{ .immediate = immediate },
            .string => unreachable,
        };
    }
    
    return Instruction{
        .mnemonic = instruction.mnemonic,
        .operands = operands,
        .location = instruction.location,
    };
}

const EvaluatedExpression = union(enum) {
    pub const Tag = std.meta.Tag(@This());

    register: intel8088.Register,
    string: []const u8,
    memory_address: u16,
    immediate: i16,

    pub fn asOperandType(self: @This()) intel8088.AllowedOperands.Key {
        return switch (self) {
            .register => .register,
            .immediate => .immediate,
            .memory_address => .memory,
            .string => unreachable,
        };
    }
};

fn evaluateExpression(
    self: *Self,
    expression: *Parser.ExpressionAstNode,
) !EvaluatedExpression {
    switch (expression.*) {
        .binary => |binary| {
            const left_any_type = try self.evaluateExpression(binary.left);
            const right_any_type = try self.evaluateExpression(binary.right);

            if (left_any_type == .immediate and right_any_type == .immediate) {
                const left = left_any_type.immediate;
                const right = right_any_type.immediate;
                switch (binary.operator) {
                    .plus =>     return .{ .immediate = left + right },
                    .minus =>    return .{ .immediate = left - right },
                    .multiply => return .{ .immediate = left * right },
                    .divide =>   return .{ .immediate = @divFloor(left, right) },
                }
            } else if (left_any_type == .memory_address and right_any_type == .memory_address) {
                const left = left_any_type.memory_address;
                const right = right_any_type.memory_address;
                switch (binary.operator) {
                    .plus =>     return .{ .memory_address = left + right },
                    .minus =>    return .{ .memory_address = left - right },
                    .multiply => return .{ .memory_address = left * right },
                    .divide =>   return .{ .memory_address = @divFloor(left, right) },
                }
            } else if (left_any_type == .immediate and right_any_type == .memory_address) {
                const left = left_any_type.immediate;
                const right = right_any_type.memory_address;
                switch (binary.operator) {
                    .plus =>     return .{ .memory_address = @as(u16, @intCast(left)) + right },
                    .minus =>    return .{ .memory_address = @as(u16, @intCast(left)) - right },
                    .multiply => return .{ .memory_address = @as(u16, @intCast(left)) * right },
                    .divide =>   return .{ .memory_address = @divFloor(@as(u16, @intCast(left)), right) },
                }
            } else if (left_any_type == .memory_address and right_any_type == .immediate) { 
                // COPYPASTA SUPER WARNING
                // BE CAREFUL WITH THAT CODE
                const left = right_any_type.immediate;
                const right = left_any_type.memory_address;
                switch (binary.operator) {
                    .plus =>     return .{ .memory_address = @as(u16, @intCast(left)) + right },
                    .minus =>    return .{ .memory_address = @as(u16, @intCast(left)) - right },
                    .multiply => return .{ .memory_address = @as(u16, @intCast(left)) * right },
                    .divide =>   return .{ .memory_address = @divFloor(@as(u16, @intCast(left)), right) },
                }
            } else {
                // TODO: There are more expression here
                // TODO: Report error
                unreachable;
            }
        },
        .unary => |unary| {
            const operand_any_type = try self.evaluateExpression(unary.operand);
            if (operand_any_type == .immediate) {
                const operand = operand_any_type.immediate;
                switch (unary.operator) {
                    .minus => return .{ .immediate = -operand },
                    .plus => return .{ .immediate = operand },
                }
            } else {
                // TODO: Report error
                unreachable;
            }
        },
        .value => |value| {
            switch (value) {
                .string => |string| return .{ .string = string },
                .identifier => |identifier| {
                    if (self.constants.get(identifier)) |constant_value| {
                        return .{ .immediate = constant_value };
                    } else if (self.labels.get(identifier)) |label| {
                        return .{ .memory_address = label.memory_field };
                    } else {
                        // TODO: Report undeclared identifier
                        print("undeclared identifier {s}\n", .{identifier});
                        unreachable;
                    }
                },
                .register => |register| {
                    return .{ .register = register };
                },
                .word => |word| {
                    return .{ .immediate = word };
                },
            }
        }
    }
}
