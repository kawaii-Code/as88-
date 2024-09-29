const std = @import("std");
const as88 = @import("as88.zig");
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

file: *as88.File,
instructions: std.ArrayList(Instruction),
memory: std.ArrayList([]const u8),
constants: std.StringHashMap(i16),
labels: std.StringHashMap(Label),
ast: std.ArrayList(Parser.AstNode),
location: SourceLocation,

pub fn typeCheckAndFinalize(file: *as88.File) !AssembledProgram {
    const allocator = file.allocator;
    const unchecked_ast = file.ast;
    var self = Self{
        .file = file,
        .labels = std.StringHashMap(Label).init(allocator),
        .constants = std.StringHashMap(i16).init(allocator),
        .memory = std.ArrayList([]const u8).init(allocator),
        .instructions = std.ArrayList(Instruction).init(allocator),
        .ast = undefined,
        .location = undefined,
    };

    // This is a temporary hack
    var ast = std.ArrayList(Parser.AstNode).init(allocator);
    for (unchecked_ast.text_section.items(.node)) |node| {
        try ast.append(node);
    }
    for (unchecked_ast.data_section.items(.node)) |node| {
        try ast.append(node);
    }
    for (unchecked_ast.bss_section.items(.node)) |node| {
        try ast.append(node);
    }
    for (unchecked_ast.outside_any_section.items(.node)) |node| {
        try ast.append(node);
    }
    var locations = std.ArrayList(SourceLocation).init(allocator);
    for (unchecked_ast.text_section.items(.location)) |location| {
        try locations.append(location);
    }
    for (unchecked_ast.data_section.items(.location)) |location| {
        try locations.append(location);
    }
    for (unchecked_ast.bss_section.items(.location)) |location| {
        try locations.append(location);
    }
    for (unchecked_ast.outside_any_section.items(.location)) |location| {
        try locations.append(location);
    }

    self.ast = ast;
    self.location = locations.items[0];

    // Label pass
    {
        var instruction_address: u16 = 0;
        var memory_address: u16 = 0;
        var last_label_name: ?[]const u8 = null;
        for (ast.items, 0..) |node, i| {
            self.location = locations.items[i];
            switch (node) {
                .instruction => |_| {
                    if (last_label_name) |label_name| {
                        const result = try self.labels.getOrPut(label_name);
                        if (result.found_existing) {
                            self.file.errors.append(self.location, "duplicate label {s}", .{label_name});
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
                        self.file.errors.append(locations.items[i - 1], "label pointing to another label is forbidden", .{});
                    }
                },
                .data_field => |data_field| {
                    if (last_label_name) |label_name| {
                        const result = try self.labels.getOrPut(label_name);
                        if (result.found_existing) {
                            self.file.errors.append(self.location, "duplicate label {s}", .{label_name});
                        } else {
                            result.value_ptr.* = .{ .memory_field = memory_address };
                        }
                    }

                    const maybe_value = self.evaluateExpression(data_field.initializer);
                    if (maybe_value) |value| {
                        const size = self.sizeOf(data_field, value);
                        memory_address += @as(u16, @intCast(size));
                    }
                },
                .constant => |_| {
                    if (last_label_name != null) {
                        self.file.errors.append(locations.items[i - 1], "label pointing to a constant is forbidden", .{});
                    }
                },
            }

            if (node == .label) {
                last_label_name = node.label.name;
            } else {
                last_label_name = null;
            }
        }
    }

    // Constants pass
    {
        for (ast.items, 0..) |node, i| {
            self.location = locations.items[i];
            switch (node) {
                .instruction => |_| {},
                .label => |_| {},
                .data_field => |data_field| {
                    const maybe_value = self.evaluateExpression(data_field.initializer);
                    if (maybe_value) |value| {
                        const size = self.sizeOf(data_field, value);
                        const maybe_bytes = switch (value) {
                            .register => blk: {
                                self.file.errors.append(self.location, "a register cannot be used in a data field", .{});
                                break :blk null;
                            },
                            .string => |string| string,
                            // memory_address Could be an error?
                            .memory_address => |address| try allocator.dupe(u8, std.mem.asBytes(&address)),
                            .immediate => |immediate| blk: {
                                std.debug.assert(size <= 2);
                                if (size == 1 and (immediate < -128 or immediate > 255)) {
                                    self.file.errors.addError(self.location, "number {} out of range for .BYTE", .{immediate});
                                    self.file.errors.addNote(".BYTE accepts numbers in range [-128, 255]", .{});
                                    self.file.errors.commit();
                                    break :blk null;
                                }
                                break :blk try allocator.dupe(u8, std.mem.asBytes(&immediate));
                            },
                        };

                        if (maybe_bytes) |bytes| {
                            try self.memory.append(bytes);
                        }
                    }
                },
                .constant => |constant| {
                    const value = self.evaluateExpression(constant.initializer) orelse break;
                    const bucket = try self.constants.getOrPut(constant.name);
                    if (bucket.found_existing) {
                        self.file.errors.append(self.location, "constant {s} is already defined", .{constant.name});
                    } else {
                        switch (value) {
                            .register => |register| {
                                self.file.errors.append(self.location, "can't assign register {} to a constant", .{register});
                            },
                            .string => |string| {
                                self.file.errors.append(self.location, "can't assign string \"{s}\" to a constant", .{string});
                            },
                            .immediate => |immediate| {
                                bucket.value_ptr.* = immediate;
                            },
                            .memory_address => |address| {
                                bucket.value_ptr.* = @as(i16, @bitCast(address));
                            },
                        }
                    }
                },
            }
        }
    }

    // Instruction pass
    {
        for (ast.items, 0..) |node, i| {
            self.location = locations.items[i];
            switch (node) {
                .instruction => |instruction| {
                    const maybe_type_checked_instruction = try self.typeCheckInstruction(instruction);
                    if (maybe_type_checked_instruction) |type_checked_instruction| {
                        try self.instructions.append(type_checked_instruction);
                    }
                },
                else => {},
            }
        }
    }

    return AssembledProgram{
        .instructions = self.instructions,
        .memory = self.memory,
        .constants = self.constants,
        .labels = self.labels,
    };
}

fn sizeOf(self: *Self, data_field: Parser.DataFieldAstNode, value: EvaluatedExpression) usize {
    return switch (data_field.data_type) {
        .word => 2,
        .byte => 1,
        .ascii => {
            if (value != .string) {
                self.file.errors.append(self.location, ".ASCII expected a string, got {}", .{value});
                return 0;
            }
            return value.string.len;
        },
        .asciz => {
            if (value != .string) {
                self.file.errors.append(self.location, ".ASCIZ expected a string, got {}", .{value});
                return 0;
            }
            return value.string.len + 1;
        },
        .space => {
            if (value != .immediate) {
                self.file.errors.append(self.location, ".SPACE expected a number after it, got {}", .{value});
                return 0;
            }
            const immediate = value.immediate;
            if (immediate < 0) {
                self.file.errors.append(self.location, "size of .SPACE can't be less than zero", .{});
                return 0;
            }
            return @as(usize, @intCast(value.immediate));
        },
        else => unreachable,
    };
}

fn typeCheckInstruction(self: *Self, instruction: Parser.InstructionAstNode) !?Instruction {
    const description = intel8088.isa.get(instruction.mnemonic);
    if (description.allowed_operands.len != instruction.operands.len) {
        self.file.errors.append(self.location, "instruction {} takes {} arguments, but got {}", .{
            instruction.mnemonic,
            description.allowed_operands.len,
            instruction.operands.len,
        });
        return null;
    }

    var operands = try self.file.allocator.alloc(intel8088.InstructionOperand, instruction.operands.len);

    for (instruction.operands, 0..) |operand, i| {
        const evaluated_operand = self.evaluateExpression(operand) orelse return null;
        if (evaluated_operand == .string) {
            self.file.errors.append(self.location, "a string can't be an instruction operand", .{});
            return null;
        }

        const evaluated_operand_type = evaluated_operand.asOperandType();
        const allowed_operand_types = description.allowed_operands[i];
        if (!allowed_operand_types.contains(evaluated_operand_type)) {
            var buf: [120]u8 = undefined;
            @memset(&buf, 0);
            intel8088.OperandType.toHumanReadable(&buf, allowed_operand_types);
            self.file.errors.append(self.location, "argument {} of instruction {} should be one of '{s}', but was {s}", .{
                i + 1,
                instruction.mnemonic,
                buf,
                evaluated_operand_type.toString(),
            });
        }

        operands[i] = switch (evaluated_operand) {
            .register => |register| .{ .register = register },
            .memory_address => |address| .{ .memory = address },
            .immediate => |immediate| .{ .immediate = immediate },
            else => unreachable,
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
) ?EvaluatedExpression {
    switch (expression.*) {
        .binary => |binary| {
            const left_any_type = self.evaluateExpression(binary.left) orelse return null;
            const right_any_type = self.evaluateExpression(binary.right) orelse return null;

            if (left_any_type == .immediate and right_any_type == .immediate) {
                const left = left_any_type.immediate;
                const right = right_any_type.immediate;
                switch (binary.operator) {
                    .plus => return .{ .immediate = left + right },
                    .minus => return .{ .immediate = left - right },
                    .multiply => return .{ .immediate = left * right },
                    .divide => return .{ .immediate = @divFloor(left, right) },
                }
            } else if (left_any_type == .memory_address and right_any_type == .memory_address) {
                const left = left_any_type.memory_address;
                const right = right_any_type.memory_address;
                switch (binary.operator) {
                    .plus => return .{ .memory_address = left + right },
                    .minus => return .{ .memory_address = left - right },
                    .multiply => return .{ .memory_address = left * right },
                    .divide => return .{ .memory_address = @divFloor(left, right) },
                }
            } else if (left_any_type == .immediate and right_any_type == .memory_address) {
                const left = left_any_type.immediate;
                const right = right_any_type.memory_address;
                switch (binary.operator) {
                    .plus => return .{ .memory_address = @as(u16, @intCast(left)) + right },
                    .minus => return .{ .memory_address = @as(u16, @intCast(left)) - right },
                    .multiply => return .{ .memory_address = @as(u16, @intCast(left)) * right },
                    .divide => return .{ .memory_address = @divFloor(@as(u16, @intCast(left)), right) },
                }
            } else if (left_any_type == .memory_address and right_any_type == .immediate) {
                // COPYPASTA SUPER WARNING
                // BE CAREFUL WITH THAT CODE
                const left = right_any_type.immediate;
                const right = left_any_type.memory_address;
                switch (binary.operator) {
                    .plus => return .{ .memory_address = @as(u16, @intCast(left)) + right },
                    .minus => return .{ .memory_address = @as(u16, @intCast(left)) - right },
                    .multiply => return .{ .memory_address = @as(u16, @intCast(left)) * right },
                    .divide => return .{ .memory_address = @divFloor(@as(u16, @intCast(left)), right) },
                }
            } else {
                // TODO: There are more expressions here
                std.debug.assert(false);
                return null;
            }
        },
        .unary => |unary| {
            const operand_any_type = self.evaluateExpression(unary.operand) orelse return null;
            if (operand_any_type == .immediate) {
                const operand = operand_any_type.immediate;
                switch (unary.operator) {
                    .minus => return .{ .immediate = -operand },
                    .plus => return .{ .immediate = operand },
                }
            } else {
                self.file.errors.append(self.location, "expected a number, but got {}", .{operand_any_type});
                return null;
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
                        self.file.errors.append(self.location, "undeclared identifier '{s}'\n", .{identifier});
                        return null;
                    }
                },
                .register => |register| {
                    return .{ .register = register };
                },
                .word => |word| {
                    return .{ .immediate = word };
                },
            }
        },
    }
}
