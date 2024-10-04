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
    data_fields: std.ArrayList(DataField),
    memory: std.ArrayList([]const u8),
    labels: std.StringHashMap(Label),
    constants: std.StringHashMap(i16),
};

pub const DataField = struct {
    start_address: u16,
    size: u16,
    initializer: ?[]const u8,
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
data_fields: std.ArrayList(DataField),
memory: std.ArrayList([]const u8),
constants: std.StringHashMap(i16),
labels: std.StringHashMap(Label),
location: SourceLocation,

pub fn typeCheckAndFinalize(file: *as88.File) !AssembledProgram {
    const allocator = file.allocator;
    const unchecked_ast = file.ast;
    var self = Self{
        .file = file,
        .labels = std.StringHashMap(Label).init(allocator),
        .constants = std.StringHashMap(i16).init(allocator),
        .data_fields = std.ArrayList(DataField).init(allocator),
        .memory = std.ArrayList([]const u8).init(allocator),
        .instructions = std.ArrayList(Instruction).init(allocator),
        .location = undefined,
    };

    // TODO:
    // Label pass, solely for error reporting
    //{
    //    var last_label_name: ?[]const u8 = null;
    //    for (ast.items, 0..) |node, i| {
    //        self.location = locations.items[i];
    //        switch (node) {
    //            .instruction => |_| {},
    //            .data_field => {},
    //            .label => |_| {
    //                // TODO: Actually, two or more labels could point
    //                // at the same data, which is a little annoying.
    //                if (last_label_name != null) {
    //                    self.file.errors.append(locations.items[i - 1], "label pointing to another label is forbidden", .{});
    //                }
    //            },
    //            .constant => |_| {
    //                if (last_label_name != null) {
    //                    self.file.errors.append(locations.items[i - 1], "label pointing to a constant is forbidden", .{});
    //                }
    //            },
    //        }
    //
    //        if (node == .label) {
    //            last_label_name = node.label.name;
    //        } else {
    //            last_label_name = null;
    //        }
    //    }
    //}

    // Data fields pass
    {
        // TODO: A label pointing to a label would ruin me here.
        var last_label_name: ?[]const u8 = null;
        var memory_address: u16 = 0;
        for (0..unchecked_ast.data_section.len) |i| {
            const item = unchecked_ast.data_section.get(i);
            self.location = item.location;
            // Maybe there is a more ergonomic way to do this?
            const node = item.node;
            if (node == .label) {
                last_label_name = node.label.name;
                continue;
            }
            const maybe_data_field = try self.typeCheckAndEvaluateDataField(node, item.location, memory_address);
            if (maybe_data_field) |data_field| {
                if (last_label_name) |label| {
                    try self.addLabel(label, .{ .memory_field = memory_address });
                }

                try self.data_fields.append(data_field);
                memory_address += data_field.size;
            } else {
                last_label_name = null;
            }
        }

        // Copypasta from above
        last_label_name = null;
        for (0..unchecked_ast.bss_section.len) |i| {
            const item = unchecked_ast.bss_section.get(i);
            self.location = item.location;
            const node = item.node;
            if (node == .label) {
                last_label_name = node.label.name;
                continue;
            }
            const maybe_data_field = try self.typeCheckAndEvaluateDataField(node, item.location, memory_address);
            if (maybe_data_field) |data_field| {
                if (last_label_name) |label| {
                    try self.addLabel(label, .{ .memory_field = memory_address });
                }

                try self.data_fields.append(data_field);
                memory_address += data_field.size;
            } else {
                last_label_name = null;
            }
        }
    }

    // Constants pass
    {
        for (0..unchecked_ast.outside_any_section.len) |i| {
            const item = unchecked_ast.outside_any_section.get(i);
            const node = item.node;
            self.location = item.location;
            switch (node) {
                .constant => |constant| {
                    try self.typeCheckAndEvaluateConstant(constant);
                },
                else => {
                    self.file.errors.append(self.location, "{} does not belong in this section", .{node});
                }
            }
        }
    }

    // Instruction labels pass
    {
        var instruction_address: u16 = 0;
        var last_label_name: ?[]const u8 = null;
        for (0..unchecked_ast.text_section.len) |i| {
            const item = unchecked_ast.text_section.get(i);
            self.location = item.location;
            const node = item.node;
            if (node == .instruction) {
                if (last_label_name) |name| {
                    try self.addLabel(name, .{ .instruction = instruction_address });
                }
                instruction_address += 1;
            }
            if (node == .label) {
                last_label_name = node.label.name;
            } else {
                last_label_name = null;
            }
        }
    }

    // Instruction pass
    {
        for (0..unchecked_ast.text_section.len) |i| {
            const item = unchecked_ast.text_section.get(i);
            self.location = item.location;
            const node = item.node;
            switch (node) {
                .instruction => |instruction| {
                    const maybe_type_checked_instruction = try self.typeCheckInstruction(instruction);
                    if (maybe_type_checked_instruction) |type_checked_instruction| {
                        try self.instructions.append(type_checked_instruction);
                    }
                },
                .label => {},
                else => {
                    self.file.errors.append(self.location, "{} does not belong in the text section", .{node});
                },
            }
        }
    }

    return AssembledProgram{
        .instructions = self.instructions,
        .data_fields = self.data_fields,
        .memory = self.memory,
        .constants = self.constants,
        .labels = self.labels,
    };
}

fn typeCheckAndEvaluateDataField(self: *Self, node: Parser.AstNode, location: SourceLocation, start_address: u16) !?DataField {
    switch (node) {
        .data_field => |data_field| {
            const maybe_value = self.evaluateExpression(data_field.initializer);
            if (maybe_value) |value| {
                if (data_field.data_type == .space) {
                    if (value != .immediate) {
                        self.file.errors.append(self.location, ".SPACE expected a number after it, got {}", .{value});
                        return null;
                    }
                    const immediate = value.immediate;
                    if (immediate < 0) {
                        self.file.errors.append(self.location, "size of .SPACE can't be less than zero", .{});
                        return null;
                    }
                    const size = @as(u16, @bitCast(value.immediate));
                    return DataField{
                        .start_address = start_address,
                        .size = size,
                        .initializer = null,
                    };
                }

                const size = self.sizeOf(data_field, value);
                const maybe_bytes: ?[]const u8 = switch (value) {
                    .register => blk: {
                        self.file.errors.append(self.location, "a register cannot be used in a data field", .{});
                        break :blk null;
                    },
                    .memory_address => blk: {
                        self.file.errors.append(self.location, "a memory access is not allowed in a data field", .{});
                        break :blk null;
                    },
                    .string => |string| string,
                    .immediate => |immediate| blk: {
                        std.debug.assert(size <= 2);
                        if (size == 1 and (immediate < -128 or immediate > 255)) {
                            self.file.errors.addError(self.location, "number {} out of range for .BYTE", .{immediate});
                            self.file.errors.addNote(".BYTE accepts numbers in range [-128, 255]", .{});
                            self.file.errors.commit();
                            break :blk null;
                        }
                        break :blk std.mem.asBytes(&immediate);
                    },
                };
                
                if (maybe_bytes) |bytes| {
                    const initializer = blk: {
                        if (data_field.data_type == .asciz) {
                            var cstring = try self.file.allocator.alloc(u8, bytes.len + 1);
                            @memcpy(cstring[0..bytes.len], bytes);
                            cstring[bytes.len] = 0;
                            break :blk cstring;
                        }
                        break :blk try self.file.allocator.dupe(u8, bytes);
                    };
                    return DataField{
                        .start_address = start_address,
                        .size = @as(u16, @intCast(bytes.len)), // TODO: Could overflow
                        .initializer = initializer,
                    };
                }
            }
            return null;
        },
        .label => return null,
        else => {
            self.file.errors.append(location, "{} is not allowed in the data section", .{node});
            return null;
        },
    }
}

fn typeCheckAndEvaluateConstant(self: *Self, constant: Parser.ConstantDefinitionAstNode) !void {
    const value = self.evaluateExpression(constant.initializer) orelse return;
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
        else => unreachable,
    };
}

fn typeCheckInstruction(
    self: *Self,
    instruction: Parser.InstructionAstNode
) !?Instruction {
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

            const left = switch (left_any_type) {
                .immediate => |value| value,
                else => {
                    self.file.errors.append(self.location, "invalid operand in expression: {}", .{left_any_type});
                    return null;
                },
            };
            const right = switch (right_any_type) {
                .immediate => |value| value,
                else => {
                    self.file.errors.append(self.location, "invalid operand in expression: {}", .{left_any_type});
                    return null;
                },
            };

            switch (binary.operator) {
                .plus => return .{ .immediate = left + right },
                .minus => return .{ .immediate = left - right },
                .multiply => return .{ .immediate = left * right },
                .divide => return .{ .immediate = @divFloor(left, right) },
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
        .address => |address| {
            switch (address) {
                .basic => |basic| {
                    const operand_any_type = self.evaluateExpression(basic) orelse return null;
                    switch (operand_any_type) {
                        .register => {
                            // TODO: intel8088 is crazy, some registers can access memory, some not, and it makes 0 sense.
                            unreachable;
                        },
                        .immediate => |value| {
                            return .{ .memory_address = @as(u16, @bitCast(value)) };
                        },
                        .memory_address => {
                            self.file.errors.append(self.location, "can't dereference something that is already dereferenced. Long story short, the code is wrong.", .{});
                            return null;
                        },
                        .string => {
                            self.file.errors.append(self.location, "string was not expected here", .{});
                            return null;
                        },
                    }
                }
            }
        },
        .value => |value| {
            switch (value) {
                .string => |string| return .{ .string = string },
                .identifier => |identifier| {
                    if (self.constants.get(identifier)) |constant_value| {
                        return .{ .immediate = constant_value };
                    } else if (self.labels.get(identifier)) |label| {
                        // what label is, doesn't matter. It's just a number.
                        const immediate = switch (label) {
                            .instruction => |address| @as(i16, @bitCast(address)),
                            .memory_field => |address| @as(i16, @bitCast(address)),
                        };
                        return .{ .immediate = immediate };
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

fn addLabel(self: *Self, name: []const u8, value: Label) !void {
    const result = try self.labels.getOrPut(name);
    if (result.found_existing) {
        self.file.errors.append(self.location, "duplicate label {s}", .{name});
    } else {
        result.value_ptr.* = value;
    }
}