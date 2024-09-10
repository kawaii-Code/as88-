const std = @import("std");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");
const Parser = @import("Parser.zig");

const print = std.debug.print;

memory: []u8,
registers: std.EnumArray(intel8088.Register, i16),
flags: std.EnumArray(intel8088.Flag, bool),

instructions: []const Parser.Instruction,
instruction_pointer: usize,
allocator: std.mem.Allocator,

pub const LogicalAddress = struct {
    segment: u16,
    offset: u16,
};

pub const MachineLocation = union(enum) {
    memory: LogicalAddress,
    register: intel8088.Register,
};

pub const Diff = struct {
    previous_value: i16,
    next_value: i16,
    location: MachineLocation,
};

pub fn init(allocator: std.mem.Allocator, assembled_code: Parser.AssembledCode) !@This() {
    // TODO: This should be 1 megabyte
    const memory = try allocator.alloc(u8, 16);

    var memory_index: u32 = 0;
    for (assembled_code.memory.items) |memory_field| {
        // This will get more complicated
        std.mem.copyForwards(u8, memory[memory_index .. ], memory_field);
        memory_index += @as(u32, @intCast(memory_field.len));
    }

    return .{
        .memory = memory,
        .registers = std.EnumArray(intel8088.Register, i16).initFill(0),
        .flags = std.EnumArray(intel8088.Flag, bool).initFill(false),

        .instructions = assembled_code.instructions.items,
        // TODO: This is a register
        .instruction_pointer = 0,
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.allocator.free(self.memory);
}

pub fn step(self: *@This()) !?std.ArrayList(Diff) {
    const instruction = self.currentInstruction();

    const op1 = common.getOrNull(intel8088.InstructionOperand, instruction.operands, 0);
    const op2 = common.getOrNull(intel8088.InstructionOperand, instruction.operands, 1);

    var diffs = std.ArrayList(Diff).init(self.allocator);
    switch (instruction.mnemonic) {
        .mov => try self.store(op1.?, self.load(op2.?), &diffs),
        .add => try self.store(op1.?, self.load(op1.?) + self.load(op2.?), &diffs),
        .sub => try self.store(op1.?, self.load(op1.?) - self.load(op2.?), &diffs),
    }
    
    print("------ DIFFS ----------\n", .{});
    for (diffs.items) |diff| {
        print("{}\n", .{diff});
    }
    print("-----------------------\n", .{});
    
    self.instruction_pointer += 1;
    if (self.instruction_pointer >= self.instructions.len) {
        return null;
    }
    
    return diffs;
}

pub fn currentLineInSourceFile(self: *const @This()) usize {
    return self.currentInstruction().location.line;
}

pub fn load(self: *const @This(), operand: intel8088.InstructionOperand) i16 {
    return switch (operand) {
        .immediate => |value| value,
        .register => |r| self.registers.get(r),
            // TODO: logical <-> physical address conversion
        .memory => |address| self.memory[address],
    };
}

pub fn store(self: *@This(), operand: intel8088.InstructionOperand, value: i16, diffs: *std.ArrayList(Diff)) !void {
    var diff = Diff{
        .previous_value = self.load(operand),
        .next_value = value,
        .location = undefined,
    };

    switch (operand) {
        .immediate => unreachable,
        .register => |register| {
            //self.registers.set(register, value);
            diff.location = .{ .register = register };
            if (register.is8Bit()) {
                // TODO
                try diffs.append(self.applyToParent(register, value));
            } else if (register.has8BitChildren()) {
                const children = register.children();
                try diffs.append(self.applyToChild(children[0], value));
                try diffs.append(self.applyToChild(children[1], value));
            }
        },
        .memory => |address| {
            // TODO: logical <-> physical address conversion
            diff.location = .{ .memory = .{ .segment = 0, .offset = address } };
        },
    }

    try diffs.append(diff);
    
    for (diffs.items) |d| {
        self.commit(d);
    }
}

pub fn commit(self: *@This(), diff: Diff) void {
    self.write(diff.location, diff.next_value);
}

pub fn revert(self: *@This(), diff: Diff) void {
    self.write(diff.location, diff.previous_value);
}

fn applyToParent(self: *const @This(), child: intel8088.Register, value: i16) Diff {
    const parent = child.parent();
    //const next_value = if (child.isLow()) {
    //    return self.registers.get(parent) & value;
    //} else {
    //    return self.registers.get(parent) & @shlExact(value, 8);
    //}
    //next_value
    return Diff {
        .previous_value = self.registers.get(parent),
        .next_value = value,
        .location = .{ .register = parent }, 
    };
}

fn applyToChild(self: *const @This(), child: intel8088.Register, value: i16) Diff {
    var next_value: i16 = undefined;
    if (child.isLow()) {
        next_value = value & 0x00FF;
    } else {
       next_value = @as(i16, @bitCast(@shrExact(@as(u16, @bitCast(value)) & 0xFF00, 8)));
    }

    return Diff {
        .previous_value = self.registers.get(child),
        .next_value = next_value,
        .location = .{ .register = child },
    };
}

fn write(self: *@This(), location: MachineLocation, value: i16) void {
    switch (location) {
        .register => |register| self.registers.set(register, value),
        .memory => |logical_address| {
            // TODO: Actual logical <-> physical address conversion
            const physical_address = logical_address.offset;
            std.mem.writeInt(i16, self.memory[physical_address .. physical_address + 2][0..2], value, .little);
        },
    }
}

fn currentInstruction(self: *const @This()) Parser.Instruction {
    return self.instructions[self.instruction_pointer];
}
