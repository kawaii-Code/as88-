const std = @import("std");
const common = @import("common.zig");

pub const InstructionMnemonic = enum {
    pub const Names = common.EnumMemberNamesToStrings(@This()).init();

    mov,
    add,
    sub,
};

pub const Register = enum {
    pub const Names = common.EnumMemberNamesToStrings(@This()).init();

    //pub const @"16 Wide" = init: {
    //    const values = std.enums.values(@This());
    //    var result: [values.len - @"8 Wide".len] = undefined;
    //    var result_index: usize = 0;
    //    for (values) |value| {
    //        if (!value.is8bit()) {
    //            result[result_index] = value;
    //            result_index += 1;
    //        }
    //    }
    //    break :init result;
    //};

    //pub const @"8 Wide" = [8]@This(){ .ah, .al, .bh, .bl, .ch, .cl, .dh, .dl };

    // General purpose registers
    ax,  ah, al,
    bx,  bh, bl,
    cx,  ch, cl,
    dx,  dh, dl,

    // Segment registers
    cs,
    ds,
    ss,
    es,

    // Pointer and index registers
    sp,
    bp,
    si,
    di,

    // Instruction pointer
    ip,

    pub fn is8bit(self: @This()) bool {
        return switch (self) {
            .ah, .al, .bh, .bl,
            .ch, .cl, .dh, .dl => true,
            else => false,
        };
    }
};

pub const Flag = enum {
    tf, df, @"if",
    of, sf, zf, af, pf, cf,  
};

pub const InstructionOperand = union(enum) {
    immediate: i16,
    register: Register,
    memory: u16,
};

pub const CPU = struct {
    // Let's assume that the CPU has a RAM chip inserted in it :)
    memory: []u8,
    // TODO: This won't work when AX, AL, etc. are introduced,
    // since they share values.
    registers: std.EnumArray(Register, i16),
    flags: std.EnumArray(Flag, bool),

    pub fn load(self: *const @This(), operand: InstructionOperand) i16 {
        return switch (operand) {
            .immediate => |value| value,
            .register => |r| self.registers.get(r),
            .memory => |address| self.memory[address],
        };
    }

    pub fn store(self: *@This(), operand: InstructionOperand, value: i16) void {
        switch (operand) {
            .immediate => unreachable,
            .register => |r| self.registers.set(r, value),
            .memory => |address| std.mem.writeInt(i16, self.memory[address .. address + 2][0..2], value, .little),
        }
    }
};

pub const InstructionDescription = struct {
    allowed_operands: []const AllowedOperands,
};

pub const AllowedOperands = std.EnumSet(enum {
    immediate,
    register,
    memory,
});

// That's a comptime generated array, which is a bit crazy.
pub const isa = std.EnumArray(InstructionMnemonic, InstructionDescription).init(.{
    .mov = .{ .allowed_operands = &.{ memOrReg(), memOrRegOrImm() } },
    .add = .{ .allowed_operands = &.{ memOrReg(), memOrRegOrImm() } },
    .sub = .{ .allowed_operands = &.{ memOrReg(), memOrRegOrImm() } },
});

pub const asm_syntax = struct {
    pub const Directive = enum {
        pub const Names = common.EnumMemberNamesToStrings(@This()).init();

        sect,
        word,
        space,
        text,
        data,
        bss,
    
        pub fn isOneOf(self: @This(), expected: []const @This()) bool {
            return std.mem.indexOfScalar(@This(), expected, self) != null;
        }
    
        pub fn isSectionType(self: @This()) bool {
            return switch (self) {
                .text, .data, .bss => true,
                else => false,
            };
        }
    
        pub fn isMemory(self: @This()) bool {
            return switch (self) {
                .word, .space => true,
                else => false,
            };
        }
    };
};

fn memOrImm() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .immediate });
}

fn memOrReg() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .register });
}

fn memOrRegOrImm() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .register, .immediate });
}
