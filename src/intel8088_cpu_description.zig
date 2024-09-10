const std = @import("std");
const common = @import("common.zig");

pub const InstructionMnemonic = enum {
    pub const Names = common.EnumMemberNamesToStrings(@This()).init();

    mov,
    add,
    sub,
};

pub const InstructionOperand = union(enum) {
    immediate: i16,
    register: Register,
    memory: u16,
};

pub const Register = enum {
    pub const Names = common.EnumMemberNamesToStrings(@This()).init();

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

    pub fn is8Bit(self: @This()) bool {
        return switch (self) {
            .ah, .al, .bh, .bl,
            .ch, .cl, .dh, .dl => true,
            else => false,
        };
    }
    
    pub fn parent(self: @This()) Register {
        return switch (self) {
            .ah, .al => .ax,
            .bh, .bl => .bx,
            .ch, .cl => .cx,
            .dh, .dl => .dx,
            else => unreachable,
        };
    }

    pub fn has8BitChildren(self: @This()) bool {
        return switch (self) {
            .ax, .bx, .cx, .dx => true,
            else => false,
        };
    }
    
    pub fn children(self: @This()) [2]Register {
        return switch (self) {
            .ax => .{ .ah, .al },
            .bx => .{ .bh, .bl },
            .cx => .{ .ch, .cl },
            .dx => .{ .dh, .dl },
            else => unreachable,
        };
    }

    pub fn isLow(self: @This()) bool {
        return switch(self) {
            .al, .bl, .cl, .dl => true,
            .ah, .bh, .ch, .dh => false,
            else => unreachable,
        };
    }
};

pub const Flag = enum {
    tf, df, @"if",
    of, sf, zf, af, pf, cf,  
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

        // Memory data types
        word,
        ascii,
        asciz,
        space,

        // Sections
        sect,
        text,
        data,
        bss,

        pub fn isMemoryDataType(self: @This()) bool {
            return switch (self) {
                .word, .ascii,. asciz, .space => true,
                .sect, .text, .data, .bss => false,
            };
        }
    
        pub fn isSectionType(self: @This()) bool {
            return switch (self) {
                .text, .data, .bss => true,
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
