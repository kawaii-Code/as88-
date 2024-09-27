const std = @import("std");
const common = @import("common.zig");

pub const InstructionMnemonic = enum {
    pub const Names = common.EnumMemberNamesToStrings(@This()).init();

    nop,
    mov,
    loop,
    push,
    pop,
    sys,

    add,
    inc,
    sub,
    dec,
    neg,
    cmp,
    mul,
    imul,
    div,
    idiv,
    
    cwd,
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
    allowed_operands: []const AllowedOperands = &.{},
};

pub const AllowedOperands = std.EnumSet(enum {
    immediate,
    register,
    memory,
});

pub const asm_syntax = struct {
    pub const BinaryOperator = enum {
        plus,
        minus,
        multiply,
        divide,
    };

    pub const UnaryOperator = enum {
        plus,
        minus,
    };

    pub const Section = enum {
        no_section,
        text_section,
        data_section,
        bss_section,
    };

    pub const Directive = enum {
        pub const Names = common.EnumMemberNamesToStrings(@This()).init();

        // Memory data types
        word,
        ascii,
        asciz,
        space,
        byte,

        // Sections
        sect,
        text,
        data,
        bss,

        pub fn size(self: @This()) u16 {
            std.debug.assert(self.hasASize());
            return switch (self) {
                .word => 2,
                .byte => 1,  
                else => unreachable,
            };
        }

        pub fn hasASize(self: @This()) bool {
            return switch (self) {
                .word, .byte => true,
                else => false,
            };
        }

        pub fn isString(self: @This()) bool {
            return switch (self) {
                .ascii, .asciz => true,
                else => false,
            };
        }

        pub fn isMemoryDataType(self: @This()) bool {
            return switch (self) {
                .word, .ascii,. asciz, .space, .byte => true,
                .sect, .text, .data, .bss => false,
            };
        }
    
        pub fn isStringDataType(self: @This()) bool {
            return switch (self) {
                .ascii, .asciz => true,
                else => false,
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

// That's a comptime generated array, which is a bit crazy.
pub const isa = std.EnumArray(InstructionMnemonic, InstructionDescription).init(.{
    .mov = .{ .allowed_operands = &.{ memOrReg(), memOrRegOrImm() } },
    .loop = .{ .allowed_operands = &.{ memOrRegOrImm() } },
    
    .add = .{ .allowed_operands = &.{ memOrReg(), memOrRegOrImm() } },
    .inc = .{ .allowed_operands = &.{ memOrReg() } },
    .sub = .{ .allowed_operands = &.{ memOrReg(), memOrRegOrImm() } },
    .dec = .{ .allowed_operands = &.{ memOrReg() } },
    .neg = .{ .allowed_operands = &.{ memOrReg() } },
    .cmp = .{ .allowed_operands = &.{ memOrRegOrImm(), memOrRegOrImm() } },
    .mul = .{ .allowed_operands = &.{ memOrRegOrImm() } },
    .imul = .{ .allowed_operands = &.{ memOrRegOrImm() } },
    .div = .{ .allowed_operands = &.{ memOrRegOrImm() } },
    .idiv = .{ .allowed_operands = &.{ memOrRegOrImm() } },
    
    .cwd = .{ },
    
    .push = .{ .allowed_operands = &.{ memOrRegOrImm() } },
    .pop = .{ .allowed_operands = &.{ memOrReg() } },
    .nop = .{ },
    .sys = .{ },
});

fn memOrImm() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .immediate });
}

fn memOrReg() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .register });
}

fn memOrRegOrImm() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .register, .immediate });
}
