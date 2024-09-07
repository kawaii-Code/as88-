const std = @import("std");


pub const InstructionMnemonic = enum {
    pub const Names = EnumMemberNamesToStrings(@This()).init();

    mov,
    add,
    sub,
};

pub const Register = enum {
    pub const Names = EnumMemberNamesToStrings(@This()).init();

    ax,
    bx,
    cx,
    dx,
};

pub const InstructionDescription = struct {
    mnemonic: InstructionMnemonic,
    // I can't make this a slice because of zig comptime limitations.
    // The next best solution is to put a reasonable upper bound.
    // 8 is fine, since no instruction takes 8 operands.
    operands: [8]AllowedOperands,
    operand_count: usize,
};

pub const AllowedOperands = std.EnumSet(enum {
    immediate,
    register,
    memory,
});

pub const Operand = union(enum) {
    immediate: i16,
    register: Register,
    memory: u16,

    const Parser = @import("Parser.zig");
};

pub const CPU = struct {
    // Let's assume that the CPU has a RAM chip inserted in it :)
    memory: []i16,
    // TODO: This won't work when AX, AL, etc. are introduced,
    // since they share values.
    registers: std.EnumArray(Register, i16),

    pub fn load(self: *const @This(), operand: Operand) i16 {
        return switch (operand) {
            .immediate => |value| value,
            .register => |r| self.registers.get(r),
            .memory => |address| self.memory[address],
        };
    }

    pub fn store(self: *@This(), operand: Operand, value: i16) void {
        switch (operand) {
            .immediate => unreachable,
            .register => |r| self.registers.set(r, value),
            .memory => |address| self.memory[address] = value,
        }
    }
};


// That's a comptime generated array, which is a bit crazy.
pub const isa = std.EnumArray(InstructionMnemonic, InstructionDescription).init(.{
    .mov = instruction(.mov, .{ memoryOrRegister(), memoryOrRegisterOrImmediate() }),
    .add = instruction(.add, .{ memoryOrRegister(), memoryOrRegisterOrImmediate() }),
    .sub = instruction(.sub, .{ memoryOrRegister(), memoryOrRegisterOrImmediate() }),
});

pub const asm_syntax = struct {
    pub const Directive = enum {
        pub const Names = EnumMemberNamesToStrings(@This()).init();

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


fn instruction(mnemonic: InstructionMnemonic, comptime allowed_operands: anytype) InstructionDescription {
    const OperandsType = @TypeOf(allowed_operands);
    const operands_type_info = @typeInfo(OperandsType);
    if (operands_type_info != .Struct) {
        @compileError("expected tuple, found " ++ @typeName(allowed_operands));
    }

    const struct_info = operands_type_info.Struct;
    if (!struct_info.is_tuple) {
        @compileError("expected tuple, found struct");
    }
    
    const fields_info = struct_info.fields;
    if (fields_info.len >= 8) {
        @compileError("too many operands, max is 8");
    }

    var resulting_operands: [8]AllowedOperands = undefined;
    inline for (fields_info, 0..) |f, i| {
        resulting_operands[i] = @field(allowed_operands, f.name);
    }

    return .{
        .mnemonic = mnemonic,
        .operands = resulting_operands,
        .operand_count = fields_info.len,
    };
}

fn memoryOrRegister() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .register });
}

fn memoryOrRegisterOrImmediate() AllowedOperands {
    return AllowedOperands.initMany(&.{ .memory, .register, .immediate });
}

fn EnumMemberNamesToStrings(comptime T: type) type {
    return struct {
        names: @TypeOf(std.meta.fieldNames(T)),
        members: []const T,

        pub fn init() @This() {
            return @This(){
                .names = std.meta.fieldNames(T),
                .members = std.enums.values(T),
            };
        }

        pub fn find(self: *const @This(), name: []const u8) ?T {
            for (0..self.names.len) |i| {
                if (std.ascii.eqlIgnoreCase(self.names[i], name)) {
                    return self.members[i];
                }
            }
            return null;
        }
    };
}
