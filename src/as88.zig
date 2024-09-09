const std = @import("std");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

const print = std.debug.print;

pub const AssembledCode = Parser.AssembledCode;
pub const ProgramSourceCode = Tokenizer.ProgramSourceCode;

pub fn assemble(
    source: ProgramSourceCode,
    arena: *std.heap.ArenaAllocator
) !Parser.AssembledCode {
    const tokens = try Tokenizer.tokenize(source, arena);
    for (0 .. tokens.slice().len) |i| {
        const token = tokens.get(i);
        print("({}:{}): {}\n", .{ token.location.line, token.location.column, token.token });
    }

    print("---------------------------\n", .{});
    const parse_result = try Parser.parse(tokens, arena.allocator());
    var label_it = parse_result.labels.valueIterator();
    while (label_it.next()) |label| {
        print("{}\n", .{label});
    }
    for (parse_result.memory.items) |memory_field| {
        print("{any}\n", .{memory_field});
    }
    for (parse_result.instructions.items) |instruction| {
        print("{}\n", .{instruction});
    }
    return parse_result;
}

//pub fn run(parse_result: Parser.ParseResult, allocator: std.mem.Allocator) !void {
//    print("---------------------------\n", .{});
//
//    // "Load" program data into memory.
//    // In the future, load program code into memory as well.
//
//    print("{any}\n\n", .{cpu});
//    for (parse_result.instructions.items) |instruction| {
//        step(&cpu, instruction);
//        print("{any}\n", .{instruction});
//        print("{any}\n\n", .{cpu});
//    }
//}

pub const Emulator = struct {
    cpu: intel8088.CPU,
    instructions: []const Parser.Instruction,
    instruction_pointer: usize,
    allocator: std.mem.Allocator,
    
    pub fn init(allocator: std.mem.Allocator, assembled_code: AssembledCode) !@This() {
        var cpu = intel8088.CPU {
            // TODO: This should be 1 megabyte
            .memory = try allocator.alloc(u8, 16),
            .registers = std.EnumArray(intel8088.Register, i16).initFill(0),
            .flags = std.EnumArray(intel8088.Flag, bool).initFill(false),
        };
        var memory_index: u32 = 0;
        for (assembled_code.memory.items) |memory_field| {
            // This will get more complicated
            std.mem.copyForwards(u8, cpu.memory[memory_index .. ], memory_field);
            memory_index += @as(u32, @intCast(memory_field.len));
        }
        return .{
            .cpu = cpu,
            .allocator = allocator,
            .instructions = assembled_code.instructions.items,
            .instruction_pointer = 0,
        };
    }
    
    pub fn deinit(self: *@This()) void {
        self.allocator.free(self.cpu.memory);
    }
    
    pub fn step(self: *@This()) bool {
        var cpu = self.cpu;
        const instruction = self.currentInstruction();

        const op1 = common.getOrNull(intel8088.InstructionOperand, instruction.operands, 0);
        const op2 = common.getOrNull(intel8088.InstructionOperand, instruction.operands, 1);
    
        switch (instruction.mnemonic) {
            .mov => cpu.store(op1.?, cpu.load(op2.?)),
            .add => cpu.store(op1.?, cpu.load(op1.?) + cpu.load(op2.?)),
            .sub => cpu.store(op1.?, cpu.load(op1.?) - cpu.load(op2.?)),
        }

        self.instruction_pointer += 1;
        if (self.instruction_pointer >= self.instructions.len) {
            return false;
        }
        return true;
    }

    fn currentInstruction(self: *const @This()) Parser.Instruction {
        return self.instructions[self.instruction_pointer];
    }

    pub fn currentLineInSourceFile(self: *const @This()) usize {
        return self.currentInstruction().location.line;
    }
};
