const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

pub const ProgramSource = Tokenizer.ProgramSource;

const print = std.debug.print;


pub fn assemble(
    source: ProgramSource,
    arena: *std.heap.ArenaAllocator
) !Parser.ParseResult {
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
        print("{}\n", .{memory_field});
    }
    for (parse_result.instructions.items) |instruction| {
        print("{}\n", .{instruction});
    }
    return parse_result;
}

const CPU = struct {
    // Let's assume that the CPU has a RAM chip inserted in it :)
    memory: []i16,
    registers: [std.enums.values(Tokenizer.Register).len]i16,
};

pub fn run(parse_result: Parser.ParseResult, allocator: std.mem.Allocator) !void {
    print("---------------------------\n", .{});
    var cpu = CPU {
        .memory = try allocator.alloc(i16, 32),
        .registers = undefined,
    };
    defer allocator.free(cpu.memory);
    for (0 .. cpu.registers.len) |i| {
        cpu.registers[i] = 0;
    }

    // "Load" program data into memory.
    // In the future, load program code into memory as well.
    var memory_index: u32 = 0;
    for (parse_result.memory.items) |memory_field| {
        // This will get more complicated
        cpu.memory[memory_index] = memory_field.value;
        memory_index += @as(u32, @intCast(memory_field.size));
    }

    print("{any}\n\n", .{cpu});
    for (parse_result.instructions.items) |instruction| {
        step(&cpu, instruction);
        print("{any}\n", .{instruction});
        print("{any}\n\n", .{cpu});
    }
}

fn step(cpu: *CPU, instruction: Parser.Instruction) void {
    switch (instruction.kind) {
        .mov => {
            const lhs = instruction.operand1;
            const rhs = instruction.operand2;
            var destination: *i16 = undefined;
            
            switch (lhs) {
                .register => destination = &cpu.registers[@as(u32, @intFromEnum(lhs.register))],
                .immediate => todo(), // TODO: Report error: Can't write into immediate
                .memory => destination = &cpu.memory[lhs.memory.memory_field],
            }
            
            switch (rhs) {
                .register => destination.* = cpu.registers[@as(u32, @intFromEnum(rhs.register))],
                .immediate => destination.* = rhs.immediate,
                .memory => destination.* = cpu.memory[rhs.memory.memory_field],
            }
        },
        .add => {
            const lhs = instruction.operand1;
            const rhs = instruction.operand2;
            var destination: *i16 = undefined;
            
            switch (lhs) {
                .register => destination = &cpu.registers[@as(u32, @intFromEnum(lhs.register))],
                .immediate => todo(), // Can't write into immediate
                .memory => destination = &cpu.memory[lhs.memory.memory_field],
            }
            
            switch (rhs) {
                .register => destination.* += cpu.registers[@as(u32, @intFromEnum(rhs.register))],
                .immediate => destination.* += rhs.immediate,
                .memory => destination.* += cpu.memory[rhs.memory.memory_field],
            }
        },
    }
}

fn todo() noreturn {
    std.debug.panic("this was not yet implemented\n", .{});
}
