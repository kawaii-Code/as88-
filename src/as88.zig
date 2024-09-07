const std = @import("std");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");

const print = std.debug.print;


pub const ProgramSource = Tokenizer.ProgramSource;

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

pub fn run(parse_result: Parser.ParseResult, allocator: std.mem.Allocator) !void {
    print("---------------------------\n", .{});
    var cpu = intel8088.CPU {
        // TODO: This should be 1 megabyte
        .memory = try allocator.alloc(i16, 16),
        .registers = std.EnumArray(intel8088.Register, i16).initFill(0),
    };
    defer allocator.free(cpu.memory);

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

fn step(cpu: *intel8088.CPU, instruction: Parser.Instruction) void {
    const op1 = common.getOrNull(intel8088.Operand, instruction.operands, 0);
    const op2 = common.getOrNull(intel8088.Operand, instruction.operands, 1);

    switch (instruction.mnemonic) {
        .mov => cpu.store(op1.?, cpu.load(op2.?)),
        .add => cpu.store(op1.?, cpu.load(op1.?) + cpu.load(op2.?)),
        .sub => cpu.store(op1.?, cpu.load(op1.?) - cpu.load(op2.?)),
    }
}
