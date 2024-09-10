const std = @import("std");
const common = @import("common.zig");
const print = std.debug.print;

pub const intel8088 = @import("intel8088_cpu_description.zig");
pub const Tokenizer = @import("Tokenizer.zig");
pub const Parser = @import("Parser.zig");
pub const Emulator = @import("Emulator.zig");

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