// Large TODO, in no particular order:
//
// - I use arenas everywhere, so, strictly speaking, I don't leak any memory.
//   Reality is harsher, however, so improve memory management (at least for tui)
// - Constant declarations are broken because of eager evaluation (see examples\constants.s)
// - Review Emulator's naming
// - Registers that share memory (e.g. AX and AH, AL) are handled with a crutch
// - Turn examples into tests, for example, by comparing diffs
// - Segments are not used
// - Load program code into emulator's memory, so that IP will be accurate
// - Store instruction execution times, so that we can measure performance of our assembly code
// - Colorize error output
// - Custom panic handler
// - "Type check" instruction operands and sections

const std = @import("std");
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