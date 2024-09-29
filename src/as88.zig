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
pub const Errors = @import("Errors.zig");
pub const TypeCheckerAndLowerer = @import("TypeCheckerAndLowerer.zig");

pub const AssembledProgram = TypeCheckerAndLowerer.AssembledProgram;
pub const ProgramSourceCode = Tokenizer.ProgramSourceCode;
pub const UncheckedAst = Parser.UncheckedAst;
pub const TokenWithLocation = Tokenizer.TokenWithLocation;
pub const SourceLocation = Tokenizer.SourceLocation;

pub const AssembledProgramOrErrors = union(enum) {
    program: AssembledProgram,
    errors:  std.ArrayList([]const u8),
};

pub const File = struct {
    allocator: std.mem.Allocator,
    path: ?[]const u8,
    text: []const u8,
    tokens: std.MultiArrayList(TokenWithLocation),
    ast: UncheckedAst,
    errors: Errors,
};


pub fn assemble(
    source: ProgramSourceCode,
    arena: *std.heap.ArenaAllocator
) !AssembledProgramOrErrors {
    const allocator = arena.allocator();
    var file = File{
        .allocator = allocator,
        .path = source.filepath,
        .text = source.contents,
        .tokens = std.MultiArrayList(TokenWithLocation){},
        .ast = undefined,
        .errors = undefined,
    };
    file.errors = Errors.init(allocator, &file);

    try Tokenizer.tokenize(&file);


    for (0 .. file.tokens.len) |i| {
        const token_and_loc = file.tokens.get(i);
        const token = token_and_loc.token;
        const location = token_and_loc.location;
        print("({}:{}): {}\n", .{ location.line, location.column, token });
    }
    print("---------------------------\n", .{});

    if (file.errors.list.items.len != 0) {
        return .{ .errors = file.errors.list };
    }

    try Parser.parse(&file);
    if (file.errors.list.items.len != 0) {
        return .{ .errors = file.errors.list };
    }

    const program = try TypeCheckerAndLowerer.typeCheckAndFinalize(&file.ast, arena.allocator());
    return .{ .program = program };
    //var label_it = parse_result.labels.valueIterator();
    //while (label_it.next()) |label| {
    //    print("{}\n", .{label});
    //}
    //for (parse_result.memory.items) |memory_field| {
    //    print("{any}\n", .{memory_field});
    //}
    //for (parse_result.instructions.items) |instruction| {
    //    print("{}\n", .{instruction});
    //}
}