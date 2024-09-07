const std = @import("std");
const common = @import("common.zig");
const as88 = @import("as88.zig");


const print = std.debug.print;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len != 2) {
        const program_name = args[0];
        print("Usage: {s} <file.s>\n", .{program_name});
        std.process.exit(1);
    }

    const filepath = args[1];
    const source_file = std.fs.cwd().openFile(filepath, .{ .mode = .read_only }) catch |err| {
        print("Error when opening {s}: {}\n", .{ filepath, err });
        std.process.exit(1);
    };

    const max_source_file_size = common.megabytes(8);
    const source = source_file.readToEndAlloc(allocator, max_source_file_size) catch |err| {
        print("Error when reading {s}: {}\n", .{ filepath, err });
        std.process.exit(1);
    };
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const assembled_code = try as88.assemble(.{ .filepath = filepath, .contents = source }, &arena);
    try as88.run(assembled_code, allocator);
}


test {
    _ = @import("Tokenizer.zig");
    _ = @import("Parser.zig");
    _ = @import("as88.zig");
}