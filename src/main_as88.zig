const std = @import("std");
const as88 = @import("as88.zig");
const common = @import("common.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len != 2) {
        const program_name = args[0];
        std.debug.print("Usage: {s} <file.s>\n", .{program_name});
        std.process.exit(1);
    }

    const filepath = args[1];
    const source = common.readEntireFile(filepath, allocator);
    defer allocator.free(source);
    
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    _ = try as88.assemble(.{
        .filepath = filepath,
        .contents = source,
    }, &arena);
    std.debug.print("Compilation OK. No errors found\n", .{});
}
