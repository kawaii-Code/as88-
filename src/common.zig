// This things should be in the standard library.
// Could be that I just can't find them.
const std = @import("std");

const print = std.debug.print;

pub fn megabytes(count: usize) usize {
    return count * 1024 * 1024;
}

pub fn getOrNull(comptime T: type, slice: []const T, index: usize) ?T {
    if (index < slice.len) {
        return slice[index];
    }
    return null;
}

pub fn readEntireFile(filepath: []const u8, allocator: std.mem.Allocator) []const u8 {
    const source_file = std.fs.cwd().openFile(filepath, .{ .mode = .read_only }) catch |err| {
        print("Error when opening {s}: {}\n", .{ filepath, err });
        std.process.exit(1);
    };

    const max_file_size = megabytes(8);
    const source = source_file.readToEndAlloc(allocator, max_file_size) catch |err| {
        print("Error when reading {s}: {}\n", .{ filepath, err });
        std.process.exit(1);
    };

    return source;
}

pub fn EnumMemberNamesToStrings(comptime T: type) type {
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
