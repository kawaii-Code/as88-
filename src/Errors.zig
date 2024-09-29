const std = @import("std");
const as88 = @import("as88.zig");

file: *as88.File,
list: std.ArrayList([]const u8),
last_error: std.ArrayList(u8),

pub fn init(file: *as88.File) @This() {
    return @This(){
        .file = file,
        .list = std.ArrayList([]const u8).init(file.allocator),
        .last_error = std.ArrayList(u8).init(file.allocator),
    };
}

pub fn append(
    self: *@This(),
    location: as88.SourceLocation,
    comptime error_fmt: []const u8,
    error_args: anytype,
) void {
    self.addError(location, error_fmt, error_args);
    self.commit();
}

pub fn addError(
    self: *@This(),
    location: as88.SourceLocation,
    comptime error_fmt: []const u8,
    error_args: anytype,
) void {
    std.debug.assert(self.last_error.items.len == 0);
    var buf = &self.last_error;
    if (self.file.path) |filepath| {
        std.fmt.format(buf.writer(), "{s}:{}:{}: error: ", .{
            std.fs.path.basename(filepath), // If we have files named the same in two different directories, this could be confusing.
            // But. This is an old assembler for a processor that no one uses. I won't do it.
            location.line,
            location.column,
        }) catch unreachable;
    } else {
        std.fmt.format(buf.writer(), "{}:{}: error: ", .{
            location.line,
            location.column,
        }) catch unreachable;
    }
    std.fmt.format(buf.writer(), error_fmt, error_args) catch unreachable;
    buf.append('\n') catch unreachable;

    if (location.length() > 0) {
        const last_newline = std.mem.lastIndexOfScalar(u8, self.file.text[0..location.start_byte], '\n');
        const line_begin = init: {
            if (last_newline) |newline_index| {
                break :init newline_index + 1;
            }
            break :init 0;
        };
        const offset_to_newline = std.mem.indexOfScalar(u8, self.file.text[location.start_byte..], '\n') orelse (self.file.text.len - location.start_byte);
        const line = self.file.text[line_begin .. location.start_byte + offset_to_newline];
        std.fmt.format(buf.writer(), "\t{s}\n", .{line}) catch unreachable;

        buf.append('\t') catch unreachable;
        for (0..location.column - 1) |_| {
            buf.append(' ') catch unreachable;
        }
        for (0..location.length()) |_| {
            buf.append('^') catch unreachable;
        }
        buf.append('\n') catch unreachable;
    }
}

pub fn addNote(
    self: *@This(),
    comptime note_fmt: []const u8,
    note_args: anytype,
) void {
    var buf = &self.last_error;
    std.fmt.format(buf.writer(), "note: ", .{}) catch unreachable;
    std.fmt.format(buf.writer(), note_fmt, note_args) catch unreachable;
    buf.append('\n') catch unreachable;
}

pub fn commit(self: *@This()) void {
    const error_copy = self.last_error.toOwnedSlice() catch unreachable;
    self.list.append(error_copy) catch unreachable;
}
