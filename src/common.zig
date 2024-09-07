// This things should be in the standard library.
// Could be that I just can't find them.

pub fn megabytes(count: usize) usize {
    return count * 1024 * 1024;
}

pub fn getOrNull(comptime T: type, slice: []const T, index: usize) ?T {
    if (index < slice.len) {
        return slice[index];
    }
    return null;
}

