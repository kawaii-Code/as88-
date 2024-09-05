const std = @import("std");

const print = std.debug.print;

// In assembly, ast is flat
const AstNode = union(enum) {
    instruction: Instruction,
    register_name: Register,
    section: Section,
};

const Instruction = struct {
    kind: InstructionKind,
    args: void, // unimplemented
};

const Register = enum {
    ax,
    bx,
    cx,
    dx,
};

const Section = enum {
    text,
    data,
    bss,
};

const Keyword = enum {
    sect,
    word,
    space,
};

const Token = struct {
    kind: TokenKind,
    loc: SourceLocation,
};

const SourceLocation = struct {
    line: usize,
    column: usize,
};

const TokenKind = union(enum) {
    none,
    comma,
    keyword: Keyword,
    section: Section,
    label: []const u8,
    number: i16,
    register: Register,
    instruction: InstructionKind,
};

const InstructionKind = enum {
    mov,
    add,
};

const instruction_names = EnumNamesToMembers(InstructionKind).init();
const register_names = EnumNamesToMembers(Register).init();
const section_names = EnumNamesToMembers(Section).init();
const keyword_names = EnumNamesToMembers(Keyword).init();

pub fn assembleAndRun(source: []const u8, allocator: std.mem.Allocator) !void {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var line_number: usize = 1;
    var line_it = std.mem.tokenizeAny(u8, source, "\n");
    while (line_it.next()) |line| : (line_number += 1) {
        var token_column: usize = 1; // this doesn't quite work
        var token_it = std.mem.tokenizeAny(u8, line, " ,\t"); // don't use tokenizer here, commas are getting eaten
        while (token_it.next()) |token| : (token_column = token_it.index + 1) {
            const location = SourceLocation{
                .line = line_number,
                .column = token_column,
            };

            if (std.ascii.startsWithIgnoreCase(token, ".")) {
                if (keyword_names.find(token[1..])) |keyword| {
                    try tokens.append(.{ .kind = .{ .keyword = keyword }, .loc = location });
                } else {
                    printError(location, "no keyword '{s}' exists.\n", .{token});
                    printNote("'{s}' was interpreted as a keyword, because it starts with '.'\n", .{token});
                }
            } else if (instruction_names.find(token)) |instruction| {
                try tokens.append(.{ .kind = .{ .instruction = instruction }, .loc = location });
            } else if (section_names.find(token)) |section| {
                try tokens.append(.{ .kind = .{ .section = section }, .loc = location });
            } else if (register_names.find(token)) |register| {
                try tokens.append(.{ .kind = .{ .register = register }, .loc = location });
            } else if ('0' <= token[0] and token[0] <= '9') {
                if (std.fmt.parseInt(i16, token, 0)) |number| {
                    try tokens.append(.{ .kind = .{ .number = number }, .loc = location });
                } else |err| switch (err) {
                    error.InvalidCharacter => printError(location, "bad character in int literal '{s}'\n", .{token}),
                    error.Overflow => printError(location, "int literal '{s}' is too big. It must be in the range [-32768 .. 32767].\n", .{token}),
                }
            } else if (std.ascii.endsWithIgnoreCase(token, ":")) {
                // TODO: Check that label identifier is valid
                try tokens.append(.{ .kind = .{ .label = token[0 .. token.len - 1] }, .loc = location });
            } else if (std.ascii.startsWithIgnoreCase(token, "!")) {
                // Skip until the end of the line
                break;
            } else {
                printError(location, "can't recognize token '{s}'\n", .{token});
            }
        }
    }

    for (tokens.items) |token| {
        print("({}:{}): {}\n", .{ token.loc.line, token.loc.column, token.kind });
    }

    const ast = std.ArrayList(AstNode).init(allocator);
    defer ast.deinit();

    for (tokens.items) |token| {
        switch (token.kind) {
            else => print("Unhandled token kind: {}\n", .{token.kind}),
        }
    }
}

fn printNote(comptime fmt: []const u8, args: anytype) void {
    print("note: ", .{});
    print(fmt, args);
}

fn printError(location: SourceLocation, comptime fmt: []const u8, args: anytype) void {
    print("({}:{}): error: ", .{ location.line, location.column });
    print(fmt, args);
}

fn EnumNamesToMembers(comptime T: type) type {
    return struct {
        const Self = @This();

        names: @TypeOf(std.meta.fieldNames(T)),
        members: []const T,

        pub fn init() Self {
            return Self{
                .names = std.meta.fieldNames(T),
                .members = std.enums.values(T),
            };
        }

        pub fn find(self: *const Self, name: []const u8) ?T {
            for (0..self.names.len) |i| {
                if (std.ascii.eqlIgnoreCase(self.names[i], name)) {
                    return self.members[i];
                }
            }
            return null;
        }
    };
}
