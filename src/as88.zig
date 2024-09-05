const std = @import("std");

const print = std.debug.print;


// In assembly, ast is flat
const AstNode = union(enum) {
    instruction:   Instruction,
    register_name: Register,
    section:       Section,
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


const Token = struct {
    kind: TokenKind,
    loc: SourceLocation,
};

const SourceLocation = struct {
    line:   usize,
    column: usize,
};

const TokenKind = union(enum) {
    none,
    comma,
    sect,
    sect_text,
    sect_data,
    sect_bss,
    label: []const u8,
    number: i16,
    register: Register,
    instruction: InstructionKind,
};

const InstructionKind = enum {
    mov,
    add,
};


const RegisterWithName = struct {
    kind: Register,
    name: []const u8,
};

// TODO: Generate this with comptime
const registers = [_]RegisterWithName {
    .{.kind = .ax, .name = "AX"},
    .{.kind = .bx, .name = "BX"},
    .{.kind = .cx, .name = "CX"},
    .{.kind = .dx, .name = "DX"},
};


pub fn assemble_and_run(
    source: []const u8,
    allocator: std.mem.Allocator
) !void {
    const eql = std.ascii.eqlIgnoreCase;
    const endsWith = std.ascii.endsWithIgnoreCase;

    const source_uppercase = try std.ascii.allocUpperString(allocator, source);
    defer allocator.free(source_uppercase);
    
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var line_number: usize = 1;
    var line_it = std.mem.tokenizeAny(u8, source_uppercase, "\n");
    while (line_it.next()) |line| : (line_number += 1) {
        var token_column: usize = 1; // this doesn't quite work
        var token_it = std.mem.tokenizeAny(u8, line, " ,\t"); // don't use tokenizer here, commas are getting eaten
        while (token_it.next()) |token| : (token_column = token_it.index + 1) {
            const location = SourceLocation {
                .line = line_number,
                .column = token_column,
            };
        
            if (eql(token, ".SECT")) {
                try tokens.append(.{.kind = .sect, .loc = location});
            } else if (eql(token, "TEXT")) {
                try tokens.append(.{.kind = .sect_text, .loc = location});
            } else if (eql(token, "DATA")) {
                try tokens.append(.{.kind = .sect_data, .loc = location});
            } else if (eql(token, "BSS")) {
                try tokens.append(.{.kind = .sect_bss, .loc = location});
            } else if (eql(token, "MOV")) {
                try tokens.append(.{.kind = .{.instruction = .mov}, .loc = location});
            } else if (eql(token, "ADD")) {
                try tokens.append(.{.kind = .{.instruction = .add}, .loc = location});
            } else if ('0' <= token[0] and token[0] <= '9') {
                // TODO: Better error reporting
                const number = try std.fmt.parseInt(i16, token, 0);
                try tokens.append(.{.kind = .{.number = number}, .loc = location});
            } else if (endsWith(token, ":")) {
                // TODO: Check that label identifier is valid
                try tokens.append(.{.kind = .{.label = token[0 .. token.len-1]}, .loc = location});
            }
            
            for (registers) |register| {
                if (eql(token, register.name)) {
                    try tokens.append(.{.kind = .{.register = register.kind }, .loc = location});
                }
            }
        }
    }
    
    for (tokens.items) |token| {
        print("({}:{}): {}\n", .{token.loc.line, token.loc.column, token.kind});
    }

    const ast = std.ArrayList(AstNode).init(allocator);
    defer ast.deinit();

    for (tokens.items) |token| {
        switch (token.kind) {
            else => print("Unhandled token kind: {}\n", .{token.kind}),
        }
    }
}