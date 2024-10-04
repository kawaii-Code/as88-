const std = @import("std");
const as88 = @import("as88.zig");
const common = @import("common.zig");
const intel8088 = @import("intel8088_cpu_description.zig");
const Tokenizer = @import("Tokenizer.zig");

const Token = Tokenizer.Token;
const print = std.debug.print;
const SourceLocation = Tokenizer.SourceLocation;

const Self = @This();

pub const UncheckedAst = struct {
    outside_any_section: std.MultiArrayList(AstNodeWithLocation),
    text_section: std.MultiArrayList(AstNodeWithLocation),
    data_section: std.MultiArrayList(AstNodeWithLocation),
    bss_section: std.MultiArrayList(AstNodeWithLocation),

    pub fn init() @This() {
        return @This(){
            .outside_any_section = std.MultiArrayList(AstNodeWithLocation){},
            .text_section = std.MultiArrayList(AstNodeWithLocation){},
            .data_section = std.MultiArrayList(AstNodeWithLocation){},
            .bss_section = std.MultiArrayList(AstNodeWithLocation){},
        };
    }
};

pub const AstNodeWithLocation = struct {
    node: AstNode,
    location: SourceLocation,
};

pub const AstNode = union(enum) {
    instruction: InstructionAstNode,
    label: LabelAstNode,
    constant: ConstantDefinitionAstNode,
    data_field: DataFieldAstNode,
};

pub const DataFieldAstNode = struct {
    data_type: intel8088.asm_syntax.Directive,
    initializer: *ExpressionAstNode,
};

pub const ConstantDefinitionAstNode = struct {
    name: []const u8,
    initializer: *ExpressionAstNode,
};

pub const ExpressionAstNode = union(enum) {
    binary: BinaryExpressionAstNode,
    unary: UnaryExpressionAstNode,
    address: AddressModeAstNode,
    value: ValueAstNode,
};

pub const BinaryExpressionAstNode = struct {
    left: *ExpressionAstNode,
    right: *ExpressionAstNode,
    operator: intel8088.asm_syntax.BinaryOperator,
};

pub const UnaryExpressionAstNode = struct {
    operand: *ExpressionAstNode,
    operator: intel8088.asm_syntax.UnaryOperator,
};

pub const ValueAstNode = union(enum) {
    string: []const u8, // this should only be allowed in data fields
    identifier: []const u8,
    register: intel8088.Register,
    word: i16, // There are more variants, like unsigned word,
    // byte and unsigned byte
};

pub const AddressModeAstNode = union(enum) {
    basic: *ExpressionAstNode,
};

pub const InstructionAstNode = struct {
    mnemonic: intel8088.InstructionMnemonic,
    operands: []const *ExpressionAstNode,
    location: SourceLocation,
};

pub const LabelAstNode = struct {
    name: []const u8,
};

const ParseState = enum {
    no_section,
    text_section,
    data_section,
    bss_section,
};

file: *as88.File,
tokens: []Token,
locations: []SourceLocation,
position: usize,

pub fn parse(file: *as88.File) !void {
    var self = Self{
        .file = file,
        .tokens = file.tokens.items(.token),
        .locations = file.tokens.items(.location),
        .position = 0,
    };

    var current_section: intel8088.asm_syntax.Section = .no_section;
    while (self.peek()) |token| {
        var parsed_node: ?AstNode = null;
        const start_location = self.peekLocation();
        switch (token) {
            .directive => |directive| {
                if (directive == .sect) {
                    _ = self.next();
                    const directive_location = self.peekLocation();
                    if (self.matchWithoutError(.directive)) |next_directive| {
                        switch (next_directive.directive) {
                            .text => current_section = .text_section,
                            .data => current_section = .data_section,
                            .bss => current_section = .bss_section,
                            else => {
                                self.file.errors.append(directive_location, "expected one of .TEXT, .DATA and .BSS after .SECT", .{});
                                parsed_node = null;
                            },
                        }
                    } else {
                        self.file.errors.append(directive_location, "expected one of .TEXT, .DATA, .BSS after .SECT", .{});
                        parsed_node = null;
                    }
                    _ = self.next();
                } else if (directive.isMemoryDataType()) {
                    parsed_node = try self.parseDataField();
                } else {
                    self.file.errors.append(self.peekLocation(), "didn't expect directive '{}' here", .{directive});
                    _ = self.next();
                }
            },
            .instruction_mnemonic => parsed_node = try self.parseInstruction(),
            .identifier => |identifier| {
                if (self.peekN(1)) |maybe_equal| {
                    if (maybe_equal == .equals_sign) {
                        parsed_node = try self.parseConstant();
                    } else {
                        self.file.errors.append(self.peekLocation(), "stray identifier '{s}'", .{identifier});
                        _ = self.next();
                    }
                } else {
                    self.file.errors.append(self.peekLocation(), "stray identifier '{s}'", .{identifier});
                    _ = self.next();
                }
            },
            .newline => {
                _ = self.next();
            },
            .label => {
                parsed_node = self.parseLabel();
            },
            .comment => {
                _ = self.next();
                _ = self.match(.newline);
            },
            else => {
                self.file.errors.append(self.peekLocation(), "stray token '{}'", .{token});
                _ = self.next();
            },
        }

        if (parsed_node) |node| {
            const end_location = self.peekLocation();
            const node_location = SourceLocation.combine(start_location, end_location);
            switch (current_section) {
                .no_section => try file.ast.outside_any_section.append(file.allocator, .{ .node = node, .location = node_location }),
                .text_section => try file.ast.text_section.append(file.allocator, .{ .node = node, .location = node_location }),
                .data_section => try file.ast.data_section.append(file.allocator, .{ .node = node, .location = node_location }),
                .bss_section => try file.ast.bss_section.append(file.allocator, .{ .node = node, .location = node_location }),
            }
        }
    }
}

fn parseConstant(self: *Self) !?AstNode {
    const constant_name_token = self.next() orelse unreachable;
    if (self.match(.equals_sign)) |_| {
        const initializer = (try self.parseExpression()) orelse return null;
        return .{ .constant = .{
            .name = constant_name_token.identifier,
            .initializer = initializer,
        } };
    } else {
        return null;
    }
}

fn parseLabel(self: *Self) AstNode {
    const label_name_token = self.next() orelse unreachable;
    std.debug.assert(label_name_token.is(.label));
    return .{
        .label = .{
            .name = label_name_token.label,
        },
    };
}

fn parseDataField(self: *Self) !?AstNode {
    const data_type_token = self.next() orelse unreachable;
    std.debug.assert(data_type_token.is(.directive));

    const data_type_directive = data_type_token.directive;
    if (data_type_directive.isMemoryDataType()) {
        const initializer = try self.parseExpression();
        if (initializer) |notnull_initializer| {
            return .{
                .data_field = .{
                    .data_type = data_type_directive,
                    .initializer = notnull_initializer,
                },
            };
        } else {
            return null;
        }
    } else {
        const location = self.peekLocationN(-1);
        self.file.errors.append(location, "the directive {} is not a memory data type", .{data_type_directive});
        return null;
    }
}

fn parseInstruction(self: *Self) !?AstNode {
    const instruction_location = self.peekLocation();
    const instruction_token = self.next() orelse unreachable;
    const description = intel8088.isa.get(instruction_token.instruction_mnemonic);

    var operands = try self.file.allocator.alloc(*ExpressionAstNode, description.allowed_operands.len);
    for (0..description.allowed_operands.len) |i| {
        const location = self.peekLocation();
        operands[i] = try self.parseExpression() orelse return null;
        if (i != description.allowed_operands.len - 1) {
            _ = self.match(.comma) orelse {
                self.file.errors.append(location, "expected a comma, since instruction '{}' takes {} arguments\n", .{
                    instruction_token.instruction_mnemonic,
                    description.allowed_operands.len,
                });
                _ = self.prev();
            };
        }
    }
    return .{
        .instruction = .{
            .mnemonic = instruction_token.instruction_mnemonic,
            .operands = operands,
            .location = instruction_location,
        },
    };
}

fn parseExpression(self: *Self) !?*ExpressionAstNode {
    return self.parseBinaryExpression(0);
}

fn parseBinaryExpression(self: *Self, last_precedence: i32) !?*ExpressionAstNode {
    const expression: *ExpressionAstNode = try self.parseUnaryExpression() orelse return null;
    while (self.peek()) |token| {
        if (!token.isOperator()) {
            break;
        }

        const operator = token.toBinaryOperator();

        const precedence = precedenceOf(operator);
        if (precedence < last_precedence) {
            break;
        }

        _ = self.next();
        const location = self.peekLocation();
        const maybe_right = try self.parseBinaryExpression(precedence + 1);
        if (maybe_right) |right| {
            const left = try self.file.allocator.create(ExpressionAstNode);
            left.* = expression.*;
            expression.* = .{ .binary = .{
                .left = left,
                .right = right,
                .operator = operator,
            } };
        } else {
            self.file.errors.append(location, "expected an expression to the right of {}\n", .{operator});
            return null;
        }
    }

    return expression;
}

fn parseUnaryExpression(self: *Self) !?*ExpressionAstNode {
    const current = self.peek() orelse unreachable;

    switch (current) {
        .plus, .minus => {
            const operator: intel8088.asm_syntax.UnaryOperator = switch (current) {
                .plus => .plus,
                .minus => .minus,
                else => unreachable,
            };
            _ = self.next();
            const operand = try self.parseUnaryExpression() orelse return null;
            const result = try self.file.allocator.create(ExpressionAstNode);
            result.* = .{ .unary = .{
                .operand = operand,
                .operator = operator,
            } };
            return result;
        },
        else => {},
    }

    return self.parseAtom();
}

fn parseAtom(self: *Self) !?*ExpressionAstNode {
    const current = self.next() orelse unreachable;
    const result = try self.file.allocator.create(ExpressionAstNode);
    result.* = switch (current) {
        .identifier => |identifier| .{ .value = .{ .identifier = identifier } },
        .register => |register| .{ .value = .{ .register = register } },
        .number => |number| .{ .value = .{ .word = number } },
        .string => |string| .{ .value = .{ .string = string } },
        .left_paren => blk: {
            const expression = (try self.parseAtom()) orelse return null;
            const right_paren = self.next();
            if (right_paren != null and right_paren.? == .right_paren) {
                break :blk .{
                    .address = .{
                        .basic = expression,
                    },
                };
            }
            const location = self.peekLocation();
            self.file.errors.append(location, "expected a ')'", .{});
            return null;
        },
        else => {
            const location = self.peekLocationN(-1);
            self.file.errors.append(location, "'{}' can't be used in this context", .{current});
            return null;
        },
    };
    return result;
}

fn precedenceOf(operator: intel8088.asm_syntax.BinaryOperator) i32 {
    return switch (operator) {
        .plus => 1,
        .minus => 1,
        .multiply => 2,
        .divide => 2,
    };
}

fn match(self: *Self, expected: Token.Tag) ?Token {
    if (self.next()) |token| {
        if (token.is(expected)) {
            return token;
        } else {
            const previous_token_location = self.peekLocationN(-1);
            self.file.errors.append(previous_token_location, "expected '{}', but found '{}'\n", .{ expected, token });
            return null;
        }
    }
    self.file.errors.append(self.peekLocationN(-1), "expected '{}', but found EOF\n", .{expected});
    return null;
}

fn matchWithoutError(self: *Self, expected: Token.Tag) ?Token {
    if (self.next()) |token| {
        if (token.is(expected)) {
            return token;
        } else {
            return null;
        }
    }
    return null;
}

fn next(self: *Self) ?Token {
    const result = self.peek();
    self.position = @min(self.position + 1, self.tokens.len);
    return result;
}

fn prev(self: *Self) ?Token {
    const result = self.peek();
    self.position = @max(self.position - 1, 0);
    return result;
}

fn peek(self: *const Self) ?Token {
    return self.peekN(0);
}

fn peekN(self: *const Self, n: usize) ?Token {
    return common.getOrNull(Token, self.tokens, self.position + n);
}

fn peekLocation(self: *const Self) SourceLocation {
    return common.getOrNull(SourceLocation, self.locations, self.position) orelse std.debug.panic("out of bounds", .{});
}

fn peekLocationN(self: *const Self, n: isize) SourceLocation {
    // This is really annoying.
    const position_as_isize = @as(isize, @intCast(self.position));
    const peek_position = @as(usize, @intCast(position_as_isize + n));
    return common.getOrNull(SourceLocation, self.locations, peek_position) orelse std.debug.panic("out of bounds", .{});
}
