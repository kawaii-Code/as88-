const std = @import("std");
const intel8088 = @import("intel8088_cpu_description.zig");
const as88 = @import("as88.zig");
const common = @import("common.zig");

const vaxis = @import("vaxis");
const TextInput = vaxis.widgets.TextInput;
const border = vaxis.widgets.border;
const print = std.debug.print;

const cool_grapheme = "┬";

const Event = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
};

const register_window_width = 25;
const register_window_height = 10;
const command_window_width = register_window_width - 2;
const command_window_height = 6;

const code_window_height = register_window_height;

pub const PreviousCommandsBuffer = struct {
    pub const Size = command_window_height - 2;

    buffer: [Size][]const u8,

    pub fn init() @This() {
        var result = @This() { .buffer = undefined };
        for (0 .. Size) |i| {
            // Creepy, but this code doesn't matter in the slightest
            result.buffer[i] = "";
        }
        return result;
    }

    pub fn add(self: *@This(), command: []const u8) void {
        for (1 .. Size) |i| {
            self.buffer[i - 1] = self.buffer[i];
        }
        self.buffer[Size - 1] = command;
    }
};

// I am being lazy here, so this is a little inefficient.
// But then again, who cares?
fn splitToLinesAlloc(source_text: []const u8, allocator: std.mem.Allocator) ![][]const u8 {
    const line_count = std.mem.count(u8, source_text, "\n");
    var result = try allocator.alloc([]const u8, line_count + 1);

    var line_it = std.mem.splitScalar(u8, source_text, '\n');
    var i: usize = 0;
    while (line_it.next()) |line| {
        const line_copy = try allocator.dupe(u8, line);
        result[i] = line_copy;
        i += 1;
    }
    
    return result;
}

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var render_arena = std.heap.ArenaAllocator.init(allocator);
    defer render_arena.deinit();

    const source = common.readEntireFile(filepath, arena.allocator());
    const source_code_lines = try splitToLinesAlloc(source, arena.allocator());
    
    const assembled_code = try as88.assemble(.{
        .filepath = filepath,
        .contents = source,
    }, &arena);

    var emulator = try as88.Emulator.init(arena.allocator(), assembled_code);
    defer emulator.deinit();

    var tty = try vaxis.Tty.init();
    defer tty.deinit();

    var vx = try vaxis.init(allocator, .{});
    defer vx.deinit(allocator, tty.anyWriter());

    var loop: vaxis.Loop(Event) = .{
      .tty = &tty,
      .vaxis = &vx,
    };
    try loop.init();

    try loop.start();
    defer loop.stop();

    try vx.enterAltScreen(tty.anyWriter());
    try vx.queryTerminal(tty.anyWriter(), 1 * std.time.ns_per_s);

    const border_style = vaxis.Style {
        .fg = .{ .index = 0 },
    };

    var previous_commands = PreviousCommandsBuffer.init();
    var current_line: usize = emulator.currentLineInSourceFile() orelse unreachable;

    var execution_history = std.ArrayList(std.ArrayList(as88.Emulator.Diff)).init(allocator);
    defer execution_history.deinit();

    var tracked_vars = std.ArrayList([]const u8).init(allocator);
    defer tracked_vars.deinit();

    try vx.resize(allocator, tty.anyWriter(), .{ .rows = 20, .cols = 20, .x_pixel = 0, .y_pixel = 0 });

    var text_input = TextInput.init(arena.allocator(), &vx.unicode);
    defer text_input.deinit();
    while (true) {
        const event = loop.nextEvent();
        switch (event) {
            .key_press => |key| {
                if (key.matches(vaxis.Key.enter, .{ .shift = true })) {
                    if (execution_history.popOrNull()) |previous_diffs| {
                        emulator.stepBack(&previous_diffs);
                        text_input.reset();
                        current_line = emulator.currentLineInSourceFile() orelse current_line + 1;
                    }
                } else if (key.matches(vaxis.Key.enter, .{})) {
                    const user_input = try text_input.toOwnedSlice();
                    if (std.mem.eql(u8, user_input, "q")) {
                        break;
                    } else if (std.mem.startsWith(u8, user_input, "/")) {
                        try tracked_vars.append(user_input[1..]);
                    } else {
                        if (try emulator.step()) |diffs| {
                            try execution_history.append(diffs);
                            current_line = emulator.currentLineInSourceFile() orelse current_line + 1;
                        } else {
                            // Program is done, exit main loop
                            break;
                        }
                    }

                    if (user_input.len > 0) {
                        previous_commands.add(user_input);
                        text_input.reset();
                    }
                } else if (key.matches('c', .{ .ctrl = true })) {
                    break;
                } else if (key.matches('l', .{ .ctrl = true })) {
                    vx.queueRefresh();
                } else {
                    try text_input.update(.{ .key_press = key });
                }
            },
            .winsize => |ws| try vx.resize(allocator, tty.anyWriter(), ws),
        }

        const root_window = vx.window();

        root_window.clear();
        const register_window = root_window.child(.{
           .x_off = 0,
           .y_off = 0,
           .width =  .{ .limit = register_window_width },
           .height = .{ .limit = register_window_height },
           .border = .{
               .where = .{ .other = .{ .right = true, .bottom = true } },
               .style = border_style,
           },
        });
        
        {
            const weird_middle_pad = 6;
            const code_window = root_window.child(.{
                .x_off = register_window_width + weird_middle_pad,
                .y_off = 0,
                .width = .expand,
                .height = .{ .limit = code_window_height },
                .border = .{
                    .where = .{ .other = .{ .left = true, .bottom = true } },
                    .style = border_style,
                },
            });
            const top_line = std.math.sub(usize, current_line + 2, code_window_height) catch 0;
            const bottom_line = @min(top_line + code_window_height, source_code_lines.len - 1);
            for (top_line .. bottom_line, 0 ..) |i, j| {
                const line = source_code_lines[i];

                var column: usize = 1;
                var token_it = std.mem.tokenizeAny(u8, line, " \t");
                while (token_it.next()) |token| {
                    if (std.mem.startsWith(u8, token, "!")) {
                        const comment_style = border_style;
                        const segment = vaxis.Segment { .text = line[column - 1 .. line.len], .style = comment_style, .link = .{} };
                        _ = try code_window.printSegment(segment, .{
                            .row_offset = j,
                            .col_offset = column,
                        });
                        break;
                    }
                
                    const style = highlightFor(token);

                    const segment = vaxis.Segment { .text = token, .style = style, .link = .{} };
                    const print_result = try code_window.printSegment(segment, .{
                        .row_offset = j,
                        .col_offset = column,
                    });
                    if (print_result.overflow) {
                        break;
                    }
                    column = print_result.col;
                    column += 1;
                }
            }
            root_window.writeCell(code_window.x_off - 1, current_line - top_line - 1, .{ .char = .{ .grapheme = "=" }, .style = border_style });
            root_window.writeCell(code_window.x_off, current_line - top_line - 1, .{ .char = .{ .grapheme = ">" }, .style = border_style });
        }

        const command_window = root_window.child(.{
            .x_off = 0,
            .y_off = register_window_height,
            .width =  .{ .limit = command_window_width },
            .height = .{ .limit = command_window_height },
            .border = .{
                .where = .{ .other = .{ .right = true, .bottom = true } },
                .style = border_style,
            },
        });
        for (previous_commands.buffer, 0..) |command, i| {
            const segment = plainTextSegment(command);
            _ = try command_window.printSegment(segment, .{
                .row_offset = i,
            });
        }
        
        const command_prompt = command_window.child(.{
            .x_off = 0,
            .y_off = command_window_height - 2,
            .width = .expand,
            .height = .expand,
        });
        text_input.draw(command_prompt);
        
        // Write a window connector. Would be interesting
        // if vaxis did it.
        root_window.writeCell(
            command_window_width - 1,
            register_window_height - 1,
            .{ .char = .{ .grapheme = cool_grapheme }, .style = border_style }
        );

        const RegisterWindow = struct {
            window: vaxis.Window,
            line: usize,
            allocator: std.mem.Allocator,

            pub fn lineStream(self: @This()) !std.io.FixedBufferStream([]u8) {
                const buf = try self.allocator.alloc(u8, register_window_width - 3);
                var stream = std.io.fixedBufferStream(buf);
                _ = stream.write(" ") catch unreachable;
                return stream;
            }

            pub fn drawLine(self: *@This(), text: []u8) void {
                const segment = plainTextSegment(text);
                _ = try self.window.printSegment(segment, .{
                    .row_offset = self.line,
                });
                self.line += 1;
            }
        };

        _ = render_arena.reset(.retain_capacity);
        var registers = RegisterWindow {
            .window = register_window,
            .line = 0,
            .allocator = render_arena.allocator(),
        };
        {
            var stream = try registers.lineStream();
            const cs = @as(u16, @bitCast(emulator.registers.get(.cs)));
            const ds = @as(u16, @bitCast(emulator.registers.get(.ds)));
            const ss = @as(u16, @bitCast(emulator.registers.get(.ss)));
            const es = @as(u16, @bitCast(emulator.registers.get(.es)));
            std.debug.assert(ds == ss and ss == es);
            try std.fmt.format(stream.writer(), "CS: {x:0>2}  DS=SS=ES: {x:0>3}", .{cs, ds});
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const ah = @as(u16, @bitCast(emulator.registers.get(.ah)));
            const al = @as(u16, @bitCast(emulator.registers.get(.al)));
            const ax = @as(i16, @bitCast(emulator.registers.get(.ax)));
            try std.fmt.format(stream.writer(), "AH:{x:0>2} AL:{x:0>2} AX:{d: >6}", .{ah, al, ax});
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const bh = @as(u16, @bitCast(emulator.registers.get(.bh)));
            const bl = @as(u16, @bitCast(emulator.registers.get(.bl)));
            const bx = @as(i16, @bitCast(emulator.registers.get(.bx)));
            try std.fmt.format(stream.writer(), "BH:{x:0>2} BL:{x:0>2} BX:{d: >6}", .{bh, bl, bx});
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const ch = @as(u16, @bitCast(emulator.registers.get(.ch)));
            const cl = @as(u16, @bitCast(emulator.registers.get(.cl)));
            const cx = @as(i16, @bitCast(emulator.registers.get(.cx)));
            try std.fmt.format(stream.writer(), "CH:{x:0>2} CL:{x:0>2} CX:{d: >6}", .{ch, cl, cx});
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const dh = @as(u16, @bitCast(emulator.registers.get(.dh)));
            const dl = @as(u16, @bitCast(emulator.registers.get(.dl)));
            const dx = @as(i16, @bitCast(emulator.registers.get(.dx)));
            try std.fmt.format(stream.writer(), "DH:{x:0>2} DL:{x:0>2} DX:{d: >6}", .{dh, dl, dx});
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const sp = @as(u16, @bitCast(emulator.registers.get(.sp)));
            try std.fmt.format(stream.writer(), "SP: {x:0>4} SF O D S Z C", .{sp});
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const bp = @as(u16, @bitCast(emulator.registers.get(.bp)));
            const of: u8 = if (emulator.flags.get(.of)) 'v' else '-';
            const df: u8 = if (emulator.flags.get(.df)) '<' else '>';
            const sf: u8 = if (emulator.flags.get(.sf)) 'n' else 'p';
            const zf: u8 = if (emulator.flags.get(.zf)) 'z' else '-';
            const cf: u8 = if (emulator.flags.get(.cf)) '?' else '-';
            try std.fmt.format(stream.writer(), "BP: {x:0>4} CC {c} {c} {c} {c} {c}", .{
                bp, of, df, sf, zf, cf
            });
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const si = @as(u16, @bitCast(emulator.registers.get(.si)));
            const ip = @as(u16, @bitCast(emulator.registers.get(.ip)));
            try std.fmt.format(stream.writer(), "SI: {x:0>4}  IP:{x:0>4}:PC ", .{si, ip});
            registers.drawLine(stream.buffer);
        }
        {
            var stream = try registers.lineStream();
            const di = @as(u16, @bitCast(emulator.registers.get(.di)));
            try std.fmt.format(stream.writer(), "DI: {x:0>4} .TEXT+0     ", .{di});
            registers.drawLine(stream.buffer);
        }

        for (tracked_vars.items, 0..) |tracked, i| {
            const buf = try render_arena.allocator().alloc(u8, register_window_width - 2);
            @memset(buf, 0);
            var stream = std.io.fixedBufferStream(buf);
        
            if (assembled_code.labels.get(tracked)) |label| {
                const value_of_tracked = @as(u16, @bitCast(emulator.load(.{ .memory = label.memory_field })));
                try std.fmt.format(stream.writer(), "{s}: {x:0>4}", .{tracked, value_of_tracked});
                const track_segment = plainTextSegment(buf);
                _ = try root_window.printSegment(track_segment, .{
                    .row_offset = command_window.y_off + command_window_height + 1 + i,
                });
            }
        }

        var buffered = tty.bufferedWriter();
        try vx.render(buffered.writer().any());
        try buffered.flush();
    }
}

fn highlightFor(token: []const u8) vaxis.Style {
    if (std.mem.startsWith(u8, token, ".")) {
        return .{ .fg = .{ .index = 3 } };
    } else if (intel8088.Register.Names.find(token)) |_| {
        return .{ .fg = .{ .index = 4 } };
    } else if (intel8088.InstructionMnemonic.Names.find(token)) |_| {
        return .{ .fg = .{ .index = 5 } };
    } else if (std.ascii.isDigit(token[0]) or
               (token.len > 1 and (token[0] == '-') and std.ascii.isDigit(token[1]))
    ) {
        return .{ .fg = .{ .index = 6 } };
    } else {
        return .{};
    } 
}

fn plainTextSegment(text: []const u8) vaxis.Segment {
    return .{
        .text = text,
        .style = .{},
        .link = .{},
    };
}