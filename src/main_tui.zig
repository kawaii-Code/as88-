const std = @import("std");
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

const command_window_height = 6;

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
    var current_line: usize = emulator.currentLineInSourceFile();
    
    var text_input = TextInput.init(arena.allocator(), &vx.unicode);
    defer text_input.deinit();
    while (true) {
        const event = loop.nextEvent();
        switch (event) {
            .key_press => |key| {
                if (key.matches(vaxis.Key.enter, .{})) {
                    const program_done = (try emulator.step()) == null;
                    if (program_done) {
                        break;
                    } else {
                        current_line = emulator.currentLineInSourceFile();
                    }
                    previous_commands.add(try text_input.toOwnedSlice());
                    text_input.reset();
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

        const test_register_window_contents =
            \\CS: 00  DS=SS=ES: 001*
            \\AH:00 AL:05 AX:     5
            \\BH:00 BL:00 BX:     0
            \\CH:00 CL:00 CX:     0 
            \\DH:00 DL:00 DX:     0
            \\SP: 7ff8 SF O D S Z C 
            \\BP: 7ff8 CC - > p - - 
            \\SI: 0000  IP:0007:PC 
            \\DI: 0000  x+4
        ;
        
        const test_code_window_contents =
            \\.SECT .TEXT
            \\.SECT .TEXT
            \\    MOV AX, 3
            \\    ADD AX, (x)
            \\>   MOV (res), AX
        ;

        root_window.clear();
        const left_side_width = 25;
        const register_window_height = 10;
        const register_window = root_window.child(.{
           .x_off = 0,
           .y_off = 0,
           .width =  .{ .limit = left_side_width },
           .height = .{ .limit = register_window_height },
           .border = .{
               .where = .{ .other = .{ .right = true, .bottom = true } },
               .style = border_style,
           },
        });
        
        const weird_middle_pad = 6;
        const code_window_height = register_window_height;
        const code_window = root_window.child(.{
            .x_off = left_side_width + weird_middle_pad,
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
            const segment = plainTextSegment(source_code_lines[i]);
            _ = try code_window.printSegment(segment, .{
                .row_offset = j,
                .col_offset = 1,
            });
        }
        root_window.writeCell(code_window.x_off - 1, current_line - top_line, .{ .char = .{ .grapheme = "=" }, .style = border_style });
        root_window.writeCell(code_window.x_off, current_line - top_line, .{ .char = .{ .grapheme = ">" }, .style = border_style });
        
        const command_window_width = left_side_width - 2;
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
        
        const command_prompt = command_window.child(.{
            .x_off = 0,
            .y_off = command_window_height - 2,
            .width = .expand,
            .height = .expand,
        });

        for (previous_commands.buffer, 0..) |command, i| {
            const segment = plainTextSegment(command);
            _ = try command_window.printSegment(segment, .{
                .row_offset = i,
            });
        }

        const rw = plainTextSegment(test_register_window_contents);
        const cw = plainTextSegment(test_code_window_contents);
        _ = cw;

        _ = try register_window.printSegment(rw, .{});
        //_ = try code_window.printSegment(cw, .{});
        text_input.draw(command_prompt);
        
        // Write a window connector. Would be interesting
        // if vaxis did it.
        root_window.writeCell(
            command_window_width - 1,
            register_window_height - 1,
            .{ .char = .{ .grapheme = cool_grapheme }, .style = border_style }
        );

        var buffered = tty.bufferedWriter();
        try vx.render(buffered.writer().any());
        try buffered.flush();
    }
}

fn plainTextSegment(text: []const u8) vaxis.Segment {
    return .{
        .text = text,
        .style = .{},
        .link = .{},
    };
}