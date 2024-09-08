const std = @import("std");
const vaxis = @import("vaxis");
const TextInput = vaxis.widgets.TextInput;
const border = vaxis.widgets.border;

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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
    
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var text_input = TextInput.init(arena.allocator(), &vx.unicode);
    defer text_input.deinit();
    while (true) {
        const event = loop.nextEvent();
        switch (event) {
            .key_press => |key| {
                if (key.matches(vaxis.Key.enter, .{})) {
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
        const code_window = root_window.child(.{
            .x_off = left_side_width + weird_middle_pad,
            .y_off = 0,
            .width = .expand,
            .height = .{ .limit = register_window_height },
            .border = .{
                .where = .{ .other = .{ .left = true, .bottom = true } },
                .style = border_style,
            },
        });
        
        const command_window = root_window.child(.{
            .x_off = 0,
            .y_off = register_window_height,
            .width =  .{ .limit = left_side_width - 2 },
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

        _ = try register_window.printSegment(rw, .{});
        _ = try code_window.printSegment(cw, .{});
        text_input.draw(command_prompt);

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