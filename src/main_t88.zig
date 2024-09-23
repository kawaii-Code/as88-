const std = @import("std");
const intel8088 = @import("intel8088_cpu_description.zig");
const as88 = @import("as88.zig");
const common = @import("common.zig");

const vaxis = @import("vaxis");
const TextInput = vaxis.widgets.TextInput;
const border = vaxis.widgets.border;
const print = std.debug.print;

const Event = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
};

const border_connector_south_east_west = "┬";
const border_connector_north_east_west = "┴";

const default_style     = vaxis.Style{};
const border_style      = vaxis.Style{ .fg = .{ .index = 8 } };
const comment_style     = border_style;
const instruction_style = vaxis.Style{ .fg = .{ .index = 9 } };
const section_style     = vaxis.Style{ .fg = .{ .index = 10 } };
const immediate_style   = vaxis.Style{ .fg = .{ .index = 13 } };
const register_style    = vaxis.Style{ .fg = .{ .index = 12 } };

const register_window_width = 24;
const register_window_height = 10;
const stack_window_width = 5;
const stack_window_height = register_window_height;
const code_window_x = register_window_width + stack_window_width;
const code_window_height = register_window_height;
const command_window_y = register_window_height;
const command_window_width = register_window_width - 2;
const command_window_height = 6;
const output_window_x = command_window_width;
const output_window_y = command_window_y;
const output_window_height = command_window_height;
const tracked_variables_window_y = register_window_height + command_window_height;

const App = struct {
    frame_allocator: std.mem.Allocator,
    program: RunningProgram,
    tracked: ThingsTrackedByUser,
    command_buffer: PreviousCommandsBuffer,
    command_input: *vaxis.widgets.TextInput,
    output_buffer: FixedAppendOnlyQueue([]const u8, output_window_height - 1),
};

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

    const program_source = common.readEntireFile(filepath, arena.allocator());
    const assembled_program = try as88.assemble(.{
        .filepath = filepath,
        .contents = program_source,
    }, &arena);

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
    // Fix for windows cmd
    try vx.resize(allocator, tty.anyWriter(), .{ .rows = 20, .cols = 20, .x_pixel = 0, .y_pixel = 0 });

    var command_input = TextInput.init(arena.allocator(), &vx.unicode);
    var app = App{
        .frame_allocator = render_arena.allocator(),
        .command_input = &command_input,
        .tracked = ThingsTrackedByUser{
            .variables = std.ArrayList([]const u8).init(allocator),
        },
        .command_buffer = PreviousCommandsBuffer.init(),
        .output_buffer = FixedAppendOnlyQueue([]const u8, output_window_height - 1).initFill(""),
        .program = try RunningProgram.init(assembled_program, program_source, arena.allocator()),
    };
    defer app.program.deinit();
    defer app.tracked.variables.deinit();
    defer app.command_input.deinit();
    var program = &app.program;
    
    while (true) {
        const event = loop.nextEvent();
        switch (event) {
            .key_press => |key| {
                if (key.matches(vaxis.Key.up, .{})) {
                    _ = try program.stepBackward();
                }
                else if (key.matches(vaxis.Key.down, .{})) {
                    if (!(try program.stepForward())) {
                        // Program is done, exit main loop
                        break;                    }
                } else if (key.matches(vaxis.Key.enter, .{})) {
                    const user_input = try app.command_input.toOwnedSlice();
                    if (std.mem.eql(u8, user_input, "q")) {
                        break;
                    } else if (std.mem.startsWith(u8, user_input, "/")) {
                        try app.tracked.variables.append(user_input[1..]);
                    } else if (user_input.len == 0) {
                        if (!(try program.stepForward())) {
                            // Program is done, exit main loop
                            break;
                        }
                    } else {
                        app.command_buffer.add(user_input);
                        app.command_input.reset();
                    }
                } else if (key.matches('c', .{ .ctrl = true })) {
                    break;
                } else if (key.matches('l', .{ .ctrl = true })) {
                    vx.queueRefresh();
                } else {
                    try app.command_input.update(.{ .key_press = key });
                }
            },
            .winsize => |ws| try vx.resize(allocator, tty.anyWriter(), ws),
        }

        _ = render_arena.reset(.retain_capacity);
        try draw(vx.window(), &app);

        var buffered = tty.bufferedWriter();
        try vx.render(buffered.writer().any());
        try buffered.flush();
    }
}



pub fn draw(
    root_window: vaxis.Window,
    app: *App,
) !void {
    const frame_allocator = app.frame_allocator;
    const program = &app.program;
    const previous_commands = app.command_buffer;
    const command_input = app.command_input;
    const tracked_variables = app.tracked.variables;

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

    var registers = LinePrinter {
        .window = register_window,
        .line = 0,
        .line_width = register_window_width,
        .allocator = frame_allocator,
    };
    {
        var stream = try registers.stream();
        const cs = @as(u16, @bitCast(program.emulator.registers.get(.cs)));
        const ds = @as(u16, @bitCast(program.emulator.registers.get(.ds)));
        const ss = @as(u16, @bitCast(program.emulator.registers.get(.ss)));
        const es = @as(u16, @bitCast(program.emulator.registers.get(.es)));
        std.debug.assert(ds == ss and ss == es);
        try std.fmt.format(stream.writer(), "CS: {x:0>2}  DS=SS=ES: {x:0>3}", .{cs, ds});
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const ah = @as(u16, @bitCast(program.emulator.registers.get(.ah)));
        const al = @as(u16, @bitCast(program.emulator.registers.get(.al)));
        const ax = @as(i16, @bitCast(program.emulator.registers.get(.ax)));
        try std.fmt.format(stream.writer(), "AH:{x:0>2} AL:{x:0>2} AX:{d: >6}", .{ah, al, ax});
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const bh = @as(u16, @bitCast(program.emulator.registers.get(.bh)));
        const bl = @as(u16, @bitCast(program.emulator.registers.get(.bl)));
        const bx = @as(i16, @bitCast(program.emulator.registers.get(.bx)));
        try std.fmt.format(stream.writer(), "BH:{x:0>2} BL:{x:0>2} BX:{d: >6}", .{bh, bl, bx});
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const ch = @as(u16, @bitCast(program.emulator.registers.get(.ch)));
        const cl = @as(u16, @bitCast(program.emulator.registers.get(.cl)));
        const cx = @as(i16, @bitCast(program.emulator.registers.get(.cx)));
        try std.fmt.format(stream.writer(), "CH:{x:0>2} CL:{x:0>2} CX:{d: >6}", .{ch, cl, cx});
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const dh = @as(u16, @bitCast(program.emulator.registers.get(.dh)));
        const dl = @as(u16, @bitCast(program.emulator.registers.get(.dl)));
        const dx = @as(i16, @bitCast(program.emulator.registers.get(.dx)));
        try std.fmt.format(stream.writer(), "DH:{x:0>2} DL:{x:0>2} DX:{d: >6}", .{dh, dl, dx});
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const sp = @as(u16, @bitCast(program.emulator.registers.get(.sp)));
        try std.fmt.format(stream.writer(), "SP: {x:0>4} SF O D S Z C", .{sp});
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const bp = @as(u16, @bitCast(program.emulator.registers.get(.bp)));
        const of: u8 = if (program.emulator.flags.get(.of)) 'v' else '-';
        const df: u8 = if (program.emulator.flags.get(.df)) '<' else '>';
        const sf: u8 = if (program.emulator.flags.get(.sf)) 'n' else 'p';
        const zf: u8 = if (program.emulator.flags.get(.zf)) 'z' else '-';
        const cf: u8 = if (program.emulator.flags.get(.cf)) '?' else '-';
        try std.fmt.format(stream.writer(), "BP: {x:0>4} CC {c} {c} {c} {c} {c}", .{
            bp, of, df, sf, zf, cf
        });
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const si = @as(u16, @bitCast(program.emulator.registers.get(.si)));
        const ip = @as(u16, @bitCast(program.emulator.registers.get(.ip)));
        try std.fmt.format(stream.writer(), "SI: {x:0>4}  IP:{x:0>4}:PC ", .{si, ip});
        registers.printLine(stream.buffer);
    }
    {
        var stream = try registers.stream();
        const di = @as(u16, @bitCast(program.emulator.registers.get(.di)));
        try std.fmt.format(stream.writer(), "DI: {x:0>4} .TEXT+0     ", .{di});
        registers.printLine(stream.buffer);
    }
    
    // Draw stack window
    {
        const stack_window = root_window.child(.{
            .x_off = register_window_width,
            .y_off = 0,
            .width =  .{ .limit = stack_window_width },
            .height = .{ .limit = stack_window_height },
            .border = .{ .style = border_style, .where = .bottom },
        });
        
        var line_printer = LinePrinter {
            .window = stack_window,
            .line = 0,
            .line_width = 5,
            .allocator = frame_allocator,
        };
        const sp = @as(u16, @bitCast(program.emulator.load(.{ .register = .sp })));
        var stack_top: u16 = sp + 2 * (stack_window_height - 2);
        if (stack_top > 32) {
            stack_top = 32;
        }
        for (0 .. stack_window_height - 1) |_| {
            stack_top -= 2;
            const value = @as(u16, @bitCast(program.emulator.load(.{ .memory = stack_top })));
            var stream = try line_printer.stream();
            try std.fmt.format(stream.writer(), "{x:0>4}", .{value});
            line_printer.printLine(stream.buffer);
            if (sp - 2 == stack_top) {
                // Draw arrow
                root_window.writeCell(stack_window.x_off - 1, stack_window.y_off + line_printer.line - 1, .{ .char = .{ .grapheme = "=" }, .style = border_style });
                root_window.writeCell(stack_window.x_off,     stack_window.y_off + line_printer.line - 1, .{ .char = .{ .grapheme = ">" }, .style = border_style });
            }
        }
    }
    
    // Draw code window
    {
        const code_window = root_window.child(.{
            .x_off = code_window_x,
            .y_off = 0,
            .width = .expand,
            .height = .{ .limit = code_window_height },
            .border = .{
                .where = .{ .other = .{ .left = true, .bottom = true } },
                .style = border_style,
            },
        });
        const top_line = std.math.sub(usize, program.line_in_source + 2, code_window_height) catch 0;
        const bottom_line = @min(top_line + code_window_height, program.source_lines.len - 1);
        for (top_line .. bottom_line, 0 ..) |i, j| {
            const line = program.source_lines[i];
            if (i == program.line_in_source) {
                root_window.writeCell(code_window.x_off - 1, code_window.y_off + j - 1, .{ .char = .{ .grapheme = "=" }, .style = border_style });
                root_window.writeCell(code_window.x_off, code_window.y_off + j - 1, .{ .char = .{ .grapheme = ">" }, .style = border_style });
            }
            
            var column: usize = 1;
            var token_it = std.mem.tokenizeAny(u8, line, " \t");
            while (token_it.next()) |token| {
                if (std.mem.startsWith(u8, token, "!")) {
                    const segment = vaxis.Segment { .text = line[column - 1 .. line.len], .style = comment_style, .link = .{} };
                    _ = try code_window.printSegment(segment, .{
                        .row_offset = j,
                        .col_offset = column,
                    });
                    break;
                }
                // TODO: Handle wrapping
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
    }
    
    const command_window = root_window.child(.{
        .x_off = 0,
        .y_off = command_window_y,
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
            .wrap = .none,
        });
    }
    const command_prompt = command_window.child(.{
        .x_off = 0,
        .y_off = command_window_height - 2,
        .width = .expand,
        .height = .expand,
    });
    command_input.draw(command_prompt);
    
    
    // Draw tracked variables
    {
        const tracked_variables_window = root_window.child(.{
            .x_off = 0,
            .y_off = tracked_variables_window_y,
            .width = .expand,
            .height = .expand,
            .border = .{ },
        });
        var line_printer = LinePrinter {
            .window = tracked_variables_window,
            .line = 0,
            .line_width = 140, // TODO: arbitrary. A long variable name could cause out of memory.
            .allocator = frame_allocator,
        };
        for (tracked_variables.items) |tracked| {
            if (program.assembly.labels.get(tracked)) |label| {
                var stream = try line_printer.stream();

                const bytes1 = @as(u16, @bitCast(program.emulator.load(.{ .memory = label.memory_field + 0 })));
                const bytes2 = @as(u16, @bitCast(program.emulator.load(.{ .memory = label.memory_field + 2 })));
                const bytes3 = @as(u16, @bitCast(program.emulator.load(.{ .memory = label.memory_field + 4 })));
                const bytes4 = @as(u16, @bitCast(program.emulator.load(.{ .memory = label.memory_field + 6 })));
                const bytes = init: {
                    var result: [8]u8 = undefined;
                    result[0] = @truncate(bytes1);
                    result[1] = @truncate(bytes1 >> 8);
                    result[2] = @truncate(bytes2);
                    result[3] = @truncate(bytes2 >> 8);
                    result[4] = @truncate(bytes3);
                    result[5] = @truncate(bytes3 >> 8);
                    result[6] = @truncate(bytes4);
                    result[7] = @truncate(bytes4 >> 8);
                    break :init result;
                };
                
                try std.fmt.format(stream.writer(), "{s} {d:>6}  ={x:0>4}: ", .{tracked, @as(i16, @bitCast(bytes1)), label.memory_field});
                try std.fmt.format(stream.writer(), "{x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} ", .{
                    bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                });
                {
                    try std.fmt.format(stream.writer(), "'", .{});
                    for (bytes) |char| {
                        try std.fmt.format(stream.writer(), "{c}", .{if (std.ascii.isPrint(char)) char else '.'});
                    }
                    try std.fmt.format(stream.writer(), "' ", .{});
                }
                try std.fmt.format(stream.writer(), "{d:>4} {d:>4} {d:>4} {d:>4}", .{bytes1, bytes2, bytes3, bytes4});
                line_printer.printLine(stream.buffer);
            }
        }
    }
    
    // Draw output window
    {
        const output_window = root_window.child(.{
            .x_off = output_window_x,
            .y_off = output_window_y,
            .width = .expand,
            .height = .{ .limit = output_window_height },
            .border = .{ .where = .bottom, .style = border_style },
        });
        
        // Hack. See comment in Emulator.zig
        // Could make a diff that has stdout. It's better
        // because undo becomes possible.
        if (program.emulator.latest_stdout) |line| {
            app.output_buffer.append(line);
            program.emulator.latest_stdout = null;
        }
        
        for (app.output_buffer.buffer, 0..) |line, i| {
            const segment = plainTextSegment(line);
            _ = try output_window.printSegment(segment, .{
                .row_offset = i,
            });
        }
    }
    
    // Write window connectors for prettiness. Would be interesting
    // if vaxis did it.
    root_window.writeCell(
        command_window_width - 1,
        register_window_height - 1,
        .{ .char = .{ .grapheme = border_connector_south_east_west }, .style = border_style }
    );
    root_window.writeCell(
        register_window_width - 1,
        register_window_height - 1,
        .{ .char = .{ .grapheme = border_connector_north_east_west }, .style = border_style }
    );
    root_window.writeCell(
        register_window_width + stack_window_width,
        register_window_height - 1,
        .{ .char = .{ .grapheme = border_connector_north_east_west }, .style = border_style }
    );
    root_window.writeCell(
        command_window_width - 1,
        output_window_y + output_window_height - 1,
        .{ .char = .{ .grapheme = border_connector_north_east_west }, .style = border_style }
    );
}

const RunningProgram = struct {
    const ExecutionHistory = std.ArrayList(std.ArrayList(as88.Emulator.Diff));

    history: ExecutionHistory,
    emulator: as88.Emulator,
    assembly: as88.AssembledProgram,
    line_in_source: usize,
    source_lines: [][]const u8,

    pub fn init(assembled_program: as88.AssembledProgram, program_source: []const u8, allocator: std.mem.Allocator) !@This() {
        var result = @This(){
            .history = @This().ExecutionHistory.init(allocator),
            .assembly = assembled_program,
            .emulator = try as88.Emulator.init(allocator, assembled_program),
            .line_in_source = 0,
            .source_lines = try splitToLinesAlloc(program_source, allocator),
        };
        result.emulator.running_interactively = true;
        result.updateLine();
        return result;
    }

    pub fn deinit(self: *@This()) void {
        self.emulator.deinit();
        self.history.deinit();
    }

    pub fn stepForward(self: *@This()) !bool {
        if (try self.emulator.step()) |diffs| {
            try self.history.append(diffs);
            self.updateLine();
            return true;
        } else {
            return false;
        }
    }

    pub fn stepBackward(self: *@This()) !bool {
        if (self.history.popOrNull()) |previous_diffs| {
            self.emulator.stepBack(&previous_diffs);
            self.updateLine();
            return true;
        } else {
            return false;
        }
    }

    fn updateLine(self: *@This()) void {
        self.line_in_source = self.emulator.currentLineInSourceFile() orelse self.line_in_source + 1;
    }
};

const LinePrinter = struct {
    window: vaxis.Window,
    line: usize,
    line_width: usize,
    allocator: std.mem.Allocator,
    
    pub fn stream(self: @This()) !std.io.FixedBufferStream([]u8) {
        const buf = try self.allocator.alloc(u8, self.line_width);
        @memset(buf, ' ');
        var result = std.io.fixedBufferStream(buf);
        _ = result.write(" ") catch unreachable;
        return result;
    }
    
    pub fn printLine(self: *@This(), text: []u8) void {
        const segment = plainTextSegment(text);
        _ = try self.window.printSegment(segment, .{
            .row_offset = self.line,
            .wrap = .none,
        });
        self.line += 1;
    }
};

fn FixedAppendOnlyQueue(comptime T: type, comptime size: usize) type {
    return struct {
        buffer: [size][]const u8,
        
        pub fn initFill(value: T) @This() {
            var result = @This() { .buffer = undefined };
            for (0 .. size) |i| {
                // Creepy, but this code doesn't matter in the slightest
                result.buffer[i] = value;
            }
            return result;
        }
        
        pub fn append(self: *@This(), item: T) void {
            for (1 .. size) |i| {
                self.buffer[i - 1] = self.buffer[i];
            }
            self.buffer[size - 1] = item;
        }
    };
}

const PreviousCommandsBuffer = struct {
    pub const Size = command_window_height - 2;

    buffer: [Size][]const u8,

    pub fn init() @This() {
        var result = @This() { .buffer = undefined };
        for (0 .. Size) |i| {
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

const ThingsTrackedByUser = struct {
    variables: std.ArrayList([]const u8),
};

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

fn highlightFor(token: []const u8) vaxis.Style {
    if (std.mem.startsWith(u8, token, ".")) {
        return section_style;
    } else if (intel8088.Register.Names.find(token)) |_| {
        return register_style;
    } else if (intel8088.InstructionMnemonic.Names.find(token)) |_| {
        return instruction_style;
    } else if (std.ascii.isDigit(token[0]) or (token.len > 1 and (token[0] == '-') and std.ascii.isDigit(token[1]))) {
        return immediate_style;
    } else {
        return default_style;
    } 
}

fn plainTextSegment(text: []const u8) vaxis.Segment {
    return .{
        .text = text,
        .style = default_style,
        .link = .{},
    };
}