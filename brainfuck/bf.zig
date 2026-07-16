const std = @import("std");

const OpType = enum {
    INC,
    PREV,
    NEXT,
    LOOP,
    PRINT,
};

const Printer = struct {
    stdout: std.Io.File.Writer,
    sum1: i32 = 0,
    sum2: i32 = 0,
    quiet: bool,

    fn init(io: std.Io, args: anytype) Printer {
        return Printer{ .stdout = if (!args.quiet) std.Io.File.stdout().writer(io, &.{}) else undefined, .quiet = args.quiet };
    }

    fn print(self: *Printer, n: i32) void {
        if (self.quiet) {
            self.sum1 = @mod(self.sum1 + n, 255);
            self.sum2 = @mod(self.sum2 + self.sum1, 255);
        } else {
            self.stdout.interface.writeByte(@intCast(n)) catch unreachable;
        }
    }

    fn getChecksum(self: *const Printer) i32 {
        return (self.sum2 << 8) | self.sum1;
    }
};

const Tape = struct {
    pos: usize = 0,
    tape: std.ArrayList(i32),
    a: std.mem.Allocator,

    fn init(alloc: std.mem.Allocator) Tape {
        var self = Tape{ .tape = .empty, .a = alloc };
        self.tape.append(alloc, 0) catch unreachable;
        return self;
    }

    fn get(self: *const Tape) i32 {
        return self.tape.items[self.pos];
    }

    fn inc(self: *Tape, x: i32) void {
        self.tape.items[self.pos] += x;
    }

    fn prev(self: *Tape) void {
        self.pos -= 1;
    }

    fn next(self: *Tape) void {
        self.pos += 1;
        if (self.pos >= self.tape.items.len) {
            self.tape.appendNTimes(self.a, 0, self.tape.items.len * 2) catch unreachable;
        }
    }
};

const Ops = std.ArrayList(Op);

const Op = struct {
    op: OpType = undefined,
    val: i32 = 0,
    loop: Ops = undefined,
};

const StrIterator = struct {
    text: []const u8,
    pos: usize = 0,

    fn next(self: *StrIterator) ?u8 {
        if (self.pos < self.text.len) {
            const res = self.text[self.pos];
            self.pos += 1;
            return res;
        }
        return null;
    }
};

const Program = struct {
    ops: Ops,
    p: *Printer,
    a: std.mem.Allocator,

    fn init(alloc: std.mem.Allocator, code: []const u8, printer: *Printer) Program {
        var iter = StrIterator{ .text = code };
        return Program{
            .ops = parse(alloc, &iter),
            .p = printer,
            .a = alloc,
        };
    }

    fn run(self: *const Program) void {
        var tape = Tape.init(self.a);
        self._run(&self.ops, &tape);
    }

    fn parse(alloc: std.mem.Allocator, iter: *StrIterator) Ops {
        var res: Ops = .empty;
        while (iter.next()) |value| {
            switch (value) {
                '+' => res.append(alloc, Op{
                    .op = OpType.INC,
                    .val = 1,
                }) catch unreachable,
                '-' => res.append(alloc, Op{
                    .op = OpType.INC,
                    .val = -1,
                }) catch unreachable,
                '>' => res.append(alloc, Op{
                    .op = OpType.NEXT,
                }) catch unreachable,
                '<' => res.append(alloc, Op{
                    .op = OpType.PREV,
                }) catch unreachable,
                '.' => res.append(alloc, Op{
                    .op = OpType.PRINT,
                }) catch unreachable,
                '[' => res.append(alloc, Op{ .op = OpType.LOOP, .loop = parse(alloc, iter) }) catch unreachable,
                ']' => return res,
                else => continue,
            }
        }
        return res;
    }

    fn _run(self: *const Program, program: *const Ops, tape: *Tape) void {
        for (program.items) |*op| {
            switch (op.op) {
                OpType.INC => tape.inc(op.val),
                OpType.PREV => tape.prev(),
                OpType.NEXT => tape.next(),
                OpType.LOOP => while (tape.get() > 0) self._run(&op.loop, tape),
                OpType.PRINT => self.p.print(tape.get()),
            }
        }
    }
};

fn notify(io: std.Io, msg: []const u8) void {
    const addr = std.Io.net.IpAddress.parse("127.0.0.1", 9001) catch unreachable;
    var stream = addr.connect(io, .{ .mode = .stream }) catch return;
    defer stream.close(io);

    var writer = stream.writer(io, &.{});
    writer.interface.writeAll(msg) catch return;
    writer.interface.flush() catch return;
}

fn readFile(alloc: std.mem.Allocator, io: std.Io, filename: []const u8) ![]const u8 {
    const file = try std.Io.Dir.cwd().openFile(io, filename, .{});
    defer file.close(io);

    const size = (try file.stat(io)).size;
    const text = try alloc.alloc(u8, size);
    _ = try file.readPositionalAll(io, text, 0);
    return text;
}

fn verify(alloc: std.mem.Allocator, io: std.Io) void {
    const text =
        \\++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        \\---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
    ;

    var p_left = Printer.init(io, .{ .quiet = true });
    Program.init(alloc, text, &p_left).run();
    const left = p_left.getChecksum();

    var p_right = Printer.init(io, .{ .quiet = true });
    for ("Hello World!\n") |c| {
        p_right.print(c);
    }
    const right = p_right.getChecksum();

    if (left != right) {
        std.debug.panic("{} != {}", .{ left, right });
    }
}

pub fn main(init: std.process.Init) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const io = init.io;

    verify(alloc, io);
    var p = Printer.init(io, .{ .quiet = init.environ_map.contains("QUIET") });

    var arg_iter = std.process.Args.Iterator.init(init.minimal.args);
    _ = arg_iter.skip(); // Skip binary name

    const name = arg_iter.next() orelse {
        std.debug.panic("Expected argument\n", .{});
    };

    const text = try readFile(alloc, io, name);
    const pid = std.posix.system.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(io, pid_str);
    Program.init(alloc, text, &p).run();
    notify(io, "stop");

    if (p.quiet) {
        std.debug.print("Output checksum: {}\n", .{p.getChecksum()});
    }
}
