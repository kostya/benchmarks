const std = @import("std");
const unistd = @cImport(@cInclude("unistd.h"));

const OpType = enum {
    INC,
    MOVE,
    LOOP,
    PRINT,
};

const Printer = struct {
    stdout: std.fs.File.Writer,
    sum1: i32 = 0,
    sum2: i32 = 0,
    quiet: bool,

    fn init(args: anytype) Printer {
        return Printer{ .stdout = if (!args.quiet) std.io.getStdOut().writer() else undefined, .quiet = args.quiet };
    }

    fn print(self: *Printer, n: i32) void {
        if (self.quiet) {
            self.sum1 = @mod(self.sum1 + n, 255);
            self.sum2 = @mod(self.sum2 + self.sum1, 255);
        } else {
            self.stdout.writeByte(@intCast(n)) catch unreachable;
        }
    }

    fn getChecksum(self: *const Printer) i32 {
        return (self.sum2 << 8) | self.sum1;
    }
};

const Tape = struct {
    pos: usize = 0,
    tape: std.ArrayList(i32),

    fn init(alloc: std.mem.Allocator) Tape {
        var self = Tape{ .tape = std.ArrayList(i32).init(alloc) };
        self.tape.append(0) catch unreachable;
        return self;
    }

    fn get(self: *const Tape) i32 {
        return self.tape.items[self.pos];
    }

    fn inc(self: *Tape, x: i32) void {
        self.tape.items[self.pos] += x;
    }

    fn move(self: *Tape, x: i32) void {
        self.pos += @intCast(x);
        if (self.pos >= self.tape.items.len) {
            self.tape.appendNTimes(0, self.tape.items.len * 2) catch unreachable;
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
        var res: Ops = Ops.init(alloc);
        while (iter.next()) |value| {
            switch (value) {
                '+' => res.append(Op{
                    .op = OpType.INC,
                    .val = 1,
                }) catch unreachable,
                '-' => res.append(Op{
                    .op = OpType.INC,
                    .val = -1,
                }) catch unreachable,
                '>' => res.append(Op{
                    .op = OpType.MOVE,
                    .val = 1,
                }) catch unreachable,
                '<' => res.append(Op{
                    .op = OpType.MOVE,
                    .val = -1,
                }) catch unreachable,
                '.' => res.append(Op{
                    .op = OpType.PRINT,
                }) catch unreachable,
                '[' => res.append(Op{ .op = OpType.LOOP, .loop = parse(alloc, iter) }) catch unreachable,
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
                OpType.MOVE => tape.move(op.val),
                OpType.LOOP => while (tape.get() > 0) self._run(&op.loop, tape),
                OpType.PRINT => self.p.print(tape.get()),
            }
        }
    }
};

fn notify(msg: []const u8) void {
    const addr = std.net.Address.parseIp("127.0.0.1", 9001) catch unreachable;
    if (std.net.tcpConnectToAddress(addr)) |stream| {
        defer stream.close();
        _ = stream.write(msg) catch unreachable;
    } else |_| {}
}

fn readFile(alloc: std.mem.Allocator, filename: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(filename, std.fs.File.OpenFlags{});
    defer file.close();

    const size = try file.getEndPos();
    const text = try alloc.alloc(u8, size);
    _ = try file.readAll(text);
    return text;
}

fn verify(alloc: std.mem.Allocator) void {
    const text =
        \\++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        \\---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
    ;

    var p_left = Printer.init(.{ .quiet = true });
    Program.init(alloc, text, &p_left).run();
    const left = p_left.getChecksum();

    var p_right = Printer.init(.{ .quiet = true });
    for ("Hello World!\n") |c| {
        p_right.print(c);
    }
    const right = p_right.getChecksum();

    if (left != right) {
        std.debug.panic("{} != {}", .{ left, right });
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    verify(alloc);
    var p = Printer.init(.{ .quiet = std.os.getenv("QUIET") != null });

    var arg_iter = std.process.args();
    _ = arg_iter.skip(); // Skip binary name

    const name = arg_iter.next() orelse {
        std.debug.panic("Expected argument\n", .{});
    };

    const text = try readFile(alloc, name);
    const pid = unistd.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(pid_str);
    Program.init(alloc, text, &p).run();
    notify("stop");

    if (p.quiet) {
        std.debug.print("Output checksum: {}\n", .{p.getChecksum()});
    }
}
