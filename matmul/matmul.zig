const std = @import("std");

fn matInit(alloc: std.mem.Allocator, x: usize, y: usize) [][]f64 {
    const mat: [][]f64 = alloc.alloc([]f64, x) catch unreachable;
    for (mat) |*row| {
        row.* = alloc.alloc(f64, y) catch unreachable;
        @memset(row.*, 0.0);
    }
    return mat;
}

fn matGen(alloc: std.mem.Allocator, n: usize, seed: f64) [][]f64 {
    const mat: [][]f64 = matInit(alloc, n, n);
    const n_f = @as(f64, @floatFromInt(n));
    const tmp = seed / n_f / n_f;

    for (mat, 0..) |*row, i| {
        for (row.*, 0..) |*x, j| {
            const i_f = @as(f64, @floatFromInt(i));
            const j_f = @as(f64, @floatFromInt(j));
            x.* = tmp * (i_f - j_f) * (i_f + j_f);
        }
    }

    return mat;
}

fn matMul(alloc: std.mem.Allocator, a: [][]f64, b: [][]f64) [][]f64 {
    const m = a.len;
    const n = a[0].len;
    const p = b[0].len;

    var b2: [][]f64 = matInit(alloc, n, p);
    var i: usize = 0;
    while (i < p) : (i += 1) {
        var j: usize = 0;
        while (j < n) : (j += 1) {
            b2[i][j] = b[j][i];
        }
    }

    var c: [][]f64 = matInit(alloc, m, p);
    i = 0;
    while (i < m) : (i += 1) {
        var j: usize = 0;
        while (j < p) : (j += 1) {
            var s: f64 = 0.0;
            var k: usize = 0;
            while (k < n) : (k += 1) {
                s += a[i][k] * b2[j][k];
            }
            c[i][j] = s;
        }
    }

    return c;
}

fn notify(io: std.Io, msg: []const u8) void {
    const addr = std.Io.net.IpAddress.parse("127.0.0.1", 9001) catch unreachable;
    var stream = addr.connect(io, .{ .mode = .stream }) catch return;
    defer stream.close(io);

    var writer = stream.writer(io, &.{});
    writer.interface.writeAll(msg) catch return;
    writer.interface.flush() catch return;
}

fn calc(alloc: std.mem.Allocator, n: usize) f64 {
    const size: usize = @divTrunc(n, 2) * @as(usize, 2);
    const a = matGen(alloc, size, 1.0);
    const b = matGen(alloc, size, 2.0);
    const x = matMul(alloc, a, b);
    const i = @as(usize, @intCast(@divTrunc(size, 2)));
    return x[i][i];
}

pub fn main(init: std.process.Init.Minimal) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const io = std.Io.Threaded.global_single_threaded.io();

    var arg_iter = std.process.Args.Iterator.init(init.args);
    _ = arg_iter.skip(); // Skip binary name

    const arg = arg_iter.next() orelse "";
    const n = std.fmt.parseInt(usize, arg, 10) catch 100;

    const left = calc(alloc, 101);
    const right = -18.67;
    if (@abs(left - right) > 0.1) {
        std.debug.panic("{d} != {d}\n", .{ left, right });
    }

    const pid = std.posix.system.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(io, pid_str);
    const result = calc(alloc, n);
    notify(io, "stop");

    std.debug.print("{d}\n", .{result});
}
