const std = @import("std");

fn matInit(alloc: *std.mem.Allocator, x: usize, y: usize) [][]f64 {
    var mat: [][]f64 = alloc.alloc([]f64, x) catch unreachable;
    for (mat) |*row| {
        row.* = alloc.alloc(f64, y) catch unreachable;
        std.mem.set(f64, row.*, 0.0);
    }
    return mat;
}

fn matGen(alloc: *std.mem.Allocator, n: usize, seed: f64) [][]f64 {
    var mat: [][]f64 = matInit(alloc, n, n);
    const tmp = seed / @intToFloat(f64, n) / @intToFloat(f64, n);

    for (mat) |*row, i| {
        for (row.*) |*x, j| {
            x.* = tmp * (@intToFloat(f64, i) - @intToFloat(f64, j)) * (@intToFloat(f64, i) + @intToFloat(f64, j));
        }
    }

    return mat;
}

fn matMul(alloc: *std.mem.Allocator, a: [][]f64, b: [][]f64) [][]f64 {
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

fn notify(msg: []const u8) void {
    const addr = std.net.Address.parseIp("127.0.0.1", 9001) catch unreachable;
    var stream = std.net.tcpConnectToAddress(addr) catch unreachable;
    _ = stream.write(msg) catch unreachable;
    stream.close();
}

fn calc(alloc: *std.mem.Allocator, n: usize) f64 {
    const size: usize = @divTrunc(n, 2) * @as(usize, 2);
    const a = matGen(alloc, size, 1.0);
    const b = matGen(alloc, size, 2.0);
    const x = matMul(alloc, a, b);
    const i = @intCast(usize, @divTrunc(size, 2));
    return x[i][i];
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    var alloc: *std.mem.Allocator = &arena.allocator;

    var arg_iter = std.process.args();
    _ = arg_iter.skip(); // Skip binary name

    const arg = try arg_iter.next(alloc) orelse "";
    const n = std.fmt.parseInt(usize, arg, 10) catch 100;

    const left = calc(alloc, 101);
    const right = -18.67;
    if (std.math.absFloat(left - right) > 0.1) {
        std.debug.panic("{d} != {d}\n", .{left, right});
    }

    const pid = std.os.linux.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(pid_str);
    const result = calc(alloc, n);
    notify("stop");

    std.debug.print("{d}\n", .{result});
}