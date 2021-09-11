const std = @import("std");
const unistd = @cImport(@cInclude("unistd.h"));

fn notify(msg: []const u8) void {
    const addr = std.net.Address.parseIp("127.0.0.1", 9001) catch unreachable;
    if (std.net.tcpConnectToAddress(addr)) |stream| {
        defer stream.close();
        _ = stream.write(msg) catch unreachable;
    } else |_| {}
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    var alloc: *std.mem.Allocator = &arena.allocator;

    const b64 = std.base64.standard;
    const fixtures: [2]struct { src: []const u8, dst: []const u8 } = .{
        .{ .src = "hello", .dst = "aGVsbG8=" },
        .{ .src = "world", .dst = "d29ybGQ=" },
    };

    for (fixtures) |fix| {
        var buffer: [0x100]u8 = undefined;
        const encoded = b64.Encoder.encode(&buffer, fix.src);
        if (!std.mem.eql(u8, encoded, fix.dst)) {
            std.debug.panic("'{s}' != '{s}'\n", .{ encoded, fix.dst });
        }

        std.mem.set(u8, &buffer, 0);
        try b64.Decoder.decode(&buffer, fix.dst);
        if (!std.mem.eql(u8, buffer[0..fix.src.len], fix.src)) {
            std.debug.panic("'{s}' != '{s}'\n", .{ &buffer, fix.src });
        }
    }

    const STR_SIZE = 131072;
    const TRIES = 8192;

    const str1 = "a" ** STR_SIZE;
    const str2 = try alloc.alloc(u8, b64.Encoder.calcSize(STR_SIZE));
    const encoded = b64.Encoder.encode(str2, str1);
    const str3 = try alloc.alloc(u8, try b64.Decoder.calcSizeForSlice(encoded));
    b64.Decoder.decode(str3, str2) catch unreachable;

    const pid = unistd.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(pid_str);

    var i: i32 = 0;
    var s_encoded: usize = 0;
    const t1 = std.time.milliTimestamp();
    while (i < TRIES) : (i += 1) {
        s_encoded += b64.Encoder.encode(str2, str1).len;
    }
    const t_encoded: f64 = @intToFloat(f64, std.time.milliTimestamp() - t1) / std.time.ms_per_s;

    i = 0;
    var s_decoded: usize = 0;
    const t2 = std.time.milliTimestamp();
    while (i < TRIES) : (i += 1) {
        b64.Decoder.decode(str3, str2) catch unreachable;
        s_decoded += str3.len;
    }
    const t_decoded: f64 = @intToFloat(f64, std.time.milliTimestamp() - t2) / std.time.ms_per_s;

    notify("stop");

    std.debug.print("encode: {s}... to {s}...: {}, {d:.2}\n", .{ str1[0..4], str2[0..4], s_encoded, t_encoded });
    std.debug.print("decode: {s}... to {s}...: {}, {d:.2}\n", .{ str2[0..4], str3[0..4], s_decoded, t_decoded });
}
