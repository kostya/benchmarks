const std = @import("std");

fn milliTimestamp(io: std.Io) i64 {
    return std.Io.Clock.awake.now(io).toMilliseconds();
}

fn notify(io: std.Io, msg: []const u8) void {
    const addr = std.Io.net.IpAddress.parse("127.0.0.1", 9001) catch unreachable;
    var stream = addr.connect(io, .{ .mode = .stream }) catch return;
    defer stream.close(io);

    var writer = stream.writer(io, &.{});
    writer.interface.writeAll(msg) catch return;
    writer.interface.flush() catch return;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const io = std.Io.Threaded.global_single_threaded.io();

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

        @memset(&buffer, 0);
        try b64.Decoder.decode(&buffer, fix.dst);
        if (!std.mem.eql(u8, buffer[0..fix.src.len], fix.src)) {
            std.debug.panic("'{s}' != '{s}'\n", .{ &buffer, fix.src });
        }
    }

    const STR_SIZE = 131072;
    const TRIES = 8192;

    const str1 = "a" ** STR_SIZE;
    const encodeSize = b64.Encoder.calcSize(STR_SIZE);
    const str2 = try alloc.alloc(u8, encodeSize);
    const encoded = b64.Encoder.encode(str2, str1);
    const decodeSize = try b64.Decoder.calcSizeForSlice(encoded);
    const str3 = try alloc.alloc(u8, decodeSize);
    b64.Decoder.decode(str3, str2) catch unreachable;

    const buffer = try alloc.alloc(u8, @max(encodeSize, decodeSize));
    var fb = std.heap.FixedBufferAllocator.init(buffer);
    const fb_alloc = fb.allocator();

    const pid = std.posix.system.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(io, pid_str);

    var i: i32 = 0;
    var s_encoded: usize = 0;
    const t1 = milliTimestamp(io);
    while (i < TRIES) : (i += 1) {
        const str21 = fb_alloc.alloc(u8, encodeSize) catch unreachable;
        s_encoded += b64.Encoder.encode(str21, str1).len;
        fb_alloc.free(str21);
    }
    const t_encoded = @as(f64, @floatFromInt(milliTimestamp(io) - t1)) / std.time.ms_per_s;

    i = 0;
    var s_decoded: usize = 0;
    const t2 = milliTimestamp(io);
    while (i < TRIES) : (i += 1) {
        const str31 = fb_alloc.alloc(u8, decodeSize) catch unreachable;
        b64.Decoder.decode(str31, str2) catch unreachable;
        s_decoded += str31.len;
        fb_alloc.free(str31);
    }
    const t_decoded = @as(f64, @floatFromInt(milliTimestamp(io) - t2)) / std.time.ms_per_s;

    notify(io, "stop");

    std.debug.print("encode: {s}... to {s}...: {}, {d:.2}\n", .{ str1[0..4], str2[0..4], s_encoded, t_encoded });
    std.debug.print("decode: {s}... to {s}...: {}, {d:.2}\n", .{ str2[0..4], str3[0..4], s_decoded, t_decoded });
}
