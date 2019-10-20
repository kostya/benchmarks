const std = @import("std");
const time = std.time;
const Timer = time.Timer;
const allocator = std.heap.direct_allocator;

const str_size = 10000000;
const tries = 100;

pub fn main() !void {
    var stdout_file = try std.io.getStdOut();
    const stdout = &stdout_file.outStream().stream;

    const str1 = try allocator.alloc(u8, str_size);
    defer allocator.free(str1);
    std.mem.set(u8, str1, 'a');
    var timer = try Timer.start();
    var s: usize = 0;

    var i: usize = 0;
    var start = timer.lap();
    while (i < tries) : (i += 1) {
        const str2 = try allocator.alloc(u8, std.base64.Base64Encoder.calcSize(str1.len));
        defer allocator.free(str2);
        std.base64.standard_encoder.encode(str2, str1);
        s += str2.len;
    }
    var end = timer.read();
    var elapsed_s = @intToFloat(f64, end - start) / time.ns_per_s;
    try stdout.print("encode: {}, {d:.2}\n", s, elapsed_s);

    const str2 = try allocator.alloc(u8, std.base64.Base64Encoder.calcSize(str1.len));
    defer allocator.free(str2);
    std.base64.standard_encoder.encode(str2, str1);

    i = 0;
    s = 0;
    start = timer.lap();
    while (i < tries) : (i += 1) {
        const str3 = try allocator.alloc(u8, try std.base64.standard_decoder.calcSize(str2));
        defer allocator.free(str3);
        try std.base64.standard_decoder.decode(str3, str2);
        s += str3.len;
    }
    end = timer.read();
    elapsed_s = @intToFloat(f64, end - start) / time.ns_per_s;
    try stdout.print("decode: {}, {d:.2}\n", s, elapsed_s);
}
