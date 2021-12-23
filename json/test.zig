const std = @import("std");
const unistd = @cImport(@cInclude("unistd.h"));

const Coordinate = struct {
    x: f64,
    y: f64,
    z: f64,

    fn eql(left: Coordinate, right: Coordinate) bool {
        return left.x == right.x and left.y == right.y and left.z == right.z;
    }
};

const TestStruct = struct {
    coordinates: []Coordinate,
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

fn calc(alloc: std.mem.Allocator, text: []const u8) Coordinate {
    var stream = std.json.TokenStream.init(text);
    const opts = std.json.ParseOptions{
        .allocator = alloc,
        .ignore_unknown_fields = true,
    };
    const obj = std.json.parse(TestStruct, &stream, opts) catch unreachable;

    var x: f64 = 0.0;
    var y: f64 = 0.0;
    var z: f64 = 0.0;
    for (obj.coordinates) |item| {
        x += item.x;
        y += item.y;
        z += item.z;
    }
    const len = @intToFloat(f64, obj.coordinates.len);
    return Coordinate{ .x = x / len, .y = y / len, .z = z / len };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const right = Coordinate{ .x = 2.0, .y = 0.5, .z = 0.25 };
    const vals = [_][]const u8{
        "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
        "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}",
    };
    for (vals) |v| {
        const left = calc(alloc, v);
        if (!Coordinate.eql(left, right)) {
            std.debug.panic("{} != {}\n", .{ left, right });
        }
    }

    const text = try readFile(alloc, "/tmp/1.json");
    const pid = unistd.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(pid_str);
    const results = calc(alloc, text);
    notify("stop");

    std.debug.print("{}\n", .{results});
}
