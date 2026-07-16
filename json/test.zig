const std = @import("std");

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

fn calc(alloc: std.mem.Allocator, text: []const u8) !Coordinate {
    const parsed = try std.json.parseFromSlice(TestStruct, alloc, text, .{
        .ignore_unknown_fields = true,
    });
    defer parsed.deinit();
    const obj = parsed.value;

    var x: f64 = 0.0;
    var y: f64 = 0.0;
    var z: f64 = 0.0;
    for (obj.coordinates) |item| {
        x += item.x;
        y += item.y;
        z += item.z;
    }
    const len = @as(f64, @floatFromInt(obj.coordinates.len));
    return Coordinate{ .x = x / len, .y = y / len, .z = z / len };
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const io = std.Io.Threaded.global_single_threaded.io();

    const right = Coordinate{ .x = 2.0, .y = 0.5, .z = 0.25 };
    const vals = [_][]const u8{
        "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
        "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}",
    };
    for (vals) |v| {
        const left = try calc(alloc, v);
        if (!Coordinate.eql(left, right)) {
            std.debug.panic("{} != {}\n", .{ left, right });
        }
    }

    const text = try readFile(alloc, io, "/tmp/1.json");
    const pid = std.posix.system.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(io, pid_str);
    const results = try calc(alloc, text);
    notify(io, "stop");

    std.debug.print("{}\n", .{results});
}
