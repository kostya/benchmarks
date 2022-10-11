const std = @import("std");
const unistd = @cImport(@cInclude("unistd.h"));

const UPPER_BOUND: i32 = 5_000_000;
const PREFIX: i32 = 32_338;

const Node = struct {
    children: std.AutoArrayHashMap(u8, *Node),
    terminal: bool = false,

    fn init(alloc: std.mem.Allocator) Node {
        return Node{
            .children = std.AutoArrayHashMap(u8, *Node).init(alloc),
        };
    }
};

const Sieve = struct {
    limit: i32,
    prime: std.DynamicBitSet,

    fn init(alloc: std.mem.Allocator, limit: i32) Sieve {
        var self = Sieve{
            .limit = limit,
            .prime = std.DynamicBitSet.initEmpty(alloc, @intCast(usize, limit + 1)) catch unreachable,
        };
        return self;
    }

    fn toList(self: *Sieve, alloc: std.mem.Allocator) std.ArrayList(i32) {
        var result = std.ArrayList(i32).init(alloc);
        result.appendSlice(&.{ 2, 3 }) catch unreachable;

        var p: i32 = 5;
        while (p <= self.limit) : (p += 1) {
            if (self.prime.isSet(@intCast(usize, p))) {
                result.append(p) catch unreachable;
            }
        }
        return result;
    }

    fn omitSquares(self: *Sieve) *Sieve {
        var r: i32 = 5;
        while (r * r < self.limit) : (r += 1) {
            if (self.prime.isSet(@intCast(usize, r))) {
                var i: i32 = r * r;
                while (i < self.limit) : (i += r * r) {
                    self.prime.unset(@intCast(usize, i));
                }
            }
        }
        return self;
    }

    fn step1(self: *Sieve, x: i32, y: i32) void {
        const n: i32 = (4 * x * x) + (y * y);
        if (n <= self.limit and (@rem(n, 12) == 1 or @rem(n, 12) == 5)) {
            self.prime.toggle(@intCast(usize, n));
        }
    }

    fn step2(self: *Sieve, x: i32, y: i32) void {
        const n: i32 = (3 * x * x) + (y * y);
        if (n <= self.limit and @rem(n, 12) == 7) {
            self.prime.toggle(@intCast(usize, n));
        }
    }

    fn step3(self: *Sieve, x: i32, y: i32) void {
        const n: i32 = (3 * x * x) - (y * y);
        if (x > y and n <= self.limit and @rem(n, 12) == 11) {
            self.prime.toggle(@intCast(usize, n));
        }
    }

    fn loopY(self: *Sieve, x: i32) void {
        var y: i32 = 1;
        while (y * y < self.limit) : (y += 1) {
            self.step1(x, y);
            self.step2(x, y);
            self.step3(x, y);
        }
    }

    fn loopX(self: *Sieve) void {
        var x: i32 = 1;
        while (x * x < self.limit) : (x += 1) {
            self.loopY(x);
        }
    }

    fn calc(self: *Sieve) *Sieve {
        self.loopX();
        return self.omitSquares();
    }
};

fn generateTrie(alloc: std.mem.Allocator, l: std.ArrayList(i32)) *Node {
    var root: *Node = alloc.create(Node) catch unreachable;
    root.* = Node.init(alloc);
    for (l.items) |el| {
        var head = root;
        const str_el = std.fmt.allocPrint(alloc, "{}", .{el}) catch unreachable;
        for (str_el) |ch| {
            if (!head.children.contains(ch)) {
                var node = alloc.create(Node) catch unreachable;
                node.* = Node.init(alloc);
                head.children.put(ch, node) catch unreachable;
            }
            head = head.children.get(ch) orelse unreachable;
        }
        head.terminal = true;
    }
    return root;
}

fn find(alloc: std.mem.Allocator, upper_bound: i32, prefix: i32) std.ArrayList(i32) {
    const primes = Sieve.init(alloc, upper_bound).calc();
    const str_prefix = std.fmt.allocPrint(alloc, "{}", .{prefix}) catch unreachable;
    var head = generateTrie(alloc, primes.toList(alloc));

    for (str_prefix) |ch| {
        head = head.children.get(ch) orelse unreachable;
    }

    const Pair = struct {
        node: *Node,
        str: []u8,
    };

    const Q = std.TailQueue(Pair);
    var queue = Q{};
    var first = Q.Node{ .data = Pair{ .node = head, .str = str_prefix } };
    queue.append(&first);

    var result = std.ArrayList(i32).init(alloc);
    while (queue.len > 0) {
        const pair = queue.pop() orelse unreachable;
        const top = pair.data.node;
        const current_prefix = pair.data.str;

        if (top.terminal) {
            const v = std.fmt.parseInt(i32, current_prefix, 10) catch unreachable;
            result.append(v) catch unreachable;
        }

        var it = top.children.iterator();
        while (it.next()) |kv| {
            const str = std.fmt.allocPrint(alloc, "{s}{c}", .{ current_prefix, kv.key_ptr.* }) catch unreachable;
            var elem = alloc.create(Q.Node) catch unreachable;
            elem.* = Q.Node{ .data = Pair{ .node = kv.value_ptr.*, .str = str } };
            queue.prepend(elem);
        }
    }
    std.sort.sort(i32, result.items, {}, comptime std.sort.asc(i32));
    return result;
}

fn notify(msg: []const u8) void {
    const addr = std.net.Address.parseIp("127.0.0.1", 9001) catch unreachable;
    if (std.net.tcpConnectToAddress(addr)) |stream| {
        defer stream.close();
        _ = stream.write(msg) catch unreachable;
    } else |_| {}
}

fn verify(alloc: std.mem.Allocator) !void {
    const left = [_]i32{ 2, 23, 29 };
    const right = find(alloc, 100, 2).toOwnedSlice();

    var i: usize = 0;
    while (i < right.len) : (i += 1) {
        if (left[i] != right[i]) {
            std.debug.panic("{} != {}\n", .{ left[i], right[i] });
        }
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    try verify(alloc);

    const pid = unistd.getpid();
    const pid_str = try std.fmt.allocPrint(alloc, "Zig\t{d}", .{pid});

    notify(pid_str);
    const results = find(alloc, UPPER_BOUND, PREFIX);
    notify("stop");

    std.debug.print("{d}\n", .{results.items});
}
