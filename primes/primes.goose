// Primes: the primes up to a bound from the sieve of Atkin, put in a trie of
// their decimal digits, and the ones that start with a prefix found by
// walking the trie breadth first from the prefix's node.

import std;
import os;

extern "goose_notify_start" fn notify_start(variant: const u8[:]);
extern "goose_notify_stop" fn notify_stop();

let UPPER_BOUND = 5000000;
let PREFIX = 32338;

// A trie node maps a digit to the node it leads to. A map cannot live
// inside an array element (it grows), so a node's map is a run of
// (digit, node) pairs in a slice pool, looked up by scanning it: a node has
// at most ten. A run's new cells start out as the default value, null, so
// the link is optional, though every one is set as its cell is added.
struct Child { ch: u8, node: Node? }
struct Node { children: Child[:], terminal: bool }

struct Sieve { limit: i64, prime: bool[>..] }

fn new_sieve(limit: i64) -> Sieve {
    var s = Sieve { limit: limit, prime: [] };
    s.prime.resize(limit + 1, false);
    return s;
}

fn to_list(s: Sieve&) -> i64[>..] {
    var result: i64[>..] = [2, 3];
    for p in 5..s.limit + 1 {
        if s.prime[p] { result.push(p); }
    }
    return result;
}

fn omit_squares(s: Sieve&) {
    var r = 5;
    while r * r < s.limit {
        if s.prime[r] {
            var i = r * r;
            while i < s.limit {
                s.prime[i] = false;
                i += r * r;
            }
        }
        r++;
    }
}

fn step1(s: Sieve&, x: i64, y: i64) {
    let n = (4 * x * x) + (y * y);
    if n <= s.limit && (n % 12 == 1 || n % 12 == 5) { s.prime[n] = !s.prime[n]; }
}

fn step2(s: Sieve&, x: i64, y: i64) {
    let n = (3 * x * x) + (y * y);
    if n <= s.limit && n % 12 == 7 { s.prime[n] = !s.prime[n]; }
}

fn step3(s: Sieve&, x: i64, y: i64) {
    let n = (3 * x * x) - (y * y);
    if x > y && n <= s.limit && n % 12 == 11 { s.prime[n] = !s.prime[n]; }
}

fn loop_y(s: Sieve&, x: i64) {
    var y = 1;
    while y * y < s.limit {
        s.step1(x, y);
        s.step2(x, y);
        s.step3(x, y);
        y++;
    }
}

fn loop_x(s: Sieve&) {
    var x = 1;
    while x * x < s.limit {
        s.loop_y(x);
        x++;
    }
}

fn calc(s: Sieve&) {
    s.loop_x();
    s.omit_squares();
}

fn child(n: Node&, ch: u8) -> Node? {
    for c in n.children {
        if c.ch == ch { return c.node; }
    }
    return null;
}

fn new_node(nodes: Node[>..]&, runs: Child[>..]&) -> Node& {
    return nodes.push(Node { children: runs.alloc_slice(0), terminal: false });
}

// The trie's nodes go in `nodes` and their maps in `runs`, which the caller
// owns. A map grows in place where there is room after it, and otherwise
// moves to where a new run of its size would go.
fn generate_trie(l: i64[:], nodes: Node[>..]&, runs: Child[>..]&) -> Node& {
    let root .= new_node(nodes, runs);
    for el in l {
        var head .= root;
        for ch in str(el) {
            let next = child(head, ch);
            if next {
                head .= next;
            } else {
                let n .= new_node(nodes, runs);
                let k = head.children.len;
                head.children = runs.realloc_slice(head.children, k + 1);
                head.children[k] = Child { ch: ch, node: n };
                head .= n;
            }
        }
        head.terminal = true;
    }
    return root;
}

// A node to visit and the digits that lead to it, which are few enough to
// sit inline in the queue entry, as a short std::string's do.
struct Entry { node: Node&, prefix: u8[..16] }

fn find(upper_bound: i64, prefix: i64) -> i64[>..] {
    var sieve = new_sieve(upper_bound);
    sieve.calc();
    let primes = sieve.to_list();
    let str_prefix = str(prefix);
    var nodes: Node[>..] = [];
    reusable[] var runs: Child[>..] = [];
    var head .= generate_trie(primes, nodes, runs);

    for ch in str_prefix {
        let next = child(head, ch);
        guard next else { abort("prefix not in the trie"); }
        head .= next;
    }

    // The queue is an array read from the front: entries are pushed at the
    // back and never taken off, so `front` is all a pop needs to move.
    var queue: Entry[>..] = [];
    queue.push(Entry { node: head, prefix: str_prefix });
    var result: i64[>..] = [];
    var front = 0;
    while front < queue.len {
        let top = queue[front];
        front++;
        if top.node.terminal {
            let v, ok = parse_int(top.prefix);
            result.push(v);
        }
        for c in top.node.children {
            let n = c.node;
            assert(n);
            var new_prefix = top.prefix;
            new_prefix.push(c.ch);
            queue.push(Entry { node: n, prefix: new_prefix });
        }
    }
    sort(result);
    return result;
}

fn verify() {
    let left: i64[3] = [2, 23, 29];
    let right = find(100, 2);
    if right != left { abort(str(left, " != ", right)); }
}

fn main() {
    verify();

    notify_start("");
    let results = find(UPPER_BOUND, PREFIX);
    notify_stop();

    print(results);
}
