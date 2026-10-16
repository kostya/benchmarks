// Brainfuck: the program parsed into a tree of ops, every loop holding its
// body, and run by walking that tree, recursing into each loop.

import std;
import os;

extern "goose_notify_start" fn notify_start(variant: const u8[:]);
extern "goose_notify_stop" fn notify_stop();

enum Op { Dec, Inc, Prev, Next, Print, Loop { body: Op[:] } }

struct Tape { pos: i64, tape: i32[>..] }

fn new_tape() -> Tape { Tape { pos: 0, tape: [0] } }

fn current_cell(t: Tape&) -> i32 { t.tape[t.pos] }

fn inc(t: Tape&, x: i32) { t.tape[t.pos] += x; }

fn prev(t: Tape&) { t.pos--; }

fn next(t: Tape&) {
    t.pos++;
    if t.pos >= t.tape.len { t.tape.resize(t.pos * 2, 0); }
}

struct Printer { sum1: i32, sum2: i32, quiet: bool }

fn print(p: Printer&, n: i32) {
    if p.quiet {
        p.sum1 = (p.sum1 + n) % 255;
        p.sum2 = (p.sum2 + p.sum1) % 255;
    } else {
        let b: u8[1] = [n as! u8];
        write_stdout(b);
        flush_stdout();
    }
}

fn checksum(p: Printer&) -> i32 { (p.sum2 << 8) | p.sum1 }

recursive fn run(program: Op[:], tape: Tape&, p: Printer&) {
    for op in program {
        match op {
            Dec => { tape.inc(-1); },
            Inc => { tape.inc(1); },
            Prev => { tape.prev(); },
            Next => { tape.next(); },
            Print => { p.print(tape.current_cell()); },
            Loop l => {
                while tape.current_cell() > 0 { run(l.body, tape, p); }
            },
        }
    }
}

// The top level and every loop body are each a run of ops in `pool`, added
// once the run's closing bracket is parsed. A recursive function cannot keep
// a growable array of its own across its recursive calls, so the ops of the
// bodies still being parsed wait on the shared `pending` stack.
recursive fn parse(code: const u8[:], pos: i64&, pool: Op[>..]&, pending: Op[>..<]&) -> Op[:] {
    let mark = pending.len;
    while pos < code.len {
        let byte = code[pos];
        pos++;
        match byte {
            '-' => { pending.push(Op.Dec); },
            '+' => { pending.push(Op.Inc); },
            '<' => { pending.push(Op.Prev); },
            '>' => { pending.push(Op.Next); },
            '.' => { pending.push(Op.Print); },
            '[' => { pending.push(Op.Loop { body: parse(code, pos, pool, pending) }); },
            ']' => { break; },
            _ => {},
        }
    }
    let start = pool.len;
    pool.append(pending[mark..]);
    pending.resize(mark);
    return pool[start..];
}

fn run_program(code: const u8[:], p: Printer&) {
    var pool: Op[>..] = [];
    var pending: Op[>..<] = [];
    var pos = 0;
    let ops = parse(code, pos, pool, pending);
    var tape = new_tape();
    run(ops, tape, p);
}

fn verify() {
    let source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    var left = Printer { sum1: 0, sum2: 0, quiet: true };
    run_program(source, left);
    var right = Printer { sum1: 0, sum2: 0, quiet: true };
    for c in "Hello World!\n" { right.print(c); }
    if left.checksum() != right.checksum() {
        abort(str(left.checksum(), " != ", right.checksum()));
    }
}

fn main() {
    verify();
    let argv = args();
    if argv.len < 2 { exit(1); }
    var source: u8[>..] = [];
    read_file(argv[1], source);
    var quiet: u8[>..] = [];
    var p = Printer { sum1: 0, sum2: 0, quiet: env("QUIET", quiet) };

    notify_start("");
    run_program(source, p);
    notify_stop();

    if p.quiet { print("Output checksum: ", p.checksum()); }
}
