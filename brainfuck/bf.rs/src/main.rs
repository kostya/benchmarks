use std::io::{self, Stdout, StdoutLock, Write};
use std::{env, fs, process};
use utils::notify;

enum Op {
    Dec,
    Inc,
    Prev,
    Next,
    Print,
    Loop(Box<[Op]>),
}

struct Tape {
    pos: usize,
    tape: Vec<i32>,
}

impl Tape {
    fn new() -> Self {
        Self {
            pos: 0,
            tape: vec![0],
        }
    }

    fn current_cell(&self) -> i32 {
        // SAFETY: `self.pos` is already checked in `self.next()`.
        unsafe { *self.tape.get_unchecked(self.pos) }
    }

    fn inc(&mut self, x: i32) {
        // SAFETY: `self.pos` is already checked in `self.next()`.
        unsafe { *self.tape.get_unchecked_mut(self.pos) += x };
    }

    fn prev(&mut self) {
        self.pos -= 1;
    }

    fn next(&mut self) {
        self.pos += 1;
        if self.pos >= self.tape.len() {
            self.tape.resize(self.pos * 2, 0);
        }
    }
}

struct Printer<'a> {
    output: StdoutLock<'a>,
    sum1: i32,
    sum2: i32,
    quiet: bool,
}

impl Printer<'_> {
    fn new(output: &Stdout, quiet: bool) -> Self {
        Self {
            output: output.lock(),
            sum1: 0,
            sum2: 0,
            quiet,
        }
    }

    fn print(&mut self, n: i32) {
        if self.quiet {
            self.sum1 = (self.sum1 + n) % 255;
            self.sum2 = (self.sum2 + self.sum1) % 255;
        } else {
            self.output.write_all(&[n as u8]).unwrap();
            self.output.flush().unwrap();
        }
    }

    const fn checksum(&self) -> i32 {
        (self.sum2 << 8) | self.sum1
    }
}

fn run(program: &[Op], tape: &mut Tape, printer: &mut Printer) {
    for op in program {
        match op {
            Op::Dec => tape.inc(-1),
            Op::Inc => tape.inc(1),
            Op::Prev => tape.prev(),
            Op::Next => tape.next(),
            Op::Print => printer.print(tape.current_cell()),
            Op::Loop(program) => {
                while tape.current_cell() > 0 {
                    run(program, tape, printer);
                }
            }
        }
    }
}

fn parse(iter: &mut impl Iterator<Item = u8>) -> Box<[Op]> {
    let mut buf = vec![];
    while let Some(byte) = iter.next() {
        buf.push(match byte {
            b'-' => Op::Dec,
            b'+' => Op::Inc,
            b'<' => Op::Prev,
            b'>' => Op::Next,
            b'.' => Op::Print,
            b'[' => Op::Loop(parse(iter)),
            b']' => break,
            _ => continue,
        });
    }
    buf.into_boxed_slice()
}

struct Program {
    ops: Box<[Op]>,
}

impl Program {
    fn new(code: impl IntoIterator<Item = u8>) -> Self {
        Self {
            ops: parse(&mut code.into_iter()),
        }
    }

    fn run(&self, printer: &mut Printer) {
        let mut tape = Tape::new();
        run(&self.ops, &mut tape, printer);
    }
}

fn verify() {
    const SOURCE: &str = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
                          ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    let output = io::stdout();
    let left = {
        let mut printer = Printer::new(&output, true);
        Program::new(SOURCE.bytes()).run(&mut printer);
        printer.checksum()
    };
    let right = {
        let mut printer = Printer::new(&output, true);
        for &byte in b"Hello World!\n" {
            printer.print(byte as i32);
        }
        printer.checksum()
    };
    assert_eq!(left, right);
}

fn main() {
    verify();
    let source = fs::read(env::args().nth(1).unwrap()).unwrap_or_default();
    let output = io::stdout();
    let mut printer = Printer::new(&output, env::var("QUIET").is_ok());

    notify!("Rust\t{pid}", pid = process::id());
    Program::new(source).run(&mut printer);
    notify!("stop");

    if printer.quiet {
        println!("Output checksum: {}", printer.checksum());
    }
}
