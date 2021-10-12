use std::io::{self, Stdout, StdoutLock, Write};
use std::net::TcpStream;
use std::{env, fs, process};

enum Op {
    Dec,
    Inc,
    Prev,
    Next,
    Loop(Box<[Op]>),
    Print,
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

    fn get(&self) -> i32 {
        // SAFETY: `self.pos` is already checked in `self.next()`
        unsafe { *self.tape.get_unchecked(self.pos) }
    }

    fn get_mut(&mut self) -> &mut i32 {
        // SAFETY: `self.pos` is already checked in `self.next()`
        unsafe { self.tape.get_unchecked_mut(self.pos) }
    }

    fn dec(&mut self) {
        *self.get_mut() -= 1;
    }

    fn inc(&mut self) {
        *self.get_mut() += 1;
    }

    fn prev(&mut self) {
        self.pos -= 1;
    }

    fn next(&mut self) {
        self.pos += 1;
        if self.pos >= self.tape.len() {
            self.tape.resize(self.pos << 1, 0);
        }
    }
}

struct Printer<'a> {
    output: StdoutLock<'a>,
    sum1: i32,
    sum2: i32,
    quiet: bool,
}

impl<'a> Printer<'a> {
    fn new(output: &'a Stdout, quiet: bool) -> Self {
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

    const fn get_checksum(&self) -> i32 {
        (self.sum2 << 8) | self.sum1
    }
}

fn run(program: &[Op], tape: &mut Tape, p: &mut Printer) {
    for op in program {
        match op {
            Op::Dec => tape.dec(),
            Op::Inc => tape.inc(),
            Op::Prev => tape.prev(),
            Op::Next => tape.next(),
            Op::Loop(program) => {
                while tape.get() > 0 {
                    run(program, tape, p);
                }
            }
            Op::Print => p.print(tape.get()),
        }
    }
}

fn parse(it: &mut impl Iterator<Item = u8>) -> Box<[Op]> {
    let mut buf = vec![];
    while let Some(c) = it.next() {
        buf.push(match c {
            b'-' => Op::Dec,
            b'+' => Op::Inc,
            b'<' => Op::Prev,
            b'>' => Op::Next,
            b'.' => Op::Print,
            b'[' => Op::Loop(parse(it)),
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
    fn new(code: &[u8]) -> Self {
        Self {
            ops: parse(&mut code.iter().copied()),
        }
    }

    fn run(&self, p: &mut Printer) {
        let mut tape = Tape::new();
        run(&self.ops, &mut tape, p);
    }
}

fn notify(msg: &str) {
    if let Ok(mut stream) = TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn verify() {
    const S: &[u8] = b"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
                       ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";

    let left = {
        let output = io::stdout();
        let mut p_left = Printer::new(&output, true);
        Program::new(S).run(&mut p_left);
        p_left.get_checksum()
    };

    let right = {
        let output = io::stdout();
        let mut p_right = Printer::new(&output, true);
        for &c in b"Hello World!\n" {
            p_right.print(c as i32);
        }
        p_right.get_checksum()
    };

    if left != right {
        eprintln!("{} != {}", left, right);
        process::exit(-1);
    }
}

fn main() {
    verify();
    let s = fs::read(env::args().nth(1).unwrap()).unwrap();
    let output = io::stdout();
    let mut p = Printer::new(&output, env::var("QUIET").is_ok());

    notify(&format!("Rust\t{}", process::id()));
    Program::new(&s).run(&mut p);
    notify("stop");

    if p.quiet {
        println!("Output checksum: {}", p.get_checksum());
    }
}
