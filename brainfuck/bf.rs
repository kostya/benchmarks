use std::io;
use std::fs;
use std::env;

enum Op {
    Inc(i32),
    Move(isize),
    Loop(Box<[Op]>),
    Print
}
use Op::*;

struct Tape {
    pos: usize,
    tape: Vec<i32>
}

impl Tape {
    fn new() -> Tape { Tape { pos: 0, tape: vec![0] } }
    fn get(&self) -> i32 { self.tape[self.pos] }
    fn inc(&mut self, x: i32) { self.tape[self.pos] += x; }
    fn mov(&mut self, x: isize) {
        self.pos = (self.pos as isize + x) as usize;
        while self.pos >= self.tape.len() { self.tape.push(0); }
    }
}

struct Printer {
    sum1: i32,
    sum2: i32,
    quiet: bool
}

impl Printer {
    fn new(quiet: bool) -> Printer { Printer { sum1: 0, sum2: 0, quiet: quiet} }

    fn print(&mut self, n: i32) {
        if self.quiet {
            self.sum1 = (self.sum1 + n) % 255;
            self.sum2 = (self.sum2 + self.sum1) % 255;
        } else {
            print!("{}", n as u8 as char);
            io::Write::flush(&mut io::stdout()).unwrap();
        }
    }

    fn get_checksum(&self) -> i32 {
        (self.sum2 << 8) | self.sum1
    }
}

fn _run(program: &[Op], tape: &mut Tape, p: &mut Printer) {
    for op in program {
        match *op {
            Inc(x) => tape.inc(x),
            Move(x) => tape.mov(x),
            Loop(ref program) => while tape.get() > 0 {
                _run(program, tape, p);
            },
            Print => {
                p.print(tape.get());
            }
        }
    }
}

fn parse<I: Iterator<Item=char>>(it: &mut I) -> Box<[Op]> {
    let mut buf = Vec::new();
    while let Some(c) = it.next() {
        buf.push( match c {
            '+' => Inc(1),
            '-' => Inc(-1),
            '>' => Move(1),
            '<' => Move(-1),
            '.' => Print,
            '[' => Loop(parse(it)),
            ']' => break,
            _ => continue,
        } );
    }
    buf.into_boxed_slice()
}

struct Program {
    ops: Box<[Op]>
}

impl Program {
    fn new(code: String) -> Program { Program { ops: parse(&mut code.chars()) } }
    fn run(&self, p: &mut Printer) {
        let mut tape = Tape::new();
        _run(&self.ops, &mut tape, p);
    }
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn verify() {
    let s = String::from("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.");
    let mut p_left = Printer::new(true);
    Program::new(s).run(&mut p_left);
    let left = p_left.get_checksum();

    let mut p_right = Printer::new(true);
    for c in "Hello World!\n".chars() {
        p_right.print(c as i32);
    }
    let right = p_right.get_checksum();
    if left != right {
        eprintln!("{:?} != {:?}", left, right);
        std::process::exit(-1);
    }
}

fn main() {
    verify();

    let arg1 = env::args().nth(1).unwrap();
    let s = fs::read_to_string(arg1).unwrap();
    let mut p = Printer::new(std::env::var("QUIET").is_ok());

    notify(&format!("Rust\t{}", std::process::id()));
    Program::new(s).run(&mut p);
    notify("stop");

    if p.quiet {
        println!("Output checksum: {}", p.get_checksum());
    }
}
