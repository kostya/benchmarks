use std::io;

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
  fn getc(&self) -> char { self.get() as u8 as char }
  fn inc(&mut self, x: i32) { self.tape[self.pos] += x; }
  fn mov(&mut self, x: isize) { 
    self.pos = (self.pos as isize + x) as usize;
    while self.pos >= self.tape.len() { self.tape.push(0); }
  }
}

fn _run(program: &[Op], tape: &mut Tape) {
    for op in program {
        match *op {
            Inc(x) => tape.inc(x),
            Move(x) => tape.mov(x),
            Loop(ref program) => while tape.get() > 0 {
              _run(program, tape);
            },
            Print => {
              print!("{}", tape.getc());
              io::Write::flush(&mut io::stdout()).unwrap();
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
  fn run(&self) { 
    let mut tape = Tape::new();
    _run(&self.ops, &mut tape);
  }
}

fn notify(msg: &str) {
    use std::io::Write;

    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn main() {
    use std::fs;
    use std::env;

    let arg1 = env::args().nth(1).unwrap();
    let s = fs::read_to_string(arg1).unwrap();

    notify(&format!("Rust\t{}", std::process::id()));

    let program = Program::new(s);
    program.run();

    notify("stop");
}
