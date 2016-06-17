use std::io;

enum Op {
    Inc(isize),
    Move(isize),
    Loop(Box<[Op]>),
    Print
}
use Op::*;

struct Tape {
  pos: usize,
  tape: Vec<isize>
}

impl Tape {
  fn new() -> Tape { Tape { pos: 0, tape: vec![0] } }
  fn get(&self) -> isize { self.tape[self.pos] }
  fn getc(&self) -> char { self.get() as u8 as char }
  fn inc(&mut self, x: isize) { self.tape[self.pos] += x; }
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
            Loop(ref program) => while tape.get() != 0 {
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

fn main() {
    use std::fs::File;
    use std::path::Path;
    use std::io::prelude::*;
    use std::env;

    let arg1 = env::args().nth(1).unwrap();
    let path = Path::new(&arg1);
    let mut s = String::new();
    let mut file = File::open(&path).unwrap();
    file.read_to_string(&mut s).unwrap();
    let program = Program::new(s);
    program.run();
}
