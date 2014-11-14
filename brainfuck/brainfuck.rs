use std::io::File;
use std::io::stdio;
use std::os;
use std::str;
use std::vec::Vec;
use std::collections::HashMap;

struct Tape {
  pos: uint,
  tape: Vec<int>
}

impl Tape {
  fn new() -> Tape { Tape { pos: 0u, tape: vec![0] } }
  fn get(&self) -> int { self.tape[self.pos] }
  fn getc(&self) -> char { self.get() as u8 as char }
  fn inc(&mut self) { let x = self.tape.get_mut(self.pos); *x.unwrap() += 1; }
  fn dec(&mut self) { let x = self.tape.get_mut(self.pos); *x.unwrap() -= 1; }
  fn advance(&mut self) { self.pos += 1; if self.tape.len() <= self.pos { self.tape.push(0) } }
  fn devance(&mut self) { if self.pos > 0 { self.pos -= 1; } }
}

struct Program {
  code: Vec<char>,
  bracket_map: HashMap<uint, uint>
}

impl Program {
  fn new(content: &str) -> Program {
    let mut code: Vec<char> = Vec::new();
    let mut bracket_map = HashMap::new();
    let mut leftstack = Vec::new();
    let mut pc = 0u;

    for c in content.chars() {
      match c {
        '+' | '-' | '.' | ',' | '<' | '>' => (),
        '[' => { leftstack.push(pc); },
        ']' => match leftstack.pop() {
          Some(left) => { bracket_map.insert(left, pc); bracket_map.insert(pc, left); }
          None => ()
        },
        _ => { continue; }
      }
      code.push(c);
      pc += 1;
    }
    Program{ code: code, bracket_map: bracket_map }
  }

  fn run(&self) {
    let mut pc: uint = 0;
    let len = self.code.len();
    let mut tape = Tape::new();

    while pc < len {
      match self.code[pc] {
        '+' => tape.inc(),
        '-' => tape.dec(),
        '>' => tape.advance(),
        '<' => tape.devance(),
        '[' => { if tape.get() == 0 { pc = self.bracket_map[pc]; } },
        ']' => { if tape.get() != 0 { pc = self.bracket_map[pc]; } },
        '.' => { print!("{:c}", tape.getc()); stdio::flush(); },
        _ => ()
      }
      pc += 1;
    }
  }
}

fn main() {
  let args = os::args();
  let bytes = File::open(&Path::new(args[1].as_slice())).read_to_end().unwrap();
  let text = str::from_utf8(bytes.as_slice()).unwrap();

  Program::new(text).run()
}
