use std::fs;
use std::io::prelude::*;
use std::vec::Vec;
use std::io;
use std::env;
use std::collections::BTreeMap;

struct Tape {
  pos: usize,
  tape: Vec<i32>
}

impl Tape {
  fn new() -> Tape { Tape { pos: 0, tape: vec![0] } }
  fn get(&self) -> i32 { self.tape[self.pos] }
  fn getc(&self) -> char { self.get() as u8 as char }
  fn inc(&mut self) { self.tape[self.pos] += 1; }
  fn dec(&mut self) { self.tape[self.pos] -= 1; }
  fn advance(&mut self) { self.pos += 1; if self.tape.len() <= self.pos { self.tape.push(0) } }
  fn devance(&mut self) { if self.pos > 0 { self.pos -= 1; } }
}

struct Program {
  code: Vec<char>,
  bracket_map: BTreeMap<usize, usize>
}

impl Program {
  fn new(content: String) -> Program {
    let mut code: Vec<char> = Vec::new();
    let mut bracket_map = BTreeMap::new();
    let mut leftstack = Vec::new();
    let mut pc = 0;

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
    let mut pc: usize = 0;
    let len = self.code.len();
    let mut tape = Tape::new();

    while pc < len {
      match self.code[pc] {
        '+' => tape.inc(),
        '-' => tape.dec(),
        '>' => tape.advance(),
        '<' => tape.devance(),
        '[' => { if tape.get() == 0 { pc = self.bracket_map[&pc]; } },
        ']' => { if tape.get() != 0 { pc = self.bracket_map[&pc]; } },
        '.' => { print!("{}", tape.getc()); io::stdout().flush().unwrap() },
        _ => ()
      }
      pc += 1;
    }
  }
}

fn notify(msg: &str) {
    if let Ok(mut stream) = std::net::TcpStream::connect("localhost:9001") {
        stream.write_all(msg.as_bytes()).unwrap();
    }
}

fn main() {
    let arg1 = env::args().nth(1).unwrap();
    let s = fs::read_to_string(arg1).unwrap();

    notify(&format!("Rust\t{}", std::process::id()));

    Program::new(s).run();

    notify("stop");
}
