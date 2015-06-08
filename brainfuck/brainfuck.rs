use std::fs::File;
use std::path::PathBuf;
use std::io::prelude::*;
use std::vec::Vec;
use std::io;
use std::env;

pub enum Op {
  Nop,
  Right, // `>`
  Left, // `<`
  Inc, // `+`
  Dec, // `-`
  Out, // `.`
  JumpLeft(usize), // `[`
  JumpRight(usize), // `]`
}

struct Tape {
  pos: usize,
  tape: Vec<isize>
}

impl Tape {
  fn new() -> Tape { Tape { pos: 0, tape: vec![0] } }
  fn get(&self) -> isize { self.tape[self.pos] }
  fn getc(&self) -> char { self.get() as u8 as char }
  fn inc(&mut self) { self.tape[self.pos] += 1; }
  fn dec(&mut self) { self.tape[self.pos] -= 1; }
  fn advance(&mut self) { self.pos += 1; if self.tape.len() <= self.pos { self.tape.push(0) } }
  fn devance(&mut self) { if self.pos > 0 { self.pos -= 1; } }
}

struct Program {
  code: Vec<Op>,
}

impl Program {
  fn new(text: String) -> Program {   
    let mut leftstack = Vec::new();
    let mut content = String::new();
    let mut code = Vec::new();

    for c in text.chars() { if "+-<>[].,".contains(c) { content.push(c); code.push(Op::Nop); } }
    for (pc, c) in content.chars().enumerate() {
      let op = match c {
        '>' => Op::Right,
        '<' => Op::Left,
        '+' => Op::Inc,
        '-' => Op::Dec,
        '.' => Op::Out,
        '[' => { leftstack.push(pc); Op::JumpLeft(0usize) },
        ']' => {
          match leftstack.pop() {
            Some(left_jump) => {
              code[left_jump] = Op::JumpLeft(pc);
              Op::JumpRight(left_jump)
            },
            None => panic!("Expected matching `[` before `]`, found lone `]` first."),
          }
        },
        _ => Op::Nop,
      };

      code[pc] = op;
    }

    if !leftstack.is_empty() { panic!("Unbalanced `[`. Expected matching `]`, found end of file."); }
    Program { code: code }
  }

  fn run(&self) {
    let mut pc = 0usize;
    let len = self.code.len();
    let mut tape = Tape::new();

    while pc < len {
      match self.code[pc] {
        Op::Inc => tape.inc(),
        Op::Dec => tape.dec(),
        Op::Right => tape.advance(),
        Op::Left => tape.devance(),
        Op::JumpLeft(jump) => { if tape.get() == 0 { pc = jump; } },
        Op::JumpRight(jump) => { if tape.get() != 0 { pc = jump; } },
        Op::Out => { print!("{}", tape.getc()); io::stdout().flush().unwrap(); },
        Op::Nop => unreachable!()
      }
      pc += 1;
    }
  }
}

fn main() {
  let path = PathBuf::from(env::args_os().nth(1).unwrap());
  let mut file = File::open(&path).unwrap();
  let mut contents = String::new();
  file.read_to_string(&mut contents).unwrap();
  Program::new(contents).run();
}
