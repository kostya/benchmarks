use std::fs::File;
use std::path::PathBuf;
use std::io::prelude::*;
use std::vec::Vec;
use std::io;
use std::env;

pub enum Instruction {
  MoveRight, // `>`
  MoveLeft, // `<`
  Increment, // `+`
  Decrement, // `-`
  Output, // `.`
  JumpToLeft(usize), // `[`
  JumpToRight(usize), // `]`
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
  code: Vec<Instruction>,
}

impl Program {
  fn new(content: String) -> Program {
    let mut code = Vec::new();
    // Vec of positions of `[` waiting for a `]`
    let mut waiting_opening_jumps = Vec::new();

    for c in content.chars() {
      let instruction = match c {
        '>' => Instruction::MoveRight,
        '<' => Instruction::MoveLeft,
        '+' => Instruction::Increment,
        '-' => Instruction::Decrement,
        '.' => Instruction::Output,
        '[' => {
          // Store the position of this `[`.
          waiting_opening_jumps.push(code.len());
          // This 0usize will be updated when matching `]` is found.
          Instruction::JumpToLeft(0usize)
        },
        ']' => {
          match waiting_opening_jumps.pop() {
            Some(left_jump) => {
              // Add the position of this `]` to the JumpToLeft added earlier.
              code[left_jump] = Instruction::JumpToLeft(code.len());
              // Construct JumpToRight using the position of the earlier `[`.
              Instruction::JumpToRight(left_jump)
            },
            None => panic!("Expected matching `[` before `]`, found lone `]` first."),
          }
        },
        _ => continue,
      };
      code.push(instruction);
    }

    if !waiting_opening_jumps.is_empty() {
      panic!("Unbalanced `[`. Expected matching `]`, found end of file.");
    }

    Program { code: code }
  }

  fn run(&self) {
    let mut pc = 0usize;
    let len = self.code.len();
    let mut tape = Tape::new();

    while pc < len {
      match self.code[pc] {
        Instruction::Increment => tape.inc(),
        Instruction::Decrement => tape.dec(),
        Instruction::MoveRight => tape.advance(),
        Instruction::MoveLeft => tape.devance(),Instruction::JumpToLeft(target_position) => {
          if tape.get() == 0 {
            pc = target_position;
            continue;
          }
        },
        Instruction::JumpToRight(target_position) => {
          if tape.get() != 0 {
            pc = target_position;
            continue;
          }
        },
        Instruction::Output => { print!("{}", tape.getc()); io::stdout().flush().unwrap(); },
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
