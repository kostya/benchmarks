use std::io::{self, Stdout, StdoutLock, Write};
use std::{env, fs, process};
use utils::notify;

#[derive(PartialEq)]
enum Op {
    Inc,
    Dec,
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

/// Wrapper around a tape cell pointer for less deref confusion
struct Cell(*mut i32);

impl Cell {
    /// Create a cell pointing to `tape[pos]`
    #[inline(always)]
    fn new(tape: &Vec<i32>, pos: usize) -> Self {
        // SAFETY: pos is always a valid index when called
        Self(unsafe { tape.as_ptr().add(pos) as *mut i32 })
    }

    /// Mutate the cell value by wrapping add/sub
    #[inline(always)]
    fn mutate(&self, delta: i32) {
        unsafe { *self.0 = (*self.0).wrapping_add(delta) };
    }

    /// Read the current cell value
    #[inline(always)]
    fn get(&self) -> i32 {
        unsafe { *self.0 }
    }

    /// Move the cell pointer to the previous position
    #[inline(always)]
    fn move_prev(&mut self) {
        // SAFETY: pos is decremented before calling, guaranteed valid by program logic
        unsafe { self.0 = self.0.sub(1) };
    }

    /// Move the cell pointer to the next position
    #[inline(always)]
    fn move_next(&mut self) {
        // SAFETY: pos is incremented before calling, guaranteed valid by program logic
        unsafe { self.0 = self.0.add(1) };
    }

    /// Update the cell pointer to a new position in the tape
    #[inline(always)]
    fn set(&mut self, tape: &Vec<i32>, pos: usize) {
        // SAFETY: pos is always a valid index when called
        self.0 = unsafe { tape.as_ptr().add(pos) as *mut i32 };
    }
}

/// Macro to greedily consume sequential identical opcodes in a tight loop
///
/// This exists for 2 reasons:
/// - Reduces branching by allowing for sequential pre-validated operations (minor)
/// - Greatly improves LLVM's ability to reason about the code, allowing it to
///     compile down to a cached register written back to the tape after the unrolled batch (major)
///
/// The greedy loop consumes as many consecutive identical ops as possible without dropping back to the main dispatch match.
/// It uses a tight while loop that checks the current op and executes the body, repeating until a different op is found.
macro_rules! dispatch_op {
    ( $program:expr, $idx:expr, $len:expr, $op:expr ;
      body = $body:block ) => {
        // Greedy loop: consume all consecutive identical ops without returning to main dispatch
        while $idx < $len && $program[$idx] == $op {
            $body;
            $idx += 1;
        }
    };
}

/// Shared dispatch loop implementation
#[inline(always)]
fn dispatch_loop(
    program: &[Op],
    len: usize,
    idx: &mut usize,
    cell: &mut Cell,
    pos: &mut usize,
    tape: &mut Tape,
    printer: &mut Printer,
) {
    while *idx < len {
        match program[*idx] {
            Op::Inc => {
                dispatch_op!(program, *idx, len, Op::Inc; body = {
                    cell.mutate(1);
                });
            }
            Op::Dec => {
                dispatch_op!(program, *idx, len, Op::Dec; body = {
                    cell.mutate(-1);
                });
            }
            Op::Prev => {
                dispatch_op!(program, *idx, len, Op::Prev; body = {
                    *pos -= 1;
                    cell.move_prev();
                });
            }
            Op::Next => {
                dispatch_op!(program, *idx, len, Op::Next; body = {
                    *pos += 1;
                    if *pos >= tape.tape.len() {
                        tape.tape.resize(*pos * 2, 0);
                        cell.set(&tape.tape, *pos);
                    } else {
                        cell.move_next();
                    }
                });
            }
            Op::Print => {
                printer.print(cell.get());
                *idx += 1;
            }
            Op::Loop(ref body) => {
                tape.pos = *pos;
                if cell.get() > 0 {
                    run_loop(body, tape, printer);
                }
                *pos = tape.pos;
                cell.set(&tape.tape, *pos);
                *idx += 1;
            }
        }
    }
}

/// Top-level entry point: execute all instructions in a program slice
fn run(program: &[Op], tape: &mut Tape, printer: &mut Printer) {
    let len = program.len();
    let mut pos: usize = tape.pos;
    let mut cell = Cell::new(&tape.tape, pos);
    let mut idx = 0;

    dispatch_loop(program, len, &mut idx, &mut cell, &mut pos, tape, printer);
    tape.pos = pos;
}

/// Loop body runner: execute the loop body repeatedly while current cell is non-zero
fn run_loop(program: &[Op], tape: &mut Tape, printer: &mut Printer) {
    let len = program.len();
    if len == 0 {
        return;
    }
    let mut pos: usize = tape.pos;
    let mut cell = Cell::new(&tape.tape, pos);

    loop {
        let mut idx = 0;
        dispatch_loop(program, len, &mut idx, &mut cell, &mut pos, tape, printer);
        if cell.get() <= 0 {
            break;
        }
    }
    tape.pos = pos;
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
