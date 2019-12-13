import os
import net

const (
  INC = 0
  MOVE = 1
  PRINT = 2
  LOOP = 3
)

struct Op {
  o int 
  v int
  loop []Op
}

fn new_op(op int, v int) Op {
  return Op{o: op, v: v}
}

fn new_op_loop(op int, l []Op) Op {
  return Op{o: op, loop: l}
}

struct Tape {
mut:
  tape []int
  pos int
}

fn new_tape() Tape {
  t := Tape {
    pos: 0
    tape: [0]
  }
  return t
}

fn (t Tape) get() int {
  return t.tape[t.pos]
}

fn (t mut Tape) inc(x int) {
  t.tape[t.pos] += x
}

fn (t mut Tape) move(x int) {
  t.pos += x
  for t.pos >= t.tape.len { 
    t.tape << 0
  }
}

struct Program {
  ops []Op
}

fn new_program(code string) Program {
  si := new_si(code)
  return Program{ops: parse(mut si)}
}

fn parse(si mut StringIterator) []Op {
  mut res := []Op
  for true {
    c := si.next()
    match c {
      `+` { res << new_op(INC, 1) }
      `-` { res << new_op(INC, -1) }
      `>` { res << new_op(MOVE, 1) }
      `<` { res << new_op(MOVE, -1) }
      `.` { res << new_op(PRINT, 0) }
      `[` { res << new_op_loop(LOOP, parse(mut si)) }
      `]` { return res }
      `\0`{ return res }
      else { continue }
    }
  }
  return res  
}

fn (p Program) run() {
  mut t := new_tape()
  run_ops(p.ops, mut t)
}

fn run_ops(ops []Op, tape mut Tape) {
  for op in ops {
    match op.o {
      INC { tape.inc(op.v) }
      MOVE { tape.move(op.v) }
      PRINT {
        print(byte(tape.get()).str())
        os.flush_stdout()
      }
      LOOP {
        for tape.get() > 0 {
          run_ops(op.loop, mut tape)
        }
      }
      else {}
    }
  }
}

struct StringIterator {
  code string
mut:
  pos int
}

fn new_si(s string) StringIterator {
  return StringIterator{code: s, pos: 0}
}

fn (si mut StringIterator) next() byte {
  if si.pos < si.code.len {
    res := si.code[si.pos]
    si.pos++
    return res
  }
  else {
    return 0
  }
}

fn notify() {
    sock := net.dial('127.0.0.1', 9001) or {
        return
    }
    mut lang := "V GCC"
    $if clang {
      lang = "V Clang"
    }
    sock.write(lang) or {}
    sock.close() or {}
}

fn main() {
    notify()

    args := os.args
    mut filename := ''

    if args.len == 2 {
        filename = args[1]
    }
    else {
      eprintln('Usage: bf2 filename.b')
      return  
    }

    code := os.read_file(filename) or {
      eprintln('Failed to open file $filename')
      return
    }

    new_program(code).run()
}
