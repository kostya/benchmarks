import os
import net

const (
	inc   = 0
	move  = 1
	print = 2
	loop  = 3
)

struct Op {
	o    int
	v    int
	loop []Op
}

fn new_op(op, v int) Op {
	return Op{
		o: op
		v: v
	}
}

fn new_op_loop(op int, l []Op) Op {
	return Op{
		o: op
		loop: l
	}
}

struct Tape {
mut:
	tape []int
	pos  int
}

fn new_tape() Tape {
	t := Tape{
		pos: 0
		tape: [0]
	}
	return t
}

fn (t Tape) get() int {
	return t.tape[t.pos]
}

fn (mut t Tape) inc(x int) {
	t.tape[t.pos] += x
}

fn (mut t Tape) move(x int) {
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
	return Program{
		ops: parse(mut si)
	}
}

fn parse(mut si StringIterator) []Op {
	mut res := []Op{}
	for true {
		c := si.next()
		match c {
			`+` { res << new_op(inc, 1) }
			`-` { res << new_op(inc, -1) }
			`>` { res << new_op(move, 1) }
			`<` { res << new_op(move, -1) }
			`.` { res << new_op(print, 0) }
			`[` { res << new_op_loop(loop, parse(mut si)) }
			`]` { return res }
			`\0` { return res }
			else { continue }
		}
	}
	return res
}

fn (p Program) run() {
	mut t := new_tape()
	run_ops(p.ops, mut t)
}

fn run_ops(ops []Op, mut tape Tape) {
	for op in ops {
		match op.o {
			inc {
				tape.inc(op.v)
			}
			move {
				tape.move(op.v)
			}
			print {
				print(byte(tape.get()).str())
				os.flush()
			}
			loop {
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
	pos  int
}

fn new_si(s string) StringIterator {
	return StringIterator{
		code: s
		pos: 0
	}
}

fn (mut si StringIterator) next() byte {
	if si.pos < si.code.len {
		res := si.code[si.pos]
		si.pos++
		return res
	} else {
		return 0
	}
}

fn notify(msg string) {
	sock := net.dial('127.0.0.1', 9001) or {
		return
	}
	sock.write(msg) or {
	}
	sock.close() or {
	}
}

fn main() {
	args := os.args
	mut filename := ''
	if args.len == 2 {
		filename = args[1]
	} else {
		eprintln('Usage: bf2 filename.b')
		return
	}
	code := os.read_file(filename) or {
		eprintln('Failed to open file $filename')
		return
	}
	mut lang := 'V GCC'
	$if clang {
		lang = 'V Clang'
	}
	notify('${lang}\t${C.getpid()}')
	new_program(code).run()
	notify('stop')
}
