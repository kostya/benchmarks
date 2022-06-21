import os
import net

enum OpType {
	inc
	move
	print
	loop
}

struct Op {
	o    OpType
	v    int
	loop []Op
}

fn new_op(op OpType, v int) Op {
	return Op{
		o: op
		v: v
	}
}

fn new_op_loop(op OpType, l []Op) Op {
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
	if t.pos >= t.tape.len {
		t.tape << [0].repeat(t.pos - t.tape.len + 1)
	}
}

struct Printer {
	quiet bool
mut:
	sum1 u8
	sum2 u8
}

fn new_printer(quiet bool) Printer {
	return Printer{
		sum1: 0
		sum2: 0
		quiet: quiet
	}
}

fn (mut p Printer) print(n int) {
	if p.quiet {
		p.sum1 = u8(p.sum1 + n)
		p.sum2 = u8(p.sum2 + p.sum1)
	} else {
		print(u8(n).ascii_str())
		os.flush()
	}
}

fn (p Printer) get_checksum() u32 {
	return (u32(p.sum2) << 8) | p.sum1
}

struct Program {
	ops []Op
}

fn new_program(code string) Program {
	mut si := new_si(code)
	return Program{
		ops: parse(mut si)
	}
}

fn parse(mut si StringIterator) []Op {
	mut res := []Op{}
	for c in si {
		match c {
			`+` {
				res << new_op(.inc, 1)
			}
			`-` {
				res << new_op(.inc, -1)
			}
			`>` {
				res << new_op(.move, 1)
			}
			`<` {
				res << new_op(.move, -1)
			}
			`.` {
				res << new_op(.print, 0)
			}
			`[` {
				res << new_op_loop(.loop, parse(mut si))
			}
			`]` {
				return res
			}
			`\0` {
				return res
			}
			else {
				continue
			}
		}
	}
	return res
}

fn (p Program) run(mut printer Printer) {
	mut t := new_tape()
	run_ops(p.ops, mut t, mut printer)
}

fn run_ops(ops []Op, mut tape Tape, mut p Printer) {
	for op in ops {
		match op.o {
			.inc {
				tape.inc(op.v)
			}
			.move {
				tape.move(op.v)
			}
			.print {
				p.print(tape.get())
			}
			.loop {
				for tape.get() > 0 {
					run_ops(op.loop, mut tape, mut p)
				}
			}
		}
	}
}

struct StringIterator {
	code string
mut:
	pos int
}

fn new_si(s string) StringIterator {
	return StringIterator{
		code: s
		pos: 0
	}
}

fn (mut si StringIterator) next() ?u8 {
	if si.pos < si.code.len {
		res := si.code[si.pos]
		si.pos++
		return res
	} else {
		return error('Out of elements')
	}
}

fn notify(msg string) {
	mut sock := net.dial_tcp('127.0.0.1:9001') or { return }
	defer {
		sock.close() or {}
	}
	sock.write_string(msg) or {}
}

fn verify() {
	text := '++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.'
	mut p_left := new_printer(true)
	new_program(text).run(mut p_left)
	left := p_left.get_checksum()
	mut p_right := new_printer(true)
	for c in 'Hello World!\n' {
		p_right.print(c)
	}
	right := p_right.get_checksum()
	if left != right {
		panic('$left != $right')
	}
}

fn main() {
	verify()
	mut filename := ''
	if os.args.len == 2 {
		filename = os.args[1]
	} else {
		eprintln('Usage: bf filename.b')
		return
	}
	code := os.read_file(filename) or {
		eprintln('Failed to open file $filename')
		return
	}
	mut p := new_printer(os.getenv('QUIET') != '')
	mut lang := 'V/gcc'
	$if clang {
		lang = 'V/clang'
	}
	notify('$lang\t$C.getpid()')
	new_program(code).run(mut p)
	notify('stop')
	if p.quiet {
		println('Output checksum: $p.get_checksum()')
	}
}
