package main

import (
	"benchmark"
	"fmt"
)

const (
	INC = iota
	MOVE
	PRINT
	LOOP
)

type Op struct {
	O    int
	V    int
	Loop []Op
}

func NewOp(op int, v int) Op {
	return Op{O: op, V: v}
}

func NewOpLoop(op int, l []Op) Op {
	return Op{O: op, Loop: l}
}

type StringIterator struct {
	Text []byte
	Pos  int
}

func NewStringIterator(s string) *StringIterator {
	si := &StringIterator{Text: []byte(s), Pos: 0}
	return si
}

func (si *StringIterator) Next() byte {
	if si.Pos < len(si.Text) {
		res := si.Text[si.Pos]
		si.Pos += 1
		return res
	} else {
		return byte(0)
	}
}

type Tape struct {
	tape []int
	pos  int
}

func NewTape() *Tape {
	t := &Tape{pos: 0}
	t.tape = make([]int, 1)
	return t
}

func (t *Tape) Inc(x int) {
	t.tape[t.pos] += x
}

func (t *Tape) Move(x int) {
	t.pos += x
	for t.pos >= len(t.tape) {
		t.tape = append(t.tape, 0)
	}
}

func (t *Tape) Get() int {
	return t.tape[t.pos]
}

type Printer struct {
	sum1  int
	sum2  int
	quiet bool
}

func NewPrinter(quiet bool) *Printer {
	return &Printer{sum1: 0, sum2: 0, quiet: quiet}
}

func (p *Printer) Print(n int) {
	if p.quiet {
		p.sum1 = (p.sum1 + n) % 255
		p.sum2 = (p.sum2 + p.sum1) % 255
	} else {
		fmt.Printf("%c", n)
	}
}

func (p *Printer) GetChecksum() int {
	return (p.sum2 << 8) | p.sum1
}

func (p *Printer) Reset(quiet bool) {
	p.sum1 = 0
	p.sum2 = 0
	p.quiet = quiet
}

type Program struct {
	Ops []Op
}

func NewProgram(code string) *Program {
	return &Program{Ops: parse(NewStringIterator(code))}
}

func (p *Program) Run(printer benchmark.Printer) {
	_run(p.Ops, NewTape(), printer)
}

func parse(si *StringIterator) []Op {
	res := make([]Op, 0)
	for {
		c := si.Next()
		var op Op
		switch c {
		case '+':
			op = NewOp(INC, 1)
		case '-':
			op = NewOp(INC, -1)
		case '>':
			op = NewOp(MOVE, 1)
		case '<':
			op = NewOp(MOVE, -1)
		case '.':
			op = NewOp(PRINT, 0)
		case '[':
			op = NewOpLoop(LOOP, parse(si))
		case ']':
			return res
		case byte(0):
			return res
		default:
			continue
		}
		res = append(res, op)
	}
}

func _run(program []Op, tape *Tape, p benchmark.Printer) {
	for i := 0; i < len(program); i++ {
		switch program[i].O {
		case INC:
			tape.Inc(program[i].V)
		case MOVE:
			tape.Move(program[i].V)
		case LOOP:
			for tape.Get() > 0 {
				_run(program[i].Loop, tape, p)
			}
		case PRINT:
			p.Print(tape.Get())
		}
	}
}

func main() {
	err := benchmark.Run(func(s string, printer benchmark.Printer) {
		NewProgram(s).Run(printer)
	}, NewPrinter(true))
	if err != nil {
		panic(err)
	}
}
