package main

import "fmt"
import "os"
import "io/ioutil"

const (
	INC = iota
	MOVE
	PRINT
	LOOP
)

type Op func(*Tape)

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
		si.Pos++
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
	t.Move(1)
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

type Program struct {
	Ops []Op
}

func NewProgram(code string) *Program {
	return &Program{Ops: parse(NewStringIterator(code))}
}

func (p *Program) Run() {
	_run(p.Ops, NewTape())
}

func parse(si *StringIterator) []Op {
	var res []Op
	var do Op
	for true {
		c := si.Next()
		switch c {
		case '+':
			do = func(t *Tape) {
				t.Inc(1)
			}
		case '-':
			do = func(t *Tape) {
				t.Inc(-1)
			}
		case '>':
			do = func(t *Tape) {
				t.Move(1)
			}
		case '<':
			do = func(t *Tape) {
				t.Move(-1)
			}
		case '.':
			do = func(t *Tape) {
				fmt.Printf("%c", t.tape[t.pos])
			}
		case '[':
			program := parse(si)
			do = func(t *Tape) {
				for t.Get() != 0 {
					// Call run as long as val != 0
					_run(program, t)
				}
			}
		case ']', 0:
			return res
		default:
			do = func(t *Tape) {} // no-op
		}
		res = append(res, do)
	}
	return res
}

func _run(program []Op, tape *Tape) {
	for _, op := range program {
		op(tape)
	}
}

func main() {
	Code, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}
	NewProgram(string(Code)).Run()
}
