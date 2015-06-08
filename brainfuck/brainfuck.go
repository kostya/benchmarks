package main

import "fmt"
import "strings"
import "os"
import "io/ioutil"

type Tape struct {
	tape []int
	pos  int
}

func NewTape() *Tape {
	t := &Tape{pos: 0}
	t.tape = make([]int, 1)
	t.Advance()
	return t
}

func (t *Tape) Inc() {
	t.tape[t.pos] += 1
}

func (t *Tape) Dec() {
	t.tape[t.pos] -= 1
}

func (t *Tape) Advance() {
	t.pos += 1
	if t.pos >= len(t.tape) {
		t.tape = append(t.tape, 0)
	}
}

func (t *Tape) Devance() {
	if t.pos > 0 {
		t.pos -= 1
	}
}

func (t *Tape) Get() int {
	return t.tape[t.pos]
}

type Op struct {
	Char byte
	Jump int
}

type Program struct {
	Code []Op
}

func NewProgram(text string) (p Program) {
	p.Code = make([]Op, 0)
	var leftstack []int

	for i := 0; i < len(text); i++ {
		if strings.Contains("[].,+-<>", string(text[i])) {
			p.Code = append(p.Code, Op{text[i], 0})
		}
	}

	for pc := 0; pc < len(p.Code); pc++ {
		ch := p.Code[pc].Char
		if ch == '[' {
			leftstack = append(leftstack, pc)
		}
		if ch == ']' && len(leftstack) > 0 {
			lasti := len(leftstack) - 1
			left := leftstack[lasti]
			leftstack = leftstack[:lasti]
			right := pc
			p.Code[left].Jump = right
			p.Code[right].Jump = left
		}
	}
	return
}

func (p Program) Run() {
	tape := NewTape()
	len := len(p.Code)

	for pc := 0; pc < len; pc += 1 {
		op := p.Code[pc]
		switch op.Char {
		case '+':
			tape.Inc()
		case '-':
			tape.Dec()
		case '>':
			tape.Advance()
		case '<':
			tape.Devance()
		case '[':
			if tape.Get() == 0 {
				pc = op.Jump
			}
		case ']':
			if tape.Get() != 0 {
				pc = op.Jump
			}
		case '.':
			fmt.Printf("%c", tape.Get())
		}
	}
}

func main() {
	Code, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}

	NewProgram(string(Code)).Run()
}
