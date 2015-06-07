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

type Program struct {
	Code       string
	BracketMap map[int]int
}

func NewProgram(text string) (p Program) {
	p.BracketMap = make(map[int]int)
	var leftstack []int

	pc := 0

	for i := 0; i < len(text); i++ {
		ch := text[i]
		chs := string(ch)
		if strings.Contains("[].,+-<>", chs) {
			p.Code += chs
			if ch == '[' {
				leftstack = append(leftstack, pc)
			}
			if ch == ']' && len(leftstack) > 0 {
				lasti := len(leftstack) - 1
				left := leftstack[lasti]
				leftstack = leftstack[:lasti]
				right := pc
				p.BracketMap[left] = right
				p.BracketMap[right] = left
			}
			pc += 1
		}
	}
	return
}

func (p Program) Run() {
	tape := NewTape()

	for pc := 0; pc < len(p.Code); pc += 1 {
		switch p.Code[pc] {
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
				pc = p.BracketMap[pc]
			}
		case ']':
			if tape.Get() != 0 {
				pc = p.BracketMap[pc]
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
