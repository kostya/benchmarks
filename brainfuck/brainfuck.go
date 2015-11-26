package main

import (
	"fmt"
	"strings"
	"os"
	)

type Program struct {
	Code       string
	BracketMap map[int]int
}

func NewProgram(reader *os.File) (p Program) {
	var leftstack []int

	pc := 0

	ch := make([]byte, 1)
	var chs string
	var err error
	err = reader.Read(ch)
	for err == nil {
		chs = string(ch)
		if strings.Contains("[].,+-<>", chs) {
			p.Code += chs
			if ch == '[' {
				leftstack = append(leftstack, pc)
			}
			if ch == ']' && len(leftstack) > 0 {
				lasti := len(leftstack) - 1
				leftstack, p.BracketMap[left], p.BracketMap[right] = leftstack[:lasti], pc, leftstack[lasti]
			}
			pc += 1
		}
		err = reader.Read(ch)
	}
	return
}

func (p Program) Run() {
	tape := make([]int, 1, 100)
	pos := 1

	for pc := 0; pc < len(p.Code); pc += 1 {
		switch p.Code[pc] {
		case '+':
			tape[pos] += 1
		case '-':
			tape[pos] -= 1
		case '>':
			pos += 1
			if pos >= len(tape) {
				tape = append(tape, 0)
			}
		case '<':
			if pos > 0 {
				pos -= 1
			}
		case '[':
			if tape[pos] == 0 {
				pc = p.BracketMap[pc]
			}
		case ']':
			if tape[pos] != 0 {
				pc = p.BracketMap[pc]
			}
		case '.':
			fmt.Printf("%c", tape[pos])
		}
	}
}

func main() {
	Code, err := os.Open(os.Args[1])
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}

	NewProgram(Code).Run()
}
