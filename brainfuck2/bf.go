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

type Op struct {
  O int 
  V int
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
  Pos int
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
  t.Move(1)
  return t
}

func (t *Tape) Inc(x int) {
  t.tape[t.pos] += x
}

func (t *Tape) Move(x int) {
  t.pos += x
  for t.pos >= len(t.tape) { t.tape = append(t.tape, 0) }
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
  res := make([]Op, 0)
  for true {
    c := si.Next()
    var op Op
    switch c {
      case '+': op = NewOp(INC, 1)
      case '-': op = NewOp(INC, -1)
      case '>': op = NewOp(MOVE, 1)
      case '<': op = NewOp(MOVE, -1)
      case '.': op = NewOp(PRINT, 0)
      case '[': op = NewOpLoop(LOOP, parse(si))
      case ']': return res
      case byte(0): return res
    }
    res = append(res, op)
  }
  return res
}

func _run(program []Op, tape *Tape) {
  for _, op := range(program) {
    switch op.O {
      case INC: tape.Inc(op.V)
      case MOVE: tape.Move(op.V)
      case LOOP: for tape.Get() != 0 { _run(op.Loop, tape) }
      case PRINT: fmt.Printf("%c", tape.Get())
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
