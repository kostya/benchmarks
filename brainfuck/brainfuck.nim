import tables
import sequtils
import os
import strutils

type
  Tape = object
    pos: int
    tape: seq[int]

proc newTape(): Tape =
  result.pos = 0
  result.tape = @[0]

{.this: self.}

proc inc(self: var Tape) =
  inc tape[pos]

proc dec(self: var Tape) =
  dec tape[pos]

proc advance(self: var Tape) =
  inc pos
  if len(tape) <= pos:
    tape.add(0)

proc devance(self: var Tape) =
  if pos > 0:
    dec pos

template get(self: Tape) =
  self.tape[self.pos]

type
  Program = object
    code: string
    bracketMap: Table[int, int]

proc newProgram(code: string): Program =
  result.bracketMap = initTable[int, int]()
  result.code = ""
  var pc = 0
  var leftstack = newSeq[int]()
  for c in code:
    if c notin {'+', '-', '<', '>', '[', ']', '.', ','}:
      continue
    elif c == '[':
      leftstack.add(pc)
    elif c == ']' and len(leftstack) > 0:
      var left: int = leftstack.pop
      result.bracketMap.add pc, left
      result.bracketMap.add left,  pc
    result.code.add(c)
    inc pc

proc run(prog: Program) =
  var pc: int = 0
  var tape = newTape()

  while pc < len(prog.code):
    case prog.code[pc]:
      of '+':
        tape.inc
      of '-':
        tape.dec
      of '>':
        tape.advance
      of '<':
        tape.devance
      of '[':
        if tape.get == 0:
          pc = prog.bracketMap[pc]
      of ']':
        if tape.get != 0:
          pc = prog.bracketMap[pc]
      of '.':
        write stdout, tape.get.char
        flushFile(stdout)
      else: discard
    inc pc

newProgram(readFile(paramStr(1))).run

