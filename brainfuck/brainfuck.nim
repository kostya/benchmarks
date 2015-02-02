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

proc inc(self: var Tape) =
  self.tape[self.pos] += 1

proc dec(self: var Tape) =
  self.tape[self.pos] -= 1

proc advance(self: var Tape) =
  self.pos += 1
  if len(self.tape) <= self.pos:
    self.tape.add(0)

proc devance(self: var Tape) =
  if self.pos > 0:
    self.pos -= 1

proc get(self: Tape): int =
  result = self.tape[self.pos]

type
  Program = object
    code: string
    bracket_map: Table[int, int]

proc newProgram(code: string): Program =
  result.bracket_map = initTable[int, int]()
  result.code = ""
  var pc = 0
  var leftstack = newSeq[int]()
  for c in code:
    if not "+-<>[].,".contains(c):
      continue
    if c == '[':
      leftstack.add(pc)
    if c == ']' and len(leftstack) > 0:
      var left: int = leftstack.pop
      result.bracket_map.add pc, left
      result.bracket_map.add left, pc
    result.code.add(c)
    pc += 1

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
          pc = prog.bracket_map[pc]
      of ']':
        if tape.get != 0:
          pc = prog.bracket_map[pc]
      of '.':
        write stdout, tape.get.chr
        flushFile(stdout)
      else: discard
    pc += 1

newProgram(readFile(paramStr(1))).run

