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
  Op = tuple[ch: char, jump: int]
  Program = object
    code: seq[Op]

proc newProgram(text: string): Program =
  result.code = newSeq[Op]()
  var leftstack = newSeq[int]()

  for c in text:
    if "+-<>[].,".contains(c): result.code.add((c, 0))

  var pc = 0
  for op in result.code:
    var c = op.ch
    if c == '[':
      leftstack.add(pc)
    if c == ']' and len(leftstack) > 0:
      var left: int = leftstack.pop
      result.code[left] = (result.code[left].ch, pc)
      result.code[pc] = (c, left)
    pc += 1

proc run(prog: Program) =
  var pc: int = 0
  var tape = newTape()
  let length = prog.code.len

  while pc < length:
    let op = prog.code[pc]
    case op.ch:
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
          pc = op.jump
      of ']':
        if tape.get != 0:
          pc = op.jump
      of '.':
        write stdout, tape.get.chr
        flushFile(stdout)
      else: discard
    pc += 1

newProgram(readFile(paramStr(1))).run

