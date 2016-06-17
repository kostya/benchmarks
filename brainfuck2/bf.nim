import tables
import sequtils
import os
import strutils

type
  OpT = enum
    INC = 1, MOVE = 2, LOOP = 3, PRINT = 4

type 
  Op = object
    op: OpT
    v: int
    loop: seq[Op]

proc newOp(op1: OpT, v1: int): Op =
  result.op = op1
  result.v = v1

proc newOp(op1: OpT, l1: seq[Op]): Op =
  result.op = op1
  result.loop = l1

type 
  StringIterator = object
    text: string
    pos: int

proc newStringIterator(t: string): StringIterator =
  result.text = t
  result.pos = 0

proc next(self: var StringIterator): char =
  result = if (self.pos < self.text.len): self.text[self.pos] else: 0.chr
  self.pos += 1

type
  Tape = object
    pos: int
    tape: seq[int]

proc newTape(): Tape =
  result.pos = 0
  result.tape = @[0]

proc inc(self: var Tape, x: int) =
  self.tape[self.pos] += x

proc move(self: var Tape, x: int) =
  self.pos += x
  while self.pos >= self.tape.len:
    self.tape.add(0)

proc get(self: Tape): int =
  result = self.tape[self.pos]

type
  Program = object
    ops: seq[Op]

proc parse(it: var StringIterator): seq[Op] = 
  result = newSeq[Op]()
  while true:
    case it.next:
      of '+': result.add(newOp(INC, 1))
      of '-': result.add(newOp(INC, -1))
      of '>': result.add(newOp(MOVE, 1))
      of '<': result.add(newOp(MOVE, -1))
      of '.': result.add(newOp(PRINT, 0))
      of '[': result.add(newOp(LOOP, parse(it)))
      of ']': break
      of 0.chr: break
      else: discard

proc newProgram(code: string): Program =
  var si = newStringIterator(code)
  result.ops = parse(si)

proc runops(program: seq[Op], tape: var Tape) =
  for op in program:
    case op.op:
      of INC: tape.inc(op.v)
      of MOVE: tape.move(op.v)
      of LOOP:
        while tape.get != 0:
          runops(op.loop, tape)
      of PRINT: 
        write stdout, tape.get.chr
        flushFile(stdout)
      else: discard

proc run(self: Program) =
  var tape = newTape()
  runops(self.ops, tape)

var text = readFile(paramStr(1))
newProgram(text).run
