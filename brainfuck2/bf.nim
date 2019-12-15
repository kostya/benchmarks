import net

type
  OpType = enum Inc, Move, Loop, Print
  Ops = seq[Op]
  Op = object
    case op: OpType
    of Inc, Move: val: int
    of Loop: loop: Ops
    else: discard

  StringIterator = iterator(): char
  Tape = object
    pos: int
    tape: seq[int]

  Program = distinct Ops

func initTape(): Tape =
  result.pos = 0
  result.tape = newSeq[int](1)

proc get(t: Tape): int {.inline.} = t.tape[t.pos]
proc inc(t: var Tape, x: int) {.inline.} = t.tape[t.pos] += x
proc move(t: var Tape, x: int) {.inline.} =
  t.pos += x
  while t.pos >= t.tape.len:
    t.tape.setLen 2 * t.tape.len

func newStringIterator(s: string): StringIterator =
  result = iterator(): char =
    for i in s:
      yield i

func parse(iter: StringIterator): Ops =
  for i in iter():
    case i
    of '+': result.add Op(op: Inc, val: 1)
    of '-': result.add Op(op: Inc, val: -1)
    of '>': result.add Op(op: Move, val: 1)
    of '<': result.add Op(op: Move, val: -1)
    of '.': result.add Op(op: Print)
    of '[': result.add Op(op: Loop, loop: parse iter)
    of ']': break
    else: discard

func parse(code: string): Program =
  let iter = newStringIterator code
  result = Program parse iter

proc run(ops: Ops, t: var Tape) =
  for op in ops:
    case op.op
    of Inc: t.inc op.val
    of Move: t.move op.val
    of Loop:
      while t.get() > 0: run(op.loop, t)
    of Print:
      stdout.write t.get().chr()
      stdout.flushFile()

proc run(ops: Program) =
  var tape = initTape()
  run Ops ops, tape

import os

try:
  var socket = newSocket()
  defer: socket.close()
  socket.connect("localhost", Port(9001))
  when defined(gcc):
    socket.send("Nim GCC")
  else:
    socket.send("Nim Clang")
except:
  discard

paramStr(1).readFile().parse().run()
