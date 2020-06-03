import platform
import socket
import sys
import os
from pathlib import Path

INC = 1
MOVE = 2
LOOP = 3
PRINT = 4

class Op(object):
    def __init__(self, op, val):
        self.op = op
        self.val = val

class Tape(object):
    def __init__(self):
        self.tape = [0]
        self.pos = 0
    def get(self):
        return self.tape[self.pos]
    def inc(self, x):
        self.tape[self.pos] += x
    def move(self, x):
        self.pos += x
        while self.pos >= len(self.tape):
            self.tape.append(0)

def parse(iterator):
    res = []
    while True:
        try:
            c = iterator.__next__()
        except StopIteration:
            break

        if c == "+":
            res.append(Op(INC, 1))
        elif c == "-":
            res.append(Op(INC, -1))
        elif c == ">":
            res.append(Op(MOVE, 1))
        elif c == "<":
            res.append(Op(MOVE, -1))
        elif c == ".":
            res.append(Op(PRINT, 0))
        elif c == "[":
            res.append(Op(LOOP, parse(iterator)))
        elif c == "]":
            break

    return res

def _run(program, tape):
    for op in program:
        if op.op == INC: tape.inc(op.val)
        elif op.op == MOVE: tape.move(op.val)
        elif op.op == LOOP: 
            while tape.get() > 0:
                _run(op.val, tape)
        elif op.op == PRINT:
            sys.stdout.write(chr(tape.get()))
            sys.stdout.flush()

class Program(object):
    def __init__(self, code):
        self.ops = parse(iter(code))

    def run(self):
        _run(self.ops, Tape())

def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, 'utf8'))

text = Path(sys.argv[1]).read_text()

notify("%s\t%d" % (platform.python_implementation(), os.getpid()))
Program(text).run()
notify("stop")
