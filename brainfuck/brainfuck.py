import platform
import sys
import socket
import os
from pathlib import Path

class Tape(object):
    def __init__(self):
        self.tape = [0]
        self.pos = 0
    def get(self):
        return self.tape[self.pos]
    def inc(self):
        self.tape[self.pos] += 1
    def dec(self):
        self.tape[self.pos] -= 1
    def advance(self):
        self.pos += 1
        if len(self.tape) <= self.pos:
            self.tape.append(0)
    def devance(self):
        if self.pos > 0:
            self.pos -= 1

class Program(object):
    def __init__(self, text):
        self.code = ""
        self.bracket_map = {}
        leftstack = []

        pc = 0
        for char in text:
            if char in ('[', ']', '<', '>', '+', '-', ',', '.'):
                self.code += char
                if char == '[':
                    leftstack.append(pc)
                elif char == ']' and len(leftstack) > 0:
                    left = leftstack.pop()
                    right = pc
                    self.bracket_map[left] = right
                    self.bracket_map[right] = left
                pc += 1

    def run(self):
        pc = 0
        tape = Tape()

        while pc < len(self.code):
            char = self.code[pc]
            if char == "+":
                tape.inc()
            elif char == "-":
                tape.dec()
            elif char == ">":
                tape.advance()
            elif char == "<":
                tape.devance()
            elif char == "[" and tape.get() == 0:
                pc = self.bracket_map[pc]
            elif char == "]" and tape.get() != 0:
                pc = self.bracket_map[pc]
            elif char == ".":
                sys.stdout.write(chr(tape.get()))
                sys.stdout.flush()
            pc += 1

def notify(msg):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        if not s.connect_ex(("localhost", 9001)):
            s.sendall(bytes(msg, 'utf8'))

text = Path(sys.argv[1]).read_text()

notify("%s\t%d" % (platform.python_implementation(), os.getpid()))

Program(text).run()

notify("stop")
