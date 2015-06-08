import sys

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
        self.code = []
        self.maps = []
        leftstack = []

        for char in text:
            if char in ('[', ']', '<', '>', '+', '-', ',', '.'):
                self.code.append(char)
                self.maps.append(0)

        for pc, op in enumerate(self.code):
            char = op[0]
            if char == '[':
                leftstack.append(pc)
            elif char == ']' and len(leftstack) > 0:
                left = leftstack.pop()
                right = pc
                self.maps[left] = right
                self.maps[right] = left

    def run(self):
        pc = 0
        tape = Tape()
        length = len(self.code)

        while pc < length:
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
                pc = self.maps[pc]
            elif char == "]" and tape.get() != 0:
                pc = self.maps[pc]
            elif char == ".":
                sys.stdout.write(chr(tape.get()))
                sys.stdout.flush()
            pc += 1

Program(open(sys.argv[1], 'r').read()).run()
