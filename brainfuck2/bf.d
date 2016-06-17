import std.algorithm;
import std.stdio;
import std.file;
import std.array;
import std.conv;

final:

enum OpT { INC, MOVE, PRINT, LOOP };

struct Op {
  OpT op;
  int v;
  Op[] loop;

  this(OpT t, int _v) { op = t; v = _v; }
  this(OpT t, Op[] _l) { op = t; loop = _l; }
};

class StringIterator {
  string text;
  int pos;
  this(string t) { text = t; pos = 0; }
  char next() { return (pos < text.length) ? text[pos++] : 0.to!char; }
};

class Tape {
  int pos;
  int[] tape;

  this() {
    pos = 0;
    tape ~= 0;
  }

final:
  int get() { return tape[pos]; }
  void inc(int x) { tape[pos] += x; }
  void move(int x) { 
    int new_pos = pos + x;
    if (new_pos >= tape.length) {
      foreach (_; 0..new_pos - tape.length + 1) { tape ~= 0; }
      if (tape.length <= pos) tape ~= 0; 
    }

    if (new_pos >= 0) pos = new_pos;
  }
};

class Program {
  Op[] ops;

  this(string code) {
    ops = parse(new StringIterator(code));
  }

  void run() {
    _run(ops, new Tape());
  }

  void _run(Op[] program, Tape tape) {
    foreach (op; program) {
      switch (op.op) {
        case OpT.INC: tape.inc(op.v); break;
        case OpT.MOVE: tape.move(op.v); break;
        case OpT.LOOP: while (tape.get() != 0) _run(op.loop, tape); break;
        case OpT.PRINT: write(tape.get().to!char); stdout.flush(); break;
        default: break;
      }
    }
  }

  Op[] parse(StringIterator it) {
    Op[] res;

    while (true) {
      char c = it.next();
      if (c.to!int == 0) break;

      switch(c) {
        case '+': res ~= Op(OpT.INC, 1); break;
        case '-': res ~= Op(OpT.INC, -1); break;
        case '>': res ~= Op(OpT.MOVE, 1); break;
        case '<': res ~= Op(OpT.MOVE, -1); break;
        case '.': res ~= Op(OpT.PRINT, 0); break;
        case '[': res ~= Op(OpT.LOOP, parse(it)); break;
        case ']': return res;
        default: break;
      }
    }

    return res;
  }
};


int main(string[] args){
  string text = readText(args[1]);
  new Program(text).run();
  return 0;
}
