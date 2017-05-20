import std.algorithm;
import std.stdio;
import std.file;
import std.array;
import std.conv;

class Tape {
  int pos;
  int[] tape;

  this() {
    pos = 0;
    tape.assumeSafeAppend ~= 0;
  }

final:
  int get() { return tape[pos]; }
  void inc() { tape[pos]++; }
  void dec() { tape[pos]--; }
  void advance() { pos++; if (tape.length <= pos) tape.assumeSafeAppend ~= 0; }
  void devance() { if (pos > 0) { pos--; } }
}

class Program {
  int[] code;
  int[] bracket_map;

  this(string text) {
    int[] leftstack;
    bracket_map = [0];
    int pc = 0;

    for (int i = 0; i < text.length; i++) {
      char c = text[i];
      if (!canFind(cast(immutable(ubyte)[])"[]<>+-,.", c)) continue;

      if (c == '[') leftstack ~= pc;
      else
        if (c == ']' && leftstack.length != 0) {
          int left = leftstack[leftstack.length - 1];
          leftstack.popBack();
          int right = pc;
          bracket_map[left] = right;
          bracket_map[right] = left;
        }

      pc++;
      code.assumeSafeAppend ~= c;
      bracket_map.assumeSafeAppend ~= 0;
    }
  }

  void run() {
    auto tape = new Tape();
    immutable int len = cast(int) code.length;
    for (int pc = 0; pc < len; pc++) {
      switch (code[pc]) {
        case '+':
          tape.inc();
          break;
        case '-':
          tape.dec();
          break;
        case '>':
          tape.advance();
          break;
        case '<':
          tape.devance();
          break;
        case '[':
          if (tape.get() == 0) pc = bracket_map[pc];
          break;
        case ']':
          if (tape.get() != 0) pc = bracket_map[pc];
          break;
        case '.':
          write(tape.get().to!char);
          stdout.flush();
          break;
        default:
          break;
      }
    }
  }
}

int main(string[] args){
  string text = readText(args[1]);
  auto p = new Program(text);
  p.run();
  return 0;
}
