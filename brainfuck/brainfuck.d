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
    tape ~= 0;
  }

final:
  int get() { return tape[pos]; }
  void inc() { tape[pos]++; }
  void dec() { tape[pos]--; }
  void advance() { pos++; if (tape.length <= pos) tape ~= 0; }
  void devance() { if (pos > 0) { pos--; } }
};

class Program {
  string code;
  int[int] bracket_map;

  this(string text) {
    int[] leftstack;
    int pc = 0;

    for (int i = 0; i < text.length; i++) {
      char c = text[i];
      if (!canFind("[]<>+-,.", c)) continue;

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
      code ~= c;
    }
  }

  void run() {
    auto tape = new Tape();
    for (int pc = 0; pc < code.length; pc++) {
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
};

int main(string[] args){
  string text = readText(args[1]);
  auto p = new Program(text);
  p.run();
  return 0;
}
