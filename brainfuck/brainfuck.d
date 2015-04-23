import std.stdio;
import std.file;
import std.conv;

struct Tape {
  int pos;
  int[] tape;

  @disable this();

  this(int[] t) {
    tape = t;
  }

  int get() { return tape[pos]; }
  void inc() { tape[pos]++; }
  void dec() { tape[pos]--; }
  void advance() { pos++; if (tape.length <= pos) tape ~= 0; }
  void devance() { if (pos > 0) { pos--; } }
};

struct Program {
  string code;
  int[int] bracket_map;

  this(string text) {
    code = text;
  }

  int jump(int spc) {
    if ( auto adr = (spc in bracket_map) ) {
      return *adr;
    }
    int epc = spc+1;
    for(int bra = 1; bra > 0 && epc < code.length; epc++){
      if (code[epc] == '[') bra++;
      if (code[epc] == ']') bra--;
    };
    bracket_map[spc] = epc - 1;
    return epc - 1;
  }

  void run() { 
    int[1024] storage;
    auto tape = Tape(storage);

    int run(int spc) {
      int pc = spc;

      for(pc= spc; pc < code.length; pc++) {
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
            if(tape.get() != 0) pc = run(pc+1);
            else pc = jump(pc);
            break;
          case ']':
            if (tape.get() != 0) pc = spc - 1;
            else return pc;
            break;
          case '.':
            write(tape.get().to!char);
            stdout.flush();
            break;
          default:
            break;
        }
      }
      return pc;
    }

    run(0);
  }
};

int main(string[] args){
  readText(args[1]).Program().run();
  return 0;
}
