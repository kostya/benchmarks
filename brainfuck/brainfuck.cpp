#include <vector>
#include <string>
#include <map>
#include <fstream>

#ifndef USE_CODE_VECTOR
#define USE_CODE_VECTOR 1
#endif

#ifndef USE_FLAT_MAP
#define USE_FLAT_MAP 1
#endif

#ifndef USE_CONST_SIZE
#define USE_CONST_SIZE 1
#endif

#ifndef USE_COMPACT_OPCODES
#define USE_COMPACT_OPCODES 1
#endif

using namespace std;

class Tape {
  int pos;
  vector<int> tape;

public:
  Tape() {
    pos = 0;
    tape.push_back(0);
  }

  int get() { return tape[pos]; }
  void inc() { tape[pos]++; }
  void dec() { tape[pos]--; }
  void advance() { pos++; if (tape.size() <= pos) tape.push_back(0); }
  void devance() { if (pos > 0) pos--; }
};

class Program {
#if USE_CODE_VECTOR
  vector<char> code;
#else
  string code;
#endif
#if USE_FLAT_MAP
  vector<int> bracket_map;
#else
  map<int, int> bracket_map;
#endif

#if USE_COMPACT_OPCODES
  enum OP {
      INC,
      DEC,
      ADV,
      DEV,
      JFW,
      JBW,
      PRI,
  };
#endif

public:
  Program(string text) {
    vector<int> leftstack;
    string symbols = "[]<>+-,.";

    int pc = 0;
    for (int i = 0; i < text.size(); i++) {
      char c = text[i];
      string str = string(1, c);

      if (symbols.find(str) == string::npos) continue;

      if (c == '[') leftstack.push_back(pc);
      else
        if (c == ']' && leftstack.size() != 0) {
          int left = leftstack[leftstack.size() - 1];
          leftstack.pop_back();
          int right = pc;
#if USE_FLAT_MAP
          bracket_map.resize(max(bracket_map.size(), size_t(left) + 1));
          bracket_map.resize(max(bracket_map.size(), size_t(right) + 1));
#endif
          bracket_map[left] = right;
          bracket_map[right] = left;
        }

      pc++;
#if USE_COMPACT_OPCODES
      switch(c)
      {
      case '+': code.push_back(INC); break;
      case '-': code.push_back(DEC); break;
      case '>': code.push_back(ADV); break;
      case '<': code.push_back(DEV); break;
      case '[': code.push_back(JFW); break;
      case ']': code.push_back(JBW); break;
      case '.': code.push_back(PRI); break;
      }
#else
#if USE_CODE_VECTOR
      code.push_back(c);
#else
      code += str;
#endif
#endif
    }
  }

  void run() {
    Tape tape;
#if USE_CONST_SIZE
#   if USE_CODE_VECTOR
    const int sz = code.size();
#   else
    const int sz = code.length();
#   endif
#endif

#if USE_CONST_SIZE
    for (int pc = 0; pc < sz; pc++) {
#else
#   if USE_CODE_VECTOR
    for (int pc = 0; pc < code.size(); pc++) {
#   else
    for (int pc = 0; pc < code.length(); pc++) {
#   endif
#endif

#if USE_COMPACT_OPCODES
      switch(code[pc]) {
        case INC: tape.inc(); break;
        case DEC: tape.dec(); break;
        case ADV: tape.advance(); break;
        case DEV: tape.devance(); break;
        case JFW: if (tape.get() == 0) pc = bracket_map[pc]; break;
        case JBW: if (tape.get() != 0) pc = bracket_map[pc]; break;
        case PRI: printf("%c", tape.get()); fflush(stdout); break;
      }
#else
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
          printf("%c", tape.get());
          fflush(stdout);
          break;
      }
#endif
    }
  }
};

string read_file(string filename){
  ifstream textstream(filename.c_str());
  textstream.seekg(0, ios_base::end);
  const int lenght = textstream.tellg();
  textstream.seekg(0);
  string text(lenght, ' ');
  textstream.read(&text[0], lenght);
  textstream.close();
  return text;
}

int main(int argc, char** argv){
  string text = read_file(string(argv[1]));
  Program p(text);
  p.run();
  return 0;
}

