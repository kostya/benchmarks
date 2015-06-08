#include <vector>
#include <string>
#include <map>
#include <fstream>

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

typedef pair<char, int> Op;

class Program {
  vector<Op> code;

public:
  Program(string text) {
    vector<int> leftstack;
    for (int i = 0; i < text.size(); i++) if (string("[]<>+-,.").find(string(1, text[i])) != string::npos) code.push_back(Op(text[i], 0));
    for (int pc = 0; pc < code.size(); pc++) {
      char c = code[pc].first;
      if (c == '[') leftstack.push_back(pc);
      else
        if (c == ']' && leftstack.size() != 0) {
          int left = leftstack[leftstack.size() - 1];
          leftstack.pop_back();
          int right = pc;
          code[left].second = right;
          code[right].second = left;
        }
    }
  }

  void run() {
    Tape tape;
    int len = code.size();
    for (int pc = 0; pc < len; pc++) {
      Op &op = code[pc];
      switch (op.first) {
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
            if (tape.get() == 0) pc = op.second;
          break;
        case ']':
          if (tape.get() != 0) pc = op.second;
          break;
        case '.':
          printf("%c", tape.get());
          fflush(stdout);
          break;
      }
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

