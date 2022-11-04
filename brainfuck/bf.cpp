#include <fstream>
#include <iostream>
#include <libnotify.h>
#include <vector>

#ifdef __clang__
static constexpr auto COMPILER = "clang++";
#else
static constexpr auto COMPILER = "g++";
#endif

using namespace std;

enum op_type { INC, MOVE, LOOP, PRINT };
struct Op;
typedef vector<Op> Ops;

struct Op {
  op_type op;
  int val;
  Ops loop;
  Op(Ops v) : op(LOOP), val{}, loop(v) {}
  Op(op_type _op, int v = 0) : op(_op), val(v), loop{} {}
};

class Tape {
  using tape_type = vector<int>;
  tape_type::size_type pos;
  tape_type tape;

public:
  Tape() : pos(0), tape{} { tape.push_back(0); }

  int get() { return tape[pos]; }
  void inc(int x) { tape[pos] += x; }
  void move(int x) {
    pos += x;
    if (pos >= tape.size())
      tape.resize(2 * tape.size());
  }
};

class Printer {
  int sum1;
  int sum2;

public:
  bool quiet;

  Printer(bool quiet) : sum1(0), sum2(0), quiet(quiet) {}

  void print(int n) {
    if (quiet) {
      sum1 = (sum1 + n) % 255;
      sum2 = (sum2 + sum1) % 255;
    } else {
      cout << static_cast<char>(n);
    }
  }

  int get_checksum() const { return (sum2 << 8) | sum1; }
};

class Program {
  Ops ops;
  Printer &p;

public:
  Program(const string &code, Printer &p) : ops{}, p(p) {
    string::const_iterator iterator = code.cbegin();
    ops = parse(iterator, code.cend());
  }

  void run() {
    Tape tape;
    _run(ops, tape);
  }

private:
  Ops parse(string::const_iterator &iterator, string::const_iterator end) {
    Ops res;
    while (iterator != end) {
      const auto c = *iterator++;
      switch (c) {
      case '+':
        res.push_back(Op(INC, 1));
        break;
      case '-':
        res.push_back(Op(INC, -1));
        break;
      case '>':
        res.push_back(Op(MOVE, 1));
        break;
      case '<':
        res.push_back(Op(MOVE, -1));
        break;
      case '.':
        res.push_back(Op(PRINT));
        break;
      case '[':
        res.push_back(Op(parse(iterator, end)));
        break;
      case ']':
        return res;
      }
    }
    return res;
  }

  void _run(const Ops &program, Tape &tape) {
    for (const auto &op : program) {
      switch (op.op) {
      case INC:
        tape.inc(op.val);
        break;
      case MOVE:
        tape.move(op.val);
        break;
      case LOOP:
        while (tape.get() > 0)
          _run(op.loop, tape);
        break;
      case PRINT:
        p.print(tape.get());
        break;
      }
    }
  }
};

string read_file(const string &filename) {
  ifstream file{filename};
  if (file.fail()) {
    return {};
  }
  return string{istreambuf_iterator<char>{file}, {}};
}

void verify() {
  const auto text = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>"
                    "---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."s;
  Printer p_left(true);
  Program(text, p_left).run();
  const auto left = p_left.get_checksum();

  Printer p_right(true);
  for (const auto &c : "Hello World!\n"s) {
    p_right.print(c);
  }
  const auto right = p_right.get_checksum();
  if (left != right) {
    cerr << left << " != " << right << endl;
    exit(EXIT_FAILURE);
  }
}

int main(int argc, char **argv) {
  verify();
  if (argc < 2) {
    exit(EXIT_FAILURE);
  }
  const auto text = read_file(string(argv[1]));
  Printer p(getenv("QUIET") != nullptr);
  cout << unitbuf; // enable automatic flushing

  notifying_invoke([&]() { Program(text, p).run(); }, "C++/{}", COMPILER);

  if (p.quiet) {
    cout << "Output checksum: " << p.get_checksum() << endl;
  }
}
