#!/usr/bin/vala
void notify(string message) {
    try {
        var address = new InetAddress.from_string ("127.0.0.1");
        var client = new SocketClient ();
        var conn = client.connect (new InetSocketAddress (address, 9001));
        conn.output_stream.write (message.data);
    } catch (Error e) {
        // Standalone usage
    }
}

void start() {
    var msg = "Vala/";
#if GCC_TEST
    msg += "gcc";
#elif CLANG_TEST
    msg += "clang";
#else
    // The preprocessor directive for the test should be specified
    Process.exit(-1);
#endif
    msg += @"\t $((uint16)Posix.getpid())";
    notify(msg);
}

enum OpT {INC, MOVE, PRINT, LOOP}
namespace Test {
    struct Op {
        public OpT op;
        public int v;
        public Op[] loop;

        public Op(OpT _op, int _v) { op = _op; v = _v; loop = null; }
        public Op.vnull(OpT _op, Op[] _l) { op = _op; loop = _l; v = 0; }
    }

    class Tape {
        private int pos = 0;
        private int[] tape = new int[1];

        public int Get() { return tape[pos]; }
        public void Inc(int x) { tape[pos] += x; }
        public void Move(int x) { pos += x; while (pos >= tape.length) tape.resize(tape.length*2);}
    }

    class Printer {
        private int sum1 = 0;
        private int sum2 = 0;
        public bool quiet;

        public Printer(bool quiet) {
            this.quiet = quiet;
        }

        public void print(int n) {
            if (quiet) {
                sum1 = (sum1 + n) % 255;
                sum2 = (sum2 + sum1) % 255;
            } else {
                stdout.printf("%c", (char)n);
                stdout.flush();
            }
        }

        public int checksum {
            get {
                return (sum2 << 8) | sum1;
            }
        }
    }

    class Program {
        private string code;
        private int pos = 0;
        private Op[] ops;
        private Printer p;

        Program(string text, Printer p) {
            code = text;
            ops = parse();
            this.p = p;
        }

        private Op[] parse() {
            Op[] res = {};
            while (pos < code.length) {
                char c = code[pos];
                pos++;
                switch (c) {
                    case '+': res += (Op(OpT.INC, 1)); break;
                    case '-': res += (Op(OpT.INC, -1)); break;
                    case '>': res += (Op(OpT.MOVE, 1)); break;
                    case '<': res += (Op(OpT.MOVE, -1)); break;
                    case '.': res += (Op(OpT.PRINT, 0)); break;
                    case '[': res += (Op.vnull(OpT.LOOP, parse())); break;
                    case ']': return res;
                }
            }
            return res;
        }

        public void run() {
            _run(ops, new Tape());
        }

        private void _run(Op[] program, Tape tape) {
            for (int i=0;i<program.length;i++) {
                switch (program[i].op) {
                case OpT.INC: tape.Inc(program[i].v); break;
                case OpT.MOVE: tape.Move(program[i].v); break;
                case OpT.LOOP: while (tape.Get() > 0) _run(program[i].loop, tape); break;
                case OpT.PRINT: p.print(tape.Get());break;
                }
            }
        }

        static void verify() {
            var text = """++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
                ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.""";
            var p_left = new Printer(true);
            new Program(text, p_left).run();
            var left = p_left.checksum;

            var p_right = new Printer(true);
            foreach (int c in "Hello World!\n".data) {
                p_right.print(c);
            }
            var right = p_left.checksum;
            if (left != right) {
                stderr.printf("%d != %d\n", left, right);
                Process.exit(1);
            }
        }

        static void main(string[] args) {
            verify();
            string text;
            try {
                FileUtils.get_contents(args[1], out text);
            } catch (FileError e) {
                stdout.printf("Error: %s\n", e.message);
            }
            if (text.length == 0) {
                Process.exit(1);
            }
            var p = new Printer(Environment.get_variable("QUIET") != null);

            start();
            var timer = new Timer();
            new Program(text, p).run();
            timer.stop();
            notify("stop");

            message("time: " + timer.elapsed().to_string() + " s");

            if (p.quiet) {
                stdout.printf("Output checksum: %d\n", p.checksum);
            }
        }
    }
}