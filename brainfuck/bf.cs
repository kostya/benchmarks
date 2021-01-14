using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Text;

namespace Test
{
    enum OpT {INC, MOVE, PRINT, LOOP};

    struct Op {
        public OpT op;
        public int v;
        public Op[] loop;

        public Op(OpT _op, int _v) { op = _op; v = _v; loop = null; }
        public Op(OpT _op, Op[] _l) { op = _op; loop = _l; v = 0; }
    }

    public class Tape
    {
        int pos = 0;
        int[] tape = new int[1];

        public int Get() { return tape[pos]; }
        public void Inc(int x) { tape[pos] += x; }
        public void Move(int x) { pos += x; while (pos >= tape.Length) Array.Resize(ref tape, tape.Length*2); }
    }

    class Printer {
        int sum1 = 0;
        int sum2 = 0;

        public bool Quiet { get; set; }

        public void Print(int n) {
            if (Quiet) {
                sum1 = (sum1 + n) % 255;
                sum2 = (sum2 + sum1) % 255;
            } else {
                Console.Write((char)n);
            }
        }

        public int Checksum {
            get {
                return (sum2 << 8) | sum1;
            }
        }
    }

    class Program
    {
        string code;
        int pos = 0;
        Op[] ops;
        Printer p;

        Program(string text, Printer p)
        {
            code = text;
            ops = parse();
            this.p = p;
        }

        private Op[] parse() {
            List<Op> res = new List<Op>();
            while (pos < code.Length) {
                char c = code[pos];
                pos++;
                switch (c) {
                    case '+': res.Add(new Op(OpT.INC, 1)); break;
                    case '-': res.Add(new Op(OpT.INC, -1)); break;
                    case '>': res.Add(new Op(OpT.MOVE, 1)); break;
                    case '<': res.Add(new Op(OpT.MOVE, -1)); break;
                    case '.': res.Add(new Op(OpT.PRINT, 0)); break;
                    case '[': res.Add(new Op(OpT.LOOP, parse())); break;
                    case ']': return res.ToArray();
                }
            }
            return res.ToArray();
        }

        public void run() {
            _run(ops, new Tape());
        }

        private void _run(Op[] program, Tape tape) {
            foreach (Op op in program) {
                switch (op.op) {
                    case OpT.INC: tape.Inc(op.v); break;
                    case OpT.MOVE: tape.Move(op.v); break;
                    case OpT.LOOP: while (tape.Get() > 0) _run(op.loop, tape); break;
                    case OpT.PRINT: p.Print(tape.Get()); break;
                }
            }
        }

        private static void Notify(string msg) {
            try {
                using (var s = new System.Net.Sockets.TcpClient("localhost", 9001)) {
                    var data = System.Text.Encoding.UTF8.GetBytes(msg);
                    s.Client.Send(data);
                }
            } catch {
                // standalone usage
            }
        }

        private static void Verify() {
            var text = @"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
                ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
            var p_left = new Printer{Quiet = true};
            new Program(text, p_left).run();
            var left = p_left.Checksum;

            var p_right = new Printer{Quiet = true};
            foreach (var c in "Hello World!\n") {
                p_right.Print(c);
            }
            var right = p_right.Checksum;
            if (left != right) {
                Console.Error.WriteLine($"{left} != {right}");
                System.Environment.Exit(1);
            }
        }

        static void Main(string[] args)
        {
            Verify();
            var text = File.ReadAllText(args[0]);
            var p = new Printer {
                Quiet = Environment.GetEnvironmentVariable("QUIET") != null
            };

            var runtime = Type.GetType("Mono.Runtime") != null ? "Mono" : ".NET Core";
            Notify($"C#/{runtime}\t{Process.GetCurrentProcess().Id}");
            var stopWatch = Stopwatch.StartNew();

            new Program(text, p).run();
            stopWatch.Stop();
            var elapsed = stopWatch.ElapsedMilliseconds / 1e3;

            Notify("stop");
            Console.Error.WriteLine($"time: {elapsed}s");

            if (p.Quiet) {
                Console.WriteLine($"Output checksum: {p.Checksum}");
            }
        }
    }
}
