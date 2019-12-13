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
        int pos;
        int[] tape;

        public Tape()
        {
            pos = 0;
            tape = new int[1];
        }

        public int Get() { return tape[pos]; }
        public void Inc(int x) { tape[pos] += x; }
        public void Move(int x) { pos += x; while (pos >= tape.Length) Array.Resize(ref tape, tape.Length*2); }
    }

    class Program
    {
        string code;
        int pos;
        Op[] ops;

        Program(string text)
        {
            code = text;
            pos = 0;
            ops = parse();
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
                    case OpT.PRINT: Console.Write((char)tape.Get()); break;
                }
            }
        }

        static void Main(string[] args)
        {
            string text = File.ReadAllText(args[0]);
            var stopWatch = new Stopwatch();
            Console.Error.WriteLine("JIT warming up");

            stopWatch.Start();
            new Program(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++").run();
            stopWatch.Stop();
            Console.Error.WriteLine("time: " + stopWatch.ElapsedMilliseconds / 1e3 + "s");

            Console.Error.WriteLine("run");
            try {
                using (var s = new System.Net.Sockets.TcpClient("localhost", 9001)) {
                    var runtime = Type.GetType("Mono.Runtime") != null ? "Mono" : ".NET Core";
                    var data = System.Text.Encoding.UTF8.GetBytes("C# " + runtime);
                    s.Client.Send(data);
                }
            } catch {
                // standalone usage
            }

            stopWatch.Restart();
            var p = new Program(text);
            p.run();
            stopWatch.Stop();
            Console.Error.WriteLine("time: " + stopWatch.ElapsedMilliseconds / 1e3 + "s");
        }
    }
}
