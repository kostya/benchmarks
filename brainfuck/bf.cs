using System;
using System.Diagnostics;
using System.IO;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace Test
{
    enum OpT { INC, MOVE, PRINT, LOOP };

    struct Op
    {
        public OpT op;
        public int v;
        public Op[] loop;

        public Op(OpT _op, int _v)
        {
            op = _op;
            v = _v;
            loop = null;
        }

        public Op(OpT _op, Op[] _l)
        {
            op = _op;
            loop = _l;
            v = 0;
        }
    }

    public class Tape
    {
        int pos = 0;
        int[] tape = new int[1];
        public int CurrentCell
        {
            get => tape[pos];
            set => tape[pos] = value;
        }

        public void Inc(int x) => CurrentCell += x;
        public void Move(int x)
        {
            pos += x;
            if (pos >= tape.Length)
                Array.Resize(ref tape, tape.Length * 2);
        }
    }

    class Printer
    {
        int sum1 = 0;
        int sum2 = 0;
        public int Checksum
        {
            get => (sum2 << 8) | sum1;
        }

        public bool Quiet { get; set; }

        public void Print(int n)
        {
            if (Quiet)
            {
                sum1 = (sum1 + n) % 255;
                sum2 = (sum2 + sum1) % 255;
            }
            else
            {
                Console.Write((char)n);
            }
        }
    }

    class Program
    {
        Op[] ops;
        Printer p;

        Program(string text, Printer p)
        {
            ops = parse(text.GetEnumerator());
            this.p = p;
        }

        private Op[] parse(IEnumerator<char> it)
        {
            List<Op> res = new List<Op>();
            while (it.MoveNext())
            {
                switch (it.Current)
                {
                    case '+': res.Add(new Op(OpT.INC, 1)); break;
                    case '-': res.Add(new Op(OpT.INC, -1)); break;
                    case '>': res.Add(new Op(OpT.MOVE, 1)); break;
                    case '<': res.Add(new Op(OpT.MOVE, -1)); break;
                    case '.': res.Add(new Op(OpT.PRINT, 0)); break;
                    case '[': res.Add(new Op(OpT.LOOP, parse(it))); break;
                    case ']': return res.ToArray();
                }
            }
            return res.ToArray();
        }

        public void run() => _run(ops, new Tape());

        [MethodImpl(MethodImplOptions.AggressiveOptimization)]
        private void _run(Op[] program, Tape tape)
        {
            for (var index = 0; index < program.Length; index++)
            {
                var op = program[index];
                if (op.op == OpT.INC)
                    tape.Inc(op.v);
                else if (op.op == OpT.MOVE)
                    tape.Move(op.v);
                else if (op.op == OpT.LOOP)
                    while (tape.CurrentCell > 0)
                        _run(op.loop, tape);
                else if (op.op == OpT.PRINT) p.Print(tape.CurrentCell);
            }
        }

        private static void Notify(string msg)
        {
            try
            {
                using (var s = new System.Net.Sockets.TcpClient("localhost", 9001))
                {
                    var data = System.Text.Encoding.UTF8.GetBytes(msg);
                    s.Client.Send(data);
                }
            }
            catch
            {
                // standalone usage
            }
        }

        private static void Verify()
        {
            var text = @"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
                ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
            var p_left = new Printer { Quiet = true };
            new Program(text, p_left).run();
            var left = p_left.Checksum;

            var p_right = new Printer { Quiet = true };
            foreach (var c in "Hello World!\n")
            {
                p_right.Print(c);
            }
            var right = p_right.Checksum;

            if (left != right)
            {
                Console.Error.WriteLine($"{left} != {right}");
                System.Environment.Exit(1);
            }
        }

        static void Main(string[] args)
        {
            Verify();
            var text = File.ReadAllText(args[0]);
            var p = new Printer
            {
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

            if (p.Quiet)
            {
                Console.WriteLine($"Output checksum: {p.Checksum}");
            }
        }
    }
}
