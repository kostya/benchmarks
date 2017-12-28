using System;
using System.IO;
using System.Collections.Generic;

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
        int[] tape = new int[1];

        public int Get() => tape[pos];
        public void Inc(int x) => tape[pos] += x;
        public void Move(int x) { pos += x; while (pos >= tape.Length) Array.Resize(ref tape, tape.Length*2); }
    }

    class Program
    {
        static void Main(string[] args)
        {
            string code = File.ReadAllText(args[0]);

            int pos = 0;
            var ops = Parse();

            Run(ops, new Tape());

            void Run(Op[] program, Tape tape) {
                foreach (Op op in program) {
                    switch (op.op) {
                        case OpT.INC: tape.Inc(op.v); break;
                        case OpT.MOVE: tape.Move(op.v); break;
                        case OpT.LOOP: while (tape.Get() > 0) Run(op.loop, tape); break;
                        case OpT.PRINT: Console.Write((char)tape.Get()); break;
                    }
                }
            }

            Op[] Parse() {
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
                        case '[': res.Add(new Op(OpT.LOOP, Parse())); break;
                        case ']': return res.ToArray();
                    }
                }
                return res.ToArray();
            }
        }
    }
}
