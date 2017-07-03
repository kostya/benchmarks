using System;
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
            var p = new Program(text);
            p.run();
        }
    }
}
