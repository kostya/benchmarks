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
        public List<Op> loop;

        public Op(OpT _op, int _v) { op = _op; v = _v; loop = new List<Op>(); }
        public Op(OpT _op, List<Op> _l) { op = _op; loop = _l; v = 0; }
    }

    public class Tape
    {
        int pos;
        List<int> tape;

        public Tape()
        {
            pos = 0;
            tape = new List<int>(new int[]{0});
        }

        public int Get() { return tape[pos]; }
        public void Inc(int x) { tape[pos] += x; }
        public void Move(int x) { pos += x; while (pos >= tape.Count) tape.Add(0); }
    }

    class Program
    {
        string code;
        int pos;
        List<Op> ops;
        
        Program(string text)
        {
            code = text;
            pos = 0;
            ops = parse();
        }

        private List<Op> parse() {
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
                    case ']': return res;
                }
            }
            return res;
        }

        public void run() {
            _run(ops, new Tape());
        }

        private void _run(List<Op> program, Tape tape) {
            foreach (Op op in program) {
                switch (op.op) {
                    case OpT.INC: tape.Inc(op.v); break;
                    case OpT.MOVE: tape.Move(op.v); break;
                    case OpT.LOOP: while (tape.Get() != 0) _run(op.loop, tape); break;
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
