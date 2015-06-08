using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Text;

namespace Test
{
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
        public void Inc() { tape[pos]++; }
        public void Dec() { tape[pos]--; }
        public void Advance() { pos++; if (tape.Count <= pos) tape.Add(0); }
        public void Devance() { if (pos > 0) { pos--; } }
    }

    public struct Op
    {
        public char ch;
        public int jump;
        public Op(char ch1, int jump1)
        {
            ch = ch1;
            jump = jump1;
        }
    }

    class Program
    {
        List<Op> code;

        Program(string text)
        {
            Stack<int> leftstack = new Stack<int>();
            code = new List<Op>();

            for (int i = 0; i < text.Length; i++) {
                if ("[]<>+-,.".IndexOf(text[i]) != -1) {
                    Op op = new Op(text[i], 0);
                    code.Add(op);
                }
            }
            for (int pc = 0; pc < code.Count; pc++) {
                char c = code[pc].ch;
                if (c == '[') leftstack.Push(pc);
                else
                {
                    if (c == ']' && leftstack.Count != 0)
                    {
                        int left = leftstack.Pop();
                        int right = pc;
                        code[left] = new Op(code[left].ch, right);
                        code[right] = new Op(c, left);
                    }
                }
            }
        }

        void Run()
        {
            Tape tape = new Tape();
            for (int pc = 0; pc < code.Count; ++pc)
            {
                Op op = code[pc];
                switch (op.ch)
                {
                case '+':
                    tape.Inc();
                    break;
                case '-':
                    tape.Dec();
                    break;
                case '>':
                    tape.Advance();
                    break;
                case '<':
                    tape.Devance();
                    break;
                case '[':
                    if (tape.Get() == 0) pc = op.jump;
                    break;
                case ']':
                    if (tape.Get() != 0) pc = op.jump;
                    break;
                case '.':
                    Console.Write((char)tape.Get());
                    break;
                }
            }
        }

        static void Main(string[] args)
        {
            string text = File.ReadAllText(args[0]);
            var p = new Program(text);
            p.Run();
        }
    }
}
