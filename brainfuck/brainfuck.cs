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

    class Program
    {
        string code;
        Dictionary<int, int> bracket_map = new Dictionary<int, int>();

        Program(string text)
        {
            var sb = new StringBuilder();
            Stack<int> leftstack = new Stack<int>();
            int pc = 0;

            for (int i = 0; i < text.Length; i++)
            {
                char c = text[i];
                if ("[]<>+-,.".IndexOf(c) == -1) continue;

                if (c == '[') leftstack.Push(pc);
                else
                {
                    if (c == ']' && leftstack.Count != 0)
                    {
                        int left = leftstack.Pop();
                        int right = pc;
                        bracket_map[left] = right;
                        bracket_map[right] = left;
                    }
                }

                pc++;
                sb.Append(c);
            }

            code = sb.ToString();
        }

        void Run()
        {
            Tape tape = new Tape();
            for (int pc = 0; pc < code.Length; ++pc)
            {
                switch (code[pc])
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
                    if (tape.Get() == 0) pc = bracket_map[pc];
                    break;
                case ']':
                    if (tape.Get() != 0) pc = bracket_map[pc];
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
