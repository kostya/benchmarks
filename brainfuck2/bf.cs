enum OpT {INC, MOVE, PRINT, LOOP};

struct Op {
    public OpT op;
    public int v;
    public Op[] loop;

    public Op(OpT _op, int _v) { op = _op; v = _v; loop = null; }
    public Op(OpT _op, Op[] _l) { op = _op; loop = _l; v = 0; }
}

sealed class Tape {
    int pos;
    int[] tape = new int[1];

    public int Get() => tape[pos];
    public void Inc(int x) => tape[pos] += x;
    public void Move(int x) { pos += x; while (pos >= tape.Length) System.Array.Resize(ref tape, tape.Length*2); }

#if NETCOREAPP2_0 // ReadAllTextAsync is not on netfx as of .NET 4.7.2
    static async System.Threading.Tasks.Task Main(string[] args) {
        var code = await System.IO.File.ReadAllTextAsync(args[0]);
#else
    static void Main(string[] args) {
        var code = System.IO.File.ReadAllText(args[0]);
#endif
        int pos = 0;
        Run(Parse(), new Tape());

        void Run(Op[] program, Tape tape) {
            foreach (Op op in program) {
                switch (op.op) {
                    case OpT.INC: tape.Inc(op.v); break;
                    case OpT.MOVE: tape.Move(op.v); break;
                    case OpT.LOOP: while (tape.Get() > 0) Run(op.loop, tape); break;
                    case OpT.PRINT: System.Console.Write((char)tape.Get()); break;
                }
            }
        }

        Op[] Parse() {
            var res = new System.Collections.Generic.List<Op>();
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