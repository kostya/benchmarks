enum OpT {INC, MOVE, PRINT, LOOP}
namespace Test{
    struct Op {
        public OpT op;
        public int v;
        public Op[] loop;
 
        public Op(OpT _op, int _v) { op = _op; v = _v; loop = null; }
        public Op.vnull(OpT _op, Op[] _l) { op = _op; loop = _l; v = 0; }
    }
    
    public class Tape {
        public int pos;
        public int[] tape;
 
        public Tape() {
            pos = 0;
            tape = new int[1];
        }
 
        public int Get() { return tape[pos]; }
        public void Inc(int x) { tape[pos] += x; }
        public void Move(int x) { pos += x; while (pos >= tape.length) tape.resize(tape.length*2);}
    }
    
    class Program {
        public string code;
        public  int pos;
        public  Op[] ops;
 
        Program(string text) {
            code = text;
            pos = 0;
            ops = parse();
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
                    case OpT.PRINT: stdout.printf("%c",(char)tape.Get()); break;
                }
            }
        }
 
        static void main(string[] args) {
            string text;
            FileUtils.get_contents(args[1],out text);
            Timer timer = new Timer ();
 
            message("run");
            timer.reset();
            var p = new Program(text);
            p.run();
            timer.stop();
            message("time: " + timer.elapsed().to_string() + " s");
        }
    }
}