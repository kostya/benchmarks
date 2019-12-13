import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class bf {

    public enum OpT {INC, MOVE, PRINT, LOOP}

    public static final class Op {
        private static final Op[] NO_LOOP_OPS = new Op[0];

        public final OpT op;
        public final int v;
        public final Op[] loop;

        public Op(final OpT _op, final int _v) { op = _op; v = _v; loop = NO_LOOP_OPS; }
        public Op(final OpT _op, final Op[] _l) { op = _op; loop = _l; v = 0; }
    }

    public static final class CharacterIterator {
        private final String str;
        private final int strLen;
        private int pos = 0;

        public CharacterIterator(final String str) {
            this.str = str;
            strLen = str.length();
        }

        public boolean hasNext() {
            return pos < strLen;
        }

        public char next() {
            return str.charAt(pos++);
        }
    }

    public static final class Tape {
        private int[] tape;
        private int pos;

        public Tape() {
            tape = new int[1];
        }

        public int get() {
            return tape[pos];
        }

        public void inc(final int x) {
            tape[pos] += x;
        }

        public void move(final int x) {
            pos += x;
            while ( pos >= tape.length ) {
                this.tape = Arrays.copyOf(this.tape, this.tape.length * 2);
            }
        }
    }

    public static final class Program {
        private final Op[] ops;

        public Program(final String code) {
            CharacterIterator it = new CharacterIterator(code);
            ops = parse(it);
        }

        private Op[] parse(final CharacterIterator it) {
            final List<Op> res = new ArrayList<>();
            while (it.hasNext()) {
                switch( it.next() ) {
                    case '+':
                        res.add(new Op(OpT.INC, 1));
                        break;
                    case '-':
                        res.add(new Op(OpT.INC, -1));
                        break;
                    case '>':
                        res.add(new Op(OpT.MOVE, 1));
                        break;
                    case '<':
                        res.add(new Op(OpT.MOVE, -1));
                        break;
                    case '.':
                        res.add(new Op(OpT.PRINT, 0));
                        break;
                    case '[':
                        res.add(new Op(OpT.LOOP, parse(it)));
                        break;
                    case ']':
                        return res.toArray(new Op[res.size()]);
                }
            }
            return res.toArray(new Op[res.size()]);
        }

        public void run() {
            _run(ops, new Tape());
        }

        private void _run(final Op[] program, final Tape tape) {
            for (final Op op : program)
                switch (op.op) {
                    case INC: tape.inc(op.v); break;
                    case MOVE: tape.move(op.v); break;
                    case LOOP: while (tape.get() > 0) _run(op.loop, tape); break;
                    case PRINT: System.out.print( (char) tape.get() ); break;
                }
        }
    }

    public static void main( final String[] args ) throws IOException {
        final byte[] code = Files.readAllBytes( Paths.get( args[0] ) );

        long start_time = System.currentTimeMillis();
        System.err.println("JIT warming up");
        new Program(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++").run();
        System.err.println("time: " + (System.currentTimeMillis()-start_time)/1e3+"s");

        System.err.println("run");

        try (var socket = new java.net.Socket("localhost", 9001);
             var out = socket.getOutputStream()) {
            out.write("Java".getBytes("UTF-8"));
        } catch (java.io.IOException e) {
            // standalone usage
        }

        start_time = System.currentTimeMillis();
        final Program program = new Program(new String(code));
        program.run();
        System.err.println("time: " + (System.currentTimeMillis()-start_time)/1e3+"s");
    }
}
