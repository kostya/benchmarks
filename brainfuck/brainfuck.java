import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class brainfuck {

    public static class Tape {
        private int[] data;
        private int ptr;

        public Tape() {
            data = new int[1024];
        }

        public int get() {
            return data[ptr];
        }

        public void inc() {
            data[ptr]++;
        }

        public void dec() {
            data[ptr]--;
        }

        public void stepForward() {
            ptr++;
            if( ptr == data.length ) {
                int[] data = new int[this.data.length*2];
                System.arraycopy( this.data, 0, data, 0, this.data.length );
                this.data = data;
            }

        }

        public void stepBack() {
            if( ptr > 0 ) {
                ptr--;
            }
        }

    }

    public static class Program {
        private byte[] code;
        private Map<Integer, Integer> jumpMap;

        public Program( byte[] code ) {
            jumpMap = new HashMap<>();

            StringBuilder sb = new StringBuilder();
            Stack<Integer> leftstack = new Stack<>();
            int pc = 0;

            for( int i = 0; i < code.length; i++ ) {
                char c = (char) code[i];
                if( "[]<>+-,.".indexOf( c ) == -1 ) {
                    continue;
                }

                if( c == '[' ) {
                    leftstack.push( pc );
                } else {
                    if( c == ']' && leftstack.size() != 0 ) {
                        int left = leftstack.pop();
                        int right = pc;
                        jumpMap.put( left, right );
                        jumpMap.put( right, left );
                    }
                }

                pc++;
                sb.append( c );
            }

            this.code = sb.toString().getBytes();
        }

        public void run( Tape tape ) {
            for( int pc = 0; pc < code.length; pc++ ) {
                switch( code[pc] ) {
                    case '+':
                        tape.inc();
                        break;
                    case '-':
                        tape.dec();
                        break;
                    case '>':
                        tape.stepForward();
                        break;
                    case '<':
                        tape.stepBack();
                        break;
                    case '[':
                        if( tape.get() == 0 ) {
                            pc = jumpMap.get( pc );
                        }
                        break;
                    case ']':
                        if( tape.get() != 0 ) {
                            pc = jumpMap.get( pc );
                        }
                        break;
                    case '.':
                        System.out.print( (char) tape.get() );
                        break;
                }
            }
        }

    }

    public static void main( String[] args ) throws IOException {
        byte[] code = Files.readAllBytes( Paths.get( args[0] ) );

        long start_time = System.currentTimeMillis();
        Program program = new Program( code );
        program.run( new Tape() );
        System.out.println("time: " + (System.currentTimeMillis()-start_time)/1e3+"s");
    }
}

