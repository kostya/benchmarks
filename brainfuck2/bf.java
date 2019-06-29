import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.PrimitiveIterator;
import java.util.function.Consumer;


final class bf {

    interface Op extends Consumer<Tape> {
    }

    static final class LoopOp implements Op {
        final Op[] loop;

        LoopOp( Op[] loop ) {
            this.loop = loop;
        }

        @Override
        public void accept( Tape tape ) {
            while ( tape.get() > 0 ) for ( Op op : loop ) {
                op.accept( tape );
            }
        }
    }

    static final class PrintOp implements Op {
        private final PrintStream out;

        PrintOp( PrintStream out ) {
            this.out = out;
        }

        @Override
        public void accept( Tape tape ) {
            out.print( ( char ) tape.get() );
        }
    }

    public static void main( String[] args ) throws IOException {
        byte[] codeBytes = Files.readAllBytes( Paths.get( args[ 0 ] ) );
        String code = new String( codeBytes, StandardCharsets.US_ASCII );

        int runs = args.length > 1 ? Integer.parseInt( args[ 1 ] ) : 1;
        for ( int i = 0; i < runs; i++ ) {
            runWithTiming( () -> new Program( code, System.out ).run() );
        }
    }

    private static void runWithTiming( Runnable runnable ) {
        long startTime = System.currentTimeMillis();
        runnable.run();
        System.err.printf( "time: %.3fs\n", ( System.currentTimeMillis() - startTime ) / 1e3 );
    }

}

final class Tape {
    private int[] tape;
    private int pos;

    Tape() {
        tape = new int[ 16 ];
    }

    int get() {
        return tape[ pos ];
    }

    void inc( int x ) {
        tape[ pos ] += x;
    }

    void move( int x ) {
        pos += x;
        while ( pos >= tape.length ) {
            int[] tape = new int[ this.tape.length * 2 ];
            System.arraycopy( this.tape, 0, tape, 0, this.tape.length );
            this.tape = tape;
        }

    }

    public int[] getState() {
        int[] tape = new int[ this.tape.length ];
        System.arraycopy( this.tape, 0, tape, 0, this.tape.length );
        return tape;
    }

    public int getPos() {
        return pos;
    }
}

final class Program {
    private static final bf.Op INCR = tape -> tape.inc( 1 );
    private static final bf.Op DECR = tape -> tape.inc( -1 );
    private static final bf.Op MV_UP = tape -> tape.move( 1 );
    private static final bf.Op MV_DOWN = tape -> tape.move( -1 );

    private final Tape tape = new Tape();
    private final bf.Op printer;
    private final bf.Op[] ops;

    Program( String code, PrintStream out ) {
        printer = new bf.PrintOp( out );
        ops = parse( code.chars().iterator() );
    }

    private bf.Op[] parse( PrimitiveIterator.OfInt it ) {
        List<bf.Op> res = new ArrayList<>();
        while ( it.hasNext() ) {
            switch ( it.nextInt() ) {
                case '+':
                    res.add( INCR );
                    break;
                case '-':
                    res.add( DECR );
                    break;
                case '>':
                    res.add( MV_UP );
                    break;
                case '<':
                    res.add( MV_DOWN );
                    break;
                case '.':
                    res.add( printer );
                    break;
                case '[':
                    res.add( new bf.LoopOp( parse( it ) ) );
                    break;
                case ']':
                    return res.toArray( new bf.Op[ 0 ] );
            }
        }
        return res.toArray( new bf.Op[ 0 ] );
    }

    Tape run() {
        for ( bf.Op op : ops ) op.accept( tape );
        return tape;
    }

}