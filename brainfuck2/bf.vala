#!/usr/bin/vala
//Compile $ valac bf.vala --disable-assert -X -O3 --pkg gee-0.8 --pkg gio-2.0 -o bin_vala_gcc
//Run like script $ ./bf.vala --disable-assert -X -O3 --pkg gee-0.8 --pkg gio-2.0 --run-args bench.b
void print_vala_version() {
    string vala_version="",error="";
    try {
        Process.spawn_command_line_sync ("valac --version", out vala_version,out error);
        if (error.length > 1) print (error);
        else print (vala_version.split(" ")[1] + "\n");
    } catch (SpawnError e) {print (@"Error: $(e.message)\n");}
}

void send_language_name () {
    var host = "localhost:9001";
    try {
        var address = new InetAddress.from_string ("127.0.0.1");
        print (@"Resolved $host to $address\n");
        // Connect
        var client = new SocketClient ();
        var conn = client.connect (new InetSocketAddress (address, 9001));
        print (@"Connected to $host\n");

        // Send HTTP GET request
        var message = @"GET / HTTP/1.1\r\nHost: $host\r\n\r\n";
        conn.output_stream.write (message.data);
        print ("Wrote request\n");

        // Receive response
        var response = new DataInputStream (conn.input_stream);
        var status_line = response.read_line (null).strip ();
        print ("Received status line: %s\n", status_line);

    } catch (Error e) {
        stderr.printf ("%s\n", e.message);
    }
}

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
            print_vala_version();
            send_language_name();

            string text;
            FileUtils.get_contents(args[1],out text);
            
            message("run");
            Timer timer = new Timer ();
            var p = new Program(text);
            p.run();
            timer.stop();
            message("time: " + timer.elapsed().to_string() + " s");
        }
    }
}