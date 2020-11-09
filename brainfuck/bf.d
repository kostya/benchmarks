import std.conv : to;
import std.exception : assumeUnique;
import std.file : readText;
import std.stdio;
import std.traits : EnumMembers;
import std.socket;
import std.compiler;
import std.format;
import core.thread;
import std.process;
import core.stdc.stdlib;

// for compatability with older versions of the standard library
static if (__VERSION__ < 2068)
    import std.typetuple : Erase;
else
    import std.meta : Erase;

void notify(string msg)
{
    try
    {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope (exit)
            socket.close();
        socket.send(msg);
    }
    catch (SocketOSException)
    {
        // standalone usage
    }
}

void verify()
{
    immutable text = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    auto p_left = new Printer(true);
    Program(text).run(p_left);
    immutable left = p_left.get_checksum();

    auto p_right = new Printer(true);
    foreach (i, c; "Hello World!\n")
    {
        p_right.print(c);
    }
    immutable right = p_right.get_checksum();
    if (left != right)
    {
        stderr.writefln("%s != %s", left, right);
        exit(1);
    }
}

void main(string[] args)
{
    verify();
    auto text = readText(args[1]);
    auto p = new Printer(environment.get("QUIET") != null);

    notify("%s\t%d".format(name, getpid()));
    Program(text).run(p);
    notify("stop");

    if (p.quiet)
    {
        writeln("Output checksum: %d".format(p.get_checksum()));
    }
}

struct Op
{
    OpT op;
    Op[] loop;
}

enum OpT : dchar
{
    inc = '+',
    dec = '-',
    movePrev = '<',
    moveNext = '>',
    print = '.',
    loop = '!',
}

struct Tape
{
    uint pos;
    int[] tape;
    Printer* p;

    this(Printer* p)
    {
        this.pos = 0;
        this.tape = new int[1];
        this.p = p;
    }

    int get()
    {
        return tape[pos];
    }

    void inc()
    {
        tape[pos]++;
    }

    void dec()
    {
        tape[pos]--;
    }

    void movePrev()
    {
        pos--;
    }

    void moveNext()
    {
        pos++;
        if (pos == tape.length)
            tape ~= 0;
    }

    void print()
    {
        p.print(get());
    }
}

struct Printer
{
    int sum1;
    int sum2;
    bool quiet;

    this(bool quiet)
    {
        this.sum1 = 0;
        this.sum2 = 0;
        this.quiet = quiet;
    }

    void print(int n)
    {
        if (quiet)
        {
            sum1 = (sum1 + n) % 255;
            sum2 = (sum2 + sum1) % 255;
        }
        else
        {
            write(cast(char) n);
            stdout.flush();
        }
    }

    int get_checksum() const
    {
        return (sum2 << 8) | sum1;
    }
}

struct Program
{
    immutable Op[] ops;

    this(string code)
    {
        this.ops = parse(code).assumeUnique;
    }

    void run(Printer* p)
    {
        auto t = Tape(p);
        run(ops, t);
    }

    static Op[] parse(ref string code)
    {
        Op[] res;

        while (code.length)
        {
            char c = code[0];
            code = code[1 .. $];
            switch (c)
            {
            case '+':
            case '-':
            case '.':
            case '>':
            case '<':
                res ~= Op(cast(OpT) c);
                break;
            case '[':
                res ~= Op(OpT.loop, parse(code));
                break;
            case ']':
                return res;
            default:
                break;
            }
        }

        return res;
    }

    static void run(immutable(Op)[] program, ref Tape tape)
    {
        loop: foreach (op; program)
        {
            switch (op.op)
            {
                foreach (type; Erase!(OpT.loop, EnumMembers!OpT))
            case type:
                    mixin("tape." ~ type.to!string ~ "(); continue loop;");

            case OpT.loop:
                while (tape.get() > 0)
                    run(op.loop, tape);
                break;
            default:
                break;
            }
        }
    }
}
