import std.conv : to;
import std.exception : assumeUnique;
import std.file : readText;
import std.stdio : write, stdout;
import std.traits : EnumMembers;
import std.socket;
import std.compiler;
import std.format;
import core.thread;

// for compatability with older versions of the standard library
static if (__VERSION__ < 2068)
    import std.typetuple : Erase;
else
    import std.meta : Erase;

void notify(string msg) {
    try {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope(exit) socket.close();
        socket.send(msg);
    } catch (SocketOSException) {
        // standalone usage
    }
}

void main(string[] args)
{
    string text = readText(args[1]);

    notify("%s\t%d".format(name, getpid()));

    Program(text).run();

    notify("stop");
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
    int[] tape = [0];

    int  get()      { return tape[pos]; }
    void inc()      { tape[pos]++; }
    void dec()      { tape[pos]--; }
    void movePrev() { pos--; }
    void moveNext() { pos++; if (pos == tape.length) tape ~= 0; }
    void print()    { write(cast(char)get()); stdout.flush(); }
}

struct Program
{
    immutable Op[] ops;

    this(string code)
    {
        this.ops = parse(code).assumeUnique;
    }

    void run()
    {
        auto t = Tape();
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
                case '+': case '-': case '.':
                case '>': case '<':
                    res ~= Op(cast(OpT)c);
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
