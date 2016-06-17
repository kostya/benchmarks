import std.algorithm;
import std.stdio;
import std.file;
import std.array;

final:

enum OpT
{
    INC,
    MOVE,
    PRINT,
    LOOP
}

struct Op
{
    OpT op;
    int v;
    Op[] loop;

@nogc @safe pure nothrow:
    this(OpT t, int _v)
    {
        op = t;
        v = _v;
    }

    this(OpT t, Op[] _l)
    {
        op = t;
        loop = _l;
    }
}

final class StringIterator
{
    string text;
    int pos;

@nogc @safe pure nothrow:
    this(string t)
    {
        text = t;
        pos = 0;
    }

    char next()
    {
        return (pos < text.length) ? text[pos++] : 0;
    }
}

final class Tape
{
    int pos;
    int[] tape;

@safe pure nothrow:
    this()
    {
        pos = 0;
        tape ~= 0;
    }

    int get() @nogc
    {
        return tape[pos];
    }

    void inc(int x) @nogc
    {
        tape[pos] += x;
    }

    void move(int x)
    {
        pos += x;
        while (pos >= tape.length)
            tape ~= 0;
    }
}

final class Program
{
    Op[] ops;

    this(string code)
    {
        ops = parse(new StringIterator(code));
    }

    void run()
    {
        _run(ops, new Tape());
    }

    void _run(Op[] program, Tape tape)
    {
        foreach (op; program)
        {
            switch (op.op)
            {
            case OpT.INC:
                tape.inc(op.v);
                break;
            case OpT.MOVE:
                tape.move(op.v);
                break;
            case OpT.LOOP:
                while (tape.get() != 0)
                    _run(op.loop, tape);
                break;
            case OpT.PRINT:
                write(cast(char) tape.get());
                stdout.flush();
                break;
            default:
                break;
            }
        }
    }

    Op[] parse(StringIterator it) pure nothrow
    {
        Op[] res;

        while (true)
        {
            char c = it.next();
            if (c == 0)
                break;

            switch (c)
            {
            case '+':
                res ~= Op(OpT.INC, 1);
                break;
            case '-':
                res ~= Op(OpT.INC, -1);
                break;
            case '>':
                res ~= Op(OpT.MOVE, 1);
                break;
            case '<':
                res ~= Op(OpT.MOVE, -1);
                break;
            case '.':
                res ~= Op(OpT.PRINT, 0);
                break;
            case '[':
                res ~= Op(OpT.LOOP, parse(it));
                break;
            case ']':
                return res;
            default:
                break;
            }
        }

        return res;
    }
}

int main(string[] args)
{
    string text = readText(args[1]);
    new Program(text).run();
    return 0;
}
