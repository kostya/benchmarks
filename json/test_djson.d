/+ dub.sdl:
name "json_d_djson"
targetPath "target"
dependency "djson" version="~>0.9.3"
dflags "-mcpu=native" "-linkonce-templates" "-enable-cross-module-inlining" platform="ldc"
+/
import core.stdc.stdlib;
import core.thread;
import djson;
import std.compiler;
import std.conv;
import std.file;
import std.format;
import std.socket;
import std.stdio;

struct Coordinate
{
    double x = 0, y = 0, z = 0;

    void toString(scope void delegate(const(char)[]) sink) const
    {
        sink("Coordinate {x: ");
        sink(to!string(x));
        sink(", y: ");
        sink(to!string(y));
        sink(", z: ");
        sink(to!string(z));
        sink("}");
    }
}

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

Coordinate calc(string text)
{
    Coordinate sum;
    size_t len;

    // dfmt off
    text.walkJSON!(
        "$.coordinates[*].x", (double v) { sum.x += v; len++; },
        "$.coordinates[*].y", (double v) { sum.y += v; },
        "$.coordinates[*].z", (double v) { sum.z += v; },
    );
    // dfmt on

    return Coordinate(sum.x / len, sum.y / len, sum.z / len);
}

void main()
{
    immutable right = Coordinate(2.0, 0.5, 0.25);
    foreach (v; [
        `{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
        `{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`
    ])
    {
        immutable left = calc(v);
        if (left != right)
        {
            stderr.writefln("%s != %s", left, right);
            exit(1);
        }
    }

    immutable text = readText("/tmp/1.json");

    notify("%s (djson)\t%d".format(name, getpid()));
    immutable results = calc(text);
    notify("stop");

    writeln(results);
}
