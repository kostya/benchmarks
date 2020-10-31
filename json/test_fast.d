import core.stdc.stdlib;
import core.thread;
import fast.json;
import std.compiler;
import std.conv;
import std.file : readText;
import std.format;
import std.socket;
import std.typecons;
import std.stdio;

struct Coordinate
{
    double x, y, z;

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
    auto x = 0.0, y = 0.0, z = 0.0;

    auto json = Json!(trustedSource, false)(text, No.simdPrep);
    auto coords = json.coordinates.read!(Coordinate[]);

    foreach (ref coord; coords)
    {
        x += coord.x;
        y += coord.y;
        z += coord.z;
    }

    auto len = coords.length;
    return Coordinate(x / len, y / len, z / len);
}

void main()
{
    auto right = Coordinate(2.0, 0.5, 0.25);
    foreach (v; [
            `{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
            `{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`
        ])
    {
        auto left = calc(v);
        if (left != right)
        {
            stderr.writefln("%s != %s", left, right);
            exit(1);
        }
    }

    auto text = readText("/tmp/1.json");
    // We need to append 16 zero bytes for SSE to work
    text ~= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";

    notify("D/gdc (fast)\t%d".format(getpid()));
    immutable results = calc(text);
    notify("stop");

    writeln(results);
}
