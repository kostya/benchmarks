import core.stdc.stdlib;
import core.thread;
import std.compiler;
import std.conv;
import std.file;
import std.format;
import std.json;
import std.socket;
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
    auto jobj = parseJSON(text).object;
    auto coordinates = jobj["coordinates"].array;
    auto len = coordinates.length;
    auto x = 0.0;
    auto y = 0.0;
    auto z = 0.0;

    for (auto i = 0; i < coordinates.length; i++)
    {
        auto coord = coordinates[i];
        x += coord["x"].floating;
        y += coord["y"].floating;
        z += coord["z"].floating;
    }

    return Coordinate(x / len, y / len, z / len);
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

    notify("%s\t%d".format(name, getpid()));
    immutable results = calc(text);
    notify("stop");

    writeln(results);
}
