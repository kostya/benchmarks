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

int main(string[] args)
{
    auto left = calc(`{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}`);
    auto right = Coordinate(1.1, 2.2, 3.3);
    if (left != right)
    {
        stderr.writefln("%s != %s", left, right);
        exit(1);
    }

    auto text = readText("/tmp/1.json");

    notify("%s\t%d".format(name, getpid()));

    writeln(calc(text));

    notify("stop");
    return 0;
}
