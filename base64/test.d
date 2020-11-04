import core.stdc.stdlib;
import core.thread;
import std.array;
import std.base64;
import std.compiler;
import std.datetime;
import std.format;
import std.range;
import std.socket;
import std.stdio;

enum STR_SIZE = 131072;
enum TRIES = 8192;

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

int main()
{
    foreach (fixture; [["hello", "aGVsbG8="], ["world", "d29ybGQ="]])
    {
        immutable src = fixture[0];
        immutable dst = fixture[1];
        immutable encoded = Base64.encode(cast(ubyte[]) src);
        if (encoded != dst)
        {
            stderr.writefln("%s != %s", encoded, dst);
            exit(1);
        }
        immutable decoded = Base64.decode(dst);
        if (decoded != src)
        {
            stderr.writefln("%s != %s", decoded, src);
            exit(1);
        }
    }

    immutable str1 = (cast(ubyte) 'a').repeat(STR_SIZE).array;
    immutable str2 = Base64.encode(str1);
    immutable str3 = Base64.decode(str2);

    notify("%s\t%d".format(name, getpid()));

    auto s_encoded = 0;
    immutable t = Clock.currTime();
    for (auto i = 0; i < TRIES; i++)
    {
        s_encoded += Base64.encode(str1).length;
    }
    immutable t_encoded = (Clock.currTime() - t).total!"msecs"() / 1000.0;

    auto s_decoded = 0;
    immutable t1 = Clock.currTime();
    for (auto i = 0; i < TRIES; i++)
    {
        s_decoded += Base64.decode(str2).length;
    }
    immutable t_decoded = (Clock.currTime() - t1).total!"msecs"() / 1000.0;

    notify("stop");

    writeln("encode %s... to %s...: %d, %.2f".format(cast(string) str1[0 .. 4],
            str2[0 .. 4], s_encoded, t_encoded));
    writeln("decode %s... to %s...: %d, %.2f".format(str2[0 .. 4],
            cast(string) str3[0 .. 4], s_decoded, t_decoded));

    return 0;
}
