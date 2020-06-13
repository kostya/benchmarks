import std.array;
import std.base64;
import std.datetime;
import std.format;
import std.range;
import std.stdio;
import std.socket;
import std.compiler;
import core.thread;

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
    auto str1 = (cast(ubyte) 'a').repeat(STR_SIZE).array;

    notify("%s\t%d".format(name, getpid()));
    uint s = 0;
    auto t = Clock.currTime();

    string str2 = Base64.encode(str1);
    write("encode %s... to %s...: ".format(cast(string) str1[0 .. 4], str2[0 .. 4]));

    for (int i = 0; i < TRIES; i++)
    {
        str2 = Base64.encode(str1);
        s += str2.length;
    }

    writeln("%d, %.2f".format(s, (Clock.currTime() - t).total!"msecs"() / 1000.0));

    auto str3 = Base64.decode(str2);
    write("decode %s... to %s...: ".format(str2[0 .. 4], cast(string) str3[0 .. 4]));

    s = 0;
    t = Clock.currTime();
    for (int i = 0; i < TRIES; i++)
    {
        str3 = Base64.decode(str2);
        s += str3.length;
    }

    writeln("%d, %.2f".format(s, (Clock.currTime() - t).total!"msecs"() / 1000.0));

    notify("stop");
    return 0;
}
