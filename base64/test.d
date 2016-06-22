import std.base64;
import std.stdio;
import std.range;
import std.datetime;

enum STR_SIZE = 10_000_000;
enum TRIES = 100;

int main()
{
    auto str1 = (cast(ubyte) 'a').repeat(STR_SIZE);
    string str2 = "";
    uint s = 0;

    auto t = Clock.currTime();
    for (int i = 0; i < TRIES; i++)
    {
        str2 = Base64.encode(str1);
        s += str2.length;
    }

    writeln("encode: ", s, ", ", (Clock.currTime() - t).total!"msecs"() / 1000.0);

    s = 0;
    t = Clock.currTime();
    for (int i = 0; i < TRIES; i++)
    {
        s += Base64.decode(str2).length;
    }

    writeln("decode: ", s, ", ", (Clock.currTime() - t).total!"msecs"() / 1000.0);

    return 0;
}
