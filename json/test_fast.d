import core.stdc.stdio;
import core.thread;
import fast.json;
import std.compiler;
import std.file : readText;
import std.format;
import std.socket;
import std.typecons;

struct Coord { double x, y, z; }

void notify(string msg) {
    try {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope(exit) socket.close();
        socket.send(msg);
    } catch (SocketOSException) {
        // standalone usage
    }
}

void main()
{
    string text = readText("/tmp/1.json");
    // We need to append 16 zero bytes for SSE to work
    text ~= "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";

    notify("GDC fast\t%d".format(getpid()));

    double x = 0, y = 0, z = 0;

    auto json = Json!(trustedSource, false)(text, No.simdPrep);
    auto coords = json.coordinates.read!(Coord[]);

    foreach (ref coord; coords)
    {
        x += coord.x;
        y += coord.y;
        z += coord.z;
    }

    auto len = coords.length;
    printf("%.8f\n%.8f\n%.8f\n", x / len, y / len, z / len);

    notify("stop");
}
