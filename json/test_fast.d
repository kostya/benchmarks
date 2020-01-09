import core.stdc.stdio;
import fast.json;
import std.socket;
import std.compiler;
import std.format;
import core.thread;
import std.file : readText;

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

    notify("GDC fast\t%d".format(getpid()));

    double x = 0, y = 0, z = 0;

    auto json = parseTrustedJSON(text);
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
