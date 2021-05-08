module mir_common_json;

import core.stdc.stdio;
import core.thread.osthread: getpid;
import mir.format;
import mir.serde;

void validate(alias calc)()
{
    foreach (v; [
        `{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}`,
        `{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}`])
        if (calc(v) != Coordinate(2.0, 0.5, 0.25))
            throw new Exception("Invalid deserialization implementation");
}

auto notify(scope const char[] msg) @trusted nothrow @nogc
{
    import core.sys.posix.arpa.inet;
    import core.sys.posix.sys.socket;
    import core.sys.posix.sys.types;
    import core.sys.posix.unistd;
    import core.sys.posix.netinet.in_;

    int sock = socket(AF_INET, SOCK_STREAM, 0);

    if (sock < 0) {
        return;
    }

    sockaddr_in serv_addr;
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(9001);
    inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr);

    if (!connect(sock, cast(sockaddr*)&serv_addr, serv_addr.sizeof)) {
        send(sock, msg.ptr, msg.length, 0);
    }
    close(sock);
}

auto notifyStart(scope const char[] msg)
{
    notify(stringBuf() << msg << "\t" << getpid << getData);
}

auto notifyStop(scope const char[] msg = "stop")
{
    notify(msg);
}

struct Coordinate
{
    double x = 0, y = 0, z = 0;

    auto print() @trusted
    {
        puts((stringBuf() << x << ", " << y << ", "  << z << "\n\0" << getData).ptr);
    }
}

struct CoordinateAvg
{
    alias serdeKeysProxy = Coordinate;

    size_t length;
    Coordinate coord;

    auto put()(Coordinate val)
    {
        length++;
        coord.x += val.x;
        coord.y += val.y;
        coord.z += val.z;
    }

    auto avg()
    {
        return Coordinate(coord.x / length, coord.y / length, coord.z / length);
    }
}

struct Avg
{
    @serdeLikeList
    @serdeProxy!Coordinate // input element type of
    CoordinateAvg coordinates; //`put` method is used
}
