import std.json;
import std.stdio;
import std.file;
import std.socket;
import std.compiler;
import std.format;
import core.thread;

void notify(string msg) {
    try {
        auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
        scope(exit) socket.close();
        socket.send(msg);
    } catch (SocketOSException) {
        // standalone usage
    }
}

int main(string[] args) {
  string text = readText("/tmp/1.json");

  notify("%s\t%d".format(name, getpid()));

  auto jobj = parseJSON(text).object;
  auto coordinates = jobj["coordinates"].array;
  ulong len = coordinates.length;
  double x = 0;
  double y = 0;
  double z = 0;

  for (int i = 0; i < coordinates.length; i++) {
    auto coord = coordinates[i];
    x += coord["x"].floating;
    y += coord["y"].floating;
    z += coord["z"].floating;
  }

  printf("%.8f\n%.8f\n%.8f\n", x / len, y / len, z / len);

  notify("stop");
  return 0;
}
