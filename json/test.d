import std.json;
import std.stdio;
import std.file;
import std.socket;
import std.compiler;

int main(string[] args) {
  try {
    auto socket = new TcpSocket(new InternetAddress("localhost", 9001));
    scope(exit) socket.close();
    socket.send(name);
  } catch (SocketOSException) {
    // standalone usage
  }

  string text = cast(string)read("./1.json");
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
  return 0;
}
