import std.json;
import std.stdio;
import std.file;

int main(string[] args) {
  string text = readText("./1.json");
  JSONValue doc = parseJSON(text).object;
  JSONValue[] coordinates = doc["coordinates"].array;
  double x = 0;
  double y = 0;
  double z = 0;

  for (int i = 0; i < coordinates.length; i++) {
    JSONValue coord = coordinates[i];
    x += coord["x"].floating;
    y += coord["y"].floating;
    z += coord["z"].floating;
  }

  printf("%.8f\n%.8f\n%.8f\n", x / coordinates.length, y / coordinates.length, z / coordinates.length);
  return 0;
}
