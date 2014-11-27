#include <json/json.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

using namespace std;

void read_file(string filename, stringstream &buffer){
  ifstream f(filename.c_str());
  if (f)
  {
    buffer << f.rdbuf();
    f.close();
  }
}

int main() {
  std::stringstream ss;
  read_file("./1.json", ss);
  string text = ss.str();
  json_object* jobj = json_tokener_parse(text.c_str());
  json_object* coordinates = json_object_object_get(jobj, "coordinates");

  int len = json_object_array_length(coordinates);
  double x = 0, y = 0, z = 0;

  for (int i = 0; i < len; i++)  {
    json_object* coord = json_object_array_get_idx(coordinates, i);
    x += json_object_get_double(json_object_object_get(coord, "x"));
    y += json_object_get_double(json_object_object_get(coord, "y"));
    z += json_object_get_double(json_object_object_get(coord, "z"));
  }

  printf("%.8f\n", x / len);
  printf("%.8f\n", y / len);
  printf("%.8f\n", z / len);
}
