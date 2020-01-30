#include <json-c/json.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <libnotify.hpp>
#include <unistd.h>

using namespace std;

void read_file(const string& filename, stringstream &buffer) {
  ifstream f(filename);
  if (f.good()) {
    buffer << f.rdbuf();
  }
}

int main() {
  stringstream ss;
  read_file("/tmp/1.json", ss);
  string text = ss.str();

  stringstream ostr;
  ostr << "C++ json-c\t" << getpid();
  notify(ostr.str());

  json_object* jobj = json_tokener_parse(text.c_str());

  json_object* coordinates;
  if (json_object_object_get_ex(jobj, "coordinates", &coordinates)) {
    int len = json_object_array_length(coordinates);
    double x = 0, y = 0, z = 0;

    for (int i = 0; i < len; i++)  {
      json_object* coord = json_object_array_get_idx(coordinates, i);
      json_object *xobj, *yobj, *zobj;
      if (json_object_object_get_ex(coord, "x", &xobj) &&
	  json_object_object_get_ex(coord, "y", &yobj) &&
	  json_object_object_get_ex(coord, "z", &zobj)) {
	x += json_object_get_double(xobj);
	y += json_object_get_double(yobj);
	z += json_object_get_double(zobj);
      }
    }

    printf("%.8f\n", x / len);
    printf("%.8f\n", y / len);
    printf("%.8f\n", z / len);
  }
  notify("stop");
}
