#include <boost/format.hpp>
#include <json-c/json.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <libnotify.hpp>
#include <unistd.h>

using namespace std;

struct coordinate_t {
  double x;
  double y;
  double z;

  auto operator<=>(const coordinate_t&) const = default;

  friend ostream& operator<< (ostream &out, const coordinate_t &point) {
    out << "coordinate_t {x: " << point.x
        << ", y: " << point.y
        << ", z: " << point.z << "}";
    return out;
  }
};

string read_file(const string& filename) {
  ifstream f(filename);
  if (!f) {
    return {};
  }
  return string(istreambuf_iterator<char>(f),
                istreambuf_iterator<char>());
}

coordinate_t calc(const string& text) {
  auto jobj = json_tokener_parse(text.c_str());

  json_object* coordinates;
  if (!json_object_object_get_ex(jobj, "coordinates", &coordinates)) {
    exit(EXIT_FAILURE);
  }

  auto len = json_object_array_length(coordinates);
  auto x = 0.0, y = 0.0, z = 0.0;

  for (size_t i = 0; i < len; i++)  {
    auto coord = json_object_array_get_idx(coordinates, i);
    json_object *xobj, *yobj, *zobj;
    if (json_object_object_get_ex(coord, "x", &xobj) &&
        json_object_object_get_ex(coord, "y", &yobj) &&
        json_object_object_get_ex(coord, "z", &zobj)) {
      x += json_object_get_double(xobj);
      y += json_object_get_double(yobj);
      z += json_object_get_double(zobj);
    }
  }

  return coordinate_t(x / len, y / len, z / len);
}

int main() {
  auto right = coordinate_t(2.0, 0.5, 0.25);
  for (auto v : {
          "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
          "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"}) {
    auto left = calc(v);
    if (left != right) {
        cerr << left << " != " << right << endl;
        exit(EXIT_FAILURE);
    }
  }

  const auto& text = read_file("/tmp/1.json");

  notify(str(boost::format("C++/g++ (json-c)\t%d") % getpid()));
  const auto& results = calc(text);
  notify("stop");

  cout << results << endl;
}
