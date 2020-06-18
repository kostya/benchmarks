#include <iostream>
#include <libnotify.hpp>
#include "simdjson.h"
#include <sstream>
#include <unistd.h>
#include <fstream>

using namespace simdjson;
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
  dom::parser pj(0);
  auto allocate_error = pj.allocate(text.size()); // allocate memory for parsing up to p.size() bytes
  if (allocate_error) {
    cerr << allocate_error << endl;
    exit(EXIT_FAILURE);
  }

  auto [doc, error] = pj.parse(text); // do the parsing, return 0 on success
  if (error) {
    cerr << error << endl;
    exit(EXIT_FAILURE);
  }

  auto x = 0.0, y = 0.0, z = 0.0;
  auto len = 0;

  for (auto coord : doc["coordinates"]) {
    double x_coord = coord["x"];
    x += x_coord;

    double y_coord = coord["y"];
    y += y_coord;

    double z_coord = coord["z"];
    z += z_coord;

    len++;
  }

  return coordinate_t(x / len, y / len, z / len);
}

int main() {
  auto left = calc("{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}");
  auto right = coordinate_t(1.1, 2.2, 3.3);
  if (left != right) {
    cerr << left << " != " << right << endl;
    exit(EXIT_FAILURE);
  }

  auto text = read_file("/tmp/1.json");

  stringstream ostr;
  ostr << "C++ simdjson\t" << getpid();
  notify(ostr.str());

  cout << calc(text) << endl;

  notify("stop");
}
