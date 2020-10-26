#include <iostream>
#include <libnotify.hpp>
#include <sstream>
#include <unistd.h>
#include <fstream>

#include "simdjson.h"

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


using namespace simdjson;
using namespace simdjson::builtin;

coordinate_t calc(const padded_string& text) {
  dom::parser pj(0);
  // allocate memory for parsing up to p.size() bytes
  auto allocate_error = pj.allocate(text.size());
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
  auto right = coordinate_t(2.0, 0.5, 0.25);
  // The _padded suffix creates a simdjson::padded_string instance
  for (const padded_string &v : {
      "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}"_padded,
      "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"_padded}) {
    auto left = calc(v);
    if (left != right) {
      cerr << left << " != " << right << endl;
      exit(EXIT_FAILURE);
    }
  }

  auto [text, error] = padded_string::load("/tmp/1.json");
  if(error) { cerr << "could not load file" << endl; return EXIT_FAILURE; }
  stringstream ostr;
  ostr << "C++/g++ (simdjson DOM)\t" << getpid();
  notify(ostr.str());
  cout << calc(text) << endl;
  notify("stop");
}
