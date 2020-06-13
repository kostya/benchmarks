#define RAPIDJSON_HAS_STDSTRING 1

#include "rapidjson/document.h"
#include <cstdio>
#include <iostream>
#include <libnotify.hpp>
#include <sstream>
#include <unistd.h>
#include <fstream>

using namespace std;
using namespace rapidjson;

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
  Document jobj;
  jobj.Parse(text);

  const Value& coordinates = jobj["coordinates"];
  auto len = coordinates.Size();
  auto x = 0.0, y = 0.0, z = 0.0;

  for (SizeType i = 0; i < len; i++) {
    const Value& coord = coordinates[i];
    x += coord["x"].GetDouble();
    y += coord["y"].GetDouble();
    z += coord["z"].GetDouble();
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
  ostr << "C++ RapidJSON\t" << getpid();
  notify(ostr.str());

  cout << calc(text) << endl;

  notify("stop");
}
