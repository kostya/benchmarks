#define RAPIDJSON_HAS_STDSTRING 1

#include "rapidjson/document.h"
#include <cstdio>
#include <fstream>
#include <iostream>
#include <libnotify.h>
#include <sstream>

#ifdef __clang__
#define COMPILER "clang++"
#else
#define COMPILER "g++"
#endif

using namespace std;
using namespace rapidjson;

struct coordinate_t {
  double x;
  double y;
  double z;

  auto operator<=>(const coordinate_t &) const = default;

  friend ostream &operator<<(ostream &out, const coordinate_t &point) {
    out << "coordinate_t {x: " << point.x << ", y: " << point.y
        << ", z: " << point.z << "}";
    return out;
  }
};

string read_file(const string &filename) {
  ifstream file{filename};
  if (file.fail()) {
    return {};
  }
  return string{istreambuf_iterator<char>{file}, {}};
}

coordinate_t calc(const string &text) {
  Document jobj;
#ifdef PRECISED
  jobj.Parse<kParseFullPrecisionFlag>(text);
#else
  jobj.Parse(text);
#endif

  const Value &coordinates = jobj["coordinates"];
  auto len = coordinates.Size();
  auto x = 0.0, y = 0.0, z = 0.0;

  for (SizeType i = 0; i < len; i++) {
    const Value &coord = coordinates[i];
    x += coord["x"].GetDouble();
    y += coord["y"].GetDouble();
    z += coord["z"].GetDouble();
  }

  return coordinate_t{x / len, y / len, z / len};
}

int main() {
  auto right = coordinate_t{2.0, 0.5, 0.25};
  for (auto v : {R"({"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]})",
                 R"({"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]})"}) {
    auto left = calc(v);
    if (left != right) {
      cerr << left << " != " << right << endl;
      exit(EXIT_FAILURE);
    }
  }

  const auto &text = read_file("/tmp/1.json");

#ifdef PRECISED
  const auto suffix = " Precise"s;
#else
  const auto suffix = ""s;
#endif

  const auto &results = notifying_invoke(
      [&]() { return calc(text); }, "C++/{} (RapidJSON{})", COMPILER, suffix);

  cout << results << endl;
}
