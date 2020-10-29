#include "gason.h"
#include <boost/format.hpp>
#include <fstream>
#include <iostream>
#include <libnotify.hpp>
#include <sstream>
#include <string.h>
#include <string>
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
  char *endptr;
  JsonValue jobj;
  JsonAllocator allocator;
  auto status = jsonParse((char *)text.c_str(), &endptr, &jobj, allocator);
  if (status != JSON_OK) {
    exit(EXIT_FAILURE);
  }

  JsonValue coordinates;
  for (auto data : jobj) {
    if (strcmp(data->key, "coordinates") == 0) {
      coordinates = data->value;
    }
  }

  auto x = 0.0, y = 0.0, z = 0.0;
  auto len = 0;

  for (auto coord : coordinates) {
    len++;
    for (auto c : coord->value) {
      auto key = c->key;
      if (strcmp(key, "x") == 0) {
        x += c->value.toNumber();
      } else if (strcmp(key, "y") == 0) {
        y += c->value.toNumber();
      } else if (strcmp(key, "z") == 0) {
        z += c->value.toNumber();
      }
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

  notify(str(boost::format("C++/g++ (gason)\t%d") % getpid()));
  const auto& results = calc(text);
  notify("stop");

  cout << results << endl;
}
