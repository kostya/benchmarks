#include <boost/json.hpp>
#include <boost/json/src.hpp>
#include <fstream>
#include <iostream>
#include <libnotify.h>

#ifdef __clang__
#define COMPILER "clang++"
#else
#define COMPILER "g++"
#endif

using namespace std;

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
  ifstream f(filename);
  if (!f) {
    return {};
  }
  return string(istreambuf_iterator<char>(f), istreambuf_iterator<char>());
}

coordinate_t calc(const string &text) {
  auto x = 0.0, y = 0.0, z = 0.0;
  auto len = 0;

  auto jv = boost::json::parse(text);
  auto &obj = jv.get_object();
  for (auto &v : obj["coordinates"].get_array()) {
    len += 1;
    auto &coord = v.get_object();
    x += coord["x"].get_double();
    y += coord["y"].get_double();
    z += coord["z"].get_double();
  }

  return coordinate_t{x / len, y / len, z / len};
}

int main() {
  auto right = coordinate_t{2.0, 0.5, 0.25};
  for (auto v : {"{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
                 "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"}) {
    auto left = calc(v);
    if (left != right) {
      cerr << left << " != " << right << endl;
      exit(EXIT_FAILURE);
    }
  }

  const auto &text = read_file("/tmp/1.json");

  const auto &results = notifying_invoke([&]() { return calc(text); },
                                         "C++/{} (Boost.JSON)", COMPILER);

  cout << results << endl;
}
