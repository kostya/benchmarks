#include <boost/json.hpp>
#include <boost/json/src.hpp>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <libnotify.hpp>
#include <unistd.h>

using namespace std;

struct coordinate_t {
  float x;
  float y;
  float z;

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
  auto x = 0.0, y = 0.0, z = 0.0;
  auto len = 0;

  auto jv = boost::json::parse(text);
  auto &obj = jv.get_object();
  for (auto& v: obj["coordinates"].get_array()) {
    len += 1;
    auto& coord = v.get_object();
    x += value_to<float>(coord["x"]);
    y += value_to<float>(coord["y"]);
    z += value_to<float>(coord["z"]);
  }

  return coordinate_t(x / len, y / len, z / len);
}

int main() {
  auto right = coordinate_t(1.1, 2.2, 3.3);
  for (auto v : {
          "{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}",
          "{\"coordinates\":[{\"y\":2.2,\"x\":1.1,\"z\":3.3}]}"}) {
    auto left = calc(v);
    if (left != right) {
        cerr << left << " != " << right << endl;
        exit(EXIT_FAILURE);
    }
  }

  const auto& text = read_file("/tmp/1.json");

  stringstream ostr;
  ostr << "C++/g++ (Boost.JSON)\t" << getpid();
  notify(ostr.str());

  cout << calc(text) << endl;

  notify("stop");
}
