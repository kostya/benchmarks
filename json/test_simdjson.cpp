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

class on_demand {
public:
  bool run(const padded_string &json);
  coordinate_t my_point{};
  size_t count{};
private:
  ondemand::parser parser{};
};

bool on_demand::run(const padded_string &json) {
  count = 0;
  auto doc = parser.iterate(json);
  for (ondemand::object point_object : doc["coordinates"]) {
    my_point.x += double(point_object["x"]);
    my_point.y += double(point_object["y"]);
    my_point.z += double(point_object["z"]);
    count++;
  }
  return true;
}

coordinate_t calc(const simdjson::padded_string& json) {
  on_demand reader;
  reader.run(json);
  reader.my_point.x /= reader.count;
  reader.my_point.y /= reader.count;
  reader.my_point.z /= reader.count;
  return reader.my_point;
}

int main() {
  auto left = calc("{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}"_padded); // The _padded suffix creates a simdjson::padded_string instance
  auto right = coordinate_t(1.1, 2.2, 3.3);
  if (left != right) {
    cerr << left << " != " << right << endl;
    exit(EXIT_FAILURE);
  }
  auto [text, error] = simdjson::padded_string::load("/tmp/1.json");
  if(error) { cerr << "could not load file" << endl; return EXIT_FAILURE; }
  stringstream ostr;
  ostr << "C++/g++ (simdjson)\t" << getpid();
  notify(ostr.str());
  cout << calc(text) << endl;
  notify("stop");
}
