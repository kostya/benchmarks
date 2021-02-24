#include <boost/format.hpp>
#include <fstream>
#include <iostream>
#include <libnotify.hpp>
#include <sstream>
#include <unistd.h>

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

coordinate_t calc(const padded_string& json) {
  on_demand reader;
  reader.run(json);
  reader.my_point.x /= reader.count;
  reader.my_point.y /= reader.count;
  reader.my_point.z /= reader.count;
  return reader.my_point;
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

  padded_string text;
  const auto& error = padded_string::load("/tmp/1.json").get(text);
  if (error) {
    cerr << "could not load file" << endl;
    return EXIT_FAILURE;
  }

  notify(str(boost::format("C++/g++ (simdjson On-Demand)\t%d") % getpid()));
  const auto& results = calc(text);
  notify("stop");

  cout << results << endl;
}
