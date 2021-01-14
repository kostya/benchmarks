#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/foreach.hpp>
#include <iostream>
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

void read_file(string filename, stringstream &buffer) {
  ifstream file(filename.c_str());
  if (file) {
    buffer << file.rdbuf();
    file.close();
  }
}

coordinate_t calc(stringstream& text) {
  boost::property_tree::ptree jobj;
  boost::property_tree::read_json(text, jobj);
  auto x = 0.0, y = 0.0, z = 0.0;
  auto len = 0;

  BOOST_FOREACH(boost::property_tree::ptree::value_type &coord,
                jobj.get_child("coordinates")) {
    len += 1;
    x += coord.second.get<double>("x");
    y += coord.second.get<double>("y");
    z += coord.second.get<double>("z");
  }

  return coordinate_t(x / len, y / len, z / len);
}

int main() {
  auto right = coordinate_t(2.0, 0.5, 0.25);
  for (auto v : {
          "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
          "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"}) {
    auto json = stringstream(v);
    auto left = calc(json);
    if (left != right) {
        cerr << left << " != " << right << endl;
        exit(EXIT_FAILURE);
    }
  }

  stringstream text;
  read_file("/tmp/1.json", text);

  notify(str(boost::format("C++/g++ (Boost.PropertyTree)\t%d") % getpid()));
  const auto& results = calc(text);
  notify("stop");

  cout << results << endl;
}
