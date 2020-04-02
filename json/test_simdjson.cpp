#include <iostream>
#include <libnotify.hpp>
#include "simdjson.h"
#include <sstream>
#include <unistd.h>
#include <fstream>

using namespace simdjson;

void read_file(const std::string& filename, std::stringstream &buffer) {
  std::ifstream f(filename);
  if (f.good()) {
    buffer << f.rdbuf();
  }
}

int main(int argc, char *argv[]) {
  std::stringstream ss;
  read_file("/tmp/1.json", ss);
  std::string text = ss.str();

  std::stringstream ostr;
  ostr << "C++ simdjson\t" << getpid();
  notify(ostr.str());

  dom::parser pj(0);
  auto allocate_error = pj.allocate(text.size()); // allocate memory for parsing up to p.size() bytes
  if (allocate_error) {
    std::cerr << allocate_error << std::endl;
    exit(1); 
  }

  auto [doc, error] = pj.parse(text); // do the parsing, return 0 on success
  if (error) {
    std::cerr << error << std::endl;
    exit(1); 
  }

  double x = 0, y = 0, z = 0;
  int len = 0;

  for (auto coord : doc["coordinates"]) {
    double x_coord = coord["x"];
    x += x_coord;

    double y_coord = coord["y"];
    y += y_coord;

    double z_coord = coord["z"];
    z += z_coord;

    len++;
  }

  std::cout << x / len << std::endl;
  std::cout << y / len << std::endl;
  std::cout << z / len << std::endl;

  notify("stop");
  return EXIT_SUCCESS;
}
