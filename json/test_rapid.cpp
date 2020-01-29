#include "rapidjson/document.h"
#include <cstdio>
#include <iostream>
#include <libnotify.hpp>
#include <sstream>
#include <unistd.h>
#include <fstream>

using namespace std;
using namespace rapidjson;

void read_file(const string& filename, stringstream &buffer) {
  ifstream f(filename);
  if (f.good()) {
    buffer << f.rdbuf();
  }
}

int main() {
    stringstream ss;
    read_file("/tmp/1.json", ss);
    string text = ss.str();

    stringstream ostr;
    ostr << "C++ RapidJSON\t" << getpid();
    notify(ostr.str());

    Document jobj;
    jobj.Parse(text.c_str());

    const Value &coordinates = jobj["coordinates"];
    SizeType len = coordinates.Size();
    double x = 0, y = 0, z = 0;

    for (SizeType i = 0; i < len; i++) {
      const Value &coord = coordinates[i];
      x += coord["x"].GetDouble();
      y += coord["y"].GetDouble();
      z += coord["z"].GetDouble();
    }

    std::cout << x / len << std::endl;
    std::cout << y / len << std::endl;
    std::cout << z / len << std::endl;

    notify("stop");
    return 0;
}
