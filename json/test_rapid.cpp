#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

using namespace std;

void read_file(string filename, stringstream &buffer){
  ifstream f(filename.c_str());
  if (f)
  {
    buffer << f.rdbuf();
    f.close();
  }
}

using namespace rapidjson;

int main() {
    std::stringstream ss;
    read_file("./1.json", ss);

    string str = ss.str();
    Document d;
    d.Parse(str.c_str());

    double x = 0.0;
    double y = 0.0;
    double z = 0.0;
    const Value &coordinates = d["coordinates"];

    for (SizeType i = 0; i < coordinates.Size(); i++) {
      const Value &coord = coordinates[i];
      x += coord["x"].GetDouble();
      y += coord["y"].GetDouble();
      z += coord["z"].GetDouble();
    }

    std::cout << x / coordinates.Size() << std::endl;
    std::cout << y / coordinates.Size() << std::endl;
    std::cout << z / coordinates.Size() << std::endl;

    return 0;
}
