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

    string text = ss.str();
    Document jobj; jobj.Parse(text.c_str());

    const Value &coordinates = jobj["coordinates"];
    int len = coordinates.Size();
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

    return 0;
}
