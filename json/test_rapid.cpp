#include "rapidjson/document.h"
#include "rapidjson/filereadstream.h"
#include <cstdio>
#include <iostream>

using namespace std;
using namespace rapidjson;

int main() {
    FILE* fp = std::fopen("./1.json", "r");
    char buffer[65536];
    FileReadStream frs(fp, buffer, sizeof(buffer));
    Document jobj; 
    jobj.ParseStream(frs);

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

    fclose(fp);

    return 0;
}
