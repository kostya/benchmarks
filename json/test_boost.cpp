#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/foreach.hpp>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

void read_file(string filename, stringstream &buffer){
  std::ifstream file(filename.c_str());
  if ( file )
  {
    buffer << file.rdbuf();
    file.close();
  }
}

int main()
{
  std::stringstream text;
  read_file("./1.json", text);

  boost::property_tree::ptree jobj;
  boost::property_tree::read_json(text, jobj);
  double x = 0, y = 0, z = 0;
  int len = 0;

  BOOST_FOREACH(boost::property_tree::ptree::value_type &coord, jobj.get_child("coordinates"))
  {
    len += 1;
    x += coord.second.get<double>("x");
    y += coord.second.get<double>("y");
    z += coord.second.get<double>("z");
  }

  printf("%.8f\n", x / len);
  printf("%.8f\n", y / len);
  printf("%.8f\n", z / len);
  return 0;
}
