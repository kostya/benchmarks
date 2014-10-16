#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/foreach.hpp>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

void read_file(string filename, stringstream &buffer){
  std::ifstream file("./1.json");
  if ( file )
  {
    buffer << file.rdbuf();
    file.close();
  }
}

int main()
{
  std::stringstream ss;
  read_file("./1.json", ss);

  boost::property_tree::ptree pt;
  boost::property_tree::read_json(ss, pt);
  double x, y, z;
  x = y = z = 0.0;
  int count = 0;

  BOOST_FOREACH(boost::property_tree::ptree::value_type &v, pt.get_child("coordinates"))
  {
    count += 1;
    x += v.second.get<double>("x");
    y += v.second.get<double>("y");
    z += v.second.get<double>("z");
  }

  printf("%.8f\n", x / count);
  printf("%.8f\n", y / count);
  printf("%.8f\n", z / count);
  return 0;
}
