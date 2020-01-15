#include <iostream>
#include <libsocket/inetclientstream.hpp>
#include "simdjson/jsonparser.h"
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

void notify(const std::string& msg) {
  try {
    libsocket::inet_stream sock("localhost", "9001", LIBSOCKET_IPv4);
    sock << msg;
  } catch (...) {
    // standalone usage
  }
}

int main(int argc, char *argv[]) {
  std::stringstream ss;
  read_file("/tmp/1.json", ss);
  std::string text = ss.str();

  std::stringstream ostr;
  ostr << "C++ simdjson\t" << getpid();
  notify(ostr.str());

  ParsedJson pj;
  int res = simdjson::SUCCESS;
  if (pj.allocate_capacity(text.size())) { // allocate memory for parsing up to p.size() bytes
    res = json_parse(text, pj); // do the parsing, return 0 on success
  }
  if (res != simdjson::SUCCESS) {
    std::cout << pj.get_error_message() << std::endl;
  }

  ParsedJson::Iterator pjh(pj);
  if (!pjh.is_ok()) {
    std::cerr << " Could not iterate parsed result. " << std::endl;
    return EXIT_FAILURE;
  }

  double x = 0, y = 0, z = 0;
  int len = 0;

  if (pjh.is_object()) {
    if (pjh.move_to_key("coordinates")) {
      if (pjh.is_array()) {
        if (pjh.down()) {
          do { // moving through array
            
            if (pjh.is_object()) {
              len++;
              if (pjh.down()) {
                do { // moving through hash {x:, y:, z:}
                  if (pjh.get_string_length() == 1) {
                      char c = pjh.get_string()[0];
                      pjh.next();

                      switch(c) {
                        case 'x':
                          x += pjh.get_double();
                          break;

                        case 'y':
                          y += pjh.get_double();
                          break;

                        case 'z':
                          z += pjh.get_double();
                          break;
                      }
                  } else {
                    pjh.next();
                  }

                } while(pjh.next());  // moving through hash {x:, y:, z:}
                pjh.up();
              }
            }
          } while(pjh.next()); // moving through array
        }
      }
    }
  }

  std::cout << x / len << std::endl;
  std::cout << y / len << std::endl;
  std::cout << z / len << std::endl;

  notify("stop");
  return EXIT_SUCCESS;
}
