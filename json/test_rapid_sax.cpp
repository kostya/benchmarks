#include "rapidjson/reader.h"
#include <rapidjson/istreamwrapper.h>
#include <cstdio>
#include <iostream>
#include <libnotify.hpp>
#include <sstream>
#include <unistd.h>
#include <fstream>

using namespace std;
using namespace rapidjson;

class CoordinateHandler : public BaseReaderHandler<UTF8<>, CoordinateHandler> {
public:
  CoordinateHandler() : state_(kStart), x_(), y_(), z_() {}

  bool Double(double d) {
    switch (state_) {
      case kX: x_ += d; state_ = kCoordinatesElement; break;
      case kY: y_ += d; state_ = kCoordinatesElement; break;
      case kZ: z_ += d; state_ = kCoordinatesElement; break;
      default: break;
    }
    return true;
  }

  bool StartObject() {
    switch (state_) {
      case kStart: state_ = kRoot; break;
      case kCoordinatesArray: state_ = kCoordinatesElement; break;
      default: break;
    }
    return true;
  }

  bool Key(const Ch* str, SizeType len, bool copy) {
    switch (state_) {
      case kRoot:
        if (len == sizeof("coordinates") - 1 && memcmp(str, "coordinates", sizeof("coordinates") - 1) == 0)
          state_ = kCoordinates;
        break;

      case kCoordinatesElement:
        if (len == 1 && str[0] >= 'x' && str[0] <= 'z')
          state_ = static_cast<State>(kX + (str[0] - 'x'));
        break;

      default: break;
    }
    return true;
  }

  bool EndObject(SizeType) {
    switch (state_) {
      case kCoordinatesElement: state_ = kCoordinatesArray; break;
      default: break;
    }
    return true;
  }

  bool StartArray() {
    switch (state_) {
      case kCoordinates: state_ = kCoordinatesArray; break;
      default: break;
    }
    return true;
  }

  bool EndArray(SizeType len) {
    switch (state_) {
      case kCoordinatesArray:
        std::cout << x_ / len << std::endl;
        std::cout << y_ / len << std::endl;
        std::cout << z_ / len << std::endl;
        state_ = kCoordinates;
        break;

      default: break;
    }
    return true;
  }

private:
  enum State {
    kStart,
    kRoot,
    kCoordinates,
    kCoordinatesArray,
    kCoordinatesElement,
    kX,
    kY,
    kZ
  }state_;
  double x_, y_, z_;
};

void read_file(const string& filename, stringstream &buffer) {
  ifstream f(filename);
  if (f.good()) {
    buffer << f.rdbuf();
  }
}

int main() {
    stringstream ss;
    read_file("/tmp/1.json", ss);

    stringstream ostr;
    ostr << "C++ RapidJSON SAX\t" << getpid();
    notify(ostr.str());

    IStreamWrapper isw(ss);
    Reader reader;
    CoordinateHandler handler;
    reader.Parse(isw, handler);

    notify("stop");
}
