#include <boost/format.hpp>
#include <cstdio>
#include <fstream>
#include <functional>
#include <iostream>
#include <libnotify.hpp>
#include <rapidjson/istreamwrapper.h>
#include <rapidjson/reader.h>
#include <sstream>
#include <unistd.h>

using namespace std;
using namespace rapidjson;

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

using TCallback = function<void(const coordinate_t&)>;

class CoordinateHandler : public BaseReaderHandler<UTF8<>, CoordinateHandler> {
public:
  CoordinateHandler(const TCallback& callback):
    state_(kStart), x_(), y_(), z_(), callback_(callback) {}

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
        callback_(coordinate_t(x_ / len, y_ / len, z_ / len));
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
  } state_;

  double x_, y_, z_;

  TCallback callback_;
};


void read_file(const string& filename, stringstream &buffer) {
  ifstream f(filename);
  if (f.good()) {
    buffer << f.rdbuf();
  }
}

void calc(stringstream& ss, const TCallback& callback) {
  IStreamWrapper isw(ss);
  Reader reader;
  CoordinateHandler handler(callback);
  reader.Parse(isw, handler);
}

int main() {
  auto right = coordinate_t(2.0, 0.5, 0.25);
  for (auto v : {
          "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
          "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"}) {
    auto json = stringstream(v);
    calc(json,
         [right](const coordinate_t& left) {
             if (left != right) {
                 cerr << left << " != " << right << endl;
                 exit(EXIT_FAILURE);
             }
         });
  }

  stringstream ss;
  read_file("/tmp/1.json", ss);

  notify(str(boost::format("C++/g++ (RapidJSON SAX)\t%d") % getpid()));
  calc(ss, [](const coordinate_t& results) {
    notify("stop");

    cout << results << endl;
  });
}
