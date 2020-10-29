// The MIT License (MIT)
//
// Copyright (c) 2020 Darrell Wright
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files( the "Software" ), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and / or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <daw/json/daw_json_iterator.h>
#include <daw/json/daw_json_link.h>

#include <boost/format.hpp>
#include <fstream>
#include <iostream>
#include <libnotify.hpp>
#include <sstream>
#include <string_view>
#include <unistd.h>

using namespace std;

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

struct coordinates_t {
  vector<coordinate_t> coordinates;
};

namespace daw::json {
  template<>
  struct json_data_contract<coordinate_t> {
#ifdef __cpp_nontype_template_parameter_class
    using type =
      json_member_list<json_number<"x">, json_number<"y">, json_number<"z">>;
#else
    constexpr inline static char const x[] = "x";
    constexpr inline static char const y[] = "y";
    constexpr inline static char const z[] = "z";
    using type =
      json_member_list<json_number<x>, json_number<y>, json_number<z>>;
#endif
  };

  template<>
  struct json_data_contract<coordinates_t> {
#ifdef __cpp_nontype_template_parameter_class
    using type = json_member_list<json_array<"coordinates", coordinate_t>>;
#else
    constexpr inline static char const coordinates[] = "coordinates";
    using type = json_member_list<json_array<coordinates, coordinate_t>>;
#endif
  };
} // namespace daw::json

string read_file(const string& filename) {
  ifstream f(filename);
  if (!f) {
    return {};
  }
  return string(istreambuf_iterator<char>(f),
                istreambuf_iterator<char>());
}

coordinate_t calc(const string& text) {
  auto const json_sv = string_view(text.data(), text.size());
  auto x = 0.0, y = 0.0, z = 0.0;
  auto len = 0;

  using range_t = daw::json::json_array_range<coordinate_t, daw::json::NoCommentSkippingPolicyUnchecked>;
  auto rng = range_t(json_sv, "coordinates");

  for (auto c : rng) {
    ++len;
    x += c.x;
    y += c.y;
    z += c.z;
  }

  return coordinate_t(x / len, y / len, z / len);
}

int main() {
  auto right = coordinate_t(2.0, 0.5, 0.25);
  for (auto v : {
          "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
          "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}"}) {
    auto left = calc(v);
    if (left != right) {
        cerr << left << " != " << right << endl;
        exit(EXIT_FAILURE);
    }
  }

  const auto& text = read_file( "/tmp/1.json" );

  notify(str(boost::format("C++/g++ (DAW JSON Link)\t%d") % getpid()));
  const auto& results = calc(text);
  notify( "stop" );

  cout << results << endl;
}
