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

#include <daw/daw_memory_mapped_file.h>
#include <daw/json/daw_json_iterator.h>
#include <daw/json/daw_json_link.h>

#include <iostream>
#include <libnotify.hpp>
#include <string_view>

struct coordinate_t {
	double x;
	double y;
	double z;
	// ignore string name
	// ignore object opts
};

struct coordinates_t {
	std::vector<coordinate_t> coordinates;
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

int main( int argc, char *argv[] ) {
	std::ios_base::sync_with_stdio( false );
	std::string_view fpath = "/tmp/1.json";
	if( argc > 1 ) {
		fpath = argv[1];
	}
	auto const text = daw::filesystem::memory_mapped_file_t<>( fpath );
	auto const json_sv = std::string_view( text.data( ), text.size( ) );
	double x = 0, y = 0, z = 0;
	int len = 0;

	using range_t = daw::json::json_array_range<coordinate_t, true>;
	auto rng = range_t( json_sv, "coordinates" );

	{
		std::stringstream ostr;
		ostr << "C++ DAW JSON Link\t" << getpid( );
		notify( ostr.str( ) );
	}

	for( auto c : rng ) {
		++len;
		x += c.x;
		y += c.y;
		z += c.z;
	}
	std::cout << x / len << '\n';
	std::cout << y / len << '\n';
	std::cout << z / len << '\n';

	notify( "stop" );

	return EXIT_SUCCESS;
}
