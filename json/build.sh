if [ ! -f 1.json ]; then
  ruby generate_json.rb
fi

crystal build test.cr --release -o json_cr
crystal build test_pull.cr --release -o json_pull_cr
crystal build test_schema.cr --release -o json_schema_cr
rustc --opt-level 3 test.rs -o json_rs
dmd -ofjson_d -O -release -inline test.d
gdc -o json_d_gdc -O3 -frelease -finline test.d
ldc2 -ofjson_d_ldc -O5 -release -inline json.d
nim c -o:json_nim -d:release --cc:clang --verbosity:0 test.nim
scalac -optimize test.scala
go build -o json_go test.go
g++ -O3 test_boost.cpp -o json_boost_cpp

if [ ! -d rapidjson ]; then
  git clone --depth 1 https://github.com/miloyip/rapidjson.git
fi
g++ -O3 test_rapid.cpp -o json_rapid_cpp -Irapidjson/include
g++ -O3 test_libjson.cpp -o json_libjson_cpp -ljson
julia -e 'Pkg.add("JSON")'
