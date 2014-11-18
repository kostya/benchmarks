crystal build test.cr --release -o json_cr
crystal build test_pull.cr --release -o json_pull_cr
crystal build test_schema.cr --release -o json_schema_cr
rustc --opt-level 3 test.rs -o json_rs
dmd -ofjson_d -O -release test.d
nimrod c -o:json_nim -d:release --verbosity:0 --hints:off test.nim
scalac -optimize test.scala
go build -o json_go test.go
g++ -O3 test_boost.cpp -o json_boost_cpp

if [ ! -d rapidjson ]; then
  git clone --depth 1 https://github.com/miloyip/rapidjson.git
fi
g++ -O3 test_rapid.cpp -o json_rapid_cpp -Irapidjson/include
g++ -O3 test_libjson.cpp -o json_libjson_cpp -ljson
