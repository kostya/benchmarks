#!/bin/sh

set -e

crystal build test.cr --release -o json_cr --no-debug
crystal build test_pull.cr --release -o json_pull_cr --no-debug
crystal build test_schema.cr --release -o json_schema_cr --no-debug
cargo build --manifest-path json.rs/Cargo.toml --release && \
  cp ./json.rs/target/release/json-pull-rs ./json_pull_rs && \
  cp ./json.rs/target/release/json-struct-rs ./json_struct_rs && \
  cp ./json.rs/target/release/json-value-rs ./json_value_rs && \
  cp ./json.rs/target/release/json-jq-rs ./json_jq_rs
dmd -ofjson_d -O -release -inline test.d
gdc -o json_d_gdc -O3 -frelease -finline test.d
ldc2 -ofjson_d_ldc -O5 -release test.d
nim c -o:json_nim_gcc -d:danger --cc:gcc --verbosity:0 test.nim
nim c -o:json_nim_clang -d:danger --cc:clang --verbosity:0 test.nim
go build -o json_go test.go
gccgo -O3 -g -o json_go_gccgo test.go
g++ -O3 test_boost.cpp -o json_boost_cpp -lsocket++

if [ ! -d fast ]; then
  git clone --depth 1 --branch v0.3.0 https://github.com/mleise/fast.git
fi
gdc -o json_d_gdc_fast -O3 -frelease test_fast.d fast/source/fast/cstring.d fast/source/fast/buffer.d fast/source/fast/helpers.d fast/source/fast/json.d fast/source/fast/parsing.d fast/source/fast/intmath.d

if [ ! -d rapidjson ]; then
  git clone --depth 1 https://github.com/miloyip/rapidjson.git
fi
g++ -O3 test_rapid.cpp -o json_rapid_cpp -Irapidjson/include -lsocket++
g++ -O3 test_rapid_sax.cpp -o json_rapid_sax_cpp -Irapidjson/include -lsocket++

if [ ! -d gason ]; then
  git clone --depth 1 https://github.com/vivkin/gason.git
fi
g++ -std=c++11 test_gason.cpp -I gason/src/ gason/src/gason.cpp -o json_gason_cpp -O3 -lsocket++

g++ -O3 test_libjson.cpp -o json_libjson_cpp -ljson-c -lsocket++
julia -e 'using Pkg; Pkg.add("JSON")'
# mono
nuget install Newtonsoft.Json
cp Newtonsoft.Json.*/lib/net45/Newtonsoft.Json.dll .
mcs -debug- -optimize+ -r:Newtonsoft.Json.dll test.cs

# .net core
dotnet build -c Release

gem install yajl-ruby

wget -qO - https://cpanmin.us | perl - -L perllib Cpanel::JSON::XS JSON::Tiny File::Slurper

# haskell
if ! [ -x "$(command -v cabal)" ]; then
  echo 'Please install cabal (https://www.haskell.org/downloads/linux/).' >&2
  exit 1
fi
cd json-hs; make; cd ..

# python/python3/pypy
# pip install ujson
# pip3 install ujson

# java builds require coursier
if ! [ -x "$(command -v coursier)" ]; then
  echo 'Please install coursier (https://get-coursier.io/docs/cli-overview.html#installation).' >&2
  exit 1
fi

# java
cd json-java; make clean target/application; cd ..

#scala
cd json-scala; make clean target/application.jar; cd ..

# simdjson
if [ ! -d simdjson ]; then
  git clone --depth 1 https://github.com/lemire/simdjson.git
fi
g++ -O3 -msse4.2 -mpclmul -std=c++17 -Isimdjson/include/ -Isimdjson/src/ simdjson/src/jsonioutil.cpp simdjson/src/jsonparser.cpp simdjson/src/simdjson.cpp simdjson/src/stage1_find_marks.cpp simdjson/src/stage2_build_tape.cpp simdjson/src/parsedjson.cpp simdjson/src/parsedjsoniterator.cpp test_simdjson.cpp -o json_simdjson_cpp -lsocket++

v -prod -cc gcc -o json_v_gcc test.v
v -prod -cc clang -o json_v_clang test.v

go get github.com/json-iterator/go
go build -o json_iter_go test_jsoniter.go

if [ ! -f 1.json ]; then
  ruby generate_json.rb
fi
