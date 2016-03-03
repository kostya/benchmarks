if [ ! -f 1.json ]; then
  ruby generate_json.rb
fi

crystal build test.cr --release -o json_cr
crystal build test_pull.cr --release -o json_pull_cr
crystal build test_schema.cr --release -o json_schema_cr
cargo build --manifest-path json.rs/Cargo.toml --release && \
  cp ./json.rs/target/release/json-pull-rs ./json_pull_rs && \
  cp ./json.rs/target/release/json-struct-rs ./json_struct_rs && \
  cp ./json.rs/target/release/json-value-rs ./json_value_rs
dmd -ofjson_d -O -release -inline test.d
gdc -o json_d_gdc -O3 -frelease -finline test.d
ldc2 -ofjson_d_ldc -O5 -release -inline test.d
nim c -o:json_nim_gcc -d:release --cc:gcc --verbosity:0 test.nim
nim c -o:json_nim_clang -d:release --cc:clang --verbosity:0 test.nim
go build -o json_go test.go
gccgo -O3 -g -o json_go_gccgo test.go
g++ -O3 test_boost.cpp -o json_boost_cpp

if [ ! -d fast ]; then
  git clone --depth 1 --branch v0.3.0 https://github.com/mleise/fast.git
fi
gdc -o json_d_gdc_fast -O3 -frelease test_fast.d fast/source/fast/cstring.d fast/source/fast/buffer.d fast/source/fast/helpers.d fast/source/fast/json.d fast/source/fast/parsing.d fast/source/fast/intmath.d

if [ ! -d rapidjson ]; then
  git clone --depth 1 https://github.com/miloyip/rapidjson.git
fi
g++ -O3 test_rapid.cpp -o json_rapid_cpp -Irapidjson/include
g++ -O3 test_rapid_sax.cpp -o json_rapid_sax_cpp -Irapidjson/include

if [ ! -d gason ]; then
  git clone --depth 1 https://github.com/vivkin/gason.git
fi
g++ -std=c++11 test_gason.cpp -I gason/src/ gason/src/gason.cpp -o json_gason_cpp -O3

g++ -O3 test_libjson.cpp -o json_libjson_cpp -ljson
julia -e 'Pkg.add("JSON")'
# mono
nuget install Newtonsoft.Json
cp Newtonsoft.Json.*/lib/net45/Newtonsoft.Json.dll .
mcs -debug- -optimize+ -r:Newtonsoft.Json.dll test.cs

gem install yajl-ruby

wget -qO - https://cpanmin.us | perl - -L perllib Cpanel::JSON::XS JSON::Tiny File::Slurper

# haskell
cd json-hs; make; cd ..

# clojure
cd json-clj; lein uberjar; cd ..; cp json-clj/test.jar ./

# python/python3/pypy
pip install ujson
pip3 install ujson

# java
cd json-java; mvn clean install; cp target/java-json-1.0-jar-with-dependencies.jar ../java-json.jar; cd ..

#scala
cd json-scala; sbt clean assembly; cp target/scala-2.11/benchmark-json-scala-assembly-1.0.jar ../scala-json.jar; cd ..