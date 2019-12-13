#!/bin/sh

set -e
echo Crystal
../xtime.rb ./json_cr
echo Crystal Pull
../xtime.rb ./json_pull_cr
echo Crystal Schema
../xtime.rb ./json_schema_cr
echo Javascript Node
../xtime.rb node test.js
echo Rust Pull
../xtime.rb ./json_pull_rs
echo Rust Struct
../xtime.rb ./json_struct_rs
echo Rust Value
../xtime.rb ./json_value_rs
echo Go
../xtime.rb ./json_go
echo Go jsoniter
../xtime.rb ./json_iter_go
echo GccGo
../xtime.rb ./json_go_gccgo
echo D DMD
../xtime.rb ./json_d
echo D Gdc
../xtime.rb ./json_d_gdc
echo D Gdc fast
../xtime.rb ./json_d_gdc_fast
echo D Ldc
../xtime.rb ./json_d_ldc
echo Nim Gcc
../xtime.rb ./json_nim_gcc
echo Nim Clang
../xtime.rb ./json_nim_clang
echo Python PyPy
../xtime.rb pypy3 test.py
echo Python
../xtime.rb python3 test.py
echo Python ujson
../xtime.rb python3 test_ujson.py
echo C++ Boost
../xtime.rb ./json_boost_cpp
echo C++ Rapid
../xtime.rb ./json_rapid_cpp
echo C++ Rapid SAX
../xtime.rb ./json_rapid_sax_cpp
echo C++ Simdjson
../xtime.rb ./json_simdjson_cpp
echo C++ Gason
../xtime.rb ./json_gason_cpp
echo C++ LibJson
../xtime.rb ./json_libjson_cpp
echo Mono
../xtime.rb mono -O=all --gc=sgen test.exe
echo C# .Net Core
../xtime.rb dotnet bin/Release/netcoreapp3.0/json.dll
echo Ruby
../xtime.rb ruby test.rb
echo Ruby YAJL
../xtime.rb ruby test-yajl.rb
echo Scala
../xtime.rb scala -cp json-scala/target/application.jar JsonTest
echo Perl
../xtime.rb perl -Iperllib/lib/perl5 test.pl
echo Perl XS
../xtime.rb perl -Iperllib/lib/perl5 test-xs.pl
echo Rust jq
../xtime.rb ./json_jq_rs
echo Java
../xtime.rb json-java/target/application
echo Php
../xtime.rb php test.php
echo V Gcc
../xtime.rb ./json_v_gcc
echo V Clang
../xtime.rb ./json_v_clang
echo Haskell
../xtime.rb ./json_hs
echo JRuby
../xtime.rb jruby test.rb
echo TruffleRuby
../xtime.rb truffleruby --jvm test.rb
echo Julia
../xtime.rb julia --optimize=3 --check-bounds=no test.jl
echo Clojure
../xtime.rb clojure -Sdeps '{:deps {cheshire {:mvn/version "5.9.0"}}}' test.clj
