echo Crystal
../xtime.rb ./json_cr
echo Crystal Pull
../xtime.rb ./json_pull_cr
echo Crystal Schema
../xtime.rb ./json_schema_cr
echo Javascript Node
../xtime.rb node test.js
echo Javascript Jx
../xtime.rb jx test.js
echo Rust Pull
../xtime.rb ./json_pull_rs
echo Rust Struct
../xtime.rb ./json_struct_rs
echo Rust Value
../xtime.rb ./json_value_rs
echo Go
../xtime.rb ./json_go
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
echo Python Pypy
../xtime.rb pypy test.py
echo Python
../xtime.rb python test.py
echo Python3
../xtime.rb python3 test.py
echo Python ujson
../xtime.rb python test_ujson.py
echo Python3 ujson
../xtime.rb python3 test_ujson.py
echo C++ Boost
../xtime.rb ./json_boost_cpp
echo C++ Rapid
../xtime.rb ./json_rapid_cpp
echo C++ Rapid SAX
../xtime.rb ./json_rapid_sax_cpp
echo C++ Gason
../xtime.rb ./json_gason_cpp
echo C++ LibJson
../xtime.rb ./json_libjson_cpp
echo Julia
../xtime.rb julia test.jl
echo Mono
../xtime.rb mono -O=all --gc=sgen test.exe
echo Ruby
../xtime.rb ruby test.rb
echo Ruby YAJL
../xtime.rb ruby test-yajl.rb
echo Scala
../xtime.rb java -server -jar scala-json.jar
echo q
../xtime.rb q test.q -q
echo Perl
../xtime.rb perl -Iperllib/lib/perl5 test.pl
echo Perl XS
../xtime.rb perl -Iperllib/lib/perl5 test-xs.pl
echo Haskell
../xtime.rb ./json_hs
echo Clojure
../xtime.rb java -server -jar test.jar
echo jq
../xtime.rb jq -r '.coordinates | length as $len | (map(.x) | add) / $len, (map(.y) | add) / $len, (map(.z) | add) / $len' 1.json
echo Java
../xtime.rb java -server -jar java-json.jar

