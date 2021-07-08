#!/bin/sh
readonly NUM=1000
echo C
../xtime.rb ./bin_test_c_clang $NUM 2>&1 > /dev/null
echo Cpp
../xtime.rb ./bin_test_c_gcc $NUM 2>&1 > /dev/null
echo Mono
../xtime.rb mono ./bin_test_cs $NUM 2>&1 > /dev/null
echo D
../xtime.rb ./bin_test_d_dmd $NUM 2>&1 > /dev/null
echo D Ldc
../xtime.rb ./bin_test_d_ldc $NUM 2>&1 > /dev/null
echo D Gdc
../xtime.rb ./bin_test_d_gdc $NUM 2>&1 > /dev/null
echo Go
../xtime.rb ./bin_test_go_gc $NUM 2>&1 > /dev/null
echo Go gcc
../xtime.rb ./bin_test_go_gccgo $NUM 2>&1 > /dev/null
echo Rust
../xtime.rb target/release/bin_test_rs $NUM 2>&1 >/dev/null
echo Nim gcc
../xtime.rb ./bin_test_nim_gcc $NUM 2>&1 > /dev/null
echo Nim clang
../xtime.rb ./bin_test_nim_clang $NUM 2>&1 > /dev/null
echo Crystal
../xtime.rb ./bin_test_cr $NUM 2>&1 > /dev/null
echo Java
../xtime.rb java -cp . test $NUM 2>&1 > /dev/null
echo Python Pypy
../xtime.rb pypy test.py $NUM 2>&1 > /dev/null
echo Python
../xtime.rb python test.py $NUM 2>&1 > /dev/null
