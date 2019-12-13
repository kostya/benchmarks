#!/bin/sh

set -e
echo Crystal
../xtime.rb ./matmul_cr 1500
echo Go
../xtime.rb ./matmul_go 1500
echo GccGo
../xtime.rb ./matmul_go_gccgo 1500
echo C
../xtime.rb ./matmul_c 1500
echo Rust
../xtime.rb ./matmul_rs 1500
echo D
../xtime.rb ./matmul_d 1500
echo D Gdc
../xtime.rb ./matmul_d_gdc 1500
echo D Ldc
../xtime.rb ./matmul_d_ldc 1500
echo D lubeck
../xtime.rb ./matmul_d_lubeck 1500
echo Nim Gcc
../xtime.rb ./matmul_nim_gcc 1500
echo Nim Clang
../xtime.rb ./matmul_nim_clang 1500
echo Scala
../xtime.rb scala MatMul 1500
echo Java
../xtime.rb java matmul 1500
echo Kotlin
../xtime.rb java -jar matmul-kt.jar 1500
echo Javascript Node
../xtime.rb node matmul.js 1500
echo Julia Native Thr
../xtime.rb julia --optimize=3 --check-bounds=no matmul-native.jl 1500
echo Julia Native
OPENBLAS_NUM_THREADS=1 ../xtime.rb julia --optimize=3 --check-bounds=no matmul-native.jl 1500
echo Julia
../xtime.rb julia --optimize=3 --check-bounds=no matmul.jl 1500
echo Mono
../xtime.rb mono -O=all --gc=sgen matmul.exe 1500
echo C# .Net Core
../xtime.rb dotnet bin/Release/netcoreapp3.0/matmul.dll 1500
echo Python PyPy
../xtime.rb pypy3 matmul.py 1500
echo Python
../xtime.rb python3 matmul.py 1500
echo Python NumPy
../xtime.rb python3 matmul-numpy.py 1500
echo Ruby
../xtime.rb ruby matmul.rb 1500
echo JRuby
../xtime.rb jruby matmul.rb 1500
echo TruffleRuby
../xtime.rb truffleruby --jvm matmul.rb 1500
echo Perl
../xtime.rb perl matmul.pl 1500
echo Tcl
../xtime.rb tclsh matmul.tcl 1500
echo Swift
../xtime.rb swift -O matmul.swift 1500
echo V Gcc
../xtime.rb ./matmul_v_gcc 1500
echo V Clang
../xtime.rb ./matmul_v_clang 1500
