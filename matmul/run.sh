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
echo Javascript V8
../xtime.rb d8 matmul.d8.js -- 1500
echo Javascript Jx
../xtime.rb jx matmul.js 1500
echo Javascript Node
../xtime.rb node matmul.js 1500
echo Julia Native
../xtime.rb julia matmul-native.jl 1500
echo Julia
../xtime.rb julia matmul.jl 1500
echo Mono
../xtime.rb mono -O=all --gc=sgen matmul.exe 1500
echo Python Pypy
../xtime.rb pypy matmul.py 1500
echo Python
../xtime.rb python matmul.py 1500
echo Python NumPy
../xtime.rb python matmul-numpy.py 1500
echo Ruby Topaz
../xtime.rb topaz matmul.rb 1500
echo Ruby
../xtime.rb ruby matmul.rb 1500
echo Perl
../xtime.rb perl matmul.pl 1500
echo Tcl
../xtime.rb tclsh matmul.tcl 1500
echo Swift
../xtime.rb swift -O matmul.swift 1500
