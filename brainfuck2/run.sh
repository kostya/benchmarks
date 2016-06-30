echo Cpp
../xtime.rb ./bin_cpp bench.b
echo Crystal
../xtime.rb ./bin_cr bench.b
echo Rust
../xtime.rb ./bin_rs bench.b
echo Scala
../xtime.rb scala BrainFuck bench.b
echo Java
../xtime.rb java bf bench.b
echo Kotlin
../xtime.rb java -jar bf2-kt.jar bench.b
echo C# Mono
../xtime.rb mono -O=all --gc=sgen bf.exe bench.b
echo Javascript V8
../xtime.rb d8 bf.d8.js
echo Javascript Node
../xtime.rb node bf.js bench.b
echo Javascript Jx
../xtime.rb jx bf.js bench.b
echo Go
../xtime.rb ./bin_go bench.b
echo Go Gcc
../xtime.rb ./bin_go_gccgo bench.b
echo D Dmd
../xtime.rb ./bin_d bench.b
echo D Gdc
../xtime.rb ./bin_d_gdc bench.b
echo D Ldc
../xtime.rb ./bin_d_ldc bench.b
echo Nim Gcc
../xtime.rb ./bin_nim_gcc bench.b
echo Nim Clang
../xtime.rb ./bin_nim_clang bench.b
echo Ruby Topaz
../xtime.rb topaz bf.rb bench.b
echo Python Pypy
../xtime.rb pypy bf.py bench.b
echo Ruby
../xtime.rb ruby bf.rb bench.b
echo Python
../xtime.rb python bf.py bench.b
echo Python3
../xtime.rb python3 bf3.py bench.b
