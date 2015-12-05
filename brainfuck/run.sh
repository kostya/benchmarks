echo Crystal
../xtime.rb ./brainfuck_cr bench.b
echo Go
../xtime.rb ./brainfuck_go bench.b
echo GccGo
../xtime.rb ./brainfuck_go_gccgo bench.b
echo Cpp
../xtime.rb ./brainfuck_cpp bench.b
echo Rust
../xtime.rb ./brainfuck_rs bench.b
echo D
../xtime.rb ./brainfuck_d bench.b
echo D Gdc
../xtime.rb ./brainfuck_d_gdc bench.b
echo D Ldc
../xtime.rb ./brainfuck_d_ldc bench.b
echo Nim Gcc
../xtime.rb ./brainfuck_nim_gcc bench.b
echo Nim Clang
../xtime.rb ./brainfuck_nim_clang bench.b
echo Scala
../xtime.rb scala BrainFuck bench.b
echo Javascript V8
../xtime.rb d8 brainfuck.d8.js
echo Javascript Node
../xtime.rb node brainfuck.js bench.b
echo Javascript Jx
../xtime.rb jx brainfuck.js bench.b
echo Julia
../xtime.rb julia brainfuck.jl bench.b
echo Mono
../xtime.rb mono -O=all --gc=sgen brainfuck.exe bench.b
echo Python Pypy
../xtime.rb pypy brainfuck.py bench.b
echo Python
../xtime.rb python brainfuck.py bench.b
echo Ruby
../xtime.rb ruby brainfuck.rb bench.b
echo Ruby Topaz
../xtime.rb topaz brainfuck.rb bench.b
echo OOC
../xtime.rb ./brainfuck_ooc bench.b
echo Felix
../xtime.rb ./brainfuck_flx bench.b
echo Tcl
../xtime.rb tclsh brainfuck.tcl bench.b
echo Java
../xtime.rb java brainfuck bench.b
