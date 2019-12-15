#!/bin/sh

set -e
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
echo C# .NET Core
../xtime.rb dotnet bin_cs/netcoreapp3.0/brainfuck2.dll bench.b
echo Elixir
../xtime.rb elixir bf.ex bench.b
echo F# .NET Core
../xtime.rb dotnet bin_fs/netcoreapp3.0/brainfuck2.dll bench.b
echo Haskell
../xtime.rb ./bin_hs bench.b
echo "Haskell (MArray)"
../xtime.rb ./bin_hs_marray bench.b
echo Javascript Node
../xtime.rb node bf.js bench.b
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
echo OCaml
../xtime.rb ./bin_ocaml bench.b
echo Python PyPy
../xtime.rb pypy3 bf3.py bench.b
echo Perl
../xtime.rb perl bf.pl bench.b
echo Ruby
../xtime.rb ruby bf.rb bench.b
echo JRuby
../xtime.rb jruby bf.rb bench.b
echo TruffleRuby
../xtime.rb truffleruby --jvm bf.rb bench.b
echo Python
../xtime.rb python3 bf3.py bench.b
echo Standard ML MLton
../xtime.rb ./bin_sml bench.b
echo Lua 5.3
../xtime.rb lua5.3 bf.lua bench.b
echo LuaJIT
../xtime.rb luajit bf.lua bench.b
echo "Tcl (FP)"
../xtime.rb tclsh bf.tcl bench.b
echo "Tcl (OO)"
../xtime.rb tclsh bf_oo.tcl bench.b
echo Racket
../xtime.rb racket bf.rkt bench.b
echo Chez Scheme
../xtime.rb scheme --script bf.ss bench.b
echo V Gcc
../xtime.rb ./bin_v_gcc bench.b
echo V Clang
../xtime.rb ./bin_v_clang bench.b
echo Julia
../xtime.rb julia --optimize=3 --check-bounds=no bf.jl bench.b
