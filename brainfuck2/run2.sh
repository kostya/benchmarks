echo Cpp
../xtime.rb ./bin_cpp mandel.b > /dev/null
echo D
../xtime.rb ./bin_d mandel.b > /dev/null
echo D Gdc
../xtime.rb ./bin_d_gdc mandel.b > /dev/null
echo D Ldc
../xtime.rb ./bin_d_ldc mandel.b > /dev/null
echo Rust
../xtime.rb ./bin_rs mandel.b > /dev/null
echo Nim Gcc
../xtime.rb ./bin_nim_gcc mandel.b > /dev/null
echo Nim Clang
../xtime.rb ./bin_nim_clang mandel.b > /dev/null
echo Crystal
../xtime.rb ./bin_cr mandel.b > /dev/null
echo F# Mono
../xtime.rb mono -O=all --gc=sgen bin_fs.exe mandel.b > /dev/null
echo "Haskell (MArray)"
../xtime.rb ./bin_hs_marray mandel.b > /dev/null
echo Java
../xtime.rb java bf mandel.b > /dev/null
echo Scala
../xtime.rb scala BrainFuck mandel.b > /dev/null
echo Kotlin
../xtime.rb java -jar bf2-kt.jar mandel.b > /dev/null
echo OCaml
../xtime.rb ./bin_ocaml mandel.b > /dev/null
echo Go
../xtime.rb ./bin_go mandel.b > /dev/null
echo Go Gcc
../xtime.rb ./bin_go_gccgo mandel.b > /dev/null
echo Javascript Node
../xtime.rb node bf.js mandel.b > /dev/null
echo C# Mono
../xtime.rb mono -O=all --gc=sgen netfx/brainfuck2.exe mandel.b > /dev/null
echo C# .NET Core
../xtime.rb dotnet netcore/brainfuck2.dll mandel.b > /dev/null
echo Python PyPy
../xtime.rb pypy bf.py mandel.b > /dev/null
echo Ruby Topaz
../xtime.rb topaz bf.rb mandel.b > /dev/null
echo Standard ML MLton
../xtime.rb ./bin_sml mandel.b > /dev/null
echo LuaJIT
../xtime.rb luajit bf.lua mandel.b > /dev/null
echo Racket
../xtime.rb racket bf.rkt mandel.b > /dev/null
echo Chez Scheme
../xtime.rb scheme --script bf.ss mandel.b > /dev/null
