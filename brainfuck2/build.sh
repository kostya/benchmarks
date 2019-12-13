#!/bin/sh

set -e
crystal build bf.cr --release -o bin_cr --no-debug
g++ -flto -O3 -o bin_cpp bf.cpp -lsocket++
rustc -C opt-level=3 -C lto bf.rs -o bin_rs
scalac bf.scala
mcs -debug- -optimize+ bf.cs
dotnet build brainfuck2.csproj -c Release
javac bf.java
kotlinc bf2.kt -include-runtime -d bf2-kt.jar
go build -o bin_go bf.go
gccgo -O3 -g -o bin_go_gccgo bf.go
dmd -ofbin_d -O -release -inline bf.d
gdc -o bin_d_gdc -O3 -frelease -finline bf.d
ldc2 -ofbin_d_ldc -O5 -release bf.d
nim c -o:bin_nim_clang -d:danger --cc:clang --verbosity:0 bf.nim
nim c -o:bin_nim_gcc -d:danger --cc:gcc --verbosity:0 bf.nim
ghc -O2 -fforce-recomp bf.hs -o bin_hs
ghc -O2 -fforce-recomp bf-marray.hs -o bin_hs_marray
ocamlopt -unsafe unix.cmxa bf.ml -o bin_ocaml
dotnet build brainfuck2.fsproj -c Release
mlton -output bin_sml bf.sml
v -prod -cc gcc -o bin_v_gcc bf.v
v -prod -cc clang -o bin_v_clang bf.v
