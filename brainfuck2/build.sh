#!/bin/sh

crystal build bf.cr --release -o bin_cr --no-debug
g++ -flto -O3 -o bin_cpp bf.cpp
rustc -C opt-level=3 -C lto bf.rs -o bin_rs
scalac -opt:l:inline -deprecation bf.scala
mcs -debug- -optimize+ bf.cs
dotnet build -c Release
javac bf.java
kotlinc bf2.kt -include-runtime -d bf2-kt.jar
go build -o bin_go bf.go
gccgo -O3 -g -o bin_go_gccgo bf.go
dmd -ofbin_d -O -release -inline bf.d
gdc -o bin_d_gdc -O3 -frelease -finline bf.d
ldc2 -ofbin_d_ldc -flto=full -O5 -release -boundscheck=off -cache-prune bf.d
nim c -o:bin_nim_clang -d:release --cc:clang --verbosity:0 bf.nim
nim c -o:bin_nim_gcc -d:release --cc:gcc --verbosity:0 bf.nim
stack ghc -- -O2 -fforce-recomp bf.hs -o bin_hs
stack ghc -- -O2 -fforce-recomp bf-marray.hs -o bin_hs_marray
ocamlopt bf.ml -o bin_ocaml
fsharpc bf.fs -o bin_fs.exe
mlton -output bin_sml bf.sml
