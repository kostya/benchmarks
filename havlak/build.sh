#!/bin/sh

set -e
crystal build havlak.cr --release -o havlak_cr --no-debug
go build -o havlak_go havlak.go
gccgo -O3 -g -o havlak_go_gccgo havlak.go
g++ -O3 -o havlak_cpp havlak.cpp -lsocket++
scalac havlak.scala
dmd -ofhavlak_d -O -release -inline havlak.d
gdc -o havlak_d_gdc -O3 -frelease -finline havlak.d
ldc2 -ofhavlak_d_ldc -O5 -release havlak.d
nim c -o:havlak_nim_gcc --cc:gcc -d:danger --verbosity:0 havlak.nim
nim c -o:havlak_nim_clang --cc:clang -d:danger --verbosity:0 havlak.nim
mcs -debug- -optimize+ havlak.cs
dotnet build -c Release
