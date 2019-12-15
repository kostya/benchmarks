#!/bin/sh

set -e
crystal build brainfuck.cr --release -o brainfuck_cr
go build -o brainfuck_go brainfuck.go
gccgo -O3 -g -o brainfuck_go_gccgo brainfuck.go
g++ -O3 -o brainfuck_cpp brainfuck.cpp -lsocket++
scalac brainfuck.scala
rustc -C opt-level=3 brainfuck.rs -o brainfuck_rs
dmd -ofbrainfuck_d -O -release -inline brainfuck.d
gdc -o brainfuck_d_gdc -O3 -frelease -finline brainfuck.d
ldc2 -ofbrainfuck_d_ldc -O5 -release brainfuck.d
nim c -o:brainfuck_nim_clang -d:danger --cc:clang --verbosity:0 brainfuck.nim
nim c -o:brainfuck_nim_gcc -d:danger --cc:gcc --verbosity:0 brainfuck.nim
mcs -debug- -optimize+ brainfuck.cs
dotnet build -c Release
javac brainfuck.java
