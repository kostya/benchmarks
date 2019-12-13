#!/bin/sh

set -e
crystal build matmul.cr --release -o matmul_cr --no-debug
go build -o matmul_go matmul.go
gccgo -O3 -g -o matmul_go_gccgo matmul.go
gcc -O3 -o matmul_c matmul.c -lsocket
scalac matmul.scala
rustc -C opt-level=3 -C lto matmul.rs -o matmul_rs
dmd -ofmatmul_d -O -release -inline matmul.d
gdc -o matmul_d_gdc -O3 -frelease -finline matmul.d
ldc2 -ofmatmul_d_ldc -O5 -release matmul.d
dub build --build=release --single matmul_d_lubeck.d --compiler=ldc2
nim c -o:matmul_nim_gcc --cc:gcc -d:danger --verbosity:0 matmul.nim
nim c -o:matmul_nim_clang --cc:clang -d:danger --verbosity:0 matmul.nim
javac matmul.java
kotlinc matmul.kt -include-runtime -d matmul-kt.jar
mcs -debug- -optimize+ matmul.cs
dotnet build -c Release
v -prod -cc gcc -o matmul_v_gcc matmul.v
v -prod -cc clang -o matmul_v_clang matmul.v

# numpy for matrix mult in python
# brew install numpy
# sudo apt-get install -y python-numpy

# To use math::linearalgebra in Tcl install the package 'tcllib'.
# sudo apt-get install tcllib
