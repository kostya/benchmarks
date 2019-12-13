#!/bin/sh

set -e
crystal build test.cr --release -o base64_cr --no-debug
go build -o base64_go test.go
gccgo -O3 -g -o base64_go_gccgo test.go
g++ -O3 -o base64_cpp test.cpp -lcrypto -lsocket++
gcc -O3 -o base64_c test.c -lsocket
scalac test.scala
javac Base64Java.java
kotlinc Test.kt -include-runtime -d Test-kt.jar
dmd -ofbase64_d -O -release -inline test.d
gdc -o base64_d_gdc -O3 -frelease -finline test.d
ldc2 -ofbase64_d_ldc -O5 -release test.d
nim c -o:base64_nim_gcc -d:danger --cc:gcc --verbosity:0 test.nim
nim c -o:base64_nim_clang -d:danger --cc:clang --verbosity:0 test.nim
cargo build --manifest-path base64.rs/Cargo.toml --release && cp ./base64.rs/target/release/base64 ./base64_rs
mcs -debug- -optimize+ test.cs
dotnet build -c Release

if [ ! -d aklomp-base64 ]; then
  git clone --depth 1 https://github.com/aklomp/base64.git aklomp-base64
  cd aklomp-base64
  AVX2_CFLAGS=-mavx2 SSSE3_CFLAGS=-mssse3 AVX_CFLAGS=-mavx make
  cd -
fi
gcc -O3 test-aklomp.c -I aklomp-base64/include/ aklomp-base64/lib/libbase64.o -o base64_c_ak -lsocket
wget -qO - https://cpanmin.us | perl - -L perllib MIME::Base64::Perl
v -prod -cc gcc -o base64_v_gcc test.v
v -prod -cc clang -o base64_v_clang test.v
