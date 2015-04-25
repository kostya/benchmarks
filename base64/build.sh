crystal build test.cr --release -o base64_cr
go build -o base64_go test.go
g++ -O3 -o base64_cpp test.cpp -lcrypto
gcc -O3 -std=c99 -o base64_c test.c
scalac -optimize test.scala
dmd -ofbase64_d -O -release -inline test.d
gdc -o base64_d_gdc -O3 -frelease -finline test.d
ldc2 -ofbase64_d_ldc -O5 -release -inline test.d
nim c -o:base64_nim -d:release --verbosity:0 test.nim
julia -e 'Pkg.add("Codecs")'
cargo build --manifest-path base64.rs/Cargo.toml --release && cp ./base64.rs/target/release/base64 ./base64_rs
mcs -debug- -optimize+ test.cs
