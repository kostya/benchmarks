crystal build test.cr --release -o base64_cr
go build -o base64_go test.go
g++ -O3 -o base64_cpp test.cpp -lcrypto
gcc -O3 -std=c99 -o base64_c test.c
scalac -optimize test.scala
rustc --opt-level 3 test.rs -o base64_rs
dmd -ofbase64_d -O -release test.d
nim c -o:base64_nim -d:release --cc:clang --verbosity:0 test.nim
julia -e 'Pkg.add("Codecs")'
