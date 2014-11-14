crystal build test.cr --release -o base64_cr
go build -o base64_go test.go
g++ -O3 -o base64_cpp test.cpp -lcrypto
scalac -optimize test.scala
rustc --opt-level 3 test.rs -o base64_rs
dmd -ofbase64_d -O -release test.d
nimrod c -o:base64_nim -d:release --verbosity:0 --hints:off test.nim
