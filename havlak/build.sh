crystal build havlak.cr --release -o havlak_cr
go build -o havlak_go havlak.go
g++ -O3 -o havlak_cpp havlak.cpp
scalac -optimize havlak.scala
rustc --opt-level 3 havlak.rs -o havlak_rs
dmd -ofhavlak_d -O -release havlak.d
nim c -o:havlak_nim -d:release --verbosity:0 --hints:off havlak.nim
