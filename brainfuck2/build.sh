crystal build bf.cr --release -o bin_cr
g++ -O3 -o bin_cpp bf.cpp
rustc -C opt-level=3 bf.rs -o bin_rs
scalac -optimize bf.scala
