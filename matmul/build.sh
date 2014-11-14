crystal build matmul.cr --release -o matmul_cr
go build -o matmul_go matmul.go
gcc -O3 -o matmul_c matmul.c
scalac -optimize matmul.scala
rustc --opt-level 3 matmul.rs -o matmul_rs
dmd -ofmatmul_d -O -release matmul.d
nimrod c -o:matmul_nim -d:release --verbosity:0 --hints:off matmul.nim
javac matmul.java
