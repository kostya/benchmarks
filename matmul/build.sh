crystal build matmul.cr --release -o matmul_cr
go build -o matmul_go matmul.go
gcc -O3 -o matmul_c matmul.c
scalac -optimize matmul.scala
rustc -C opt-level=3 matmul.rs -o matmul_rs
dmd -ofmatmul_d -O -release -inline matmul.d
gdc -o matmul_d_gdc -O3 -frelease -finline matmul.d
ldc2 -ofmatmul_d_ldc -O5 -release -inline matmul.d
nim c -o:matmul_nim -d:release --verbosity:0 matmul.nim
javac matmul.java
