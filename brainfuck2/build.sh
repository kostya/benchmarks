crystal build bf.cr --release -o bin_cr
g++ -O3 -o bin_cpp bf.cpp
rustc -C opt-level=3 bf.rs -o bin_rs
scalac -optimize bf.scala
mcs -debug- -optimize+ bf.cs
javac bf.java
kotlinc bf2.kt -include-runtime -d bf2-kt.jar
go build -o bin_go bf.go
gccgo -O3 -g -o bin_go_gccgo bf.go
dmd -ofbin_d -O -release -inline bf.d
gdc -o bin_d_gdc -O3 -frelease -finline bf.d
ldc2 -ofbin_d_ldc -O5 -release -inline bf.d
nim c -o:bin_nim_clang -d:release --cc:clang --verbosity:0 bf.nim
nim c -o:bin_nim_gcc -d:release --cc:gcc --verbosity:0 bf.nim

