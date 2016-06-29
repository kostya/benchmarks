crystal build matmul.cr --release -o matmul_cr
go build -o matmul_go matmul.go
gccgo -O3 -g -o matmul_go_gccgo matmul.go
gcc -O3 -o matmul_c matmul.c
scalac -optimize matmul.scala
rustc -C opt-level=3 matmul.rs -o matmul_rs
dmd -ofmatmul_d -O -release -inline matmul.d
gdc -o matmul_d_gdc -O3 -frelease -finline matmul.d
ldc2 -ofmatmul_d_ldc -O5 -release -inline matmul.d
nim c -o:matmul_nim_gcc --cc:gcc -d:release --verbosity:0 matmul.nim
nim c -o:matmul_nim_clang --cc:clang -d:release --verbosity:0 matmul.nim
javac matmul.java
kotlinc matmul.kt -include-runtime -d matmul-kt.jar
mcs -debug- -optimize+ matmul.cs

# numpy for matrix mult in python
# brew install numpy
# sudo apt-get install -y python-numpy

# To use math::linearalgebra in Tcl install the package 'tcllib'.
# sudo apt-get install tcllib
