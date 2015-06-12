crystal build test.cr --release -o base64_cr
go build -o base64_go test.go
gccgo -O3 -g -o base64_go_gccgo test.go
g++ -O3 -o base64_cpp test.cpp -lcrypto
gcc -O3 -std=c99 -o base64_c test.c
scalac -optimize test.scala
dmd -ofbase64_d -O -release -inline test.d
gdc -o base64_d_gdc -O3 -frelease -finline test.d
ldc2 -ofbase64_d_ldc -O5 -release -inline test.d
nim c -o:base64_nim_gcc -d:release --cc:gcc --verbosity:0 test.nim
nim c -o:base64_nim_clang -d:release --cc:clang --verbosity:0 test.nim
julia -e 'Pkg.add("Codecs")'
cargo build --manifest-path base64.rs/Cargo.toml --release && cp ./base64.rs/target/release/base64 ./base64_rs
mcs -debug- -optimize+ test.cs

if [ ! -d aklomp-base64 ]; then
  git clone --depth 1 https://github.com/aklomp/base64.git aklomp-base64
  cd aklomp-base64
  #export AVX2_CFLAGS=-mavx2
  #export SSSE3_CFLAGS=-mssse3
  make
  cd -
fi
gcc --std=c99 -O3 test-aklomp.c -I aklomp-base64/include/ aklomp-base64/lib/libbase64.o -o base64_c_aklomp
