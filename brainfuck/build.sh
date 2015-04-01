crystal build brainfuck.cr --release -o brainfuck_cr
go build -o brainfuck_go brainfuck.go
g++ -O3 -o brainfuck_cpp brainfuck.cpp
scalac -optimize brainfuck.scala
rustc --opt-level 3 brainfuck.rs -o brainfuck_rs
dmd -ofbrainfuck_d -O -release -inline brainfuck.d
gdc -o brainfuck_d_gdc -O3 -frelease -finline brainfuck.d
nim c -o:brainfuck_nim_clang -d:release --cc:clang --verbosity:0 brainfuck.nim
nim c -o:brainfuck_nim_gcc -d:release --cc:gcc --verbosity:0 brainfuck.nim
