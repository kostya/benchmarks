crystal brainfuck.cr --release -o brainfuck_cr
go build -o brainfuck_go brainfuck.go
g++ -O3 -o brainfuck_cpp brainfuck.cpp
scalac -optimize brainfuck.scala
rustc --opt-level 3 brainfuck.rs -o brainfuck_rs
dmd -ofbrainfuck_d -O -release brainfuck.d
nimrod c -o:brainfuck_nim -d:release --verbosity:0 --hints:off brainfuck.nim
