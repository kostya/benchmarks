echo Crystal
../xtime.rb ./base64_cr
echo Go
../xtime.rb ./base64_go
echo GccGo
../xtime.rb ./base64_go_gccgo
echo Cpp
../xtime.rb ./base64_cpp
echo C
../xtime.rb ./base64_c
echo Rust
../xtime.rb ./base64_rs
echo D
../xtime.rb ./base64_d
echo D Gdc
../xtime.rb ./base64_d_gdc
echo D Ldc
../xtime.rb ./base64_d_ldc
echo Nim Gcc
../xtime.rb ./base64_nim_gcc
echo Nim Clang
../xtime.rb ./base64_nim_clang
echo Julia
../xtime.rb julia test.jl
echo Scala
../xtime.rb scala Base64
echo Javascript Node
../xtime.rb nodejs test.js
echo Python Pypy
../xtime.rb pypy test.py
echo Python
../xtime.rb python test.py
echo Ruby
../xtime.rb ruby test.rb
echo Mono
../xtime.rb mono -O=all --gc=sgen test.exe
