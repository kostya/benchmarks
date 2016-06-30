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
echo C aklomp SSSE3
../xtime.rb ./base64_c_ak_ssse
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
echo Java
../xtime.rb java -XX:+AggressiveOpts Base64Java
echo Kotlin
../xtime.rb java -jar Test-kt.jar
echo Javascript Node
../xtime.rb node test.js
echo Javascript Jx
../xtime.rb jx test.js
echo Python Pypy
../xtime.rb pypy test.py
echo Python
../xtime.rb python test.py
echo Python3
../xtime.rb python3 test.py
echo Ruby
../xtime.rb ruby test.rb
echo Mono
../xtime.rb mono -O=all --gc=sgen test.exe
echo Perl
../xtime.rb perl -Iperllib/lib/perl5 test.pl
echo Perl XS
../xtime.rb perl test-xs.pl
echo Tcl
../xtime.rb tclsh test.tcl
