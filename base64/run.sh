#!/bin/sh

set -e
echo Crystal
../xtime.rb ./base64_cr
echo Go
../xtime.rb ./base64_go
echo GccGo
../xtime.rb ./base64_go_gccgo
echo Cpp libcrypto
../xtime.rb ./base64_cpp
echo C
../xtime.rb ./base64_c
echo C aklomp
../xtime.rb ./base64_c_ak
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
../xtime.rb julia --optimize=3 --check-bounds=no test.jl
echo Scala
../xtime.rb scala Base64
echo Java
../xtime.rb java Base64Java
echo Kotlin
../xtime.rb java -jar Test-kt.jar
echo Javascript Node
../xtime.rb node test.js
echo Python PyPy
../xtime.rb pypy3 test.py
echo Python
../xtime.rb python3 test.py
echo Ruby
../xtime.rb ruby test.rb
echo JRuby
../xtime.rb jruby test.rb
echo TruffleRuby
../xtime.rb truffleruby --jvm test.rb
echo Mono
../xtime.rb mono -O=all --gc=sgen test.exe
echo C# .Net Core
../xtime.rb dotnet bin/Release/netcoreapp3.0/base64.dll
echo Perl
../xtime.rb perl -Iperllib/lib/perl5 test.pl
echo Perl XS
../xtime.rb perl test-xs.pl
echo Tcl
../xtime.rb tclsh test.tcl
echo Php
../xtime.rb php test.php
echo V Gcc
../xtime.rb ./base64_v_gcc
echo V Clang
../xtime.rb ./base64_v_clang
