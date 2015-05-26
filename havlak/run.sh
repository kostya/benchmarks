echo Cpp
../xtime.rb ./havlak_cpp
echo Go
../xtime.rb ./havlak_go
echo GccGo
../xtime.rb ./havlak_go_gccgo
echo Crystal
../xtime.rb ./havlak_cr
echo Scala
../xtime.rb scala -J-Xss100m LoopTesterApp
echo D
../xtime.rb ./havlak_d
echo D Gdc
../xtime.rb ./havlak_d_gdc
echo D Ldc
../xtime.rb ./havlak_d_ldc
echo Python Pypy
../xtime.rb pypy havlak.py
echo Nim Gcc
../xtime.rb ./havlak_nim_gcc
echo Nim Clang
../xtime.rb ./havlak_nim_clang
echo Python
../xtime.rb python havlak.py
echo Mono
../xtime.rb mono -O=all --gc=sgen havlak.exe
