echo Cpp
../xtime.rb ./havlak_cpp
echo Go
../xtime.rb ./havlak_go
echo Crystal
../xtime.rb ./havlak_cr
echo Scala
../xtime.rb scala -J-Xss100m LoopTesterApp
echo D
../xtime.rb ./havlak_d
echo Python Pypy
../xtime.rb pypy havlak.py
echo Nim
../xtime.rb ./havlak_nim
echo Python
../xtime.rb python havlak.py
