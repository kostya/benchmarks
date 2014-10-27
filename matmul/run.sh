echo Crystal
../xtime.rb ./matmul_cr 1500
echo Go
../xtime.rb ./matmul_go 1500
echo C
../xtime.rb ./matmul_c 1500
echo Rust
../xtime.rb ./matmul_rs 1500
echo D
../xtime.rb ./matmul_d 1500
echo Nimrod
../xtime.rb ./matmul_nim 1500
echo Scala
../xtime.rb scala MatMul 1500
echo Java
../xtime.rb java matmul 1500
echo Javascript V8
../xtime.rb d8 matmul.js -- 1500
echo Python Pypy
../xtime.rb pypy matmul.py 1500
echo Python
../xtime.rb python matmul.py 1500
echo Ruby Topaz
../xtime.rb topaz matmul.rb 1500
echo Ruby
../xtime.rb ruby matmul.rb 1500
