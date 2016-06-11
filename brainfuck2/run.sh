echo Crystal
../xtime.rb ./bin_cr bench.b
echo Cpp
../xtime.rb ./bin_cpp bench.b
echo Rust
../xtime.rb ./bin_rs bench.b
echo Scala
../xtime.rb scala BrainFuck bench.b
echo C#
../xtime.rb mono -O=all --gc=sgen bf.exe bench.b
echo Javascript V8
../xtime.rb d8 bf.d8.js
echo Javascript Node
../xtime.rb node bf.js bench.b
