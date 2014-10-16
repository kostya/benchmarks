echo Crystal
../xtime.rb ./json_cr
echo Crystal Pull
../xtime.rb ./json_pull_cr 
echo Crystal Schema
../xtime.rb ./json_schema_cr
echo Javascript Node
../xtime.rb node test.js bench.b
echo Ruby
../xtime.rb ruby test.rb bench.b
echo Rust
../xtime.rb ./json_rs
