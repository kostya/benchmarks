crystal test.cr --release -o json_cr
crystal test_pull.cr --release -o json_pull_cr
crystal test_schema.cr --release -o json_schema_cr
rustc --opt-level 3 test.rs -o json_rs
dmd -ofjson_d -O -release test.d
nimrod c -o:json_nim -d:release --verbosity:0 --hints:off test.nim
scalac -optimize test.scala
go build -o json_go test.go
g++ -O3 test.cpp -o json_cpp
