Brainfuck interepter and benchmark
----------------------------------

In benchmark i using only standard language containers and compile keys, without any kind of hacks to fair compare. This is old benchmark, which is used hash_map based implementation of interepter, which considered by all as bad.

To compile all: `sh build.sh`

To run all: `sh run.sh`


### bench.b

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 3.21    | 0.8        |
| Felix           | 4.07    | 1.3        |
| Nim Gcc         | 4.35    | 0.7        |
| Java            | 4.94    | 147.6      |
| C++             | 5.08    | 1.1        |
| Rust            | 5.15    | 4.9        |
| Scala           | 5.90    | 116.3      |
| Julia           | 6.35    | 98.8       |
| D               | 6.57    | 1.0        |
| D Ldc           | 6.61    | 0.9        |
| Crystal         | 6.78    | 1.2        |
| Go              | 7.11    | 1.0        |
| D Gdc           | 8.87    | 1.0        |
| Javascript Node | 9.02    | 15.6       |
| Javascript V8   | 9.41    | 8.1        |
| Go Gcc          | 13.60   | 10.0       |
| Python Pypy     | 13.94   | 55.4       |
| Javascript Jx   | 17.14   | 11.0       |
| C# Mono         | 18.08   | 15.4       |
| OOC             | 48.86   | 1.3        |
| Ruby JRuby      | 87.05   | 124.1      |
| Ruby Topaz      | 112.91  | 36.0       |
| Ruby JRuby9K    | 160.15  | 297.2      |
| Ruby            | 226.86  | 8.0        |
| Tcl             | 262.20  | 2.7        |
| Python          | 452.44  | 4.9        |
| Ruby Rbx        | 472.08  | 45.0       |
| Python3         | 480.78  | 5.5        |

### mandel.b

[Mandel in Brainfuck](https://github.com/kostya/benchmarks/blob/master/brainfuck/mandel.b)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 28.96   | 1.0        |
| Felix           | 40.06   | 3.7        |
| D Ldc           | 43.30   | 0.9        |
| D               | 45.29   | 1.2        |
| Crystal         | 49.11   | 1.2        |
| Nim Gcc         | 50.98   | 0.9        |
| Rust            | 52.33   | 4.9        |
| Go              | 52.27   | 7.4        |
| Java            | 55.14   | 69.9       |
| Cpp             | 56.63   | 1.1        |
| Scala           | 64.37   | 126.4      |
| D Gdc           | 70.12   | 1.5        |
| Julia           | 85.25   | 98.0       |
| Go Gcc          | 85.67   | 10.7       |
| Javascript Node | 89.57   | 16.4       |
| C# Mono         | 118.72  | 13.6       |
| Python Pypy     | 126.46  | 64.5       |
| Javascript Jx   | 192.23  | 12.4       |
