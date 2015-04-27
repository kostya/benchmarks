Brainfuck interepter and benchmark
----------------------------------

In benchmark i using only standard language containers and compile keys, without any kind of hacks to fair compare.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark bench.b

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 2.55    | 0.7        |
| Nim Gcc         | 4.75    | 0.6        |
| C++             | 5.08    | 1.1        |
| D Ldc           | 6.61    | 0.9        |
| Crystal         | 6.76    | 0.9        |
| Go              | 7.57    | 1.0        |
| D               | 8.16    | 0.9        |
| D Gdc           | 8.53    | 1.0        |
| Julia           | 9.00    | 56.0       |
| Javascript V8   | 9.41    | 8.1        |
| Scala           | 11.99   | 1.6        |
| C# Mono         | 12.01   | 10.4       |
| Go Gcc          | 13.60   | 10.0       |
| Rust            | 14.56   | 4.9        |
| Javascript Node | 17.72   | 9.5        |
| Python Pypy     | 20.12   | 20.8       |
| Ruby JRuby      | 96.20   | 97.0       |
| Ruby Topaz      | 112.91  | 36.0       |
| Ruby            | 226.86  | 8.0        |
| Ruby JRuby9k    | 241.16  | 256.6      |
| Python          | 452.44  | 4.9        |
| Ruby Rbx        | 472.08  | 45.0       |

# Benchmark mandel.b

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 28.20   | 0.9        |
| D Ldc           | 43.30   | 0.9        |
| Crystal         | 47.34   | 1.1        |
| Nim Gcc         | 49.38   | 0.9        |
| D               | 49.73   | 1.2        |
| Go              | 52.29   | 1.5        |
| Cpp             | 56.63   | 1.1        |
| D Gdc           | 71.20   | 1.5        |
| Rust            | 74.43   | 4.9        |
| Julia           | 76.39   | 56.1       |
| C# Mono         | 83.58   | 10.4       |
| Go Gcc          | 85.67   | 10.7       |
| Scala           | 105.57  | 1.6        |
| Javascript Node | 206.88  | 11.3       |
| Python Pypy     | 206.20  | 31.8       |

