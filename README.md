Some benchmarks of different languages
--------------------------------------

# Brainfuck

[Brainfuck](https://github.com/kostya/benchmarks/tree/master/brainfuck)

### bench.b

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 2.96    | 0.7        |
| Nim Gcc         | 4.15    | 0.6        |
| Rust            | 4.73    | 4.9        |
| C++             | 5.08    | 1.1        |
| D Ldc           | 6.61    | 0.9        |
| Crystal         | 7.06    | 1.1        |
| Go              | 7.57    | 1.0        |
| D               | 8.16    | 0.9        |
| D Gdc           | 8.53    | 1.0        |
| Julia           | 9.00    | 56.0       |
| Javascript V8   | 9.41    | 8.1        |
| Scala           | 11.99   | 1.6        |
| Go Gcc          | 13.60   | 10.0       |
| Javascript Node | 17.72   | 9.5        |
| C# Mono         | 18.08   | 15.4       |
| Python Pypy     | 20.12   | 20.8       |
| Ruby JRuby      | 96.20   | 97.0       |
| Ruby Topaz      | 112.91  | 36.0       |
| Ruby            | 226.86  | 8.0        |
| Ruby JRuby9k    | 241.16  | 256.6      |
| Python          | 452.44  | 4.9        |
| Ruby Rbx        | 472.08  | 45.0       |

### mandel.b

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 27.98   | 1.0        |
| D Ldc           | 43.30   | 0.9        |
| Crystal         | 43.32   | 1.1        |
| Rust            | 47.57   | 4.9        |
| D               | 49.73   | 1.2        |
| Nim Gcc         | 50.01   | 0.9        |
| Go              | 52.29   | 1.5        |
| Cpp             | 56.63   | 1.1        |
| D Gdc           | 71.20   | 1.5        |
| Julia           | 76.39   | 56.1       |
| Go Gcc          | 85.67   | 10.7       |
| Scala           | 105.57  | 1.6        |
| C# Mono         | 118.72  | 13.6       |
| Javascript Node | 206.88  | 11.3       |
| Python Pypy     | 206.20  | 31.8       |


# Base64

[Base64](https://github.com/kostya/benchmarks/tree/master/base64)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| D Gdc           | 2.48    | 44.3       |
| C               | 2.70    | 32.3       |
| Ruby            | 2.73    | 125.3      |
| D Ldc           | 3.27    | 44.1       |
| Rust            | 3.65    | 42.9       |
| Crystal         | 3.68    | 82.7       |
| Ruby Rbx        | 4.29    | 30.7       |
| Nim Gcc         | 4.60    | 52.7       |
| Nim Clang       | 4.67    | 52.7       |
| C++ Openssl     | 5.45    | 65.2       |
| D               | 6.18    | 89.1       |
| Python          | 7.62    | 52.6       |
| Javascript Node | 7.93    | 777.1      |
| Python Pypy     | 8.22    | 114.6      |
| C# Mono         | 9.01    | 71.7       |
| Ruby JRuby      | 16.76   | 496.6      |
| Julia           | 16.86   | 163.9      |
| Ruby JRuby9k    | 17.72   | 417.1      |
| Go              | 21.24   | 94.2       |
| Scala           | 35.06   | 301.2      |
| Go Gcc          | 39.56   | 185.5      |

# Json

[Json](https://github.com/kostya/benchmarks/tree/master/json)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C++ Rapid       | 0.79    | 687.1      |
| C++ Gason       | 0.83    | 582.2      |
| Crystal Schema  | 1.48    | 294.5      |
| Crystal Pull    | 1.60    | 1.4        |
| Crystal         | 2.27    | 1091.4     |
| Nim Clang       | 3.30    | 1280.3     |
| Nim Gcc         | 3.57    | 1284.0     |
| Python Pypy     | 4.99    | 1365.4     |
| Rust            | 5.48    | 2915.1     |
| C++ LibJson     | 5.49    | 2796.3     |
| Go              | 6.27    | 420.9      |
| Python          | 9.85    | 1409.1     |
| D               | 9.87    | 1316.6     |
| Julia           | 10.21   | 2335.9     |
| Ruby            | 10.54   | 2086.2     |
| Javascript Node | 11.61   | 926.4      |
| C++ Boost       | 16.44   | 2915.2     |
| D Gdc           | 17.68   | 1008.4     |
| Go Gcc          | 18.69   | 494.4      |
| Ruby JRuby9k    | 18.89   | 1996.8     |
| C# Mono         | 25.74   | 3757.9     |
| Ruby JRuby      | 25.78   | 2712.6     |
| D Ldc           | 27.23   | 919.6      |
| Ruby Rbx        | 67.13   | 4681.0     |
| Scala           | 343.13  | 2373.0     |

# Matmul

[Matmul](https://github.com/kostya/benchmarks/tree/master/matmul)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Julia Native    | 0.15    | 69.9       |
| D Ldc           | 2.01    | 68.9       |
| D               | 2.30    | 71.3       |
| D Gdc           | 2.32    | 73.3       |
| C               | 3.64    | 69.2       |
| Java            | 3.68    | 134.3      |
| Rust            | 3.70    | 76.9       |
| Nim Clang       | 3.71    | 141.1      |
| Nim Gcc         | 3.74    | 132.1      |
| Crystal         | 3.82    | 72.2       |
| Go Gcc          | 3.90    | 84.5       |
| Go              | 4.77    | 75.6       |
| Javascript V8   | 6.87    | 81.5       |
| Python Pypy     | 7.10    | 89.2       |
| Scala           | 10.26   | 154.0      |
| C# Mono         | 15.17   | 83.6       |
| Julia           | 30.87   | 362.8      |
| Ruby Topaz      | 81.41   | 206.2      |
| Ruby            | 338.40  | 82.8       |
| Python          | 447.39  | 74.0       |
| Ruby JRuby      | 412.61  | 574.9      |
| Ruby JRuby9k    | 467.99  | 602.3      |
| Ruby Rbx        | 591.70  | 325.0      |

# Havlak

[Havlak](https://github.com/kostya/benchmarks/tree/master/havlak)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Crystal         | 15.80   | 397.0      |
| Nim Gcc         | 17.26   | 913.0      |
| Nim Clang       | 17.82   | 874.4      |
| C++             | 17.72   | 174.5      |
| D               | 24.97   | 370.2      |
| D Ldc           | 25.15   | 214.9      |
| D Gdc           | 25.75   | 230.6      |
| Go Gcc          | 30.36   | 436.7      |
| Scala           | 33.38   | 341.0      |
| C# Mono         | 40.54   | 270.0      |
| Go              | 44.56   | 424.9      |
| Python Pypy     | 69.46   | 730.2      |
| Python          | 396.54  | 724.0      |



# Hardware:

Intel(R) Core(TM) i5-2400 CPU @ 3.10GHz (Ubuntu 14.04.1 LTS x86_64)

# Versions:

* gcc (Ubuntu 4.8.2-19ubuntu1) 4.8.2
* Nim Compiler Version 0.11.2 (2015-05-04) [Linux: amd64]
* `Crystal 0.7.1 [86d387c] (Fri May  1 01:08:43 UTC 2015)`
* go version go1.4 linux/amd64
* gccgo (Ubuntu 4.9.1-0ubuntu1) 4.9.1
* DMD64 D Compiler v2.067.0
* gdc (Ubuntu 4.8.2-19ubuntu1) 4.8.2
* LDC - the LLVM D compiler (0.15.2-beta1):
* V8 version 3.29.62 (candidate)
* rustc 1.2.0-nightly (0cc99f9cc 2015-05-17) (built 2015-05-18)
* Scala compiler version 2.11.4 -- Copyright 2002-2013, LAMP/EPFL
* Nodejs v0.10.25
* PyPy 2.4.0 with GCC 4.6.3
* topaz (ruby-1.9.3p125) (git rev b95c858) [x86_64-linux]
* ruby 2.1.2p95 (2014-05-08 revision 45877) [x86_64-linux]
* Python 2.7.6
* rubinius 2.2.10 (2.1.0 bf61ae2e 2014-06-27 JI) [x86_64-linux-gnu]
* jruby 1.7.19 (1.9.3p551) 2015-01-29 20786bd on Java HotSpot(TM) 64-Bit Server VM 1.7.0_67-b01 +jit [linux-amd64]
* jruby 9.0.0.0.pre1 (2.2.0p0) 2015-01-20 d537cab Java HotSpot(TM) 64-Bit Server VM 24.65-b04 on 1.7.0_67-b01 +jit [linux-amd64]
* Java HotSpot(TM) 64-Bit Server VM (build 24.65-b04, mixed mode)
* julia version 0.3.7
* clang version 3.5-1ubuntu1 (trunk) (based on LLVM 3.5)
* Mono JIT compiler version 4.0.1 (tarball Tue May 12 15:39:23 UTC 2015)
