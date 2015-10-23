Some benchmarks of different languages
--------------------------------------

# Brainfuck

[Brainfuck](https://github.com/kostya/benchmarks/tree/master/brainfuck)

### bench.b

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 2.96    | 0.7        |
| Felix           | 4.07    | 1.3        |
| Nim Gcc         | 4.15    | 0.6        |
| Rust            | 4.84    | 4.9        |
| C++             | 5.08    | 1.1        |
| D               | 6.57    | 1.0        |
| D Ldc           | 6.61    | 0.9        |
| Crystal         | 6.97    | 1.3        |
| Go              | 7.29    | 1.3        |
| D Gdc           | 8.87    | 1.0        |
| Julia           | 9.25    | 59.0       |
| Javascript V8   | 9.41    | 8.1        |
| Scala           | 11.63   | 114.0      |
| Go Gcc          | 13.60   | 10.0       |
| Javascript Node | 17.72   | 9.5        |
| C# Mono         | 18.08   | 15.4       |
| Python Pypy     | 20.12   | 20.8       |
| OOC             | 48.86   | 1.3        |
| Ruby JRuby      | 87.05   | 124.1      |
| Ruby Topaz      | 112.91  | 36.0       |
| Ruby JRuby9K    | 160.15  | 297.2      |
| Ruby            | 226.86  | 8.0        |
| Python          | 452.44  | 4.9        |
| Ruby Rbx        | 472.08  | 45.0       |

### mandel.b

[Mandel in Brainfuck](https://github.com/kostya/benchmarks/blob/master/brainfuck/mandel.b)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Nim Clang       | 27.98   | 1.0        |
| Felix           | 40.06   | 3.7        |
| Rust            | 42.87   | 4.9        |
| D Ldc           | 43.30   | 0.9        |
| D               | 45.29   | 1.2        |
| Crystal         | 48.62   | 1.3        |
| Nim Gcc         | 50.01   | 0.9        |
| Go              | 52.56   | 7.6        |
| Cpp             | 56.63   | 1.1        |
| D Gdc           | 70.12   | 1.5        |
| Go Gcc          | 85.67   | 10.7       |
| Julia           | 94.33   | 56.9       |
| Scala           | 96.73   | 126.0      |
| C# Mono         | 118.72  | 13.6       |
| Javascript Node | 206.88  | 11.3       |
| Python Pypy     | 206.20  | 31.8       |


# Base64

[Base64](https://github.com/kostya/benchmarks/tree/master/base64)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C aklomp SSSE3  | 1.09    | 32.3       |
| C               | 1.85    | 32.2       |
| Crystal         | 2.21    | 85.8       |
| Ruby            | 2.73    | 125.3      |
| D Gdc           | 3.16    | 45.2       |
| D Ldc           | 3.27    | 44.1       |
| Perl XS         | 3.63    | 47.9       |
| Rust            | 4.17    | 42.9       |
| Ruby Rbx        | 4.29    | 30.7       |
| Nim Gcc         | 4.60    | 52.7       |
| Nim Clang       | 4.67    | 52.7       |
| C++ Openssl     | 5.45    | 65.2       |
| D               | 6.18    | 89.1       |
| Python          | 7.62    | 52.6       |
| Javascript Node | 7.93    | 777.1      |
| Python Pypy     | 8.22    | 114.6      |
| C# Mono         | 9.01    | 71.7       |
| Scala           | 11.0    | 292.5      |
| Go              | 12.15   | 96.2       |
| Ruby JRuby9K    | 12.16   | 530.6      |
| Ruby JRuby      | 12.65   | 514.9      |
| Julia           | 14.76   | 380.2      |
| Perl            | 33.30   | 99.7       |
| Go Gcc          | 39.56   | 185.5      |

# Json

[Json](https://github.com/kostya/benchmarks/tree/master/json)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| D Gdc Fast      | 0.34    | 226.7      |
| C++ Rapid       | 0.79    | 687.1      |
| C++ Gason       | 0.83    | 582.2      |
| Rust            | 1.26    | 234.7      |
| Crystal Schema  | 1.57    | 296.2      |
| Crystal         | 2.55    | 1059.4     |
| Crystal Pull    | 2.63    | 1.2        |
| Perl XS         | 2.68    | 888.4      |
| Nim Clang       | 3.30    | 1280.3     |
| Nim Gcc         | 3.57    | 1284.0     |
| Python Pypy     | 4.99    | 1365.4     |
| Q               | 5.18    | 684.0      |
| C++ LibJson     | 5.49    | 2796.3     |
| Go              | 6.07    | 479.4      |
| Clojure         | 6.65    | 1143.4     |
| Ruby YAJL       | 8.23    | 1085.5     |
| Haskell         | 8.31    | 70.5       |
| Python          | 9.85    | 1409.1     |
| Julia           | 10.27   | 2353.9     |
| Javascript Node | 11.61   | 926.4      |
| D               | 12.42   | 1417.1     |
| Ruby            | 12.67   | 2013.9     |
| C++ Boost       | 16.44   | 2915.2     |
| Ruby JRuby9K    | 16.53   | 2050.5     |
| Go Gcc          | 18.69   | 494.4      |
| Ruby JRuby      | 21.98   | 2761.1     |
| C# Mono         | 25.74   | 3757.9     |
| D Gdc           | 25.86   | 926.1      |
| D Ldc           | 27.23   | 919.6      |
| Perl            | 46.02   | 1635.4     |
| Ruby Rbx        | 67.13   | 4681.0     |
| Scala           | 356.09  | 2789.0     |

# Matmul

[Matmul](https://github.com/kostya/benchmarks/tree/master/matmul)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Julia Native    | 0.15    | 69.9       |
| D Ldc           | 2.01    | 68.9       |
| D               | 2.30    | 71.3       |
| D Gdc           | 2.33    | 73.0       |
| C               | 3.64    | 69.2       |
| Java            | 3.64    | 136.3      |
| Rust            | 3.70    | 76.9       |
| Nim Clang       | 3.71    | 141.1      |
| Nim Gcc         | 3.74    | 132.1      |
| Crystal         | 3.83    | 72.2       |
| Go Gcc          | 3.90    | 84.5       |
| Go              | 4.76    | 73.3       |
| Javascript V8   | 6.87    | 81.5       |
| Python Pypy     | 7.10    | 89.2       |
| Scala           | 10.14   | 154.0      |
| C# Mono         | 15.17   | 83.6       |
| Julia           | 35.02   | 326.8      |
| Ruby Topaz      | 81.41   | 206.2      |
| Ruby            | 338.40  | 82.8       |
| Python          | 447.39  | 74.0       |
| Ruby JRuby      | 416.12  | 582.4      |
| Ruby JRuby9k    | 467.59  | 608.3      |
| Ruby Rbx        | 591.70  | 325.0      |

# Havlak

[Havlak](https://github.com/kostya/benchmarks/tree/master/havlak)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Crystal         | 15.87   | 398.1      |
| Nim Gcc         | 17.26   | 913.0      |
| Nim Clang       | 17.82   | 874.4      |
| C++             | 17.72   | 174.5      |
| D Ldc           | 25.15   | 214.9      |
| D               | 28.90   | 418.2      |
| Go Gcc          | 30.36   | 436.7      |
| D Gdc           | 31.79   | 197.6      |
| Scala           | 32.13   | 363.0      |
| Go              | 35.78   | 392.9      |
| C# Mono         | 40.54   | 270.0      |
| Python Pypy     | 69.46   | 730.2      |
| Python          | 396.54  | 724.0      |



# Hardware:

Intel(R) Core(TM) i5-2400 CPU @ 3.10GHz (Ubuntu 14.04.1 LTS x86_64)

# Versions:

* gcc (Ubuntu 4.8.2-19ubuntu1) 4.8.2
* Nim Compiler Version 0.11.2 (2015-05-04) [Linux: amd64]
* `Crystal 0.9.0 [be93828] (Fri Oct 16 20:57:21 UTC 2015)`
* go version go1.5 linux/amd64
* gccgo (Ubuntu 4.9.1-0ubuntu1) 4.9.1
* DMD64 D Compiler v2.068.0
* gdc (crosstool-NG crosstool-ng-1.20.0-232-gc746732 - 20150830-2.066.1-dadb5a3784) 5.2.0
* LDC - the LLVM D compiler (0.15.2-beta1):
* V8 version 3.29.62 (candidate)
* rustc 1.4.0-nightly (7e13faee1 2015-08-19)
* Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_45)
* Nodejs v0.10.25
* PyPy 2.4.0 with GCC 4.6.3
* topaz (ruby-1.9.3p125) (git rev b95c858) [x86_64-linux]
* ruby 2.1.2p95 (2014-05-08 revision 45877) [x86_64-linux]
* Python 2.7.6
* rubinius 2.2.10 (2.1.0 bf61ae2e 2014-06-27 JI) [x86_64-linux-gnu]
* jruby 1.7.20 (1.9.3p551) 2015-05-04 3086e6a on Java HotSpot(TM) 64-Bit Server VM 1.8.0_45-b14 +jit [linux-amd64]
* jruby 9.0.0.0.pre2 (2.2.2) 2015-04-28 2755ae0 Java HotSpot(TM) 64-Bit Server VM 25.45-b02 on 1.8.0_45-b14 +jit [linux-amd64]
* java version "1.8.0_45" Java HotSpot(TM) 64-Bit Server VM (build 25.45-b02, mixed mode)
* julia version 0.3.11
* clang version 3.5-1ubuntu1 (trunk) (based on LLVM 3.5)
* Mono JIT compiler version 4.0.1 (tarball Tue May 12 15:39:23 UTC 2015)
* rock 0.9.10-head codename sapphire, built on Wed Jul  1 20:09:58 2015
* Felix version 15.04.03
* Q KDB+ 3.3 2015.09.02 Copyright (C) 1993-2015 Kx Systems
* perl 5, version 18, subversion 2 (v5.18.2) built for x86_64-linux-gnu-thread-multi
* The Glorious Glasgow Haskell Compilation System, version 7.10.2
