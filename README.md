Some benchmarks of different languages
--------------------------------------

# Brainfuck v2.0

[Brainfuck v2.0](https://github.com/kostya/benchmarks/tree/master/brainfuck2)
[Brainfuck v1.0](https://github.com/kostya/benchmarks/tree/master/brainfuck)

### bench.b

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Scala           | 1.95    | 120.12     |
| C++ Gcc         | 2.25    | 1.0        |
| Rust            | 2.53    | 4.9        |
| Nim Gcc         | 2.56    | 0.7        |
| Crystal         | 2.82    | 1.3        |
| D Gdc           | 3.07    | 1.4        |
| Nim Clang       | 3.09    | 0.8        |
| D Ldc           | 3.12    | 0.9        |
| Java            | 4.03    | 513.8      |
| Go Gcc          | 4.20    | 10.0       |
| Go              | 5.99    | 1.0        |
| D Dmd           | 6.32    | 1.0        |
| Javascript V8   | 6.53    | 7.9        |
| Javascript Node | 7.18    | 15.7       |
| Javascript Jx   | 19.08   | 11.0       |
| C# Mono         | 19.86   | 14.6       |
| Python Pypy     | 20.64   | 77.9       |
| Ruby Topaz      | 59.53   | 36.5       |
| Ruby Rbx        | 120.38  | 32.3       |
| Ruby Jruby      | 129.75  | 267.6      |
| Ruby Jruby9k    | 134.18  | 286.8      |
| Ruby            | 181.44  | 7.2        |
| Python3         | 310.75  | 5.5        |
| Python          | 314.79  | 4.9        |

### mandel.b

[Mandel in Brainfuck](https://github.com/kostya/benchmarks/blob/master/brainfuck2/mandel.b)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C++ Gcc         | 22.12   | 1.7        |
| Crystal         | 22.92   | 1.5        |
| Rust            | 24.13   | 4.9        |
| D Gdc           | 29.71   | 2.4        |
| Nim Gcc         | 31.04   | 2.7        |
| Nim Clang       | 37.39   | 2.9        |
| Go Gcc          | 37.59   | 11.4       |
| D Ldc           | 36.43   | 1.4        |
| Scala           | 58.51   | 120.12     |
| Java            | 58.86   | 423.9      |
| Go              | 75.13   | 2.3        |
| D Dmd           | 85.89   | 1.7        |
| Javascript Node | 89.63   | 18.7       |
| Python Pypy     | 126.47  | 78.9       |
| C# Mono         | 147.69  | 12.6       |
| Javascript Jx   | 227.07  | 16.7       |
| Ruby Topaz      | 305.79  | 38.8       |

# Base64

[Base64](https://github.com/kostya/benchmarks/tree/master/base64)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C aklomp SSSE3  | 1.09    | 32.3       |
| C               | 1.85    | 32.2       |
| Crystal         | 2.24    | 137.5       |
| Ruby            | 2.73    | 125.3      |
| D Gdc           | 3.16    | 45.2       |
| D Ldc           | 3.27    | 44.1       |
| Perl XS         | 3.63    | 47.9       |
| Rust            | 3.83    | 42.8       |
| Ruby Rbx        | 4.29    | 30.7       |
| Javascript Node | 4.57    | 683.4      |
| Nim Gcc         | 4.62    | 52.7       |
| Nim Clang       | 4.70    | 52.7       |
| Julia           | 4.41    | 190.0      |
| C++ Openssl     | 5.45    | 65.2       |
| D               | 6.18    | 89.1       |
| Javascript Jx   | 6.97    | 710.6      |
| Tcl             | 7.20    | 66.0       |
| Python Pypy     | 7.32    | 582.3      |
| Python          | 7.62    | 52.6       |
| Python3         | 8.16    | 47.5       |
| C# Mono         | 9.01    | 71.7       |
| Java            | 9.06    | 971.2      |
| Scala           | 10.69   | 292.5      |
| Ruby JRuby9K    | 12.16   | 530.6      |
| Ruby JRuby      | 12.65   | 514.9      |
| Go              | 13.36   | 73.3       |
| Perl            | 33.30   | 99.7       |
| Go Gcc          | 39.56   | 185.5      |

# Json

[Json](https://github.com/kostya/benchmarks/tree/master/json)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| D Gdc Fast      | 0.34    | 226.7      |
| C++ Rapid SAX   | 0.72    | 1.0        |
| C++ Gason       | 0.83    | 582.2      |
| C++ Rapid       | 0.94    | 243.6      |
| Rust Struct     | 1.33    | 232.9      |
| Rust Pull       | 1.35    | 208.7      |
| Java            | 1.48    | 518.3      |
| Crystal Schema  | 1.85    | 337.2      |
| Crystal         | 2.66    | 1118.2     |
| Perl XS         | 2.68    | 888.4      |
| Javascript Node | 2.69    | 865.6      |
| Javascript Jx   | 2.73    | 706.8      |
| Rust Value      | 3.23    | 1970.6     |
| Crystal Pull    | 3.43    | 1.3        |
| Nim Clang       | 4.12    | 1089.6     |
| Python3 ujson   | 4.15    | 1303.2     |
| Nim Gcc         | 4.46    | 1090.1     |
| Python Pypy     | 4.81    | 1553.0     |
| Python ujson    | 5.07    | 1352.9     |
| Q               | 5.18    | 684.0      |
| C++ LibJson     | 5.49    | 2796.3     |
| Go              | 5.64    | 478.6      |
| Clojure         | 5.81    | 1148.5     |
| Python3         | 5.92    | 1037.8     |
| Ruby YAJL       | 8.23    | 1085.5     |
| Haskell         | 8.31    | 70.5       |
| Python          | 9.85    | 1409.1     |
| C# Mono         | 10.57   | 812.1      |
| Julia           | 11.89   | 2622.4     |
| D               | 12.42   | 1417.1     |
| Ruby            | 12.67   | 2013.9     |
| Scala           | 14.11   | 1408.2     |
| JQ              | 14.92   | 1714.5     |
| C++ Boost       | 16.44   | 2915.2     |
| Ruby JRuby9K    | 16.53   | 2050.5     |
| Go Gcc          | 17.64   | 473.1      |
| Ruby JRuby      | 21.98   | 2761.1     |
| D Gdc           | 25.86   | 926.1      |
| D Ldc           | 27.23   | 919.6      |
| Perl            | 46.02   | 1635.4     |
| Ruby Rbx        | 67.13   | 4681.0     |

# Matmul

[Matmul](https://github.com/kostya/benchmarks/tree/master/matmul)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Julia Native    | 0.11    | 148.3      |
| D Ldc           | 2.01    | 68.9       |
| D               | 2.30    | 71.3       |
| D Gdc           | 2.33    | 73.0       |
| Python Numpy    | 3.08    | 65.3       |
| Java            | 3.50    | 136.2      |
| Scala           | 3.62    | 136.2      |
| C               | 3.64    | 69.2       |
| Nim Clang       | 3.73    | 142.3      |
| Nim Gcc         | 3.82    | 146.0      |
| Crystal         | 3.88    | 72.1       |
| Go Gcc          | 3.90    | 84.5       |
| Swift           | 4.48    | 110.8      |
| Rust            | 4.63    | 76.9       |
| Go              | 4.85    | 73.9       |
| Javascript Node | 5.75    | 86.6       |
| Javascript Jx   | 5.92    | 83.8       |
| Javascript V8   | 6.87    | 81.5       |
| Python Pypy     | 7.68    | 122.6      |
| C# Mono         | 15.17   | 83.6       |
| Julia           | 20.98   | 187.4      |
| Ruby Topaz      | 81.41   | 206.2      |
| Ruby            | 338.40  | 82.8       |
| Python          | 447.39  | 74.0       |
| Ruby JRuby      | 416.12  | 582.4      |
| Ruby JRuby9k    | 467.59  | 608.3      |
| Ruby Rbx        | 591.70  | 325.0      |
| Perl            | 666.46  | 604.1      |
| Tcl             | 1066.66 | 279.9      |

# Havlak

[Havlak](https://github.com/kostya/benchmarks/tree/master/havlak)

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Crystal         | 15.43   | 445.2      |
| Nim Gcc         | 16.59   | 484.3      |
| Nim Clang       | 16.91   | 477.9      |
| C++             | 17.72   | 174.5      |
| D Ldc           | 25.15   | 214.9      |
| D               | 28.90   | 418.2      |
| D Gdc           | 31.79   | 197.6      |
| Scala           | 32.18   | 363.0      |
| Go Gcc          | 32.94   | 365.7      |
| Go              | 35.59   | 307.3      |
| C# Mono         | 40.54   | 270.0      |
| Python Pypy     | 45.51   | 625.9      |
| Python          | 396.54  | 724.0      |



# Hardware:

Intel(R) Core(TM) i5-2400 CPU @ 3.10GHz (Ubuntu 14.04.1 LTS x86_64)

# Versions:

* gcc (Ubuntu 4.8.2-19ubuntu1) 4.8.2
* Nim Compiler Version 0.14.0 (2016-06-06) [Linux: amd64]
* `Crystal 0.16.0 [fc89c1a] (Thu May  5 17:06:08 UTC 2016)`
* go version go1.6 linux/amd64
* gccgo (Ubuntu 4.9.1-0ubuntu1) 4.9.1
* DMD64 D Compiler v2.068.0
* gdc (crosstool-NG crosstool-ng-1.20.0-232-gc746732 - 20150830-2.066.1-dadb5a3784) 5.2.0
* LDC - the LLVM D compiler (0.15.2-beta1):
* V8 version 3.29.62 (candidate)
* rustc 1.8.0-nightly (4b615854f 2016-01-26)
* Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_45)
* Nodejs v6.0.0
* PyPy 4.0.0 with GCC 4.8.4
* topaz (ruby-1.9.3p125) (git rev b95c858) [x86_64-linux]
* ruby 2.1.2p95 (2014-05-08 revision 45877) [x86_64-linux]
* Python 2.7.6
* Python 3.4.3
* rubinius 2.2.10 (2.1.0 bf61ae2e 2014-06-27 JI) [x86_64-linux-gnu]
* jruby 1.7.20 (1.9.3p551) 2015-05-04 3086e6a on Java HotSpot(TM) 64-Bit Server VM 1.8.0_45-b14 +jit [linux-amd64]
* jruby 9.0.0.0.pre2 (2.2.2) 2015-04-28 2755ae0 Java HotSpot(TM) 64-Bit Server VM 25.45-b02 on 1.8.0_45-b14 +jit [linux-amd64]
* java version "1.8.0_45" Java HotSpot(TM) 64-Bit Server VM (build 25.45-b02, mixed mode)
* julia version 0.4.3
* clang version 3.5-1ubuntu1 (trunk) (based on LLVM 3.5)
* Mono JIT compiler version 4.0.1 (tarball Tue May 12 15:39:23 UTC 2015)
* rock 0.9.10-head codename sapphire, built on Wed Jul  1 20:09:58 2015
* Felix version 15.04.03
* Q KDB+ 3.3 2015.09.02 Copyright (C) 1993-2015 Kx Systems
* perl 5, version 18, subversion 2 (v5.18.2) built for x86_64-linux-gnu-thread-multi
* The Glorious Glasgow Haskell Compilation System, version 7.10.2
* Tcl 8.6
* jq version 1.3
* JXCore v0.10.40
* Swift version 2.2-dev (LLVM ae2eb212e4, Clang ef4c02f431, Swift 634acb40a1)
