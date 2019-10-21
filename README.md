Some benchmarks of different languages
--------------------------------------

The benchmarks follow the criteria:

  - They are written as the average software developer would write them, i.e.

    - The algorithms are implemented as cited in public sources;
    - The libraries are used as described in the tutorials, documentation and examples;
    - Used data structures are idiomatic.

  - The used algorithms are similar between the languages (reference implementations), variants are acceptable if the reference implementation exists.
  - All final binaries are releases (optimized for performance if possible) as debug performance may vary too much depending on the compiler.

# UPDATE 

2019-10-11

# Brainfuck v2.0

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck v2.0](brainfuck2)
[Brainfuck v1.0](brainfuck)

### bench.b

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| Kotlin          | 1.56    | 38.9        |
| C++ Gcc         | 1.74    | 1.8         |
| Nim Gcc         | 2.27    | 0.8         |
| D Gdc           | 2.32    | 6.3         |
| Nim Clang       | 2.33    | 1.1         |
| Java            | 2.54    | 39.3        |
| Rust            | 2.58    | 0.9         |
| Go              | 2.65    | 1.4         |
| D Ldc           | 2.74    | 1.5         |
| Go Gcc          | 2.80    | 19.1        |
| ML MLton        | 2.85    | 1.2         |
| C# .Net Core    | 3.00    | 28.0        |
| Crystal         | 3.03    | 2.7         |
| Scala           | 3.51    | 136.3       |
| OCaml           | 3.61    | 4.0         |
| V Gcc           | 4.03    | 0.7         |
| D Dmd           | 4.60    | 1.9         |
| V Clang         | 5.36    | 1.1         |
| C# Mono         | 6.13    | 17.7        |
| Javascript Node | 6.23    | 31.2        |
| Haskell (MArray)| 7.06    | 4.1         |
| F# Mono         | 7.34    | 25.4        |
| Javascript V8   | 7.61    | 24.1        |
| LuaJIT          | 10.89   | 1.3         |
| Racket          | 16.85   | 62.2        |
| Python PyPy     | 17.75   | 95.0        |
| Chez Scheme     | 18.48   | 29.4        |
| Haskell         | 23.01   | 4.1         |
| Ruby truffle    | 29.21   | 603.3       |
| Ruby            | 119.07  | 13.7        |
| Lua 5.3         | 131.13  | 1.5         |
| Ruby JRuby      | 142.68  | 240.1       |
| Elixir          | 155.58  | 41.4        |
| Python          | 282.38  | 6.9         |
| Python3         | 284.03  | 8.9         |
| Tcl (FP)        | 362.41  | 4.6         |
| Perl            | 530.66  | 5.0         |
| Tcl (OO)        | 701.28  | 5.0         |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| C++ Gcc         | 22.98   | 3.7         |
| D Ldc           | 24.96   | 3.7         |
| D Gdc           | 25.31   | 7.1         |
| V Gcc           | 25.34   | 2.2         |
| Crystal         | 25.60   | 3.1         |
| Rust            | 25.74   | 2.0         |
| Kotlin          | 27.29   | 45.3        |
| Nim Gcc         | 29.27   | 3.3         |
| Scala           | 30.72   | 142.6       |
| C# .Net Core    | 31.40   | 29.7        |
| ML MLton        | 32.35   | 4.0         |
| V Clang         | 32.78   | 2.7         |
| Java            | 32.88   | 45.4        |
| Nim Clang       | 32.95   | 3.7         |
| Go Gcc          | 33.72   | 34.5        |
| OCaml           | 44.71   | 11.7        |
| Go              | 47.86   | 3.1         |
| D Dmd           | 58.42   | 4.3         |
| Javascript Node | 64.23   | 34.4        |
| C# Mono         | 78.52   | 18.1        |
| Python PyPy     | 105.04  | 96.3        |
| Haskell (MArray)| 107.81  | 5.4         |
| LuaJIT          | 110.26  | 3.0         |
| Ruby truffle    | 138.00  | 1014.1      |
| F# Mono         | 166.75  | 33.5        |
| Racket          | 203.15  | 63.9        |
| Chez Scheme     | 212.33  | 29.3        |

# Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| C aklomp SSSE3  | 0.52    | 33.1        |
| C               | 1.67    | 33.2        |
| Rust            | 1.88    | 46.4        |
| Crystal         | 2.10    | 57.3        |
| D Ldc           | 2.18    | 54.0        |
| Ruby            | 2.48    | 197.9       |
| Javascript Node | 2.76    | 220.9       |
| Java            | 2.93    | 351.1       |
| Perl XS         | 3.24    | 50.3        |
| Nim Clang       | 3.25    | 59.2        |
| D Gdc           | 3.29    | 57.8        |
| Kotlin          | 3.32    | 403.4       |
| V Clang         | 3.42    | 2195.3      |
| Nim Gcc         | 3.63    | 58.9        |
| Scala           | 3.64    | 128.2       |
| Go              | 3.65    | 178.5       |
| V Gcc           | 3.77    | 2199.2      |
| Go Gcc          | 4.47    | 219.1       |
| C# .Net Core    | 4.70    | 224.4       |
| Php             | 4.90    | 57.2        |
| C++ Openssl     | 5.53    | 68.1        |
| Python3         | 6.01    | 44.1        |
| D               | 6.11    | 54.3        |
| Tcl             | 6.20    | 68.7        |
| Python PyPy     | 7.40    | 156.1       |
| Python          | 7.60    | 55.0        |
| C# Mono         | 7.89    | 127.6       |
| Julia           | 9.18    | 355.4       |
| Ruby JRuby      | 12.29   | 254.7       |
| Perl            | 26.60   | 142.6       |
| Ruby truffle    | 29.37   | 478.4       |

# Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| D Gdc Fast      | 0.30    | 191.3       |
| Rust Pull       | 0.38    | 202.6       |
| Rust Struct     | 0.39    | 225.2       |
| C++ Simdjson    | 0.47    | 486.2       |
| C++ Rapid SAX   | 0.55    | 1.8         |
| C++ Rapid       | 0.78    | 218.5       |
| C++ Gason       | 0.85    | 591.0       |
| Java            | 0.99    | 383.8       |
| Scala           | 1.22    | 536.8       |
| Rust Value      | 2.08    | 1745.9      |
| Javascript Node | 2.22    | 507.6       |
| Crystal Schema  | 2.23    | 282.0       |
| Perl XS         | 2.37    | 966.4       |
| Crystal         | 2.94    | 962.1       |
| Clojure         | 3.24    | 1676.0      |
| V Clang         | 3.37    | 1128.1      |
| Go              | 3.42    | 511.2       |
| Php             | 3.43    | 1476.7      |
| Python3 ujson   | 3.61    | 1184.0      |
| Julia           | 4.14    | 2369.6      |
| Python3         | 4.65    | 910.4       |
| Python PyPy     | 4.78    | 1293.0      |
| Crystal Pull    | 4.94    | 4.3         |
| Nim Clang       | 5.08    | 1321.5      |
| Python ujson    | 5.10    | 1404.7      |
| Nim Gcc         | 5.25    | 1321.2      |
| Go Gcc          | 5.28    | 456.9       |
| V Gcc           | 5.40    | 1127.5      |
| C++ LibJson     | 5.46    | 2888.8      |
| Q               | 5.85    | 691.5       |
| Haskell         | 6.17    | 9.9         |
| Ruby            | 6.33    | 842.4       |
| C# .Net Core    | 6.67    | 842.2       |
| D Ldc           | 6.69    | 1546.7      |
| Ruby Yajl       | 6.94    | 840.4       |
| D Gdc           | 7.52    | 1362.2      |
| Python          | 8.99    | 1411.1      |
| C++ Boost       | 9.53    | 2952.4      |
| C# Mono         | 10.16   | 859.0       |
| JQ              | 10.43   | 1321.9      |
| D Dmd           | 12.35   | 1546.9      |
| Ruby JRuby      | 13.70   | 2189.4      |
| Perl            | 38.04   | 1415.5      |
| Ruby truffle    | 159.02  | 5162.5      |

# Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| D lubeck        | 0.12    | 13.9        |
| Julia Native Thr| 0.13    | 216.9       |
| Julia Native    | 0.29    | 214.7       |
| Python Numpy    | 1.08    | 52.3        |
| D Ldc           | 2.00    | 73.2        |
| D               | 2.24    | 73.1        |
| D Gdc           | 2.36    | 76.8        |
| C               | 3.11    | 69.7        |
| Rust            | 3.17    | 70.7        |
| Julia           | 3.22    | 246.5       |
| Nim Gcc         | 3.23    | 72.5        |
| Nim Clang       | 3.23    | 72.9        |
| Crystal         | 3.32    | 63.3        |
| V Clang         | 3.35    | 70.4        |
| Go Gcc          | 3.41    | 106.6       |
| Go              | 3.54    | 60.4        |
| Kotlin          | 3.59    | 129.6       |
| Scala           | 3.67    | 171.5       |
| Java            | 3.81    | 129.5       |
| V Gcc           | 4.19    | 70.2        |
| Javascript Node | 4.36    | 101.7       |
| Swift           | 7.13    | 186.4       |
| Javascript V8   | 5.92    | 95.0        |
| Python PyPy     | 8.18    | 132.9       |
| C# .Net Core    | 8.21    | 102.9       |
| C# Mono         | 14.44   | 88.4        |
| Ruby truffle    | 59.52   | 580.5       |
| Ruby            | 246.34  | 83.0        |
| Python          | 340.32  | 76.0        |
| Ruby JRuby      | 534.32  | 1085.6      |
| Tcl             | 481.88  | 281.1       |
| Perl            | 558.50  | 606.7       |

# Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| Crystal         | 10.04   | 228.2       |
| Nim Gcc         | 14.52   | 475.2       |
| Nim Clang       | 15.09   | 471.8       |
| C++             | 16.16   | 179.2       |
| Scala           | 22.84   | 384.9       |
| D Ldc           | 23.37   | 460.1       |
| Go              | 24.24   | 347.0       |
| D               | 28.51   | 461.4       |
| D Gdc           | 30.17   | 419.3       |
| C# Mono         | 31.33   | 326.7       |
| Go Gcc          | 36.52   | 430.7       |
| Python PyPy     | 40.09   | 624.4       |
| C# .Net Core    | 43.10   | 542.1       |
| Python          | 345.14  | 725.5       |

# Hardware:

Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz (Ubuntu 18.04.3 LTS x86_64)

# Versions:

* gcc (Ubuntu 9.1.0-2ubuntu2~18.04) 9.1.0
* Nim Compiler Version 1.0.0 [Linux: amd64]
* `Crystal 0.31.1 [0e2e1d067] (2019-09-30) LLVM 8.0.0`
* go version go1.13.1 linux/amd64
* gccgo (Ubuntu 9.1.0-2ubuntu2~18.04) 9.1.0
* DMD64 D Compiler v2.088.0
* gdc (Ubuntu 9.1.0-2ubuntu2~18.04) 9.1.0
* LDC - the LLVM D compiler (1.18.0-beta1): based on DMD v2.088.0 and LLVM 8.0.1
* V8 version 7.9.0 (candidate)
* rustc 1.38.0 (625451e37 2019-09-23)
* Scala 2.13.0 (OpenJDK 64-Bit Server VM, Java 11.0.4)
* Nodejs v12.10.0
* PyPy 7.1.1 with GCC 7.4.0
* ruby 2.6.5p114 (2019-10-01 revision 67812) [x86_64-linux]
* Python 2.7.15+
* Python 3.6.8
* JRuby 9.2.8.0 (2.5.3) 2019-08-12 a1ac7ff OpenJDK 64-Bit Server VM 11.0.4+11-post-Ubuntu-1ubuntu218.04.3 +jit [linux-x86_64]
* java openjdk version "11.0.4" 2019-07-16
* julia version 1.2.0
* clang version 7.0.0
* Mono JIT compiler version 6.4.0.198 (tarball Tue Sep 24 01:21:28 UTC 2019)
* rock 0.9.11-head codename sapporo
* Felix version 2019.01.06
* Q KDB+ 3.6 2019.04.02 Copyright (C) 1993-2019 Kx Systems
* perl 5, version 26, subversion 1 (v5.26.1) built for x86_64-linux-gnu-thread-multi
* The Glorious Glasgow Haskell Compilation System, version 8.6.5
* Tcl 8.6
* jq-1.5-1-a5b5cbe
* Swift version 5.1 (swift-5.1-RELEASE)
* Kotlin version 1.3.50-release-112 (JRE 11.0.4+11-post-Ubuntu-1ubuntu218.04.3)
* PHP 7.2.19-0ubuntu0.18.04.2
* .NET Core SDK 3.0.100
* Elixir 1.9.1 Erlang/OTP 22 [erts-10.5.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]
* Lua 5.3.3  Copyright (C) 1994-2016 Lua.org, PUC-Rio
* LuaJIT 2.1.0-beta3 -- Copyright (C) 2005-2017 Mike Pall. http://luajit.org/
* truffleruby 19.2.0.1, like ruby 2.6.2, GraalVM CE Native [x86_64-linux]
* MLton 20130715 (built Fri Apr 28 06:06:34 UTC 2017 on lcy01-11)
* Microsoft (R) F# Compiler version 10.2.3 for F# 4.5
* OCaml 4.07.0
* Racket v6.11
* Chez Scheme Version 9.5
* V 0.1.21 c18578a
