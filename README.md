Some benchmarks of different languages
--------------------------------------

# Brainfuck v2.0

[Brainfuck v2.0](https://github.com/kostya/benchmarks/tree/master/brainfuck2)
[Brainfuck v1.0](https://github.com/kostya/benchmarks/tree/master/brainfuck)

### bench.b

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| Kotlin          | 1.78    | 28.4        |
| C++ Gcc         | 1.94    | 1.0         |
| D Ldc           | 2.02    | 0.9         |
| Nim Gcc         | 2.16    | 0.6         |
| Rust            | 2.42    | 4.8         |
| Crystal         | 2.91    | 1.2         |
| D Gdc           | 3.05    | 1.4         |
| ML MLton        | 3.14    | 0.7         |
| Nim Clang       | 3.14    | 0.8         |
| Scala           | 3.43    | 120.12      |
| Java            | 4.03    | 513.8       |
| OCaml           | 4.04    | 2.7         |
| Go Gcc          | 4.20    | 10.0        |
| D Dmd           | 5.23    | 1.0         |
| Go              | 5.54    | 0.9         |
| Javascript V8   | 6.53    | 7.9         |
| Javascript Node | 7.38    | 16.9        |
| F# Mono         | 7.52    | 21.1        |
| Haskell (MArray)| 7.95    | 2.2         |
| C# .Net Core    | 16.03   | 16.9        |
| LuaJIT          | 16.73   | 1.1         |
| C# Mono         | 19.86   | 14.6        |
| Python PyPy     | 22.14   | 75.9        |
| Haskell         | 25.34   | 2.2         |
| Ruby truffle    | 33.79   | 1089.8      |
| Ruby Topaz      | 59.53   | 36.5        |
| Ruby Rbx        | 120.38  | 32.3        |
| Ruby Jruby      | 129.75  | 267.6       |
| Ruby Jruby9k    | 134.18  | 286.8       |
| Ruby            | 143.80  | 6.6         |
| Elixir          | 211.20  | 31.9        |
| Lua 5.1         | 220.16  | 1.0         |
| Python          | 314.79  | 4.9         |
| Python3         | 412.13  | 5.5         |
| Tcl (FP)        | 457.33  | 2.5         |
| Tcl (OO)        | 821.96  | 2.5         |

### mandel.b

[Mandel in Brainfuck](https://github.com/kostya/benchmarks/blob/master/brainfuck2/mandel.b)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| C++ Gcc         | 20.49   | 1.7         |
| Crystal         | 23.39   | 1.5         |
| Rust            | 24.13   | 4.8         |
| D Ldc           | 24.90   | 1.4         |
| D Gdc           | 29.49   | 2.4         |
| Nim Gcc         | 30.58   | 2.7         |
| ML MLton        | 36.03   | 2.5         |
| Go Gcc          | 37.59   | 11.4        |
| Nim Clang       | 40.01   | 2.9         |
| Kotlin          | 40.79   | 34.9        |
| OCaml           | 52.18   | 5.3         |
| Scala           | 58.51   | 120.12      |
| Java            | 58.86   | 423.9       |
| D Dmd           | 62.69   | 1.7         |
| Javascript Node | 88.34   | 18.6        |
| Go              | 105.04  | 2.2         |
| Haskell (MArray)| 124.60  | 3.2         |
| Python PyPy     | 133.73  | 76.0        |
| Ruby truffle    | 138.00  | 1014.1      |
| C# .Net Core    | 142.30  | 17.8        |
| LuaJIT          | 142.59  | 1.8         |
| C# Mono         | 147.69  | 12.6        |
| F# Mono         | 166.38  | 29.1        |
| Ruby Topaz      | 305.79  | 38.8        |

# Base64

[Base64](https://github.com/kostya/benchmarks/tree/master/base64)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| C aklomp SSSE3  | 0.93    | 32.3        |
| C               | 1.85    | 32.2        |
| Crystal         | 2.30    | 113.8       |
| Rust            | 2.38    | 40.8        |
| D Gdc           | 2.52    | 33.3        |
| Ruby            | 2.77    | 130.4       |
| D Ldc           | 3.14    | 53.1        |
| Nim Clang       | 3.20    | 51.4        |
| Perl XS         | 3.63    | 47.9        |
| Ruby Rbx        | 4.29    | 30.7        |
| Nim Gcc         | 4.32    | 51.5        |
| Julia           | 4.41    | 190.0       |
| Javascript Node | 4.76    | 551.5       |
| C++ Openssl     | 5.45    | 65.2        |
| Php             | 6.34    | 53.4        |
| C# .Net Core    | 6.52    | 121.1       |
| D               | 7.18    | 55.3        |
| Tcl             | 7.20    | 66.0        |
| Python          | 7.62    | 52.6        |
| Go              | 8.40    | 95.7        | 
| Python PyPy     | 8.05    | 153.9       |
| Python3         | 8.13    | 54.5        |
| C# Mono         | 9.01    | 71.7        |
| Java            | 9.06    | 971.2       |
| Kotlin          | 9.75    | 932.9       |
| Scala           | 10.69   | 292.5       |
| Ruby JRuby9K    | 12.16   | 530.6       |
| Ruby JRuby      | 12.65   | 514.9       |
| Ruby truffle    | 17.17   | 908.3       |
| Perl            | 33.30   | 99.7        |
| Go Gcc          | 39.56   | 185.5       |

# Json

[Json](https://github.com/kostya/benchmarks/tree/master/json)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| D Gdc Fast      | 0.34    | 226.7       |
| Rust Pull       | 0.52    | 207.7       |
| Rust Struct     | 0.54    | 230.3       |
| C++ Rapid SAX   | 0.72    | 1.0         |
| C++ Gason       | 0.83    | 582.2       |
| C++ Rapid       | 0.94    | 243.6       |
| Java            | 1.47    | 621.2       |
| Rust Value      | 1.78    | 1673.9      |
| Crystal Schema  | 2.05    | 337.2       |
| Perl XS         | 2.68    | 888.4       |
| Crystal         | 2.99    | 1118.2      |
| Javascript Node | 3.21    | 863.7       |
| Crystal Pull    | 3.48    | 1.2         |
| Python3 ujson   | 4.15    | 1303.2      |
| Nim Clang       | 4.95    | 1338.1      |
| Nim Gcc         | 5.02    | 1337.3      |
| Python ujson    | 5.07    | 1352.9      |
| Q               | 5.18    | 684.0       |
| Go              | 5.30    | 479.3       |
| Python PyPy     | 5.35    | 1534.5      |
| C++ LibJson     | 5.49    | 2796.3      |
| Clojure         | 5.81    | 1148.5      |
| Python3         | 5.82    | 1037.8      |
| C# .Net Core    | 6.31    | 834.9       |
| Php             | 6.37    | 1502.0      |
| Haskell         | 8.31    | 70.5        |
| Ruby            | 8.67    | 1074.6      |
| Ruby Yajl       | 9.52    | 1054.6      |
| Python          | 9.85    | 1409.1      |
| C# Mono         | 10.57   | 812.1       |
| Julia           | 11.89   | 2622.4      |
| D               | 12.42   | 1417.1      |
| JQ              | 14.92   | 1714.5      |
| Scala           | 15.47   | 1415.8      |
| C++ Boost       | 16.44   | 2915.2      |
| Ruby JRuby9K    | 16.53   | 2050.5      |
| Go Gcc          | 17.64   | 473.1       |
| Ruby JRuby      | 21.98   | 2761.1      |
| D Gdc           | 25.86   | 926.1       |
| D Ldc           | 27.23   | 919.6       |
| Perl            | 46.02   | 1635.4      |
| Ruby Rbx        | 67.13   | 4681.0      |
| Ruby truffle    |   -     | 1251.2      |

# Matmul

[Matmul](https://github.com/kostya/benchmarks/tree/master/matmul)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| Julia Native Thr| 0.11    | 148.3       |
| Julia Native    | 0.31    | 175.8       |
| D Mir GLAS      | 0.32    | 56.7        |
| D Ldc           | 2.01    | 68.9        |
| D               | 2.30    | 71.3        |
| D Gdc           | 2.33    | 73.0        |
| Python Numpy    | 3.08    | 65.3        |
| Java            | 3.50    | 136.2       |
| Rust            | 3.52    | 76.8        |
| Scala           | 3.62    | 136.2       |
| Kotlin          | 3.62    | 132.2       |
| C               | 3.64    | 69.2        |
| Crystal         | 3.70    | 61.7        |
| Nim Gcc         | 3.72    | 187.9       |
| Go              | 3.84    | 58.1        |
| Go Gcc          | 3.90    | 84.5        |
| Swift           | 4.48    | 110.8       |
| Nim Clang       | 4.60    | 199.2       |
| Javascript Node | 5.95    | 88.3        |
| Javascript V8   | 6.87    | 81.5        |
| Python PyPy     | 7.98    | 121.7       |
| C# .Net Core    | 10.87   | 85.7        |
| C# Mono         | 15.17   | 83.6        |
| Julia           | 20.98   | 187.4       |
| Ruby truffle    | 35.43   | 934.2       |
| Ruby Topaz      | 81.41   | 206.2       |
| Ruby            | 326.44  | 76.9        |
| Python          | 447.39  | 74.0        |
| Ruby JRuby      | 416.12  | 582.4       |
| Ruby JRuby9k    | 467.59  | 608.3       |
| Ruby Rbx        | 591.70  | 325.0       |
| Perl            | 666.46  | 604.1       |
| Tcl             | 1066.66 | 279.9       |

# Havlak

[Havlak](https://github.com/kostya/benchmarks/tree/master/havlak)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| Crystal         | 15.80   | 416.9       |
| Nim Clang       | 17.02   | 480.9       |
| Nim Gcc         | 17.23   | 485.0       |
| C++             | 17.72   | 174.5       |
| D Ldc           | 25.15   | 214.9       |
| D               | 28.90   | 418.2       |
| D Gdc           | 31.79   | 197.6       |
| Scala           | 32.18   | 363.0       |
| Go Gcc          | 32.94   | 365.7       |
| Go              | 33.25   | 386.4       |
| C# Mono         | 40.54   | 270.0       |
| Python PyPy     | 50.21   | 797.0       |
| C# .Net Core    | 61.38   | 388.8       |
| Python          | 396.54  | 724.0       |



# Hardware:

Intel(R) Core(TM) i5-2400 CPU @ 3.10GHz (Ubuntu 14.04.1 LTS x86_64)

# Versions:

* gcc (Ubuntu 4.8.2-19ubuntu1) 4.8.2
* Nim Compiler Version 0.16.0 (2017-01-08) [Linux: amd64]
* `Crystal 0.20.0 [b0cc6f7] (2016-11-22)`
* go version go1.8 linux/amd64
* gccgo (Ubuntu 4.9.1-0ubuntu1) 4.9.1
* DMD64 D Compiler v2.068.0
* gdc (crosstool-NG crosstool-ng-1.20.0-232-gc746732 - 20150830-2.066.1-dadb5a3784) 5.2.0
* LDC - the LLVM D compiler (0.15.2-beta1):
* V8 version 3.29.62 (candidate)
* rustc 1.15.1 (021bd294c 2017-02-08)
* Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_45)
* Nodejs v7.0.0
* PyPy 5.6.0 with GCC 4.8.2
* topaz (ruby-1.9.3p125) (git rev b95c858) [x86_64-linux]
* ruby 2.4.0p0 (2016-12-24 revision 57164) [x86_64-linux]
* Python 2.7.6
* Python 3.5.2
* rubinius 2.2.10 (2.1.0 bf61ae2e 2014-06-27 JI) [x86_64-linux-gnu]
* jruby 1.7.20 (1.9.3p551) 2015-05-04 3086e6a on Java HotSpot(TM) 64-Bit Server VM 1.8.0_45-b14 +jit [linux-amd64]
* jruby 9.0.0.0.pre2 (2.2.2) 2015-04-28 2755ae0 Java HotSpot(TM) 64-Bit Server VM 25.45-b02 on 1.8.0_45-b14 +jit [linux-amd64]
* java version "1.8.0_45" Java HotSpot(TM) 64-Bit Server VM (build 25.45-b02, mixed mode)
* julia version 0.4.3
* clang version 3.9.0 (tags/RELEASE_390/final)
* Mono JIT compiler version 4.0.1 (tarball Tue May 12 15:39:23 UTC 2015)
* rock 0.9.10-head codename sapphire, built on Wed Jul  1 20:09:58 2015
* Felix version 15.04.03
* Q KDB+ 3.3 2015.09.02 Copyright (C) 1993-2015 Kx Systems
* perl 5, version 18, subversion 2 (v5.18.2) built for x86_64-linux-gnu-thread-multi
* The Glorious Glasgow Haskell Compilation System, version 7.10.2
* Tcl 8.6
* jq version 1.3
* Swift version 2.2-dev (LLVM ae2eb212e4, Clang ef4c02f431, Swift 634acb40a1)
* Kotlin version 1.0.3 (JRE 1.8.0_45-b14)
* PHP 7.0.9-1+deb.sury.org~trusty+1 (cli) ( NTS )
* .Net Core 1.0.0-preview2-003121
* Elixir 1.2.0-rc.0 (a2860b3) Erlang/OTP 18 [erts-7.2] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]
* Lua 5.1.5  Copyright (C) 1994-2012 Lua.org, PUC-Rio
* LuaJIT 2.0.2 -- Copyright (C) 2005-2013 Mike Pall. http://luajit.org/
* truffleruby SNAPSHOT, like ruby 2.3.1 <Java HotSpot(TM) 64-Bit Server VM 1.8.0_111-b14 with Graal> [linux-x86_64]
* MLton 20100608 (built Sun Oct 27 04:18:37 UTC 2013 on allspice)
* F# Compiler for F# 3.0 (Open Source Edition)
* OCaml 4.01.0
