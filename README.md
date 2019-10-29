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
| Kotlin          | 2.01    | 37.6        |
| Nim Gcc         | 2.17    | 0.7         |
| C++ Gcc         | 2.41    | 1.7         |
| Go              | 3.01    | 1.3         |
| Java            | 3.05    | 37.2        |
| Crystal         | 3.06    | 2.7         |
| ML MLton        | 3.22    | 0.7         |
| Go Gcc          | 3.30    | 19.2        |
| Nim Clang       | 3.43    | 1.0         |
| Rust            | 3.51    | 0.8         |
| D Ldc           | 3.57    | 1.4         |
| D Gdc           | 3.72    | 5.8         |
| OCaml           | 3.75    | 3.9         |
| Scala           | 4.30    | 136.3       |
| C# .Net Core    | 4.48    | 23.8        |
| D Dmd           | 4.77    | 1.7         |
| Haskell (MArray)| 6.88    | 3.5         |
| C# Mono         | 6.88    | 17.6        |
| Javascript Node | 7.10    | 31.2        |
| V Gcc           | 7.38    | 0.7         |
| V Clang         | 9.26    | 1.1         |
| LuaJIT          | 10.99   | 2.1         |
| F# Mono         | 12.81   | 25.1        |
| Racket          | 17.52   | 87.4        |
| Python PyPy     | 21.51   | 95.4        |
| Chez Scheme     | 24.72   | 29.2        |
| Haskell         | 29.14   | 3.4         |
| Ruby truffle    | 32.52   | 613.3       |
| Ruby            | 191.36  | 13.1        |
| Ruby JRuby      | 195.63  | 284.4       |
| Lua 5.3         | 201.26  | 1.4         |
| Elixir          | 279.03  | 48.9        |
| Python3         | 396.27  | 7.9         |
| Python          | 399.75  | 6.2         |
| Tcl (FP)        | 494.78  | 4.3         |
| Perl            | 769.17  | 5.2         |
| Tcl (OO)        | 1000.55 | 4.3         |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| C++ Gcc         | 21.42   | 3.5         |
| Crystal         | 22.89   | 2.9         |
| Kotlin          | 27.38   | 44.6        |
| V Gcc           | 27.61   | 2.0         |
| Java            | 28.44   | 43.9        |
| Nim Gcc         | 30.00   | 1.9         |
| Scala           | 30.65   | 139.4       |
| Nim Clang       | 31.39   | 2.4         |
| D Ldc           | 31.56   | 3.7         |
| D Gdc           | 31.87   | 7.2         |
| ML MLton        | 32.04   | 3.6         |
| Go Gcc          | 32.88   | 20.6        |
| Rust            | 33.12   | 1.9         |
| C# .Net Core    | 36.38   | 25.8        |
| V Clang         | 39.26   | 3.0         |
| OCaml           | 44.12   | 7.1         |
| Go              | 45.55   | 2.9         |
| D Dmd           | 56.08   | 4.1         |
| Javascript Node | 69.00   | 34.5        |
| C# Mono         | 71.27   | 18.0        |
| Python PyPy     | 106.44  | 96.2        |
| LuaJIT          | 107.96  | 2.9         |
| Haskell (MArray)| 122.81  | 4.9         |
| Ruby truffle    | 171.47  | 630.7       |
| F# Mono         | 195.99  | 40.0        |
| Racket          | 198.86  | 88.2        |
| Chez Scheme     | 244.82  | 29.3        |

# Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| C aklomp        | 0.38    | 2.0         |
| C               | 1.81    | 1.9         |
| Rust            | 1.90    | 2.4         |
| Crystal         | 2.31    | 5.2         |
| D Ldc           | 2.46    | 4.2         |
| Ruby            | 2.54    | 73.1        |
| V Gcc           | 2.70    | 1.7         |
| V Clang         | 2.78    | 2.1         |
| Java            | 2.98    | 371.0       |
| D Gdc           | 3.08    | 10.7        |
| Scala           | 3.09    | 131.3       |
| Kotlin          | 3.23    | 377.4       |
| Nim Gcc         | 3.30    | 7.4         |
| Perl XS         | 3.35    | 6.2         |
| Javascript Node | 3.44    | 99.9        |
| Nim Clang       | 3.70    | 7.7         |
| C++ libcrypto   | 4.15    | 5.5         |
| Php             | 4.27    | 16.7        |
| Go              | 4.57    | 8.1         |
| C# .Net Core    | 5.21    | 30.6        |
| D               | 5.58    | 11.4        |
| Python3         | 6.35    | 10.0        |
| Tcl             | 6.44    | 5.6         |
| Python          | 6.51    | 8.2         |
| Go Gcc          | 6.68    | 43.8        |
| Python PyPy     | 7.26    | 95.0        |
| C# Mono         | 8.43    | 36.5        |
| Julia           | 9.93    | 214.1       |
| Ruby JRuby      | 14.55   | 241.8       |
| Perl            | 27.59   | 8.0         |
| Ruby truffle    | 32.01   | 384.1       |

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
| Java            | 1.03    | 403.0       |
| Scala           | 1.20    | 242.8       |
| Rust Value      | 2.08    | 1745.9      |
| Javascript Node | 2.22    | 507.6       |
| Crystal Schema  | 2.23    | 282.0       |
| Perl XS         | 2.37    | 966.4       |
| Crystal         | 2.94    | 962.1       |
| Clojure         | 3.07    | 1652.1      |
| V Clang         | 3.37    | 1128.0      |
| Go              | 3.42    | 511.2       |
| Php             | 3.43    | 1476.7      |
| Python3 ujson   | 3.61    | 1184.0      |
| Julia           | 4.14    | 2369.6      |
| Python3         | 4.65    | 910.4       |
| Python PyPy     | 4.78    | 1293.0      |
| Crystal Pull    | 4.94    | 4.3         |
| Python ujson    | 5.10    | 1404.7      |
| Nim Clang       | 5.19    | 1321.6      |
| Go Gcc          | 5.28    | 456.9       |
| C++ LibJson     | 5.46    | 2888.8      |
| V Gcc           | 5.47    | 1127.5      |
| Nim Gcc         | 5.62    | 1321.3      |
| Haskell         | 5.91    | 9.9         |
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
| Nim Gcc         | 3.21    | 72.5        |
| Nim Clang       | 3.22    | 73.0        |
| Julia           | 3.22    | 246.5       |
| Crystal         | 3.32    | 63.3        |
| V Clang         | 3.35    | 70.6        |
| Go Gcc          | 3.41    | 106.6       |
| Go              | 3.54    | 60.4        |
| Kotlin          | 3.59    | 129.6       |
| Scala           | 3.67    | 141.6       |
| Java            | 3.81    | 129.5       |
| V Gcc           | 4.17    | 70.2        |
| Javascript Node | 4.36    | 101.7       |
| Swift           | 7.13    | 186.4       |
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
| Nim Gcc         | 14.40   | 505.8       |
| Nim Clang       | 14.83   | 470.8       |
| C++             | 16.16   | 179.2       |
| Scala           | 23.29   | 382.7       |
| D Ldc           | 23.37   | 460.1       |
| Go              | 24.24   | 347.0       |
| D               | 28.51   | 461.4       |
| D Gdc           | 30.17   | 419.3       |
| C# Mono         | 31.33   | 326.7       |
| Go Gcc          | 36.52   | 430.7       |
| Python PyPy     | 40.09   | 624.4       |
| C# .Net Core    | 43.10   | 542.1       |
| Python          | 345.14  | 725.5       |

# Using Docker

Build the image:

    $ docker build docker/ -t benchmarks

Run the image:

    $ docker run -it --rm --name test -v $(pwd):/src benchmarks <cmd>

where <cmd> is:

 - `versions` (print installed language versions)
 - `shell` (start the shell)
 - `brainfuck2 bench` (build and run Brainfuck2 bench.b benchmarks)
 - `brainfuck2 mandel` (build and run Brainfuck2 mandel.b benchmarks)
 - `base64` (build and run Base64 benchmarks)
 - `json` (build and run Json benchmarks)
 - `matmul` (build and run Matmul benchmarks)
 - `havlak` (build and run Havlak benchmarks)

Please note that some tests are unstable under Docker and should be run manually (use `shell` command to get an access to the image).

# Environment

CPU: Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz

OS: Ubuntu 18.04.3 LTS x86_64

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.0.100                         |
| C# .NET Core | 3.3.1-beta4-19462-11 (66a912c9) |
| C# Mono      | 6.4.0.198                       |
| Chez Scheme  | 9.5                             |
| Clang        | 7.0.0 (tags/RELEASE_700/final)  |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.31.1                          |
| DMD          | v2.088.1                        |
| Elixir       | 1.9.1                           |
| F#           | 10.2.3 for F# 4.5               |
| GCC          | 9.2.1                           |
| GCC Go       | 9.2.1                           |
| GDC          | 9.2.1                           |
| Go           | go1.13.1                        |
| Haskell      | 8.8.1                           |
| JRuby        | 9.2.8.0                         |
| Java         | 11.0.4                          |
| Julia        | v"1.2.0"                        |
| Kotlin       | 1.3.50                          |
| LDC          | 1.18.0-beta1                    |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20130715                        |
| Nim          | 1.0.2                           |
| Node.js      | v12.13.0                        |
| OCaml        | 4.07.0                          |
| PHP          | 7.2.19-0ubuntu0.18.04.2         |
| Perl         | v5.26.1                         |
| PyPy         | 7.1.1-final0 for Python 2.7.13  |
| Python 2     | 2.7.15+                         |
| Python 3     | 3.6.8                           |
| Racket       | "6.11"                          |
| Ruby         | 2.6.5p114                       |
| Rust         | 1.38.0                          |
| Scala        | 2.13.1                          |
| Swift        | swift-5.1-RELEASE               |
| Tcl          | 8.6                             |
| TruffleRuby  | 19.2.0.1                        |
| V            | 0.1.21 d4c1bba                  |
| jq           | jq-1.5-1-a5b5cbe                |
| ooc          | 0.9.11-head codename sapporo    |
