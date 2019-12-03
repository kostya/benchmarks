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

2019-11-09

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
| OCaml           | 2.50    | 4.4         |
| Go              | 2.94    | 1.5         |
| Java            | 3.05    | 37.2        |
| Crystal         | 3.06    | 2.7         |
| ML MLton        | 3.22    | 0.7         |
| Go Gcc          | 3.30    | 19.2        |
| Rust            | 3.43    | 0.8         |
| Nim Clang       | 3.43    | 1.0         |
| D Ldc           | 3.57    | 1.4         |
| D Gdc           | 3.72    | 5.8         |
| Julia           | 4.02    | 162.9       |
| C# .NET Core    | 4.30    | 24.3        |
| Scala           | 4.30    | 136.3       |
| F# .NET Core    | 4.35    | 24.5        |
| D Dmd           | 4.74    | 3.3         |
| Haskell (MArray)| 6.88    | 3.5         |
| C# Mono         | 6.88    | 17.6        |
| Javascript Node | 6.97    | 31.5        |
| V Gcc           | 7.30    | 0.8         |
| V Clang         | 9.06    | 1.0         |
| Racket          | 10.49   | 77.4        |
| LuaJIT          | 10.99   | 2.1         |
| Python PyPy     | 21.51   | 95.4        |
| Chez Scheme     | 24.72   | 29.2        |
| Haskell         | 29.14   | 3.4         |
| Ruby truffle    | 32.52   | 613.3       |
| Ruby JRuby      | 180.65  | 241.7       |
| Ruby            | 191.36  | 13.1        |
| Lua 5.3         | 201.26  | 1.4         |
| Elixir          | 279.03  | 48.9        |
| Python3         | 388.22  | 7.8         |
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
| V Gcc           | 27.28   | 2.2         |
| Kotlin          | 27.38   | 44.6        |
| Java            | 28.44   | 43.9        |
| Nim Gcc         | 30.00   | 1.9         |
| Scala           | 30.65   | 139.4       |
| Rust            | 31.01   | 1.9         |
| Nim Clang       | 31.39   | 2.4         |
| D Ldc           | 31.56   | 3.7         |
| D Gdc           | 31.87   | 7.2         |
| ML MLton        | 32.04   | 3.6         |
| Go Gcc          | 32.88   | 20.6        |
| F# .NET Core    | 36.23   | 25.9        |
| C# .NET Core    | 36.38   | 25.8        |
| V Clang         | 37.10   | 2.5         |
| Go              | 45.28   | 2.9         |
| OCaml           | 47.90   | 6.9         |
| D Dmd           | 56.16   | 4.1         |
| Javascript Node | 62.70   | 34.5        |
| C# Mono         | 71.27   | 18.0        |
| Julia           | 79.88   | 163.3       |
| Python PyPy     | 106.44  | 96.2        |
| LuaJIT          | 107.96  | 2.9         |
| Haskell (MArray)| 122.81  | 4.9         |
| Ruby truffle    | 171.47  | 630.7       |
| Racket          | 171.86  | 77.4        |
| Chez Scheme     | 244.82  | 29.3        |

# Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| C aklomp        | 0.37    | 1.9         |
| Rust            | 1.84    | 2.3         |
| C               | 1.86    | 1.8         |
| Crystal         | 2.32    | 5.3         |
| D Ldc           | 2.44    | 4.0         |
| V Gcc           | 2.71    | 1.6         |
| Ruby            | 2.72    | 72.8        |
| V Clang         | 2.83    | 2.1         |
| D Gdc           | 2.89    | 10.6        |
| Java            | 3.11    | 375.4       |
| Scala           | 3.20    | 154.9       |
| Perl XS         | 3.25    | 6.3         |
| Nim Clang       | 3.27    | 7.4         |
| Kotlin          | 3.29    | 346.0       |
| Nim Gcc         | 3.30    | 7.1         |
| Javascript Node | 3.44    | 100.3       |
| Php             | 3.93    | 16.1        |
| C++ libcrypto   | 4.06    | 5.6         |
| Go              | 4.71    | 12.9        |
| C# .Net Core    | 5.39    | 27.1        |
| D               | 5.70    | 11.3        |
| Tcl             | 6.05    | 5.1         |
| Python          | 6.06    | 7.4         |
| Python PyPy     | 6.22    | 99.9        |
| Python3         | 6.97    | 9.3         |
| Go Gcc          | 7.15    | 43.0        |
| C# Mono         | 8.86    | 36.0        |
| Julia           | 11.15   | 236.1       |
| Ruby JRuby      | 18.73   | 199.0       |
| Perl            | 28.00   | 8.0         |
| Ruby truffle    | 32.39   | 381.7       |

# Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| D Gdc Fast      | 0.30    | 179.8       |
| Rust Pull       | 0.41    | 202.3       |
| Rust Struct     | 0.42    | 221.9       |
| C++ Rapid SAX   | 0.51    | 1.8         |
| C++ Simdjson    | 0.59    | 543.3       |
| C++ Rapid       | 0.86    | 232.6       |
| C++ Gason       | 0.99    | 593.4       |
| Java            | 1.07    | 310.6       |
| Scala           | 1.26    | 227.4       |
| Javascript Node | 2.38    | 508.8       |
| Rust Value      | 2.41    | 1745.8      |
| Crystal Schema  | 2.92    | 283.3       |
| Perl XS         | 2.92    | 941.2       |
| Clojure         | 3.35    | 1472.7      |
| Go              | 3.97    | 510.7       |
| V Gcc           | 4.10    | 1127.4      |
| V Clang         | 4.10    | 1127.8      |
| Python3 ujson   | 4.15    | 1287.4      |
| Php             | 4.25    | 1483.1      |
| Julia           | 4.78    | 2689.5      |
| Crystal         | 5.46    | 961.9       |
| Crystal Pull    | 5.48    | 4.2         |
| C++ LibJson     | 5.65    | 3331.3      |
| Python3         | 5.75    | 979.8       |
| Python ujson    | 5.75    | 1454.6      |
| Nim Gcc         | 6.11    | 1321.1      |
| Nim Clang       | 6.18    | 1321.5      |
| C# .Net Core    | 6.79    | 854.3       |
| Python PyPy     | 6.84    | 1294.9      |
| Go Gcc          | 6.91    | 454.1       |
| D Ldc           | 7.63    | 1546.6      |
| Ruby            | 8.15    | 842.4       |
| Haskell         | 8.24    | 10.0        |
| Ruby Yajl       | 8.63    | 839.7       |
| D Gdc           | 10.16   | 1362.4      |
| Python          | 10.31   | 1465.5      |
| JQ              | 11.58   | 1321.8      |
| C# Mono         | 12.11   | 1046.4      |
| C++ Boost       | 12.45   | 2952.2      |
| Ruby JRuby      | 13.80   | 3113.2      |
| D Dmd           | 14.03   | 1546.7      |
| Perl            | 49.11   | 1212.7      |
| Ruby truffle    | 173.70  | 5276.9      |

# Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| Julia Native Thr| 0.10    | 253.9       |
| Julia Native    | 0.26    | 282.9       |
| D Ldc lubeck    | 0.38    | 62.8        |
| Python Numpy    | 1.17    | 81.6        |
| D Ldc           | 1.93    | 73.1        |
| D               | 2.17    | 72.8        |
| D Gdc           | 2.28    | 76.7        |
| C               | 3.12    | 69.8        |
| Rust            | 3.17    | 70.5        |
| Julia           | 3.20    | 248.8       |
| Nim Clang       | 3.21    | 73.1        |
| Nim Gcc         | 3.23    | 72.4        |
| Go              | 3.32    | 60.6        |
| Crystal         | 3.33    | 63.1        |
| Go Gcc          | 3.41    | 110.7       |
| Kotlin          | 3.43    | 125.9       |
| V Clang         | 3.47    | 70.5        |
| Java            | 3.81    | 124.7       |
| Scala           | 4.15    | 168.7       |
| V Gcc           | 4.21    | 70.1        |
| Javascript Node | 4.40    | 102.4       |
| Swift           | 8.20    | 205.2       |
| C# .Net Core    | 8.21    | 99.3        |
| Python PyPy     | 8.50    | 138.1       |
| C# Mono         | 14.66   | 88.2        |
| Ruby truffle    | 60.39   | 559.7       |
| Ruby            | 365.68  | 82.5        |
| Ruby JRuby      | 423.94  | 1219.8      |
| Tcl             | 580.68  | 280.5       |
| Python          | 606.50  | 75.7        |
| Perl            | 654.90  | 606.5       |

# Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

| Language        | Time, s | Memory, MiB |
| --------------- | ------- | ----------- |
| Crystal         | 10.70   | 226.4       |
| C++             | 17.23   | 179.2       |
| Nim Gcc         | 17.81   | 506.5       |
| Nim Clang       | 18.46   | 509.9       |
| Go              | 24.71   | 342.1       |
| D Ldc           | 25.86   | 460.4       |
| Scala           | 26.97   | 387.9       |
| D               | 31.57   | 460.6       |
| D Gdc           | 33.26   | 419.0       |
| C# Mono         | 37.37   | 318.8       |
| Go Gcc          | 40.92   | 453.3       |
| C# .Net Core    | 44.02   | 491.4       |
| Python PyPy     | 46.53   | 777.4       |
| Python          | 429.51  | 744.9       |

# Using Docker

Build the image:

    $ docker build docker/ -t benchmarks

Run the image:

    $ docker run -it --rm -v $(pwd):/src benchmarks <cmd>

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

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.0.100                         |
| C# .NET Core | 3.3.1-beta4-19462-11 (66a912c9) |
| C# Mono      | 6.4.0.198                       |
| Chez Scheme  | 9.5                             |
| Clang        | 9.0.0 (tags/RELEASE_900/final)  |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.31.1                          |
| DMD          | v2.089.0                        |
| Elixir       | 1.9.1                           |
| F# .NET Core | 10.6.0.0 for F# 4.7             |
| GCC          | 9.2.1                           |
| GCC Go       | 9.2.1                           |
| GDC          | 9.2.1                           |
| Go           | go1.13.4                        |
| Haskell      | 8.8.1                           |
| JRuby        | 9.2.9.0                         |
| Java         | 13.0.1                          |
| Julia        | v"1.2.0"                        |
| Kotlin       | 1.3.50                          |
| LDC          | 1.18.0                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.0.2                           |
| Node.js      | v13.1.0                         |
| OCaml        | 4.09.0                          |
| PHP          | 7.3.10-1+b1                     |
| Perl         | v5.30.0                         |
| PyPy         | 7.1.1-final0 for Python 2.7.13  |
| Python 2     | 2.7.17                          |
| Python 3     | 3.7.5                           |
| Racket       | "7.4"                           |
| Ruby         | 2.6.5p114                       |
| Rust         | 1.39.0                          |
| Scala        | 2.13.1                          |
| Swift        | swift-5.1.2-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 19.2.1                          |
| V            | 0.1.22 ab37081                  |
| jq           | jq-1.6                          |
