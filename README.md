<!-- md-toc-begin -->
# Table of Content
* [Overview](#overview)
* [Test Cases](#test-cases)
  * [Brainfuck](#brainfuck)
    * [bench.b](#benchb)
    * [mandel.b](#mandelb)
  * [Base64](#base64)
  * [Json](#json)
  * [Matmul](#matmul)
  * [Havlak](#havlak)
* [Tests Execution](#tests-execution)
  * [Environment](#environment)
  * [Using Docker](#using-docker)
  * [Manual Execution](#manual-execution)
    * [Prerequisites](#prerequisites)
* [Contribution](#contribution)
  * [Makefile guide](#makefile-guide)
    * [Binary executables](#binary-executables)
    * [Compiled artifacts](#compiled-artifacts)
    * [Scripting language](#scripting-language)
<!-- md-toc-end -->

# Overview

The benchmarks follow the criteria:

  - They are written as the average software developer would write them, i.e.

    - The algorithms are implemented as cited in public sources;
    - The libraries are used as described in the tutorials, documentation and examples;
    - The used data structures are idiomatic.

  - The used algorithms are similar between the languages (as the reference implementations), variants are acceptable if the reference implementation exists.
  - All final binaries are releases (optimized for performance if possible) as debug performance may vary too much depending on the compiler.

My other benchmarks: [jit-benchmarks](https://github.com/kostya/jit-benchmarks), [crystal-benchmarks-game](https://github.com/kostya/crystal-benchmarks-game)

UPDATE: 2020-07-21

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck](brainfuck)

### bench.b

|           Language |        Time, s |    Memory, MiB |          Energy, J |
| :----------------- | -------------: | -------------: | -----------------: |
|                C++ |   1.00 ± 00.03 |   1.49 ± 00.02 |      18.64 ± 01.03 |
|              OCaml |   1.72 ± 00.12 |   5.07 ± 00.03 |      35.41 ± 05.20 |
|           Vala GCC |   1.73 ± 00.08 |   4.01 ± 00.30 |      35.54 ± 03.16 |
|            Nim GCC |   1.75 ± 00.10 |   1.87 ± 00.06 |      35.09 ± 03.26 |
|          Nim Clang |   1.78 ± 00.05 |   2.34 ± 00.06 |      41.79 ± 01.49 |
|              C GCC |   1.81 ± 00.09 |   0.52 ± 00.03 |      35.94 ± 03.60 |
|                LDC |   1.86 ± 00.07 |   2.84 ± 00.08 |      35.52 ± 03.22 |
|             Kotlin |   1.87 ± 00.08 |  40.32 ± 00.22 |      37.07 ± 03.34 |
|         Vala Clang |   1.92 ± 00.08 |   3.94 ± 00.03 |      41.37 ± 04.49 |
|                GDC |   1.99 ± 00.09 |   6.27 ± 00.06 |      38.35 ± 03.92 |
|               Rust |   2.02 ± 00.07 |   1.94 ± 00.10 |      39.22 ± 03.13 |
|            C Clang |   2.16 ± 00.12 |   0.52 ± 00.03 |      44.93 ± 04.01 |
|             GCC Go |   2.17 ± 00.11 |  24.58 ± 05.98 |      43.27 ± 04.25 |
|       C# .NET Core |   2.38 ± 00.13 |  34.57 ± 00.15 |      48.19 ± 05.44 |
|               Java |   2.38 ± 00.15 |  38.72 ± 00.23 |      48.32 ± 04.68 |
|              V GCC |   2.42 ± 00.10 |   0.50 ± 00.02 |      51.49 ± 04.92 |
|                 Go |   2.42 ± 00.13 |   3.09 ± 00.39 |      50.28 ± 05.67 |
|              MLton |   2.46 ± 00.12 |   0.55 ± 00.03 |      51.31 ± 05.18 |
|            Crystal |   2.58 ± 00.13 |   3.28 ± 00.04 |      53.16 ± 05.03 |
|            V Clang |   2.70 ± 00.14 |   0.86 ± 00.02 |      56.23 ± 05.90 |
|       F# .NET Core |   2.72 ± 00.06 | 124.28 ± 04.48 |      53.36 ± 03.92 |
|        Chez Scheme |   2.80 ± 00.15 |  29.23 ± 00.10 |      59.26 ± 06.64 |
|              Julia |   3.33 ± 00.15 | 175.79 ± 00.83 |      59.69 ± 05.34 |
|                DMD |   3.53 ± 00.14 |   3.50 ± 00.05 |      72.68 ± 06.50 |
|              Scala |   3.91 ± 00.22 | 140.07 ± 04.56 |      80.82 ± 08.19 |
|            C# Mono |   4.27 ± 00.21 |  20.09 ± 00.07 |      94.18 ± 08.81 |
|     Haskell MArray |   4.70 ± 00.16 |   5.26 ± 00.09 |      89.81 ± 08.47 |
|            Node.js |   4.78 ± 00.25 |  33.82 ± 00.18 |      99.72 ± 10.03 |
|             LuaJIT |   7.47 ± 00.30 |   2.79 ± 00.10 |     129.95 ± 12.48 |
|             Racket |   7.78 ± 00.37 | 106.48 ± 00.10 |     151.95 ± 13.28 |
|               PyPy |  13.45 ± 01.41 | 108.40 ± 00.41 |     295.49 ± 38.07 |
|    TruffleRuby JVM |  15.89 ± 00.52 | 883.28 ± 58.63 |     486.31 ± 11.03 |
|            Haskell |  16.10 ± 00.68 |   5.34 ± 00.08 |     351.80 ± 34.26 |
| TruffleRuby Native |  17.47 ± 00.77 | 598.38 ± 14.73 |     400.83 ± 27.16 |
|                Lua |  57.69 ± 02.13 |   2.60 ± 00.04 |    1132.52 ± 96.50 |
|           Ruby JIT |  60.77 ± 02.71 |  14.19 ± 00.02 |   1077.52 ± 330.20 |
|               Ruby |  83.79 ± 03.32 |  13.94 ± 00.08 |   1671.38 ± 185.56 |
|              JRuby | 108.85 ± 03.98 | 436.88 ± 22.06 |   2111.08 ± 102.63 |
|             Elixir | 115.81 ± 04.24 |  53.78 ± 00.77 |   2359.25 ± 196.03 |
|             Python | 229.53 ± 09.04 |   9.71 ± 00.08 |   4809.67 ± 429.13 |
|           Tcl (FP) | 270.07 ± 09.24 |   4.23 ± 00.09 |   5480.68 ± 470.17 |
|               Perl | 344.48 ± 18.54 |   6.39 ± 00.07 |   7096.08 ± 460.11 |
|           Tcl (OO) | 541.01 ± 16.86 |   4.25 ± 00.06 | 10817.92 ± 1036.36 |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|           Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------------- | -------------: | -------------: | ---------------: |
|              C GCC |  12.58 ± 00.55 |   1.60 ± 00.02 |   246.11 ± 23.62 |
|           Vala GCC |  12.79 ± 00.61 |   5.64 ± 00.07 |   261.75 ± 25.31 |
|              V GCC |  14.31 ± 00.59 |   2.41 ± 00.08 |   292.81 ± 24.00 |
|                GDC |  14.60 ± 00.66 |   7.15 ± 00.07 |   263.84 ± 24.75 |
|                LDC |  15.42 ± 00.55 |   3.78 ± 00.09 |   286.79 ± 21.82 |
|         Vala Clang |  15.87 ± 00.78 |   5.76 ± 00.08 |   306.51 ± 31.64 |
|            Crystal |  16.08 ± 00.60 |   3.76 ± 00.04 |   295.03 ± 22.95 |
|            C Clang |  17.52 ± 00.78 |   1.58 ± 00.04 |   370.09 ± 34.15 |
|                C++ |  17.73 ± 00.62 |   3.68 ± 00.06 |   333.45 ± 28.89 |
|       C# .NET Core |  17.83 ± 00.74 |  35.71 ± 00.15 |   332.17 ± 34.57 |
|               Rust |  18.04 ± 00.70 |   2.32 ± 00.11 |   362.78 ± 31.17 |
|            Nim GCC |  18.34 ± 00.63 |   2.39 ± 00.07 |   353.44 ± 27.76 |
|            V Clang |  18.53 ± 00.81 |   2.94 ± 00.19 |   359.60 ± 35.70 |
|          Nim Clang |  20.29 ± 00.85 |   2.87 ± 00.05 |   383.41 ± 34.47 |
|             Kotlin |  21.23 ± 00.85 |  46.24 ± 00.26 |   402.52 ± 38.88 |
|               Java |  21.55 ± 01.31 |  45.19 ± 00.38 |   447.03 ± 62.30 |
|              MLton |  22.25 ± 00.82 |   3.75 ± 00.04 |   445.50 ± 36.08 |
|              Scala |  24.75 ± 00.62 | 120.67 ± 00.98 |   527.55 ± 30.95 |
|             GCC Go |  26.12 ± 01.24 |  26.05 ± 05.95 |   503.29 ± 53.13 |
|              OCaml |  32.08 ± 01.15 |  10.69 ± 01.36 |   665.38 ± 49.47 |
|                 Go |  40.15 ± 01.66 |   4.76 ± 00.25 |   785.16 ± 65.45 |
|        Chez Scheme |  40.60 ± 01.61 |  29.35 ± 00.15 |   872.80 ± 78.33 |
|                DMD |  44.80 ± 01.60 |   4.38 ± 00.05 |   860.25 ± 76.41 |
|            C# Mono |  47.32 ± 01.86 |  21.03 ± 00.09 |   950.04 ± 88.75 |
|              Julia |  60.12 ± 01.88 | 176.15 ± 00.85 |  1135.64 ± 82.61 |
|            Node.js |  60.21 ± 02.37 |  36.38 ± 00.25 | 1208.97 ± 113.99 |
|     Haskell MArray |  62.83 ± 01.62 |   6.53 ± 00.07 | 1318.71 ± 281.21 |
|               PyPy |  67.02 ± 02.01 | 109.52 ± 00.07 | 1504.60 ± 106.03 |
|             LuaJIT |  67.12 ± 02.11 |   3.63 ± 00.05 | 1275.35 ± 102.17 |
|       F# .NET Core | 125.26 ± 03.32 | 128.17 ± 00.20 |  2507.18 ± 70.02 |
|    TruffleRuby JVM | 130.09 ± 07.33 | 902.10 ± 41.63 | 2566.12 ± 224.86 |
|             Racket | 133.09 ± 04.47 | 106.65 ± 00.65 | 2669.83 ± 190.92 |
| TruffleRuby Native | 163.31 ± 06.71 | 611.69 ± 10.07 | 3249.85 ± 249.63 |
|            Haskell | 223.72 ± 07.99 |   6.61 ± 00.07 | 4674.47 ± 405.46 |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |     Memory, MiB |      Energy, J |
| :---------------------- | ------------: | --------------: | -------------: |
|                C aklomp |  0.16 ± 00.01 |    1.98 ± 00.04 |   3.68 ± 00.25 |
|                    Rust |  1.33 ± 00.07 |    2.59 ± 00.05 |  25.57 ± 02.67 |
|                       C |  1.47 ± 00.08 |    1.93 ± 00.04 |  25.85 ± 03.13 |
|               Nim Clang |  1.65 ± 00.10 |    7.91 ± 00.08 |  32.95 ± 04.21 |
|                 Nim GCC |  1.66 ± 00.08 |    7.47 ± 00.04 |  32.99 ± 03.66 |
|                 V Clang |  2.02 ± 00.12 |    2.42 ± 00.04 |  40.71 ± 04.43 |
|                   V GCC |  2.13 ± 00.11 |    1.98 ± 00.02 |  40.27 ± 04.43 |
|                 Crystal |  2.13 ± 00.02 |    5.20 ± 00.07 |  52.52 ± 02.33 |
|                     LDC |  2.17 ± 00.05 |   10.96 ± 00.05 |  46.26 ± 01.66 |
|                    Ruby |  2.17 ± 00.11 |   73.34 ± 00.13 |  43.77 ± 05.15 |
|                Ruby JIT |  2.20 ± 00.14 |   73.49 ± 00.13 |  43.22 ± 06.30 |
|                     GDC |  2.29 ± 00.13 |   10.72 ± 00.03 |  45.55 ± 05.16 |
|                    Java |  2.49 ± 00.08 |  354.41 ± 35.05 |  54.34 ± 05.76 |
|                  Kotlin |  2.62 ± 00.10 |  351.69 ± 23.74 |  54.55 ± 04.73 |
|                   Scala |  2.67 ± 00.10 |  155.88 ± 08.04 |  57.99 ± 05.18 |
|       Perl MIME::Base64 |  2.68 ± 00.16 |    7.26 ± 00.07 |  50.43 ± 06.67 |
|                 Node.js |  2.91 ± 00.13 | 1059.21 ± 02.18 |  59.02 ± 05.80 |
|                      Go |  2.93 ± 00.00 |    9.16 ± 00.53 |  61.68 ± 02.32 |
|           C++ libcrypto |  3.12 ± 00.19 |    5.52 ± 00.03 |  63.45 ± 07.86 |
|                     PHP |  3.15 ± 00.17 |   15.64 ± 00.06 |  64.64 ± 07.72 |
|                  GCC Go |  3.68 ± 00.02 |   29.83 ± 01.18 |  78.09 ± 02.81 |
|                     DMD |  4.01 ± 00.15 |   11.56 ± 00.06 |  79.26 ± 06.01 |
|                     Tcl |  4.28 ± 00.23 |    5.05 ± 00.03 |  78.37 ± 09.61 |
|                    PyPy |  4.83 ± 00.20 |  109.72 ± 00.39 |  88.87 ± 08.93 |
|            C# .NET Core |  5.25 ± 00.16 |   72.23 ± 02.87 | 103.33 ± 05.43 |
|                  Python |  5.38 ± 00.28 |    9.82 ± 00.10 | 104.01 ± 12.29 |
|                   Julia |  5.95 ± 00.24 |  258.94 ± 09.04 | 124.44 ± 14.78 |
|         TruffleRuby JVM |  6.31 ± 00.20 |  743.00 ± 92.15 | 121.49 ± 09.61 |
|                 C# Mono |  6.85 ± 00.32 |   39.47 ± 00.05 | 135.66 ± 15.68 |
|                   JRuby | 10.49 ± 00.71 |  368.64 ± 36.40 | 204.51 ± 15.30 |
| Perl MIME::Base64::Perl | 16.17 ± 00.71 |    8.96 ± 00.09 | 328.73 ± 28.29 |
|      TruffleRuby Native | 23.07 ± 00.90 |  500.61 ± 00.11 | 475.35 ± 46.47 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |      Memory, MiB |       Energy, J |
| :-------------------- | ------------: | ---------------: | --------------: |
|     C++ DAW JSON Link |  0.09 ± 00.00 |   109.33 ± 00.05 |    1.86 ± 00.24 |
|              GDC fast |  0.10 ± 00.00 |   231.36 ± 00.32 |    2.41 ± 00.24 |
|     Rust Serde Custom |  0.13 ± 00.01 |   108.48 ± 00.12 |    2.73 ± 00.25 |
|      Rust Serde Typed |  0.15 ± 00.01 |   120.11 ± 00.32 |    3.01 ± 00.32 |
|          C++ simdjson |  0.16 ± 00.01 |   286.25 ± 00.82 |    3.72 ± 00.55 |
|             C++ gason |  0.17 ± 00.01 |   206.42 ± 00.09 |    3.60 ± 00.37 |
|         C++ RapidJSON |  0.23 ± 00.01 |   238.15 ± 00.08 |    4.89 ± 00.58 |
|                  Java |  0.51 ± 00.01 |   337.24 ± 01.15 |   13.57 ± 00.64 |
|     C++ RapidJSON SAX |  0.56 ± 00.03 |   109.49 ± 00.05 |   10.62 ± 01.16 |
|                 Scala |  0.57 ± 00.02 |   402.22 ± 01.34 |   14.48 ± 01.12 |
|               Node.js |  0.67 ± 00.03 |   429.89 ± 00.91 |   15.70 ± 01.25 |
|           Go jsoniter |  0.71 ± 00.03 |   226.16 ± 00.24 |   14.96 ± 01.21 |
|          Crystal Pull |  0.78 ± 00.06 |   128.51 ± 00.05 |   13.88 ± 01.95 |
|                  PyPy |  0.80 ± 00.03 |   404.73 ± 00.15 |   18.71 ± 01.94 |
|        Crystal Schema |  0.81 ± 00.05 |   157.26 ± 00.13 |   14.94 ± 01.90 |
|           Julia JSON3 |  0.83 ± 00.02 |   633.71 ± 03.12 |   19.93 ± 00.99 |
|    Rust Serde Untyped |  0.87 ± 00.03 |   948.48 ± 00.06 |   19.05 ± 01.50 |
| Perl Cpanel::JSON::XS |  0.96 ± 00.03 |   524.39 ± 00.08 |   21.13 ± 01.87 |
|               Crystal |  1.09 ± 00.07 |   503.65 ± 00.05 |   22.28 ± 02.56 |
|                 V GCC |  1.14 ± 00.07 |   590.72 ± 00.61 |   23.86 ± 02.92 |
|                    Go |  1.15 ± 00.06 |   209.53 ± 00.30 |   23.88 ± 02.83 |
|               V Clang |  1.20 ± 00.06 |   591.03 ± 00.70 |   22.73 ± 02.89 |
|                   PHP |  1.23 ± 00.06 |   803.61 ± 00.10 |   24.30 ± 02.31 |
|                GCC Go |  1.42 ± 00.08 |   232.08 ± 05.84 |   29.91 ± 03.58 |
|    Nim Packedjson GCC |  1.51 ± 00.07 |   399.35 ± 00.02 |   30.52 ± 04.42 |
|  Nim Packedjson Clang |  1.55 ± 00.12 |   399.83 ± 00.06 |   30.50 ± 02.76 |
|               Clojure |  1.58 ± 00.08 |   974.60 ± 34.94 |   39.95 ± 02.90 |
|            C++ json-c |  1.62 ± 00.06 |  1649.58 ± 00.06 |   38.66 ± 02.75 |
|     CPython UltraJSON |  1.64 ± 00.04 |   661.58 ± 02.49 |   35.39 ± 01.44 |
|                Python |  1.74 ± 00.04 |   493.52 ± 00.07 |   38.23 ± 02.02 |
|               Haskell |  1.78 ± 00.10 |     9.72 ± 00.05 |   36.43 ± 04.49 |
|               Nim GCC |  1.86 ± 00.08 |   904.85 ± 00.13 |   36.62 ± 03.61 |
|             Nim Clang |  1.88 ± 00.08 |   905.30 ± 00.11 |   37.72 ± 03.88 |
|          C# .NET Core |  2.08 ± 00.11 |   761.07 ± 00.10 |   39.73 ± 05.21 |
|               C# Mono |  2.17 ± 00.13 |   462.42 ± 00.13 |   43.94 ± 05.05 |
|              Ruby JIT |  2.21 ± 00.09 |   397.03 ± 00.05 |   51.96 ± 04.49 |
|                  Ruby |  2.28 ± 00.07 |   396.82 ± 00.06 |   44.05 ± 03.74 |
|             Ruby YAJL |  2.28 ± 00.09 |   406.23 ± 00.11 |   46.43 ± 05.99 |
|                   GDC |  2.29 ± 00.16 |   713.58 ± 00.05 |   43.39 ± 05.10 |
|                   LDC |  2.51 ± 00.06 |   789.57 ± 00.06 |   52.35 ± 02.38 |
|               Rust jq |  3.76 ± 00.21 |   886.16 ± 00.70 |   71.57 ± 07.46 |
|                 JRuby |  3.86 ± 00.12 |  1955.12 ± 41.38 |  115.41 ± 07.62 |
|             C++ Boost |  3.95 ± 00.18 |  1549.75 ± 00.07 |   89.29 ± 09.82 |
|                   DMD |  4.89 ± 00.14 |   790.32 ± 00.09 |   97.55 ± 05.20 |
|   C# System.Text.Json |  6.83 ± 00.39 |   646.99 ± 00.15 |  135.90 ± 18.07 |
|       Perl JSON::Tiny | 11.90 ± 00.50 |   647.40 ± 00.11 |  243.43 ± 27.21 |
|       TruffleRuby JVM | 19.30 ± 00.62 |  2038.99 ± 69.88 |  546.16 ± 13.79 |
|    TruffleRuby Native | 53.27 ± 01.61 | 2820.76 ± 144.45 | 1080.59 ± 92.07 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |     Memory, MiB |         Energy, J |
| :-------------------- | -------------: | --------------: | ----------------: |
| Nim Clang Arraymancer |   0.14 ± 00.01 |   56.22 ± 00.09 |      7.86 ± 00.26 |
|            LDC lubeck |   0.14 ± 00.00 |   61.40 ± 00.15 |      8.03 ± 00.20 |
|   Nim GCC Arraymancer |   0.15 ± 00.00 |   56.27 ± 00.14 |      8.67 ± 00.18 |
|             Java ND4J |   0.19 ± 00.02 |  239.27 ± 02.95 |      9.30 ± 01.26 |
|          Python NumPy |   0.21 ± 00.01 |   79.96 ± 00.06 |     10.89 ± 00.89 |
|    Julia (threads: 8) |   0.43 ± 00.02 |  290.68 ± 00.31 |     18.90 ± 00.64 |
|    Julia (threads: 1) |   0.72 ± 00.03 |  290.65 ± 00.28 |     15.28 ± 01.61 |
|                   LDC |   1.97 ± 00.01 |   73.64 ± 00.05 |     43.62 ± 01.36 |
|                   DMD |   2.13 ± 00.03 |   74.19 ± 00.16 |     46.74 ± 01.72 |
|                   GDC |   2.13 ± 00.05 |   77.40 ± 00.08 |     46.84 ± 02.98 |
|                  Java |   3.31 ± 00.03 |  124.31 ± 00.26 |     76.87 ± 02.10 |
|                     C |   3.33 ± 00.02 |   70.21 ± 00.07 |     68.73 ± 02.46 |
|                  Rust |   3.37 ± 00.02 |   70.99 ± 00.09 |     69.07 ± 02.88 |
|               Nim GCC |   3.42 ± 00.04 |   77.49 ± 07.19 |     69.68 ± 04.32 |
|                 Scala |   3.42 ± 00.05 |  167.17 ± 06.40 |     79.18 ± 03.54 |
|             Nim Clang |   3.44 ± 00.03 |   81.10 ± 05.61 |     70.90 ± 04.19 |
|                GCC Go |   3.58 ± 00.05 |  102.85 ± 05.74 |     75.68 ± 02.42 |
|                    Go |   3.59 ± 00.08 |   76.85 ± 00.25 |     74.21 ± 03.61 |
|       Julia (no BLAS) |   3.66 ± 00.08 |  252.30 ± 00.49 |     76.44 ± 02.54 |
|               Crystal |   3.68 ± 00.08 |   63.79 ± 00.06 |     77.36 ± 03.24 |
|                 Swift |   3.69 ± 00.10 |  207.16 ± 06.38 |     75.69 ± 04.82 |
|                 V GCC |   3.70 ± 00.15 |   70.87 ± 00.07 |     76.38 ± 08.59 |
|               V Clang |   3.83 ± 00.15 |   71.31 ± 00.05 |     80.89 ± 10.38 |
|                Kotlin |   3.83 ± 00.31 |  123.00 ± 00.46 |     80.38 ± 08.57 |
|               Node.js |   5.45 ± 00.24 |  104.58 ± 00.64 |    111.33 ± 13.69 |
|                  PyPy |   6.31 ± 00.28 |  132.91 ± 00.12 |    124.90 ± 13.76 |
|          C# .NET Core |   7.06 ± 00.30 |  102.39 ± 00.17 |    142.13 ± 14.23 |
|               C# Mono |  11.29 ± 00.51 |   89.07 ± 00.05 |    208.51 ± 19.70 |
|    TruffleRuby Native |  28.84 ± 00.62 |  719.77 ± 20.73 |    646.51 ± 50.78 |
|       TruffleRuby JVM |  40.95 ± 00.62 |  989.06 ± 45.47 |    947.37 ± 40.99 |
|              Ruby JIT | 221.07 ± 07.97 |   84.19 ± 00.07 |  3957.84 ± 440.43 |
|                  Ruby | 227.27 ± 08.71 |   83.89 ± 00.06 |  4474.48 ± 444.37 |
|                Python | 251.18 ± 10.09 |   78.63 ± 00.06 |  5055.99 ± 504.82 |
|                   Tcl | 353.54 ± 10.47 |  407.63 ± 00.06 |  6974.97 ± 474.03 |
|                  Perl | 394.69 ± 08.78 |  608.48 ± 00.12 | 7980.59 ± 2772.79 |
|                 JRuby | 478.94 ± 18.93 | 982.83 ± 167.84 | 10523.90 ± 432.37 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |      Memory, MiB |        Energy, J |
| :----------- | -------------: | ---------------: | ---------------: |
|      Crystal |   6.90 ± 00.35 |   212.29 ± 01.24 |   157.84 ± 20.27 |
|      Nim GCC |  11.72 ± 00.41 |   481.89 ± 10.62 |   259.41 ± 28.82 |
|    Nim Clang |  12.21 ± 00.39 |   479.58 ± 02.70 |   252.95 ± 28.43 |
|          C++ |  14.20 ± 00.49 |   178.33 ± 00.07 |   286.27 ± 29.40 |
| C# .NET Core |  15.05 ± 00.95 | 1395.71 ± 134.02 |   341.69 ± 58.46 |
|           Go |  18.72 ± 00.11 |   353.66 ± 09.63 |   435.16 ± 06.41 |
|          LDC |  18.81 ± 00.29 |   469.03 ± 18.56 |   488.31 ± 22.18 |
|        Scala |  19.15 ± 01.31 |  762.87 ± 277.05 |   544.31 ± 57.54 |
|          GDC |  22.47 ± 00.67 |   351.47 ± 00.06 |   459.13 ± 33.01 |
|          DMD |  23.45 ± 00.26 |   470.75 ± 10.78 |   563.73 ± 14.59 |
|       GCC Go |  26.40 ± 00.59 |   386.30 ± 18.31 |   648.70 ± 13.90 |
|      C# Mono |  28.22 ± 02.32 |   338.05 ± 10.17 |   458.32 ± 84.58 |
|         PyPy |  29.25 ± 00.90 |   649.62 ± 60.58 |   627.23 ± 49.19 |
|       Python | 104.78 ± 02.32 |   409.38 ± 00.05 | 2306.60 ± 146.07 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.1.106                         |
| C# .NET Core | 3.4.1-beta4-20127-10 (d8180a5e) |
| C# Mono      | 6.10.0.104                      |
| Chez Scheme  | 9.5                             |
| Clang        | 10.0.1                          |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.35.1                          |
| DMD          | v2.093.0                        |
| Elixir       | 1.10.3                          |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 10.1.0                          |
| GCC Go       | 10.1.0                          |
| GDC          | 10.1.0                          |
| Go           | go1.14.6                        |
| Haskell      | 8.10.1                          |
| JRuby        | 9.2.12.0                        |
| Java         | 14.0.2                          |
| Julia        | v"1.4.2"                        |
| Kotlin       | 1.3.72                          |
| LDC          | 1.22.0                          |
| Lua          | Lua 5.4                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.2.4                           |
| Node.js      | v14.5.0                         |
| OCaml        | 4.10.0                          |
| PHP          | 7.4.5                           |
| Perl         | v5.30.3                         |
| PyPy         | 7.3.1-final0 for Python 3.6.9   |
| Python       | 3.8.4                           |
| Racket       | "7.7"                           |
| Ruby         | 2.7.1p83                        |
| Rust         | 1.45.0                          |
| Scala        | 2.13.3                          |
| Swift        | swift-5.2.4-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 20.1.0                          |
| V            | 0.1.28                          |
| Vala         | 0.48.7                          |

## Using Docker

Build the image:

    $ docker build docker/ -t benchmarks

Run the image:

    $ docker run -it --rm -v $(pwd):/src benchmarks <cmd>

where <cmd> is:

 - `versions` (print installed language versions);
 - `shell` (start the shell);
 - `brainfuck bench` (build and run Brainfuck bench.b benchmarks);
 - `brainfuck mandel` (build and run Brainfuck mandel.b benchmarks);
 - `base64` (build and run Base64 benchmarks);
 - `json` (build and run Json benchmarks);
 - `matmul` (build and run Matmul benchmarks);
 - `havlak` (build and run Havlak benchmarks).

Please note that the actual measurements provided in the project are taken semi-manually (via `shell`) as the full update takes days and could have occassional issues in Docker.

There is a `Makefile` that could be used to simlify Docker usage:

 - `make build` (build the image);
 - `make versions` (run the image with the `versions` command);
 - `make shell` (run the image with the `shell' command);
 - `make toc` (utility rule to update ToC in this README, requires
[git-markdown-toc](https://github.com/ildar-shaimordanov/git-markdown-toc)
available in `PATH`).

Please note that the `make shell` rule requires `cpupower` utility installed
that is invoked with `sudo` to set cpufreq's performance governon
(it runs the CPU at the maximum frequence to eliminate throttling issues).

## Manual Execution

Makefiles contain recipes for building and executing tests with the
proper dependencies. Please use `make run` (and `make run2` where applicable).
The measurements are taken using `analyze.rb` script:

    $ cd <test suite>
    $ ../analyze.rb make run
    $ ../analyze.rb make run[<single test>]

Please note that the measurements could take hours. It uses 10 iterations
by default, but it could be changed using ATTEMPTS environment variable:

    $ ATTEMPTS=1 ../analyze.rb make run

### Prerequisites

Please use [Dockerfile](docker/Dockerfile) as a reference regarding which
packages and tools are required.

For all (optional):

 - [Powercap](https://github.com/powercap/powercap) for reading energy
counters in Linux (Debian package `powercap-utils`).

For Python:

 - [NumPy](https://numpy.org/) for matmul tests
(Debian package `python3-numpy`).
 - [UltraJSON](https://pypi.org/project/ujson/) for JSON tests
(Debian package `python3-ujson`).


For C++:

 - [Boost](https://www.boost.org/) for JSON tests
(Debian package `libboost-dev`).
 - [JSON-C](https://github.com/json-c/json-c) for JSON tests
(Debian package `libjson-c-dev`).

For Rust:

 - [libjq](https://stedolan.github.io/jq/) for jq test
(Debian packages `libjq-dev`, `libonig-dev` and environment variable
`JQ_LIB_DIR=/usr/lib/x86_64-linux-gnu/`).

For Java, Scala:

 - [Coursier](https://get-coursier.io/) for downloading Maven artifacts.

For Lua:

 - [LuaRocks](https://luarocks.org/) for installing dependencies
(Debian package `luarocks`).

For Haskell:

 - [network-simple](http://hackage.haskell.org/package/network-simple) for
TCP connectivity between the tests and the test runner.

For Perl:

 - [cpanminus](https://metacpan.org/pod/App::cpanminus) for installing
modules from CPAN (Debian package `cpanminus`).

# Contribution

Please follow the criteria specified in the [overview](#overview). Besides
that please ensure that the communication protocol between a test and the
test runner is satisfied:

 - The test runner listens on localhost:9001;
 - All messages are sent using TCP sockets closed immediately after the
message has been sent;
 - There are two messages sent from a test (it establishes the measurement
boundary):
    1. The beginning message having the format *name of the test*/t*process ID*
(the process ID is used to measure the memory consumption). Please note that
the name of the test couldn't use Tab character as it's a delimiter;
    2. The end message with any content (mostly it's "stop" for consistency).
 - The test runner could be unavailable (if the test is launched as is) and
the test should gracefully handle it.

## Makefile guide

### Binary executables

If the test is compiled into a single binary, then two sections of
the `Makefile` require changes:

 - append a new target (the final binary location) into `executables`
variable;
 - append the proper target rule.

### Compiled artifacts

If the test is compiled, but can't be executed directly as a binary, then
three sections of the `Makefile` require changes:

 - append a new target (the final artifact location) into `artifacts`
variable;
 - append the proper target rule to compile the test;
 - append `run[<target_artifact>]` rule to run the test.

### Scripting language

If the test doesn't require compilation, then two sections of the `Makefile`
requires changes:

 - append `run[<script_file>]` into `all_runners` variable;
 - append `run[<script_file>]` rule to run the test.
