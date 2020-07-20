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

UPDATE: 2020-06-14

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck](brainfuck)

### bench.b

|           Language |        Time, s |    Memory, MiB |         Energy, J |
| :----------------- | -------------: | -------------: | ----------------: |
|                C++ |   1.00 ± 00.04 |   1.68 ± 00.08 |     18.70 ± 01.26 |
|              OCaml |   1.68 ± 00.08 |   5.20 ± 00.07 |     35.92 ± 03.63 |
|            Nim GCC |   1.72 ± 00.07 |   1.90 ± 00.06 |     35.69 ± 03.26 |
|           Vala GCC |   1.72 ± 00.07 |   3.90 ± 00.37 |     35.88 ± 02.55 |
|              C GCC |   1.81 ± 00.08 |   0.72 ± 00.03 |     36.39 ± 03.47 |
|                LDC |   1.81 ± 00.09 |   2.96 ± 00.06 |     37.95 ± 03.87 |
|          Nim Clang |   1.85 ± 00.08 |   2.31 ± 00.08 |     37.07 ± 03.56 |
|             Kotlin |   1.87 ± 00.07 |  40.52 ± 00.28 |     37.01 ± 02.66 |
|         Vala Clang |   1.93 ± 00.13 |   4.75 ± 00.33 |     43.58 ± 04.16 |
|                GDC |   2.02 ± 00.09 |   6.20 ± 00.05 |     39.25 ± 04.93 |
|               Rust |   2.04 ± 00.15 |   2.05 ± 00.09 |     40.83 ± 03.80 |
|            C Clang |   2.15 ± 00.08 |   0.73 ± 00.05 |     45.27 ± 04.11 |
|             GCC Go |   2.20 ± 00.09 |  23.29 ± 05.25 |     41.56 ± 04.79 |
|               Java |   2.29 ± 00.09 |  38.64 ± 00.23 |     47.83 ± 04.92 |
|       C# .NET Core |   2.39 ± 00.11 |  34.54 ± 00.11 |     46.13 ± 04.60 |
|                 Go |   2.44 ± 00.12 |   3.05 ± 00.27 |     50.16 ± 04.85 |
|              V GCC |   2.49 ± 00.10 |   0.73 ± 00.05 |     49.25 ± 04.45 |
|              MLton |   2.50 ± 00.12 |   0.71 ± 00.02 |     49.58 ± 06.10 |
|            Crystal |   2.58 ± 00.26 |   3.33 ± 00.06 |     53.87 ± 04.39 |
|            V Clang |   2.67 ± 00.14 |   1.07 ± 00.05 |     59.10 ± 05.55 |
|       F# .NET Core |   2.73 ± 00.07 | 125.44 ± 00.47 |     53.80 ± 02.91 |
|        Chez Scheme |   2.81 ± 00.14 |  29.35 ± 00.09 |     57.87 ± 06.99 |
|              Julia |   3.13 ± 00.17 | 176.85 ± 00.91 |     68.64 ± 05.50 |
|                DMD |   3.68 ± 00.16 |   3.54 ± 00.08 |     66.35 ± 05.51 |
|              Scala |   3.92 ± 00.31 | 138.72 ± 08.56 |     81.94 ± 05.46 |
|            C# Mono |   4.38 ± 00.20 |  20.24 ± 00.08 |     89.99 ± 09.42 |
|     Haskell MArray |   4.50 ± 00.18 |   5.30 ± 00.07 |     96.88 ± 09.16 |
|            Node.js |   4.81 ± 00.22 |  33.60 ± 00.42 |     96.31 ± 09.31 |
|             LuaJIT |   7.24 ± 00.37 |   2.78 ± 00.08 |    139.36 ± 14.64 |
|             Racket |   7.82 ± 00.29 | 106.43 ± 00.11 |    148.36 ± 16.49 |
|               PyPy |  12.55 ± 00.44 | 108.25 ± 00.08 |    301.34 ± 22.77 |
|    TruffleRuby JVM |  16.00 ± 00.34 | 894.47 ± 56.00 |    482.30 ± 18.38 |
|            Haskell |  16.43 ± 00.72 |   5.35 ± 00.08 |    331.84 ± 32.04 |
| TruffleRuby Native |  18.02 ± 00.43 | 591.80 ± 23.07 |    396.52 ± 21.27 |
|           Ruby JIT |  59.91 ± 02.28 |  14.22 ± 00.04 |  1143.46 ± 109.74 |
|                Lua |  82.75 ± 02.30 |   2.88 ± 00.03 |  1718.58 ± 145.96 |
|               Ruby |  85.30 ± 02.47 |  13.96 ± 00.06 |  1654.54 ± 136.02 |
|              JRuby | 109.36 ± 05.18 | 435.30 ± 07.28 |  2088.42 ± 149.36 |
|             Elixir | 115.51 ± 04.25 |  53.65 ± 01.30 |  2358.72 ± 165.42 |
|             Python | 228.62 ± 07.86 |   9.87 ± 00.07 |  4645.29 ± 377.13 |
|           Tcl (FP) | 267.67 ± 10.55 |   4.32 ± 00.04 |  5600.98 ± 351.48 |
|               Perl | 344.09 ± 08.35 |   6.45 ± 00.08 | 6295.55 ± 1894.38 |
|           Tcl (OO) | 535.07 ± 16.86 |   4.32 ± 00.07 | 10918.49 ± 748.96 |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|           Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------------- | -------------: | -------------: | ---------------: |
|              C GCC |  12.74 ± 00.45 |   1.69 ± 00.02 |   239.12 ± 20.54 |
|           Vala GCC |  12.76 ± 00.42 |   5.50 ± 00.10 |   265.76 ± 21.94 |
|                GDC |  14.09 ± 00.53 |   7.17 ± 00.07 |   283.20 ± 24.54 |
|              V GCC |  14.24 ± 00.65 |   2.48 ± 00.05 |   294.88 ± 26.70 |
|                LDC |  15.00 ± 00.68 |   3.84 ± 00.06 |   309.28 ± 30.43 |
|         Vala Clang |  16.21 ± 00.40 |   5.48 ± 00.06 |   287.47 ± 19.31 |
|            Crystal |  17.20 ± 01.42 |   3.72 ± 00.06 |   328.24 ± 30.52 |
|                C++ |  17.64 ± 00.64 |   3.76 ± 00.05 |   341.65 ± 29.02 |
|            C Clang |  17.74 ± 00.84 |   1.69 ± 00.03 |   355.24 ± 39.07 |
|               Rust |  17.88 ± 00.67 |   2.33 ± 00.07 |   370.02 ± 33.47 |
|       C# .NET Core |  17.95 ± 00.91 |  35.71 ± 00.17 |   334.59 ± 27.70 |
|            Nim GCC |  18.17 ± 00.68 |   2.39 ± 00.05 |   358.56 ± 32.25 |
|            V Clang |  18.90 ± 00.85 |   2.97 ± 00.09 |   343.88 ± 36.41 |
|          Nim Clang |  20.21 ± 00.88 |   2.84 ± 00.04 |   383.68 ± 41.31 |
|             Kotlin |  20.99 ± 01.70 |  46.51 ± 00.50 |   427.62 ± 39.49 |
|               Java |  21.88 ± 01.53 |  45.53 ± 00.60 |   435.04 ± 35.56 |
|              MLton |  22.48 ± 00.92 |   3.89 ± 00.05 |   432.34 ± 42.45 |
|              Scala |  24.61 ± 00.51 | 120.83 ± 05.20 |   530.15 ± 25.41 |
|             GCC Go |  26.44 ± 01.11 |  29.85 ± 06.36 |   484.80 ± 50.08 |
|              OCaml |  32.57 ± 01.25 |  14.21 ± 02.15 |   635.87 ± 61.25 |
|                 Go |  39.55 ± 01.64 |   4.83 ± 00.25 |   801.88 ± 75.11 |
|        Chez Scheme |  40.43 ± 01.54 |  29.41 ± 00.13 |  800.38 ± 280.28 |
|                DMD |  44.00 ± 01.29 |   4.43 ± 00.05 |   893.39 ± 64.62 |
|            C# Mono |  48.49 ± 01.75 |  20.99 ± 00.08 |   901.78 ± 82.37 |
|            Node.js |  60.88 ± 01.87 |  36.43 ± 00.22 |  1173.50 ± 84.55 |
|              Julia |  61.11 ± 03.25 | 175.92 ± 00.86 |  1123.34 ± 86.81 |
|     Haskell MArray |  64.74 ± 01.71 |   6.53 ± 00.04 |  1313.50 ± 84.60 |
|             LuaJIT |  65.00 ± 02.02 |   3.64 ± 00.07 | 1363.46 ± 112.27 |
|               PyPy |  68.56 ± 03.00 | 109.60 ± 00.28 |  1431.78 ± 76.26 |
|       F# .NET Core | 125.94 ± 02.96 | 128.16 ± 00.17 | 2456.31 ± 100.45 |
|    TruffleRuby JVM | 131.79 ± 06.38 | 899.65 ± 49.45 | 2612.15 ± 145.06 |
|             Racket | 135.15 ± 04.84 | 106.43 ± 00.09 | 2550.82 ± 232.20 |
| TruffleRuby Native | 159.74 ± 05.72 | 606.06 ± 13.20 | 3288.64 ± 213.43 |
|            Haskell | 221.94 ± 07.65 |   6.59 ± 00.07 | 4783.84 ± 412.11 |

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
| .NET Core    | 3.1.105                         |
| C# .NET Core | 3.4.1-beta4-20127-10 (d8180a5e) |
| C# Mono      | 6.10.0.104                      |
| Chez Scheme  | 9.5                             |
| Clang        | 10.0.1                          |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.35.0                          |
| DMD          | v2.093.0                        |
| Elixir       | 1.10.3                          |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 10.1.0                          |
| GCC Go       | 10.1.0                          |
| GDC          | 10.1.0                          |
| Go           | go1.14.5                        |
| Haskell      | 8.10.1                          |
| JRuby        | 9.2.12.0                        |
| Java         | 14.0.2                          |
| Julia        | v"1.4.2"                        |
| Kotlin       | 1.3.72                          |
| LDC          | 1.22.0                          |
| Lua          | Lua 5.1                         |
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

 - [LuaSocket](http://w3.impa.br/~diego/software/luasocket/) for TCP
connectivity between the tests and the test runner
(Debian package `lua-socket`).
 - [luaposix](http://luaposix.github.io/luaposix/) for getting PID of the
tests (Debian package `lua-posix`).

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
