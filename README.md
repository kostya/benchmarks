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

UPDATE: 2020-08-28

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck](brainfuck)

### bench.b

|           Language |        Time, s |    Memory, MiB |         Energy, J |
| :----------------- | -------------: | -------------: | ----------------: |
|                C++ |   0.88 ± 00.02 |   1.51 ± 00.04 |     15.98 ± 00.42 |
|            Nim GCC |   1.83 ± 00.04 |   1.89 ± 00.07 |     33.53 ± 00.69 |
|           Vala GCC |   1.84 ± 00.01 |   3.60 ± 01.26 |     32.63 ± 00.77 |
|              OCaml |   1.85 ± 00.03 |   5.06 ± 00.03 |     37.39 ± 00.80 |
|              C GCC |   1.94 ± 00.05 |   0.53 ± 00.03 |     33.18 ± 01.06 |
|          Nim Clang |   1.94 ± 00.02 |   2.38 ± 00.06 |     35.54 ± 00.86 |
|                LDC |   1.94 ± 00.03 |   2.95 ± 00.04 |     35.44 ± 00.41 |
|             Kotlin |   1.96 ± 00.01 |  40.54 ± 00.32 |     35.65 ± 00.51 |
|            C Clang |   2.05 ± 00.02 |   0.51 ± 00.03 |     41.16 ± 01.39 |
|                GDC |   2.07 ± 00.03 |   6.29 ± 00.06 |     37.75 ± 00.69 |
|         Vala Clang |   2.11 ± 00.19 |   3.59 ± 01.26 |     38.82 ± 03.23 |
|               Rust |   2.18 ± 00.02 |   1.98 ± 00.09 |     39.89 ± 00.79 |
|             GCC Go |   2.26 ± 00.05 |  21.95 ± 03.91 |     42.04 ± 00.93 |
|               Java |   2.43 ± 00.04 |  38.65 ± 00.16 |     44.16 ± 00.91 |
|       C# .NET Core |   2.49 ± 00.02 |  34.59 ± 00.08 |     45.14 ± 00.96 |
|                 Go |   2.56 ± 00.01 |   3.05 ± 00.36 |     44.08 ± 01.05 |
|              MLton |   2.60 ± 00.03 |   0.56 ± 00.03 |     47.35 ± 00.84 |
|              V GCC |   2.63 ± 00.02 |   0.53 ± 00.03 |     46.11 ± 00.82 |
|            Crystal |   2.69 ± 00.04 |   3.36 ± 00.06 |     49.15 ± 00.67 |
|       F# .NET Core |   2.81 ± 00.05 | 125.07 ± 01.04 |     53.11 ± 01.71 |
|        Chez Scheme |   2.95 ± 00.04 |  29.27 ± 00.10 |     54.47 ± 00.89 |
|            V Clang |   3.00 ± 00.04 |   0.88 ± 00.04 |     54.79 ± 01.11 |
|              Julia |   3.30 ± 00.06 | 179.62 ± 00.67 |     60.99 ± 01.60 |
|                DMD |   3.82 ± 00.02 |   3.46 ± 00.09 |     65.93 ± 02.28 |
|              Scala |   3.96 ± 00.25 | 131.90 ± 08.08 |     84.01 ± 04.81 |
|     Haskell MArray |   4.58 ± 00.08 |   5.33 ± 00.05 |     92.84 ± 02.44 |
|            C# Mono |   4.66 ± 00.07 |  20.42 ± 00.08 |     84.29 ± 01.38 |
|            Node.js |   5.22 ± 00.06 |  33.50 ± 00.67 |     94.70 ± 03.30 |
|             LuaJIT |   7.53 ± 00.16 |   2.79 ± 00.06 |    136.00 ± 05.65 |
|             Racket |   7.99 ± 00.16 | 107.16 ± 00.59 |    147.94 ± 03.30 |
|               PyPy |  13.86 ± 00.09 | 108.41 ± 00.13 |    255.38 ± 07.51 |
|            Haskell |  16.60 ± 00.35 |   5.41 ± 00.07 |    327.81 ± 10.34 |
|    TruffleRuby JVM |  18.66 ± 00.69 | 926.18 ± 48.83 |    522.18 ± 17.10 |
| TruffleRuby Native |  28.63 ± 16.10 | 524.35 ± 15.64 |   562.97 ± 320.93 |
|                Lua |  60.82 ± 00.59 |   2.63 ± 00.03 |   1036.06 ± 26.33 |
|           Ruby JIT |  62.80 ± 00.84 |  14.23 ± 00.05 |   1157.44 ± 19.38 |
|               Ruby |  87.55 ± 03.35 |  13.99 ± 00.08 |   1586.41 ± 73.12 |
|              JRuby | 109.41 ± 06.20 | 434.85 ± 09.63 |  2052.60 ± 218.09 |
|             Elixir | 120.07 ± 01.08 |  53.81 ± 00.84 |   2216.11 ± 20.26 |
|             Python | 238.39 ± 02.95 |   9.36 ± 00.06 |   4357.88 ± 86.61 |
|           Tcl (FP) | 277.21 ± 02.69 |   4.30 ± 00.06 |   5370.28 ± 85.00 |
|               Perl | 361.72 ± 07.03 |   6.43 ± 00.10 |  6255.85 ± 317.04 |
|           Tcl (OO) | 552.64 ± 05.32 |   4.27 ± 00.06 | 10635.80 ± 127.41 |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|           Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------------- | -------------: | -------------: | ---------------: |
|                C++ |  13.53 ± 00.19 |   3.72 ± 00.06 |   226.11 ± 04.35 |
|              C GCC |  13.61 ± 00.14 |   1.61 ± 00.04 |   232.03 ± 03.29 |
|           Vala GCC |  14.29 ± 00.95 |   5.74 ± 00.06 |   297.69 ± 67.49 |
|                GDC |  14.82 ± 00.24 |   7.20 ± 00.05 |   265.60 ± 03.09 |
|              V GCC |  15.19 ± 00.14 |   2.45 ± 00.06 |   265.75 ± 04.96 |
|         Vala Clang |  16.03 ± 00.70 |   5.74 ± 00.09 |   320.07 ± 29.42 |
|                LDC |  16.21 ± 00.15 |   3.83 ± 00.06 |   287.54 ± 02.17 |
|            Crystal |  16.68 ± 00.46 |   3.78 ± 00.04 |   289.60 ± 08.86 |
|       C# .NET Core |  18.62 ± 00.24 |  35.84 ± 00.11 |   317.25 ± 04.43 |
|            C Clang |  18.77 ± 00.03 |   1.63 ± 00.03 |   327.17 ± 07.29 |
|            Nim GCC |  19.12 ± 00.08 |   2.43 ± 00.07 |   335.24 ± 05.71 |
|               Rust |  19.43 ± 00.19 |   2.37 ± 00.10 |   351.49 ± 04.64 |
|          Nim Clang |  20.93 ± 00.21 |   2.90 ± 00.05 |   370.88 ± 07.46 |
|             Kotlin |  22.11 ± 00.52 |  46.57 ± 00.21 |   385.33 ± 10.35 |
|            V Clang |  22.90 ± 00.62 |   2.94 ± 00.11 |   418.71 ± 09.14 |
|               Java |  23.13 ± 01.10 |  45.24 ± 00.43 |   408.94 ± 25.10 |
|              MLton |  24.51 ± 00.28 |   4.28 ± 00.35 |   444.16 ± 03.48 |
|              Scala |  25.83 ± 00.15 | 119.56 ± 06.60 |   511.08 ± 10.68 |
|             GCC Go |  27.31 ± 00.51 |  22.30 ± 00.17 |   485.26 ± 07.00 |
|              OCaml |  30.39 ± 00.97 |  10.10 ± 01.04 |   601.53 ± 31.40 |
|                 Go |  35.52 ± 00.32 |   4.15 ± 00.23 |   604.12 ± 07.73 |
|        Chez Scheme |  43.31 ± 00.43 |  29.40 ± 00.11 |   802.00 ± 11.67 |
|                DMD |  48.18 ± 00.08 |   4.38 ± 00.06 |   741.57 ± 18.28 |
|            C# Mono |  49.54 ± 00.43 |  21.27 ± 00.06 |  829.25 ± 199.73 |
|            Node.js |  59.01 ± 00.28 |  37.04 ± 00.32 |  1028.03 ± 15.02 |
|              Julia |  62.33 ± 01.23 | 180.54 ± 00.62 |  1170.54 ± 23.17 |
|     Haskell MArray |  66.00 ± 01.50 |   6.54 ± 00.06 |  1293.02 ± 40.39 |
|             LuaJIT |  70.51 ± 00.28 |   3.71 ± 00.08 |  1163.13 ± 23.67 |
|               PyPy |  72.34 ± 01.07 | 109.61 ± 00.09 |  1317.74 ± 27.72 |
|    TruffleRuby JVM | 117.29 ± 03.37 | 938.28 ± 51.22 |  2322.82 ± 88.00 |
|       F# .NET Core | 130.70 ± 04.16 | 128.23 ± 00.14 |  2439.07 ± 77.97 |
|             Racket | 139.07 ± 02.33 | 107.14 ± 00.62 |  2552.32 ± 41.02 |
| TruffleRuby Native | 210.02 ± 49.65 | 531.55 ± 31.39 | 3803.79 ± 926.04 |
|            Haskell | 222.17 ± 01.03 |   6.58 ± 00.05 |  4385.49 ± 34.12 |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |     Memory, MiB |      Energy, J |
| :---------------------- | ------------: | --------------: | -------------: |
|                C aklomp |  0.16 ± 00.00 |    1.98 ± 00.04 |   3.65 ± 00.08 |
|                 V Clang |  0.89 ± 00.01 |    2.48 ± 00.05 |  13.97 ± 00.30 |
|                       C |  1.33 ± 00.01 |    1.96 ± 00.04 |  20.90 ± 00.73 |
|                    Rust |  1.36 ± 00.03 |    2.58 ± 00.10 |  25.62 ± 00.83 |
|                   V GCC |  1.71 ± 00.04 |    1.91 ± 00.05 |  29.10 ± 00.93 |
|               Nim Clang |  1.73 ± 00.04 |    8.00 ± 00.06 |  31.18 ± 01.13 |
|                 Nim GCC |  1.76 ± 00.04 |    7.54 ± 00.04 |  30.40 ± 00.90 |
|                 Crystal |  2.04 ± 00.01 |    5.26 ± 00.07 |  51.09 ± 01.44 |
|                Ruby JIT |  2.15 ± 00.03 |   73.44 ± 00.20 |  40.58 ± 00.43 |
|                    Ruby |  2.16 ± 00.04 |   73.39 ± 00.17 |  38.70 ± 00.86 |
|                     LDC |  2.26 ± 00.05 |   11.03 ± 00.05 |  52.67 ± 01.95 |
|                     GDC |  2.39 ± 00.06 |   10.74 ± 00.04 |  42.60 ± 01.61 |
|                      Go |  2.53 ± 00.00 |    9.44 ± 00.59 |  49.28 ± 01.53 |
|                    Java |  2.64 ± 00.07 |  341.42 ± 15.24 |  49.14 ± 02.57 |
|                  Kotlin |  2.67 ± 00.04 |  335.49 ± 04.77 |  51.24 ± 01.09 |
|                   Scala |  2.73 ± 00.08 |  154.52 ± 05.22 |  54.49 ± 02.34 |
|       Perl MIME::Base64 |  2.77 ± 00.09 |    7.29 ± 00.08 |  49.50 ± 02.71 |
|                 Node.js |  2.97 ± 00.03 | 1061.87 ± 00.62 |  55.00 ± 00.61 |
|           C++ libcrypto |  3.30 ± 00.07 |    5.57 ± 00.07 |  59.09 ± 01.68 |
|                     PHP |  3.38 ± 00.03 |   15.44 ± 00.07 |  57.58 ± 01.07 |
|                  GCC Go |  3.71 ± 00.01 |   29.10 ± 00.15 |  87.61 ± 02.80 |
|                     DMD |  4.12 ± 00.15 |   11.62 ± 00.07 |  80.83 ± 05.53 |
|                     Tcl |  4.46 ± 00.10 |    5.09 ± 00.10 |  75.90 ± 01.67 |
|                    PyPy |  5.01 ± 00.07 |  109.73 ± 00.28 |  84.55 ± 01.54 |
|                  Python |  5.56 ± 00.03 |    9.39 ± 00.10 |  99.24 ± 01.45 |
|            C# .NET Core |  5.66 ± 00.12 |   72.43 ± 02.13 | 104.66 ± 02.06 |
|                   Julia |  6.27 ± 00.04 |  260.17 ± 00.61 | 110.52 ± 01.86 |
|         TruffleRuby JVM |  6.54 ± 00.57 |  738.86 ± 23.36 | 124.21 ± 09.87 |
|                 C# Mono |  7.18 ± 00.08 |   39.82 ± 00.07 | 132.93 ± 01.15 |
|                   JRuby | 10.97 ± 00.37 |  345.42 ± 17.59 | 203.84 ± 09.96 |
| Perl MIME::Base64::Perl | 17.31 ± 00.44 |    9.02 ± 00.10 | 313.23 ± 10.33 |
|      TruffleRuby Native | 24.41 ± 00.36 |  618.79 ± 00.75 | 438.77 ± 06.41 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |      Memory, MiB |       Energy, J |
| :-------------------- | ------------: | ---------------: | --------------: |
|     C++ DAW JSON Link |  0.09 ± 00.00 |   109.24 ± 00.05 |    1.69 ± 00.05 |
|              GDC fast |  0.11 ± 00.00 |   231.26 ± 00.27 |    2.10 ± 00.06 |
|     Rust Serde Custom |  0.14 ± 00.00 |   108.45 ± 00.10 |    2.65 ± 00.12 |
|      Rust Serde Typed |  0.14 ± 00.00 |   120.15 ± 00.21 |    2.76 ± 00.15 |
|          C++ simdjson |  0.17 ± 00.00 |   286.69 ± 00.12 |    3.33 ± 00.08 |
|             C++ gason |  0.18 ± 00.00 |   206.36 ± 00.06 |    3.47 ± 00.07 |
|         C++ RapidJSON |  0.22 ± 00.00 |   238.08 ± 00.06 |    4.34 ± 00.07 |
|                  Java |  0.50 ± 00.02 |   337.26 ± 00.87 |   12.96 ± 00.23 |
|                 Scala |  0.57 ± 00.02 |   402.14 ± 01.11 |   13.50 ± 00.98 |
|     C++ RapidJSON SAX |  0.58 ± 00.02 |   109.44 ± 00.08 |   10.38 ± 00.42 |
|               Node.js |  0.69 ± 00.02 |   430.58 ± 01.27 |   15.61 ± 00.24 |
|           Go jsoniter |  0.72 ± 00.01 |   238.87 ± 00.27 |   13.77 ± 00.25 |
|        Crystal Schema |  0.82 ± 00.03 |   157.31 ± 00.15 |   14.98 ± 01.13 |
|          Crystal Pull |  0.83 ± 00.04 |   128.55 ± 00.05 |   15.48 ± 01.16 |
|                  PyPy |  0.85 ± 00.02 |   404.74 ± 00.06 |   16.13 ± 00.51 |
|           Julia JSON3 |  0.86 ± 00.02 |   624.19 ± 06.10 |   16.23 ± 00.61 |
|    Rust Serde Untyped |  0.93 ± 00.01 |   948.40 ± 00.09 |   17.06 ± 00.53 |
| Perl Cpanel::JSON::XS |  1.03 ± 00.03 |   524.35 ± 00.08 |   18.56 ± 00.58 |
|               Crystal |  1.12 ± 00.03 |   503.67 ± 00.05 |   20.40 ± 01.17 |
|                    Go |  1.18 ± 00.03 |   209.46 ± 00.29 |   21.97 ± 00.87 |
|                   PHP |  1.30 ± 00.03 |   803.38 ± 00.11 |   22.21 ± 01.32 |
|                 V GCC |  1.48 ± 00.02 |   591.97 ± 00.05 |   26.97 ± 00.32 |
|               V Clang |  1.53 ± 00.03 |   592.42 ± 00.07 |   27.64 ± 00.70 |
|                GCC Go |  1.58 ± 00.05 |   229.49 ± 03.85 |   29.27 ± 01.29 |
|    Nim Packedjson GCC |  1.58 ± 00.07 |   399.37 ± 00.06 |   28.50 ± 01.13 |
|            C++ json-c |  1.58 ± 00.03 |  1325.50 ± 00.04 |   29.78 ± 00.25 |
|  Nim Packedjson Clang |  1.60 ± 00.03 |   399.80 ± 00.03 |   28.35 ± 00.56 |
|               Clojure |  1.63 ± 00.04 |   974.43 ± 31.29 |   41.27 ± 02.59 |
|     CPython UltraJSON |  1.74 ± 00.02 |   661.36 ± 01.77 |   29.17 ± 00.34 |
|                Python |  1.85 ± 00.01 |   493.18 ± 00.05 |   33.03 ± 00.46 |
|               Haskell |  1.88 ± 00.04 |     9.74 ± 00.07 |   36.19 ± 00.98 |
|               Nim GCC |  1.91 ± 00.03 |   904.88 ± 00.15 |   35.31 ± 00.78 |
|             Nim Clang |  1.96 ± 00.03 |   905.18 ± 00.05 |   36.07 ± 00.73 |
|          C# .NET Core |  2.10 ± 00.06 |   761.23 ± 00.16 |   38.49 ± 01.74 |
|               C# Mono |  2.21 ± 00.08 |   462.74 ± 00.23 |   42.48 ± 02.11 |
|                  Ruby |  2.30 ± 00.02 |   396.91 ± 00.05 |   42.26 ± 00.33 |
|              Ruby JIT |  2.34 ± 00.03 |   397.05 ± 00.05 |   43.71 ± 00.47 |
|             Ruby YAJL |  2.35 ± 00.03 |   406.30 ± 00.07 |   42.57 ± 01.58 |
|                   GDC |  2.35 ± 00.05 |   713.51 ± 00.05 |   42.66 ± 01.46 |
|                   LDC |  2.65 ± 00.02 |   789.61 ± 00.11 |   48.43 ± 00.91 |
|               Rust jq |  3.92 ± 00.08 |   886.17 ± 00.99 |   68.92 ± 02.53 |
|                 JRuby |  3.97 ± 00.14 |  1933.21 ± 40.82 |  118.31 ± 05.68 |
|             C++ Boost |  4.19 ± 00.05 |  1549.65 ± 00.05 |   78.35 ± 00.65 |
|                   DMD |  5.12 ± 00.05 |   790.29 ± 00.11 |   91.04 ± 01.80 |
|   C# System.Text.Json |  7.53 ± 00.18 |   646.86 ± 00.12 |  139.29 ± 02.51 |
|       Perl JSON::Tiny | 12.17 ± 00.22 |   647.41 ± 00.12 |  234.14 ± 06.15 |
|       TruffleRuby JVM | 25.55 ± 01.86 | 2256.26 ± 146.49 |  648.22 ± 58.55 |
|    TruffleRuby Native | 71.74 ± 02.35 |  2876.86 ± 75.82 | 1123.94 ± 71.06 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |      Memory, MiB |         Energy, J |
| :-------------------- | -------------: | ---------------: | ----------------: |
| Nim Clang Arraymancer |   0.15 ± 00.01 |    57.39 ± 00.13 |      8.26 ± 00.37 |
|            LDC lubeck |   0.15 ± 00.01 |    58.49 ± 00.19 |      8.32 ± 00.20 |
|   Nim GCC Arraymancer |   0.16 ± 00.01 |    57.37 ± 00.19 |      9.15 ± 00.25 |
|          Python NumPy |   0.18 ± 00.00 |    78.86 ± 00.09 |      9.31 ± 00.30 |
|             Java ND4J |   0.22 ± 00.04 |   241.22 ± 01.24 |     10.43 ± 01.31 |
|    Julia (threads: 8) |   0.42 ± 00.01 |   276.56 ± 00.20 |     19.41 ± 00.80 |
|    Julia (threads: 1) |   0.73 ± 00.02 |   276.63 ± 00.21 |     13.50 ± 00.33 |
|                   LDC |   2.15 ± 00.01 |    73.61 ± 00.05 |     34.17 ± 00.64 |
|                   GDC |   2.27 ± 00.01 |    77.43 ± 00.07 |     38.34 ± 00.77 |
|                   DMD |   2.30 ± 00.01 |    74.20 ± 00.08 |     37.24 ± 00.69 |
|                  Java |   3.68 ± 00.15 |   124.19 ± 00.71 |     59.59 ± 06.19 |
|                     C |   3.71 ± 00.02 |    70.10 ± 00.04 |     53.36 ± 01.21 |
|                  Rust |   3.73 ± 00.01 |    70.94 ± 00.10 |     53.47 ± 01.11 |
|                 Scala |   3.76 ± 00.05 |   169.48 ± 07.47 |     62.84 ± 01.23 |
|               Nim GCC |   3.80 ± 00.02 |    79.33 ± 05.75 |     55.37 ± 01.76 |
|             Nim Clang |   3.84 ± 00.02 |    79.14 ± 07.32 |     55.64 ± 01.61 |
|                GCC Go |   3.93 ± 00.01 |    95.30 ± 03.99 |     59.28 ± 01.36 |
|                    Go |   3.94 ± 00.01 |    76.94 ± 00.24 |     60.81 ± 01.13 |
|                 V GCC |   3.95 ± 00.02 |    70.79 ± 00.06 |     62.05 ± 01.06 |
|                Kotlin |   3.97 ± 00.21 |   123.06 ± 00.33 |     69.62 ± 08.25 |
|                 Swift |   3.98 ± 00.01 |   205.22 ± 00.16 |     61.31 ± 00.96 |
|               V Clang |   3.99 ± 00.02 |    71.23 ± 00.06 |     72.14 ± 01.07 |
|               Crystal |   4.00 ± 00.01 |    63.80 ± 00.06 |     71.88 ± 01.95 |
|       Julia (no BLAS) |   4.00 ± 00.02 |   248.61 ± 00.62 |     61.20 ± 00.80 |
|               Node.js |   4.10 ± 00.01 |   104.31 ± 00.18 |     69.59 ± 01.62 |
|                  PyPy |   6.50 ± 00.08 |   132.99 ± 00.22 |    118.55 ± 02.31 |
|          C# .NET Core |   7.35 ± 00.08 |   102.35 ± 00.13 |    125.16 ± 31.39 |
|               C# Mono |  11.60 ± 00.19 |    89.30 ± 00.11 |    208.25 ± 04.93 |
|    TruffleRuby Native |  53.36 ± 00.41 |   746.00 ± 02.50 |   1024.34 ± 11.25 |
|       TruffleRuby JVM |  75.65 ± 00.76 |  1053.89 ± 58.90 |   1357.34 ± 85.79 |
|              Ruby JIT | 218.47 ± 04.01 |    84.24 ± 00.05 |  4229.42 ± 124.67 |
|                  Ruby | 227.97 ± 06.35 |    83.96 ± 00.05 |  4434.98 ± 144.64 |
|                Python | 253.88 ± 05.16 |    78.46 ± 00.04 |   4935.25 ± 92.99 |
|                   Tcl | 359.98 ± 05.38 |   407.67 ± 00.05 |  6924.36 ± 103.36 |
|                  Perl | 419.78 ± 01.20 |   608.53 ± 00.13 |  6736.55 ± 565.15 |
|                 JRuby | 524.04 ± 23.36 | 1026.01 ± 195.32 | 10047.78 ± 475.40 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |      Memory, MiB |       Energy, J |
| :----------- | -------------: | ---------------: | --------------: |
|      Crystal |   7.15 ± 00.06 |   229.12 ± 01.36 |  151.40 ± 05.69 |
|      Nim GCC |  12.05 ± 00.06 |   485.71 ± 09.19 |  225.42 ± 01.38 |
|    Nim Clang |  12.38 ± 00.06 |   485.02 ± 14.23 |  228.44 ± 01.82 |
|          C++ |  15.06 ± 00.11 |   178.24 ± 00.05 |  235.89 ± 03.36 |
| C# .NET Core |  15.49 ± 00.37 | 1383.07 ± 126.03 |  314.74 ± 24.08 |
|        Scala |  18.81 ± 00.72 |  834.73 ± 232.71 |  569.90 ± 18.64 |
|          LDC |  19.16 ± 00.29 |   463.99 ± 19.78 |  450.12 ± 23.39 |
|           Go |  19.19 ± 00.05 |   364.35 ± 09.54 |  392.82 ± 07.42 |
|          GDC |  23.46 ± 00.09 |   351.45 ± 00.04 |  394.74 ± 03.70 |
|          DMD |  24.27 ± 00.10 |   474.63 ± 03.06 |  518.65 ± 03.07 |
|      C# Mono |  26.09 ± 00.44 |   336.29 ± 01.39 |  562.45 ± 15.80 |
|       GCC Go |  26.80 ± 00.26 |   380.33 ± 14.67 |  623.16 ± 03.73 |
|         PyPy |  30.38 ± 00.50 |   664.47 ± 82.61 |  545.93 ± 10.51 |
|       Python | 108.91 ± 00.28 |   403.83 ± 00.05 | 1896.41 ± 15.93 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.1.107                         |
| C# .NET Core | 3.4.1-beta4-20127-10 (d8180a5e) |
| C# Mono      | 6.10.0.104                      |
| Chez Scheme  | 9.5                             |
| Clang        | 10.0.1                          |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.35.1                          |
| DMD          | v2.093.1                        |
| Elixir       | 1.10.3                          |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 10.2.0                          |
| GCC Go       | 10.2.0                          |
| GDC          | 10.2.0                          |
| Go           | go1.15                          |
| Haskell      | 8.10.2                          |
| JRuby        | 9.2.13.0                        |
| Java         | 14.0.2                          |
| Julia        | v"1.5.1"                        |
| Kotlin       | 1.4.0                           |
| LDC          | 1.23.0                          |
| Lua          | Lua 5.4                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20200817                        |
| Nim          | 1.2.6                           |
| Node.js      | v14.9.0                         |
| OCaml        | 4.11.0                          |
| PHP          | 7.4.5                           |
| Perl         | v5.30.3                         |
| PyPy         | 7.3.1-final0 for Python 3.6.9   |
| Python       | 3.8.5                           |
| Racket       | "7.8"                           |
| Ruby         | 2.7.1p83                        |
| Rust         | 1.46.0                          |
| Scala        | 2.13.3                          |
| Swift        | swift-5.2.5-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 20.2.0                          |
| V            | 0.1.29                          |
| Vala         | 0.48.9                          |

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
