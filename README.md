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
|                C++ |   1.00 ± 00.03 |   1.69 ± 00.04 |     19.22 ± 01.46 |
|            Nim GCC |   1.72 ± 00.06 |   1.87 ± 00.06 |     34.07 ± 02.61 |
|           Vala GCC |   1.73 ± 00.07 |   3.94 ± 00.04 |     35.68 ± 03.03 |
|              OCaml |   1.76 ± 00.06 |   5.18 ± 00.06 |     32.24 ± 03.27 |
|              C GCC |   1.83 ± 00.11 |   0.73 ± 00.04 |     36.78 ± 03.23 |
|          Nim Clang |   1.83 ± 00.07 |   2.31 ± 00.04 |     37.68 ± 03.10 |
|                LDC |   1.85 ± 00.08 |   2.95 ± 00.06 |     36.42 ± 03.70 |
|             Kotlin |   1.87 ± 00.07 |  40.24 ± 00.15 |     36.47 ± 03.10 |
|                GDC |   1.94 ± 00.08 |   6.28 ± 00.07 |     40.83 ± 03.42 |
|               Rust |   2.00 ± 00.11 |   2.07 ± 00.07 |     38.56 ± 04.36 |
|         Vala Clang |   2.01 ± 00.12 |   5.05 ± 00.08 |     40.84 ± 06.24 |
|            C Clang |   2.18 ± 00.09 |   0.71 ± 00.03 |     44.52 ± 04.64 |
|               Java |   2.36 ± 00.11 |  38.68 ± 00.14 |     49.51 ± 05.18 |
|       C# .NET Core |   2.38 ± 00.12 |  34.84 ± 00.08 |     46.57 ± 05.09 |
|                 Go |   2.43 ± 00.11 |   3.32 ± 00.42 |     50.51 ± 05.07 |
|              V GCC |   2.44 ± 00.10 |   0.71 ± 00.03 |     50.82 ± 04.51 |
|             GCC Go |   2.48 ± 00.40 |  23.04 ± 05.31 |     49.57 ± 09.93 |
|              MLton |   2.50 ± 00.12 |   0.76 ± 00.04 |     49.09 ± 05.08 |
|            Crystal |   2.52 ± 00.11 |   3.33 ± 00.06 |     53.57 ± 04.84 |
|       F# .NET Core |   2.75 ± 00.07 | 124.84 ± 01.17 |     52.06 ± 04.13 |
|            V Clang |   2.76 ± 00.17 |   1.09 ± 00.03 |     58.67 ± 05.05 |
|        Chez Scheme |   2.80 ± 00.14 |  29.15 ± 00.56 |     58.14 ± 05.87 |
|              Julia |   3.29 ± 00.15 | 177.16 ± 01.02 |     62.03 ± 05.23 |
|                DMD |   3.69 ± 00.12 |   3.50 ± 00.05 |     66.54 ± 05.72 |
|              Scala |   3.86 ± 00.26 | 134.95 ± 15.94 |     85.68 ± 05.80 |
|            C# Mono |   4.51 ± 00.23 |  20.46 ± 00.11 |     86.97 ± 10.92 |
|     Haskell MArray |   4.56 ± 00.21 |   5.30 ± 00.07 |     94.47 ± 10.06 |
|            Node.js |   4.56 ± 00.23 |  34.07 ± 00.18 |     85.45 ± 09.44 |
|             LuaJIT |   7.33 ± 00.37 |   2.96 ± 00.08 |    136.93 ± 14.32 |
|             Racket |   7.77 ± 00.33 | 106.49 ± 00.11 |    151.48 ± 14.52 |
|               PyPy |  12.66 ± 00.52 | 108.36 ± 00.13 |    298.10 ± 26.78 |
|            Haskell |  16.04 ± 00.73 |   5.38 ± 00.06 |    353.39 ± 34.33 |
|    TruffleRuby JVM |  16.06 ± 00.39 | 885.58 ± 57.02 |    476.30 ± 24.99 |
| TruffleRuby Native |  17.39 ± 00.47 | 600.02 ± 12.92 |    400.29 ± 34.52 |
|           Ruby JIT |  59.53 ± 02.40 |  14.20 ± 00.05 |  1172.53 ± 106.32 |
|                Lua |  73.62 ± 02.59 |   3.09 ± 00.06 |  1440.12 ± 111.21 |
|               Ruby |  84.53 ± 03.39 |  13.99 ± 00.04 |  1630.96 ± 141.00 |
|              JRuby | 107.47 ± 05.29 | 433.65 ± 04.39 |  2222.46 ± 117.27 |
|             Elixir | 115.68 ± 04.51 |  52.89 ± 00.61 |  2417.08 ± 217.93 |
|             Python | 231.18 ± 08.55 |   9.88 ± 00.07 |  4823.24 ± 417.40 |
|           Tcl (FP) | 266.99 ± 07.42 |   4.29 ± 00.06 |  5618.69 ± 250.76 |
|               Perl | 360.18 ± 14.27 |   6.34 ± 00.06 |  7471.42 ± 574.59 |
|           Tcl (OO) | 535.51 ± 19.47 |   4.31 ± 00.07 | 11217.49 ± 701.46 |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|           Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------------- | -------------: | -------------: | ---------------: |
|              C GCC |  12.81 ± 00.56 |   1.69 ± 00.03 |   236.93 ± 23.45 |
|           Vala GCC |  13.26 ± 00.46 |   0.00 ± 00.00 |   238.86 ± 18.97 |
|                GDC |  14.65 ± 00.57 |   7.16 ± 00.07 |   262.46 ± 23.67 |
|                LDC |  14.90 ± 00.63 |   3.84 ± 00.07 |   283.02 ± 25.27 |
|         Vala Clang |  15.57 ± 00.67 |   0.00 ± 00.00 |   315.22 ± 29.62 |
|              V GCC |  16.48 ± 00.66 |   2.46 ± 00.05 |   349.74 ± 34.85 |
|            Crystal |  16.72 ± 00.76 |   3.76 ± 00.04 |   333.14 ± 30.06 |
|               Rust |  16.84 ± 00.76 |   2.39 ± 00.07 |   332.64 ± 31.59 |
|       C# .NET Core |  17.24 ± 00.65 |  36.14 ± 00.09 |   353.81 ± 26.85 |
|                C++ |  17.40 ± 00.68 |   3.81 ± 00.07 |   346.54 ± 29.48 |
|            C Clang |  18.09 ± 00.74 |   1.69 ± 00.02 |   347.47 ± 31.96 |
|          Nim Clang |  18.17 ± 00.78 |   2.85 ± 00.04 |   371.08 ± 34.14 |
|            V Clang |  18.74 ± 00.85 |   2.94 ± 00.06 |   357.05 ± 31.76 |
|            Nim GCC |  19.09 ± 00.83 |   2.42 ± 00.04 |   372.92 ± 38.58 |
|             Kotlin |  21.38 ± 00.83 |  46.54 ± 00.59 |   394.40 ± 43.31 |
|               Java |  22.21 ± 01.56 |  45.24 ± 00.49 |   442.28 ± 48.37 |
|              MLton |  22.35 ± 01.02 |   3.91 ± 00.04 |   441.27 ± 42.13 |
|              Scala |  24.61 ± 00.63 | 119.22 ± 10.11 |   501.45 ± 98.85 |
|             GCC Go |  26.29 ± 01.24 |  25.81 ± 05.91 |   493.32 ± 52.22 |
|              OCaml |  32.62 ± 01.32 |  12.42 ± 01.50 |   635.98 ± 52.94 |
|                 Go |  40.09 ± 01.75 |   4.76 ± 00.40 |   772.41 ± 76.80 |
|        Chez Scheme |  40.72 ± 01.59 |  29.36 ± 00.08 |   872.11 ± 76.92 |
|            Node.js |  42.21 ± 00.26 |  36.93 ± 00.22 |   912.18 ± 14.94 |
|                DMD |  44.46 ± 01.61 |   4.45 ± 00.06 |   874.07 ± 78.66 |
|            C# Mono |  47.07 ± 01.74 |  21.04 ± 00.10 |   964.68 ± 87.59 |
|              Julia |  61.02 ± 02.24 | 176.61 ± 00.79 | 1174.03 ± 105.96 |
|     Haskell MArray |  63.67 ± 02.26 |   6.56 ± 00.05 | 1357.92 ± 109.95 |
|             LuaJIT |  65.86 ± 02.60 |   3.86 ± 00.11 | 1299.74 ± 138.08 |
|               PyPy |  67.10 ± 01.64 | 109.64 ± 00.07 |  1484.32 ± 83.20 |
|       F# .NET Core | 124.21 ± 00.59 | 128.49 ± 00.16 |  2481.95 ± 42.87 |
|    TruffleRuby JVM | 130.93 ± 08.36 | 905.70 ± 42.53 | 2649.01 ± 224.78 |
|             Racket | 134.97 ± 04.49 | 106.57 ± 00.10 | 2612.35 ± 203.97 |
| TruffleRuby Native | 159.66 ± 06.81 | 603.00 ± 10.74 | 3269.89 ± 235.20 |
|            Haskell | 223.17 ± 06.61 |   6.61 ± 00.07 | 4759.04 ± 355.39 |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |    Memory, MiB |      Energy, J |
| :---------------------- | ------------: | -------------: | -------------: |
|                C aklomp |  0.16 ± 00.01 |   2.00 ± 00.04 |   3.69 ± 00.24 |
|                       C |  1.44 ± 00.08 |   1.98 ± 00.03 |  27.05 ± 02.79 |
|                    Rust |  1.54 ± 00.07 |   2.59 ± 00.07 |  30.81 ± 03.06 |
|               Nim Clang |  1.62 ± 00.09 |   7.96 ± 00.07 |  33.63 ± 04.13 |
|                 Nim GCC |  1.67 ± 00.10 |   7.47 ± 00.05 |  32.86 ± 03.81 |
|                 V Clang |  1.79 ± 00.08 |   2.41 ± 00.04 |  37.00 ± 02.71 |
|                   V GCC |  1.96 ± 00.11 |   1.98 ± 00.04 |  36.75 ± 04.04 |
|                 Crystal |  2.15 ± 00.01 |   5.19 ± 00.05 |  51.61 ± 02.89 |
|                Ruby JIT |  2.18 ± 00.11 |  73.52 ± 00.17 |  44.47 ± 05.31 |
|                    Ruby |  2.23 ± 00.10 |  73.42 ± 00.16 |  41.19 ± 04.65 |
|                     LDC |  2.27 ± 00.05 |   3.97 ± 00.04 |  52.72 ± 02.58 |
|                     GDC |  2.30 ± 00.12 |  10.71 ± 00.07 |  44.72 ± 05.11 |
|                    Java |  2.52 ± 00.13 | 357.32 ± 18.96 |  52.21 ± 04.65 |
|                  Kotlin |  2.65 ± 00.14 | 346.86 ± 24.23 |  55.85 ± 04.37 |
|       Perl MIME::Base64 |  2.67 ± 00.13 |   7.19 ± 00.06 |  50.28 ± 05.55 |
|                 Node.js |  2.67 ± 00.10 | 136.51 ± 18.43 |  56.62 ± 05.44 |
|                   Scala |  2.67 ± 00.10 | 155.36 ± 08.90 |  57.55 ± 03.68 |
|                     PHP |  2.90 ± 00.15 |  15.42 ± 00.07 |  55.44 ± 06.61 |
|                      Go |  2.94 ± 00.00 |   9.14 ± 00.24 |  61.38 ± 02.62 |
|           C++ libcrypto |  3.06 ± 00.16 |   5.53 ± 00.09 |  65.58 ± 05.80 |
|                  GCC Go |  3.72 ± 00.01 |  29.66 ± 01.37 |  78.47 ± 03.44 |
|                     DMD |  4.05 ± 00.18 |  11.55 ± 00.05 |  78.97 ± 06.42 |
|                     Tcl |  4.24 ± 00.23 |   5.12 ± 00.06 |  79.82 ± 09.45 |
|                    PyPy |  4.71 ± 00.22 | 109.83 ± 00.25 |  93.96 ± 09.64 |
|                  Python |  5.13 ± 00.26 |   9.98 ± 00.06 |  98.65 ± 12.01 |
|            C# .NET Core |  5.39 ± 00.15 |  73.05 ± 02.35 | 104.32 ± 04.18 |
|                   Julia |  5.83 ± 00.26 | 250.24 ± 07.61 | 128.33 ± 14.38 |
|         TruffleRuby JVM |  6.14 ± 00.15 | 736.40 ± 73.26 | 127.66 ± 07.42 |
|                 C# Mono |  7.08 ± 00.39 |  40.07 ± 00.08 | 141.32 ± 15.72 |
|                   JRuby | 10.51 ± 00.59 | 390.07 ± 14.25 | 218.33 ± 20.97 |
| Perl MIME::Base64::Perl | 16.22 ± 00.66 |   8.78 ± 00.10 | 340.22 ± 30.72 |
|      TruffleRuby Native | 23.61 ± 00.91 | 502.20 ± 00.13 | 472.10 ± 34.48 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |     Memory, MiB |       Energy, J |
| :-------------------- | ------------: | --------------: | --------------: |
|     C++ DAW JSON Link |  0.10 ± 00.00 |  109.30 ± 00.04 |    1.95 ± 00.22 |
|              GDC fast |  0.10 ± 00.01 |  231.19 ± 00.21 |    2.46 ± 00.26 |
|     Rust Serde Custom |  0.15 ± 00.01 |  108.49 ± 00.09 |    2.96 ± 00.31 |
|      Rust Serde Typed |  0.16 ± 00.01 |  120.21 ± 00.32 |    3.20 ± 00.43 |
|          C++ simdjson |  0.16 ± 00.01 |  286.62 ± 00.38 |    3.69 ± 00.43 |
|             C++ gason |  0.17 ± 00.01 |  206.43 ± 00.04 |    3.86 ± 00.40 |
|         C++ RapidJSON |  0.23 ± 00.01 |  238.19 ± 00.06 |    4.83 ± 00.52 |
|                  Java |  0.51 ± 00.02 |  337.77 ± 01.13 |   13.88 ± 00.36 |
|     C++ RapidJSON SAX |  0.55 ± 00.03 |  109.48 ± 00.07 |   11.07 ± 01.23 |
|                 Scala |  0.56 ± 00.02 |  403.48 ± 03.91 |   15.03 ± 01.27 |
|               Node.js |  0.67 ± 00.02 |  323.91 ± 01.10 |   16.45 ± 01.19 |
|           Go jsoniter |  0.71 ± 00.02 |  226.33 ± 00.32 |   15.17 ± 01.25 |
|        Crystal Schema |  0.78 ± 00.05 |  157.27 ± 00.15 |   15.54 ± 01.56 |
|          Crystal Pull |  0.79 ± 00.05 |  128.51 ± 00.04 |   13.96 ± 01.66 |
|           Julia JSON3 |  0.81 ± 00.04 |  543.80 ± 12.10 |   17.45 ± 01.82 |
|                  PyPy |  0.82 ± 00.05 |  404.68 ± 00.08 |   17.79 ± 02.20 |
|    Rust Serde Untyped |  0.92 ± 00.03 |  916.44 ± 00.09 |   20.20 ± 01.34 |
| Perl Cpanel::JSON::XS |  0.99 ± 00.05 |  524.24 ± 00.08 |   21.16 ± 02.35 |
|               Crystal |  1.10 ± 00.06 |  503.61 ± 00.03 |   22.80 ± 02.12 |
|               V Clang |  1.15 ± 00.05 |  591.27 ± 00.61 |   25.06 ± 01.91 |
|                 V GCC |  1.16 ± 00.05 |  591.16 ± 00.72 |   24.66 ± 02.48 |
|                    Go |  1.19 ± 00.06 |  209.73 ± 00.30 |   22.55 ± 02.15 |
|                   PHP |  1.28 ± 00.05 |  803.45 ± 00.08 |   24.30 ± 02.10 |
|                GCC Go |  1.46 ± 00.08 |  231.60 ± 05.96 |   28.60 ± 03.67 |
|    Nim Packedjson GCC |  1.54 ± 00.11 |  399.35 ± 00.05 |   31.83 ± 03.79 |
|               Clojure |  1.54 ± 00.05 |  958.30 ± 30.99 |   43.23 ± 02.27 |
|  Nim Packedjson Clang |  1.57 ± 00.08 |  399.85 ± 00.05 |   30.85 ± 03.41 |
|            C++ json-c |  1.69 ± 00.08 | 1649.58 ± 00.08 |   36.21 ± 03.80 |
|     CPython UltraJSON |  1.72 ± 00.04 |  660.95 ± 02.22 |   37.06 ± 01.07 |
|               Haskell |  1.80 ± 00.09 |    9.58 ± 00.06 |   36.81 ± 04.33 |
|                Python |  1.81 ± 00.05 |  493.65 ± 00.04 |   38.12 ± 02.16 |
|               Nim GCC |  1.86 ± 00.07 |  904.83 ± 00.16 |   39.94 ± 03.20 |
|             Nim Clang |  1.87 ± 00.08 |  905.21 ± 00.10 |   41.08 ± 03.51 |
|          C# .NET Core |  2.05 ± 00.11 |  761.41 ± 00.10 |   41.40 ± 05.37 |
|                  Ruby |  2.25 ± 00.10 |  396.87 ± 00.05 |   46.57 ± 05.34 |
|              Ruby JIT |  2.26 ± 00.09 |  396.96 ± 00.06 |   50.66 ± 04.21 |
|                   GDC |  2.26 ± 00.11 |  713.47 ± 00.09 |   44.04 ± 05.04 |
|             Ruby YAJL |  2.27 ± 00.09 |  406.19 ± 00.10 |   47.19 ± 04.90 |
|                   LDC |  2.61 ± 00.06 |  789.67 ± 00.13 |   53.23 ± 01.87 |
|               C# Mono |  2.78 ± 00.08 |  462.62 ± 00.10 |   49.92 ± 03.60 |
|               Rust jq |  3.66 ± 00.14 |  885.77 ± 01.23 |   77.74 ± 06.47 |
|                 JRuby |  3.86 ± 00.10 | 1945.10 ± 35.50 |  118.64 ± 06.22 |
|             C++ Boost |  3.99 ± 00.16 | 1549.71 ± 00.05 |   92.27 ± 08.22 |
|                   DMD |  4.95 ± 00.20 |  790.36 ± 00.07 |   97.80 ± 06.64 |
|   C# System.Text.Json |  6.68 ± 00.32 |  647.14 ± 00.06 |  150.34 ± 14.24 |
|       Perl JSON::Tiny | 11.98 ± 00.52 |  647.27 ± 00.10 |  230.18 ± 24.94 |
|       TruffleRuby JVM | 19.77 ± 00.66 | 2022.78 ± 86.17 |  540.06 ± 21.90 |
|    TruffleRuby Native | 53.80 ± 01.82 | 2747.48 ± 79.90 | 1163.16 ± 94.78 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |     Memory, MiB |         Energy, J |
| :-------------------- | -------------: | --------------: | ----------------: |
|            LDC lubeck |   0.13 ± 00.00 |   61.47 ± 00.20 |      7.75 ± 00.20 |
| Nim Clang Arraymancer |   0.15 ± 00.05 |   56.34 ± 00.22 |      7.41 ± 01.32 |
|   Nim GCC Arraymancer |   0.16 ± 00.06 |   56.38 ± 00.12 |      8.10 ± 01.47 |
|          Python NumPy |   0.19 ± 00.01 |   84.75 ± 00.13 |      9.96 ± 00.58 |
|             Java ND4J |   0.19 ± 00.03 |  240.06 ± 02.92 |      9.57 ± 00.71 |
|    Julia (threads: 8) |   0.43 ± 00.02 |  290.51 ± 00.41 |     19.10 ± 00.88 |
|    Julia (threads: 1) |   0.71 ± 00.03 |  290.65 ± 00.39 |     15.85 ± 01.46 |
|                   LDC |   1.96 ± 00.01 |   73.61 ± 00.05 |     43.86 ± 01.04 |
|                   DMD |   2.10 ± 00.02 |   74.14 ± 00.14 |     47.66 ± 01.16 |
|                   GDC |   2.10 ± 00.03 |   77.40 ± 00.06 |     47.54 ± 02.37 |
|                     C |   3.32 ± 00.02 |   70.19 ± 00.07 |     68.78 ± 02.13 |
|                  Java |   3.32 ± 00.03 |  124.13 ± 00.62 |     76.47 ± 01.35 |
|                  Rust |   3.35 ± 00.01 |   70.97 ± 00.08 |     69.07 ± 02.34 |
|                 Scala |   3.41 ± 00.04 |  164.67 ± 02.25 |     79.37 ± 01.50 |
|               Nim GCC |   3.44 ± 00.15 |   79.95 ± 06.37 |     67.80 ± 07.48 |
|             Nim Clang |   3.48 ± 00.18 |   76.71 ± 04.02 |     68.41 ± 08.11 |
|                GCC Go |   3.51 ± 00.03 |   95.13 ± 03.94 |     75.49 ± 02.52 |
|                    Go |   3.60 ± 00.10 |   76.72 ± 00.22 |     74.12 ± 05.07 |
|                 Swift |   3.62 ± 00.10 |  205.05 ± 00.23 |     77.10 ± 04.12 |
|                 V GCC |   3.64 ± 00.12 |   70.74 ± 00.15 |     76.63 ± 05.60 |
|       Julia (no BLAS) |   3.64 ± 00.07 |  252.16 ± 00.74 |     76.15 ± 02.71 |
|               Crystal |   3.71 ± 00.20 |   63.74 ± 00.04 |     75.08 ± 09.14 |
|               V Clang |   3.79 ± 00.16 |   71.26 ± 00.06 |     80.58 ± 09.85 |
|                Kotlin |   4.04 ± 00.13 |  122.80 ± 00.11 |     76.52 ± 06.62 |
|               Node.js |   5.48 ± 00.23 |  104.62 ± 00.46 |    109.61 ± 13.66 |
|                  PyPy |   6.11 ± 00.20 |  133.04 ± 00.23 |    132.44 ± 10.66 |
|          C# .NET Core |   7.01 ± 00.31 |  102.71 ± 00.19 |    145.63 ± 13.66 |
|               C# Mono |  10.39 ± 00.41 |   89.27 ± 00.13 |    202.31 ± 22.87 |
|    TruffleRuby Native |  29.30 ± 00.67 |  719.89 ± 19.82 |    635.30 ± 32.04 |
|       TruffleRuby JVM |  41.16 ± 00.42 | 1005.51 ± 52.51 |    963.95 ± 22.59 |
|              Ruby JIT | 211.73 ± 08.09 |   84.19 ± 00.05 |  4142.66 ± 339.16 |
|                  Ruby | 230.50 ± 07.36 |   83.94 ± 00.08 |  4421.55 ± 430.79 |
|                Python | 253.37 ± 06.63 |   78.75 ± 00.05 |  5005.83 ± 491.37 |
|                   Tcl | 353.73 ± 06.63 |  407.59 ± 00.07 |  7317.93 ± 549.38 |
|                  Perl | 397.81 ± 06.14 |  608.42 ± 00.08 |  8642.06 ± 608.42 |
|                 JRuby | 486.73 ± 26.67 | 989.22 ± 197.84 | 10799.30 ± 800.81 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |      Memory, MiB |        Energy, J |
| :----------- | -------------: | ---------------: | ---------------: |
|      Crystal |   6.78 ± 00.12 |   212.04 ± 01.09 |   164.88 ± 08.38 |
|      Nim GCC |  11.91 ± 00.28 |   479.60 ± 11.84 |   276.72 ± 21.65 |
|    Nim Clang |  12.24 ± 00.27 |   486.60 ± 12.88 |   270.83 ± 21.23 |
|          C++ |  13.80 ± 00.21 |   178.32 ± 00.06 |   296.32 ± 11.14 |
| C# .NET Core |  15.01 ± 00.37 | 1508.10 ± 134.56 |   357.34 ± 28.55 |
|           Go |  18.70 ± 00.12 |   358.30 ± 10.14 |   436.77 ± 07.09 |
|        Scala |  18.78 ± 00.41 |  733.99 ± 295.97 |   578.36 ± 20.69 |
|          LDC |  19.22 ± 00.45 |   474.38 ± 27.65 |   457.12 ± 21.69 |
|          GDC |  22.13 ± 00.74 |   361.71 ± 32.53 |   469.49 ± 31.43 |
|          DMD |  23.46 ± 00.16 |   441.00 ± 00.74 |   560.86 ± 09.55 |
|      C# Mono |  25.44 ± 00.33 |   334.11 ± 00.69 |   543.68 ± 30.78 |
|       GCC Go |  26.37 ± 00.31 |   379.97 ± 17.02 |   656.82 ± 07.86 |
|         PyPy |  29.14 ± 00.81 |   663.59 ± 67.98 |   640.75 ± 42.90 |
|       Python | 106.33 ± 01.82 |   405.67 ± 00.05 | 2253.50 ± 125.39 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.1.105                         |
| C# .NET Core | 3.4.1-beta4-20127-10 (d8180a5e) |
| C# Mono      | 6.8.0.123                       |
| Chez Scheme  | 9.5                             |
| Clang        | 10.0.0                          |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.35.0                          |
| DMD          | v2.092.1                        |
| Elixir       | 1.10.3                          |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 10.1.0                          |
| GCC Go       | 10.1.0                          |
| GDC          | 10.1.0                          |
| Go           | go1.14.4                        |
| Haskell      | 8.10.1                          |
| JRuby        | 9.2.11.1                        |
| Java         | 14.0.1                          |
| Julia        | v"1.4.2"                        |
| Kotlin       | 1.3.72                          |
| LDC          | 1.21.0                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.2.0                           |
| Node.js      | v14.4.0                         |
| OCaml        | 4.10.0                          |
| PHP          | 7.3.15-3                        |
| Perl         | v5.30.2                         |
| PyPy         | 7.3.1-final0 for Python 3.6.9   |
| Python       | 3.8.3                           |
| Racket       | "7.7"                           |
| Ruby         | 2.7.1p83                        |
| Rust         | 1.44.0                          |
| Scala        | 2.13.2                          |
| Swift        | swift-5.2.4-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 20.1.0                          |
| V            | 0.1.27                          |
| Vala         | 0.48.6                          |

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
