<!-- md-toc-begin -->
# Table of Content
* [Overview](#overview)
* [Test Cases](#test-cases)
  * [Brainfuck v2.0](#brainfuck-v20)
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
<!-- md-toc-end -->

# Overview

The benchmarks follow the criteria:

  - They are written as the average software developer would write them, i.e.

    - The algorithms are implemented as cited in public sources;
    - The libraries are used as described in the tutorials, documentation and examples;
    - Used data structures are idiomatic.

  - The used algorithms are similar between the languages (reference implementations), variants are acceptable if the reference implementation exists.
  - All final binaries are releases (optimized for performance if possible) as debug performance may vary too much depending on the compiler.
  - JIT warming up is applied when necessary, and the actual measurements are taken only within the specified boundaries.

UPDATE: 2020-04-28

# Test Cases

## Brainfuck v2.0

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck v2.0](brainfuck2)
[Brainfuck v1.0](brainfuck)

### bench.b

|           Language |        Time, s |     Memory, MiB |          Energy, J |
| :----------------- | -------------: | --------------: | -----------------: |
|              OCaml |   1.69 ± 00.07 |    5.18 ± 00.04 |      35.26 ± 03.23 |
|           Vala GCC |   1.74 ± 00.09 |    4.02 ± 00.03 |      35.14 ± 03.66 |
|                C++ |   1.76 ± 00.05 |    1.68 ± 00.06 |      40.75 ± 02.15 |
|            Nim GCC |   1.78 ± 00.08 |    1.86 ± 00.07 |      37.92 ± 03.22 |
|              C GCC |   1.87 ± 00.10 |    0.71 ± 00.04 |      36.13 ± 03.30 |
|                LDC |   1.87 ± 00.07 |    2.95 ± 00.04 |      35.91 ± 03.13 |
|          Nim Clang |   1.92 ± 00.23 |    2.33 ± 00.08 |      39.88 ± 07.59 |
|         Vala Clang |   1.92 ± 00.10 |    5.04 ± 00.39 |      40.48 ± 03.45 |
|               Rust |   1.99 ± 00.11 |    2.00 ± 00.06 |      39.30 ± 04.52 |
|                GDC |   1.99 ± 00.11 |    6.47 ± 00.07 |      40.56 ± 04.09 |
|             Kotlin |   2.03 ± 00.12 |   40.64 ± 00.08 |      40.75 ± 03.70 |
|             GCC Go |   2.11 ± 00.11 |   20.32 ± 00.13 |      43.10 ± 04.70 |
|            C Clang |   2.17 ± 00.10 |    0.72 ± 00.04 |      45.56 ± 04.43 |
|       C# .NET Core |   2.36 ± 00.11 |   29.77 ± 00.10 |      47.57 ± 05.28 |
|                 Go |   2.45 ± 00.11 |    3.21 ± 00.32 |      51.06 ± 04.68 |
|               Java |   2.46 ± 00.19 |   39.83 ± 00.13 |      44.56 ± 03.99 |
|              MLton |   2.49 ± 00.13 |    0.75 ± 00.05 |      51.07 ± 06.06 |
|            Crystal |   2.55 ± 00.15 |    3.30 ± 00.04 |      52.70 ± 05.99 |
|       F# .NET Core |   2.62 ± 00.24 |   38.96 ± 00.12 |      55.72 ± 06.20 |
|        Chez Scheme |   2.81 ± 00.15 |   29.32 ± 00.11 |      58.38 ± 07.23 |
|              V GCC |   2.95 ± 00.13 |    0.70 ± 00.03 |      64.80 ± 06.72 |
|              Julia |   3.09 ± 00.20 |  177.22 ± 01.00 |      63.10 ± 06.60 |
|                DMD |   3.56 ± 00.17 |    3.55 ± 00.08 |      74.03 ± 07.27 |
|            V Clang |   3.70 ± 00.19 |    1.05 ± 00.07 |      70.46 ± 08.69 |
|              Scala |   3.71 ± 00.17 |  131.80 ± 12.36 |      77.50 ± 03.20 |
|            C# Mono |   3.96 ± 00.19 |   20.19 ± 00.08 |      84.10 ± 09.65 |
|     Haskell MArray |   4.44 ± 00.24 |    5.33 ± 00.07 |      99.74 ± 10.61 |
|            Node.js |   4.52 ± 00.26 |   34.54 ± 00.19 |      88.99 ± 10.94 |
|             LuaJIT |   6.92 ± 00.31 |    2.98 ± 00.06 |     154.80 ± 11.78 |
|             Racket |   7.04 ± 00.50 |  104.69 ± 00.64 |     141.22 ± 15.97 |
|               PyPy |  12.90 ± 00.78 |  108.37 ± 00.17 |     298.91 ± 26.84 |
|            Haskell |  16.20 ± 00.83 |    5.34 ± 00.07 |     347.29 ± 41.43 |
|    TruffleRuby JVM |  32.89 ± 01.45 | 1051.67 ± 63.15 |     931.57 ± 42.84 |
| TruffleRuby Native |  34.73 ± 01.42 |  647.68 ± 04.65 |     824.29 ± 67.12 |
|           Ruby JIT |  60.04 ± 02.56 |   14.21 ± 00.07 |   1157.71 ± 126.35 |
|                Lua |  70.42 ± 02.86 |    3.10 ± 00.05 |   1507.49 ± 125.07 |
|               Ruby |  80.06 ± 02.51 |   14.01 ± 00.04 |   1490.11 ± 156.46 |
|              JRuby | 107.41 ± 06.17 |  432.44 ± 12.38 |   2034.95 ± 364.03 |
|             Elixir | 115.42 ± 03.21 |   52.27 ± 01.01 |   2493.89 ± 215.42 |
|             Python | 224.22 ± 09.38 |    9.85 ± 00.06 |   4924.38 ± 458.83 |
|           Tcl (FP) | 264.66 ± 11.04 |    4.29 ± 00.06 |   5617.21 ± 549.58 |
|               Perl | 362.00 ± 15.92 |    6.34 ± 00.09 |   7294.13 ± 831.78 |
|           Tcl (OO) | 535.20 ± 23.85 |    4.32 ± 00.07 | 11061.17 ± 1244.66 |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

|           Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------------- | -------------: | -------------: | ---------------: |
|           Vala GCC |  13.29 ± 00.60 |   5.81 ± 00.09 |   249.54 ± 26.80 |
|              C GCC |  14.53 ± 00.58 |   1.72 ± 00.03 |   268.20 ± 24.63 |
|         Vala Clang |  15.00 ± 00.61 |   5.88 ± 00.08 |   293.77 ± 29.07 |
|                LDC |  15.05 ± 00.63 |   3.85 ± 00.07 |   275.41 ± 28.93 |
|              V GCC |  15.53 ± 00.71 |   2.47 ± 00.07 |   304.32 ± 30.53 |
|               Rust |  15.72 ± 00.75 |   2.32 ± 00.08 |   306.91 ± 37.21 |
|                GDC |  15.79 ± 00.83 |   7.39 ± 00.07 |   306.51 ± 33.87 |
|            Crystal |  16.33 ± 00.84 |   3.75 ± 00.05 |   289.70 ± 29.54 |
|          Nim Clang |  16.95 ± 00.79 |   2.89 ± 00.06 |   343.54 ± 32.19 |
|            C Clang |  17.33 ± 00.82 |   1.70 ± 00.04 |   366.17 ± 40.09 |
|       C# .NET Core |  17.76 ± 00.75 |  31.16 ± 00.09 |   329.86 ± 33.70 |
|                C++ |  18.49 ± 00.96 |   3.77 ± 00.04 |   344.79 ± 31.55 |
|            V Clang |  19.30 ± 00.81 |   3.23 ± 00.04 |   370.68 ± 42.03 |
|             Kotlin |  21.73 ± 01.52 |  46.34 ± 00.21 |   407.04 ± 78.97 |
|              MLton |  22.52 ± 00.90 |   3.93 ± 00.05 |   439.55 ± 41.38 |
|             GCC Go |  22.53 ± 01.06 |  25.16 ± 05.73 |   420.80 ± 43.53 |
|               Java |  23.17 ± 01.11 |  46.08 ± 00.19 |   465.36 ± 57.92 |
|            Nim GCC |  23.54 ± 00.94 |   2.42 ± 00.03 |   504.37 ± 44.94 |
|              Scala |  24.67 ± 00.71 | 132.76 ± 08.57 |   527.22 ± 29.91 |
|              OCaml |  31.14 ± 01.36 |  12.17 ± 01.19 |   648.05 ± 62.68 |
|                 Go |  34.40 ± 01.24 |   4.89 ± 00.14 |   627.85 ± 57.28 |
|        Chez Scheme |  41.58 ± 01.80 |  29.50 ± 00.13 |   839.75 ± 88.64 |
|            C# Mono |  41.93 ± 01.68 |  20.88 ± 00.14 |   756.37 ± 77.23 |
|            Node.js |  43.83 ± 01.72 |  36.99 ± 00.23 |   840.74 ± 83.67 |
|                DMD |  44.63 ± 01.96 |   4.44 ± 00.06 |   868.80 ± 81.30 |
|              Julia |  62.95 ± 02.47 | 176.70 ± 00.67 | 1180.30 ± 112.83 |
|     Haskell MArray |  65.30 ± 02.86 |   6.60 ± 00.08 | 1303.95 ± 134.28 |
|             LuaJIT |  66.08 ± 02.91 |   3.91 ± 00.09 | 1345.13 ± 138.18 |
|               PyPy |  69.26 ± 02.58 | 109.61 ± 00.09 | 1382.87 ± 111.72 |
|       F# .NET Core | 113.41 ± 05.33 |  40.13 ± 00.07 | 2288.07 ± 486.43 |
|             Racket | 130.29 ± 08.36 | 104.59 ± 00.64 | 2583.83 ± 185.01 |
|    TruffleRuby JVM | 155.29 ± 09.11 | 947.64 ± 94.47 | 3211.97 ± 223.03 |
| TruffleRuby Native | 212.31 ± 07.44 | 609.54 ± 12.20 | 4262.47 ± 333.57 |

## Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |     Memory, MiB |      Energy, J |
| :---------------------- | ------------: | --------------: | -------------: |
|                C aklomp |  0.16 ± 00.01 |    2.00 ± 00.05 |   3.47 ± 00.23 |
|                       C |  1.39 ± 00.08 |    1.98 ± 00.03 |  26.08 ± 02.81 |
|                    Rust |  1.55 ± 00.10 |    2.62 ± 00.06 |  29.47 ± 03.82 |
|               Nim Clang |  1.67 ± 00.11 |    7.90 ± 00.05 |  32.20 ± 04.27 |
|                 Nim GCC |  2.00 ± 00.12 |    7.48 ± 00.05 |  38.27 ± 05.22 |
|                     GDC |  2.17 ± 00.13 |   10.90 ± 00.07 |  44.13 ± 05.84 |
|                 Crystal |  2.23 ± 00.02 |    5.25 ± 00.06 |  53.15 ± 02.32 |
|                    Ruby |  2.24 ± 00.09 |   73.44 ± 00.17 |  44.62 ± 04.08 |
|                     LDC |  2.28 ± 00.06 |    3.96 ± 00.07 |  52.12 ± 02.36 |
|                    Java |  2.29 ± 00.11 |  360.05 ± 17.44 |  45.67 ± 05.14 |
|                Ruby JIT |  2.31 ± 00.09 |   73.51 ± 00.14 |  41.91 ± 04.56 |
|                  Kotlin |  2.39 ± 00.07 |  352.37 ± 33.25 |  50.40 ± 03.40 |
|                      Go |  2.47 ± 00.00 |   10.57 ± 03.03 |  49.64 ± 02.19 |
|                   Scala |  2.57 ± 00.09 |  149.54 ± 09.41 |  53.01 ± 03.55 |
|       Perl MIME::Base64 |  2.63 ± 00.15 |    7.20 ± 00.09 |  51.35 ± 06.85 |
|                 Node.js |  2.63 ± 00.10 |  133.00 ± 16.00 |  55.59 ± 05.33 |
|                 V Clang |  2.65 ± 00.16 |    2.36 ± 00.05 |  50.72 ± 07.06 |
|                     PHP |  2.93 ± 00.16 |   15.46 ± 00.10 |  53.76 ± 06.58 |
|           C++ libcrypto |  3.10 ± 00.16 |    5.52 ± 00.05 |  62.74 ± 07.32 |
|                   V GCC |  3.47 ± 00.21 |    1.90 ± 00.06 |  67.77 ± 08.95 |
|                  GCC Go |  3.70 ± 00.01 |   48.20 ± 05.30 |  75.41 ± 02.97 |
|            C# .NET Core |  4.06 ± 00.22 |   32.99 ± 00.12 |  83.40 ± 11.22 |
|                     DMD |  4.17 ± 00.20 |   11.50 ± 00.07 |  81.29 ± 08.30 |
|                     Tcl |  4.27 ± 00.21 |    5.06 ± 00.06 |  78.70 ± 09.57 |
|                    PyPy |  4.74 ± 00.25 |  109.85 ± 00.41 |  92.22 ± 11.27 |
|                  Python |  5.27 ± 00.26 |    9.98 ± 00.05 | 109.04 ± 11.52 |
|                   Julia |  5.69 ± 00.28 |  251.53 ± 09.99 | 119.72 ± 12.90 |
|                 C# Mono |  6.75 ± 00.33 |   39.59 ± 00.13 | 142.47 ± 18.35 |
|         TruffleRuby JVM |  6.90 ± 00.39 | 810.78 ± 108.64 | 149.08 ± 16.10 |
|                   JRuby | 10.27 ± 00.73 |  375.82 ± 20.94 | 207.97 ± 17.12 |
| Perl MIME::Base64::Perl | 16.02 ± 00.94 |    8.91 ± 00.10 | 328.41 ± 40.23 |
|      TruffleRuby Native | 26.49 ± 00.92 |  630.51 ± 01.19 | 479.10 ± 41.86 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |      Memory, MiB |        Energy, J |
| :-------------------- | ------------: | ---------------: | ---------------: |
|     C++ DAW JSON Link |  0.08 ± 00.00 |   109.30 ± 00.07 |     1.78 ± 00.21 |
|              GDC fast |  0.11 ± 00.01 |   231.60 ± 00.27 |     2.15 ± 00.27 |
|     Rust Serde Custom |  0.15 ± 00.01 |   108.47 ± 00.09 |     2.66 ± 00.23 |
|      Rust Serde Typed |  0.15 ± 00.01 |   120.11 ± 00.25 |     2.75 ± 00.32 |
|             C++ gason |  0.19 ± 00.01 |   312.78 ± 00.08 |     3.89 ± 00.53 |
|          C++ simdjson |  0.20 ± 00.01 |   492.83 ± 03.15 |     4.51 ± 00.60 |
|         C++ RapidJSON |  0.23 ± 00.01 |   344.53 ± 00.09 |     5.25 ± 00.54 |
|                  Java |  0.39 ± 00.02 |   563.43 ± 25.56 |     8.03 ± 00.78 |
|                 Scala |  0.45 ± 00.02 |   473.48 ± 04.37 |    10.00 ± 00.56 |
|           Julia JSON3 |  0.55 ± 00.02 |   850.22 ± 01.66 |    11.68 ± 01.13 |
|     C++ RapidJSON SAX |  0.56 ± 00.03 |   109.50 ± 00.07 |    10.80 ± 01.54 |
|    Rust Serde Untyped |  0.61 ± 00.03 |   916.46 ± 00.08 |    12.18 ± 01.44 |
|               Node.js |  0.68 ± 00.04 |   324.54 ± 01.10 |    15.21 ± 01.51 |
|           Go jsoniter |  0.74 ± 00.02 |   226.44 ± 00.27 |    14.84 ± 01.05 |
| Perl Cpanel::JSON::XS |  0.79 ± 00.06 |   517.11 ± 00.08 |    17.36 ± 01.44 |
|          Crystal Pull |  0.81 ± 00.03 |   128.49 ± 00.06 |    15.53 ± 01.62 |
|        Crystal Schema |  0.83 ± 00.06 |   157.27 ± 00.15 |    15.98 ± 02.30 |
|                  PyPy |  0.85 ± 00.04 |   406.39 ± 00.82 |    17.85 ± 02.78 |
|               Crystal |  1.10 ± 00.07 |   503.60 ± 00.06 |    22.50 ± 02.73 |
|                   PHP |  1.14 ± 00.05 |   803.39 ± 00.08 |    22.58 ± 02.47 |
|                    Go |  1.20 ± 00.09 |   209.29 ± 00.22 |    22.60 ± 03.53 |
|               Clojure |  1.26 ± 00.06 |  2214.88 ± 82.04 |    32.15 ± 02.30 |
|               V Clang |  1.38 ± 00.06 |   592.47 ± 00.07 |    30.65 ± 03.10 |
|                 V GCC |  1.40 ± 00.06 |   592.00 ± 00.07 |    30.18 ± 02.95 |
|                GCC Go |  1.58 ± 00.06 |   207.14 ± 00.51 |    32.67 ± 02.47 |
|               Haskell |  1.69 ± 00.06 |    10.51 ± 00.11 |    38.05 ± 02.92 |
|            C++ json-c |  1.72 ± 00.08 |  1755.93 ± 00.03 |    35.27 ± 04.42 |
|                Python |  1.73 ± 00.07 |   493.61 ± 00.06 |    37.12 ± 02.79 |
|     CPython UltraJSON |  1.81 ± 00.04 |   662.36 ± 01.96 |    42.54 ± 03.42 |
|               Nim GCC |  1.87 ± 00.11 |   908.81 ± 00.04 |    39.63 ± 04.62 |
|             Nim Clang |  1.91 ± 00.09 |   909.26 ± 00.06 |    39.99 ± 04.08 |
|          C# .NET Core |  1.92 ± 00.09 |   286.63 ± 00.13 |    36.31 ± 04.00 |
|                  Ruby |  2.22 ± 00.09 |   396.77 ± 00.06 |    49.01 ± 05.91 |
|             Ruby YAJL |  2.24 ± 00.08 |   406.15 ± 00.11 |    49.85 ± 04.30 |
|              Ruby JIT |  2.32 ± 00.09 |   396.97 ± 00.05 |    47.95 ± 05.13 |
|                   LDC |  2.52 ± 00.07 |   796.21 ± 00.08 |    51.49 ± 02.79 |
|               C# Mono |  2.60 ± 00.11 |   329.16 ± 00.07 |    52.04 ± 05.13 |
|                   GDC |  2.99 ± 00.18 |   713.75 ± 00.06 |    58.03 ± 07.32 |
|             C++ Boost |  3.69 ± 00.18 |  1549.60 ± 00.06 |    79.97 ± 08.86 |
|               Rust jq |  3.73 ± 00.17 |   886.08 ± 01.06 |    73.94 ± 08.34 |
|                 JRuby |  3.93 ± 00.15 |  1945.75 ± 43.98 |   113.71 ± 06.84 |
|                   DMD |  4.86 ± 00.18 |   796.75 ± 00.08 |    94.37 ± 07.56 |
|   C# System.Text.Json |  6.87 ± 00.25 |   606.01 ± 00.11 |   137.51 ± 11.68 |
|       Perl JSON::Tiny | 11.63 ± 00.52 |   640.22 ± 00.07 |   237.83 ± 26.19 |
|       TruffleRuby JVM | 18.43 ± 00.61 | 2189.94 ± 235.00 |   450.28 ± 38.15 |
|    TruffleRuby Native | 57.52 ± 02.37 |  2776.84 ± 64.54 | 1186.06 ± 107.49 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |      Memory, MiB |         Energy, J |
| :-------------------- | -------------: | ---------------: | ----------------: |
| Nim Clang Arraymancer |   0.14 ± 00.00 |    56.48 ± 00.26 |      7.41 ± 00.24 |
|            LDC lubeck |   0.14 ± 00.00 |    61.45 ± 00.22 |      7.79 ± 00.16 |
|   Nim GCC Arraymancer |   0.15 ± 00.00 |    56.35 ± 00.15 |      8.45 ± 00.17 |
|          Python NumPy |   0.15 ± 00.01 |    84.38 ± 00.14 |      7.49 ± 00.32 |
|    Julia (threads: 8) |   0.19 ± 00.00 |   263.07 ± 00.35 |      9.24 ± 00.32 |
|             Java ND4J |   0.23 ± 00.01 |   235.98 ± 01.07 |     10.33 ± 00.59 |
|    Julia (threads: 1) |   0.56 ± 00.03 |   263.21 ± 00.18 |     11.56 ± 01.39 |
|                   LDC |   1.97 ± 00.02 |    73.31 ± 00.08 |     44.99 ± 01.64 |
|                   DMD |   2.12 ± 00.03 |    73.34 ± 00.08 |     47.15 ± 01.28 |
|                   GDC |   2.14 ± 00.05 |    77.06 ± 00.06 |     46.10 ± 02.43 |
|                  Rust |   3.37 ± 00.01 |    70.87 ± 00.08 |     70.16 ± 01.84 |
|                     C |   3.39 ± 00.05 |    70.32 ± 00.07 |     74.81 ± 01.42 |
|                 Scala |   3.39 ± 00.08 |   171.21 ± 08.64 |     76.91 ± 02.59 |
|               Nim GCC |   3.43 ± 00.03 |    77.54 ± 04.48 |     70.87 ± 01.49 |
|             Nim Clang |   3.48 ± 00.02 |    79.81 ± 06.39 |     71.50 ± 01.59 |
|       Julia (no BLAS) |   3.50 ± 00.06 |   277.72 ± 00.28 |     73.06 ± 02.22 |
|                    Go |   3.65 ± 00.07 |    76.69 ± 00.20 |     75.46 ± 03.50 |
|                GCC Go |   3.67 ± 00.09 |   105.58 ± 02.35 |     82.98 ± 04.69 |
|               Crystal |   3.68 ± 00.09 |    63.79 ± 00.05 |     76.25 ± 03.15 |
|                 Swift |   3.68 ± 00.14 |   206.16 ± 06.48 |     86.26 ± 07.13 |
|                Kotlin |   3.79 ± 00.33 |   125.85 ± 00.18 |     73.86 ± 04.56 |
|                  Java |   3.93 ± 00.18 |   125.48 ± 00.16 |     82.72 ± 10.92 |
|               Node.js |   5.31 ± 00.25 |   106.09 ± 00.07 |    118.29 ± 14.12 |
|                  PyPy |   6.20 ± 00.33 |   133.14 ± 00.16 |    128.10 ± 18.51 |
|          C# .NET Core |   6.83 ± 00.32 |   105.35 ± 00.11 |    151.09 ± 16.93 |
|               V Clang |  10.55 ± 00.07 |    71.02 ± 00.06 |    202.21 ± 05.32 |
|                 V GCC |  10.62 ± 00.15 |    70.57 ± 00.07 |    225.50 ± 05.79 |
|               C# Mono |  11.29 ± 00.59 |    91.36 ± 00.08 |    208.68 ± 26.48 |
|    TruffleRuby Native |  15.89 ± 00.64 |   648.72 ± 02.29 |    358.43 ± 38.17 |
|       TruffleRuby JVM |  38.25 ± 00.85 |   872.92 ± 52.37 |    826.20 ± 86.89 |
|              Ruby JIT | 214.26 ± 07.09 |    83.72 ± 00.07 |  4178.38 ± 441.51 |
|                  Ruby | 224.61 ± 09.11 |    83.13 ± 00.07 |  4062.45 ± 276.03 |
|                Python | 261.31 ± 09.81 |    78.66 ± 00.05 |  5082.66 ± 523.56 |
|                   Tcl | 341.85 ± 10.16 |   280.53 ± 00.07 | 7283.74 ± 1013.59 |
|                  Perl | 397.39 ± 07.93 |   608.40 ± 00.11 | 7998.07 ± 2258.72 |
|                 JRuby | 491.54 ± 16.43 | 1036.33 ± 110.08 | 11293.54 ± 369.69 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |     Memory, MiB |        Energy, J |
| :----------- | -------------: | --------------: | ---------------: |
|      Crystal |   6.86 ± 00.11 |  225.44 ± 07.45 |   160.35 ± 07.28 |
|      Nim GCC |  11.60 ± 00.28 |  484.67 ± 10.82 |   274.64 ± 24.07 |
|    Nim Clang |  11.95 ± 00.36 |  478.79 ± 04.87 |   286.50 ± 24.10 |
|          C++ |  14.90 ± 01.10 |  179.87 ± 00.06 |   285.13 ± 49.60 |
|           Go |  18.83 ± 00.61 |  349.96 ± 14.30 |   428.17 ± 28.49 |
|          LDC |  19.30 ± 00.62 |  469.63 ± 20.16 |   467.36 ± 31.51 |
|        Scala |  19.53 ± 02.23 | 832.01 ± 243.86 |   539.82 ± 43.30 |
|          GDC |  24.92 ± 00.68 |  418.99 ± 01.02 |   512.37 ± 37.08 |
|      C# Mono |  25.17 ± 01.63 |  336.06 ± 07.23 |   560.03 ± 85.99 |
|         PyPy |  28.67 ± 00.94 |  679.14 ± 57.48 |   652.00 ± 56.38 |
|          DMD |  28.94 ± 02.24 |  440.05 ± 03.37 |   381.57 ± 78.07 |
|       GCC Go |  29.60 ± 00.63 |  407.00 ± 18.69 |   712.68 ± 34.59 |
| C# .NET Core |  35.01 ± 01.93 |  472.74 ± 50.59 |  700.46 ± 113.09 |
|       Python | 105.48 ± 02.25 |  408.88 ± 00.06 | 2254.97 ± 134.60 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.1.103                         |
| C# .NET Core | 3.4.1-beta4-20127-10 (d8180a5e) |
| C# Mono      | 6.8.0.105                       |
| Chez Scheme  | 9.5                             |
| Clang        | 9.0.1                           |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.34.0                          |
| DMD          | v2.091.1                        |
| Elixir       | 1.9.1                           |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 9.3.0                           |
| GCC Go       | 9.3.0                           |
| GDC          | 9.3.0                           |
| Go           | go1.14.2                        |
| Haskell      | 8.10.1                          |
| JRuby        | 9.2.11.1                        |
| Java         | 14.0.1                          |
| Julia        | v"1.4.1"                        |
| Kotlin       | 1.3.72                          |
| LDC          | 1.21.0                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.2.0                           |
| Node.js      | v14.0.0                         |
| OCaml        | 4.10.0                          |
| PHP          | 7.3.15-3                        |
| Perl         | v5.30.0                         |
| PyPy         | 7.3.1-final0 for Python 3.6.9   |
| Python       | 3.8.2                           |
| Racket       | "7.6"                           |
| Ruby         | 2.7.1p83                        |
| Rust         | 1.43.0                          |
| Scala        | 2.13.2                          |
| Swift        | swift-5.2.2-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 20.0.0                          |
| V            | 0.1.24                          |
| Vala         | 0.48.2                          |

## Using Docker

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

Please note that the actual measurements provided in the project are taken semi-manually (via `shell`) as the full update takes days and could have occassional issues in Docker.

## Manual Execution

The tests should be built first (using `build.sh`) and after that executed (using `run.sh` and `run2.sh` where applicable). The measurements are taken using `analyze.rb` script:

    $ cd <test>
    $ ../analyze.rb ./run.sh
    $ ../analyze.rb ../xtime.rb <single test>

Please note that the measurements could take hours (it uses 10 iterations by default).

### Prerequisites

Please use [Dockerfile](docker/Dockerfile) as a reference regarding which packages and tools are required.

For all (optional):

 - [Powercap](https://github.com/powercap/powercap) for reading energy counters in Linux (Debian package `powercap-utils`)

For Python:

 - [NumPy](https://numpy.org/) for matmul tests (Debian package `python3-numpy`)
 - [UltraJSON](https://pypi.org/project/ujson/) for JSON tests (Debian package `python3-ujson`)

For C++:

 - [Boost](https://www.boost.org/) for JSON tests (Debian package `libboost-dev`)
 - [JSON-C](https://github.com/json-c/json-c) for JSON tests (Debian package `libjson-c-dev`)

For Rust:

 - [libjq](https://stedolan.github.io/jq/) for jq test (Debian packages `libjq-dev`, `libonig-dev` and environment variable `JQ_LIB_DIR=/usr/lib/x86_64-linux-gnu/`)

For Java, Scala:

 - [Coursier](https://get-coursier.io/) for downloading Maven artifacts

For Lua:

 - [LuaSocket](http://w3.impa.br/~diego/software/luasocket/) for TCP connectivity between the tests and the test runner (Debian package `lua-socket`)
 - [luaposix](http://luaposix.github.io/luaposix/) for getting PID of the tests (Debian package `lua-posix`)

For Haskell:

 - [network-simple](http://hackage.haskell.org/package/network-simple) for TCP connectivity between the tests and the test runner

# Contribution

Please follow the criteria specified in the [Overview](#overview). Besides that please ensure that the communication protocol between a test and the test runner is satisfied:

 - The test runner listens on localhost:9001;
 - All messages are sent using TCP sockets closed immediately after the message has been sent;
 - There are two messages sent from a test (it establishes the measurement boundary):
  1. The beginning message having the format *name of the test*/t*process ID* (the process ID is used to measure the memory consumption). Please note that the name of the test couldn't use Tab character as it's a delimiter;
  2. The end message with any content (mostly it's "stop" for consistency).
 - The test runner could be unavailable (if the test is launched as is) and the test should gracefully handle it.
