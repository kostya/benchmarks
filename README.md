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

UPDATE: 2020-01-09

# Test Cases

## Brainfuck v2.0

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck v2.0](brainfuck2)
[Brainfuck v1.0](brainfuck)

### bench.b

|        Language |        Time, s |    Memory, MiB |          Energy, J |
| :-------------- | -------------: | -------------: | -----------------: |
|          Kotlin |   2.01 ± 00.03 |  39.79 ± 00.10 |      52.40 ± 01.25 |
|         Nim GCC |   2.19 ± 00.02 |   1.92 ± 00.04 |      58.50 ± 02.40 |
|             C++ |   2.25 ± 00.08 |   3.64 ± 00.08 |      60.07 ± 03.83 |
|        Vala GCC |   2.29 ± 00.19 |   4.95 ± 00.04 |      61.24 ± 06.66 |
|           OCaml |   2.53 ± 00.01 |   5.19 ± 00.05 |      65.70 ± 01.26 |
|              Go |   2.93 ± 00.01 |   3.46 ± 00.15 |      77.78 ± 01.97 |
|          GCC Go |   3.01 ± 00.38 |  19.69 ± 00.79 |      74.44 ± 22.82 |
|         Crystal |   3.07 ± 00.03 |   3.26 ± 00.07 |      80.89 ± 02.69 |
|            Java |   3.19 ± 00.28 |  39.10 ± 00.10 |      83.72 ± 06.34 |
|           MLton |   3.22 ± 00.01 |   0.73 ± 00.03 |      86.26 ± 01.87 |
|            Rust |   3.40 ± 00.04 |   2.14 ± 00.06 |      89.82 ± 04.20 |
|       Nim Clang |   3.45 ± 00.03 |   2.36 ± 00.05 |      89.09 ± 01.47 |
|      Vala Clang |   3.51 ± 00.02 |   4.87 ± 00.34 |      90.82 ± 02.24 |
|             GDC |   3.53 ± 00.04 |   6.56 ± 00.06 |      92.56 ± 01.33 |
|             LDC |   3.57 ± 00.04 |   3.06 ± 00.06 |      90.72 ± 01.77 |
|           Julia |   3.91 ± 00.03 | 168.14 ± 01.11 |     102.41 ± 03.16 |
|           Scala |   4.29 ± 00.04 | 135.44 ± 15.33 |     122.11 ± 04.16 |
|    C# .NET Core |   4.31 ± 00.01 |  29.69 ± 00.13 |     110.41 ± 03.06 |
|             DMD |   4.85 ± 00.02 |   3.65 ± 00.07 |     125.25 ± 03.02 |
|  Haskell MArray |   4.96 ± 00.01 |   5.35 ± 00.08 |     124.97 ± 34.38 |
|    F# .NET Core |   5.20 ± 00.03 |  36.92 ± 00.12 |     128.44 ± 02.32 |
|         Node.js |   5.96 ± 00.03 |  33.67 ± 00.20 |     151.71 ± 02.28 |
|         C# Mono |   6.85 ± 00.07 |  20.67 ± 00.09 |     176.13 ± 04.51 |
|           V GCC |   7.37 ± 00.16 |   0.70 ± 00.04 |     182.53 ± 04.00 |
|         V Clang |   9.09 ± 00.11 |   1.04 ± 00.07 |     223.01 ± 03.88 |
|          Racket |  10.27 ± 00.18 | 107.01 ± 00.09 |     272.60 ± 05.86 |
|          LuaJIT |  11.38 ± 00.74 |   3.02 ± 00.08 |     289.56 ± 20.03 |
|            PyPy |  19.81 ± 00.80 |  90.78 ± 00.33 |     534.50 ± 25.61 |
|     Chez Scheme |  24.65 ± 00.28 |  29.63 ± 00.08 |     610.17 ± 08.64 |
| TruffleRuby JVM |  28.21 ± 00.27 | 779.25 ± 44.97 |   1011.36 ± 477.53 |
|         Haskell |  29.55 ± 00.74 |   5.36 ± 00.07 |     760.33 ± 52.90 |
|           JRuby | 180.16 ± 09.61 | 254.88 ± 18.74 |   4975.36 ± 240.23 |
|             Lua | 202.11 ± 01.43 |   3.11 ± 00.09 |    5024.84 ± 61.73 |
|            Ruby | 202.26 ± 02.27 |  13.99 ± 00.07 |    5136.54 ± 78.33 |
|          Elixir | 272.06 ± 02.57 |  50.53 ± 00.79 |   6837.75 ± 121.56 |
|          Python | 393.68 ± 05.91 |  10.26 ± 00.02 |  10261.17 ± 194.43 |
|        Tcl (FP) | 486.57 ± 06.73 |   4.33 ± 00.05 |  12510.32 ± 224.40 |
|            Perl | 766.05 ± 07.47 |   6.36 ± 00.06 |  20075.60 ± 306.32 |
|        Tcl (OO) | 972.01 ± 08.58 |   4.32 ± 00.04 | 23629.42 ± 2315.38 |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

|        Language |        Time, s |    Memory, MiB |         Energy, J |
| :-------------- | -------------: | -------------: | ----------------: |
|        Vala GCC |  19.35 ± 00.30 |   5.79 ± 00.09 |    492.49 ± 09.66 |
|             C++ |  22.93 ± 00.14 |   4.21 ± 00.05 |    575.49 ± 05.73 |
|           V GCC |  25.14 ± 00.71 |   2.42 ± 00.06 |    642.38 ± 17.80 |
|         Crystal |  25.30 ± 00.16 |   3.74 ± 00.05 |    648.93 ± 04.29 |
|         Nim GCC |  25.51 ± 00.18 |   2.43 ± 00.04 |    623.75 ± 06.08 |
|          Kotlin |  27.07 ± 00.37 |  45.99 ± 00.66 |    699.37 ± 05.90 |
|      Vala Clang |  29.42 ± 00.26 |   5.82 ± 00.09 |    754.41 ± 11.39 |
|            Java |  29.45 ± 01.75 |  45.79 ± 00.27 |    775.76 ± 44.77 |
|           Scala |  30.95 ± 00.21 | 139.00 ± 13.72 |    844.12 ± 33.41 |
|            Rust |  31.10 ± 00.35 |   2.42 ± 00.02 |    786.58 ± 11.68 |
|             LDC |  31.63 ± 00.21 |   3.97 ± 00.04 |   736.19 ± 161.00 |
|       Nim Clang |  31.71 ± 00.48 |   2.89 ± 00.04 |    795.82 ± 15.49 |
|             GDC |  31.96 ± 00.24 |   7.39 ± 00.09 |    812.72 ± 09.21 |
|           MLton |  32.46 ± 00.73 |   3.84 ± 00.05 |    840.48 ± 21.67 |
|          GCC Go |  32.76 ± 00.47 |  27.18 ± 06.22 |    849.19 ± 13.27 |
|    C# .NET Core |  35.85 ± 00.19 |  31.46 ± 00.09 |    902.29 ± 09.92 |
|         V Clang |  37.53 ± 00.25 |   3.19 ± 00.06 |    965.64 ± 10.14 |
|              Go |  45.41 ± 00.19 |   4.79 ± 00.08 |  1096.38 ± 295.26 |
|           OCaml |  47.73 ± 00.50 |  13.11 ± 03.19 |   1275.57 ± 15.95 |
|         Node.js |  57.78 ± 00.14 |  36.26 ± 00.35 |   1443.78 ± 09.79 |
|             DMD |  57.83 ± 00.99 |   4.55 ± 00.07 |   1437.82 ± 31.34 |
|         C# Mono |  71.50 ± 00.70 |  21.53 ± 00.08 |   1826.08 ± 24.21 |
|           Julia |  77.07 ± 00.43 | 168.41 ± 00.80 |   1968.86 ± 19.36 |
|  Haskell MArray |  97.70 ± 00.57 |   6.56 ± 00.12 |  2568.68 ± 113.59 |
|            PyPy | 106.03 ± 00.95 |  91.15 ± 00.18 |  2787.26 ± 119.98 |
|          LuaJIT | 109.08 ± 01.45 |   3.94 ± 00.06 |   2623.31 ± 39.63 |
|    F# .NET Core | 157.80 ± 09.26 |  39.88 ± 00.11 |  3939.25 ± 203.35 |
|          Racket | 171.21 ± 01.00 | 107.16 ± 00.69 |  4369.42 ± 470.61 |
| TruffleRuby JVM | 186.68 ± 02.26 | 828.89 ± 35.34 | 4462.33 ± 1621.33 |
|     Chez Scheme | 246.63 ± 01.13 |  29.70 ± 00.07 | 5861.64 ± 1430.51 |

## Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |    Memory, MiB |      Energy, J |
| :---------------------- | ------------: | -------------: | -------------: |
|                C aklomp |  0.33 ± 00.00 |   2.17 ± 00.04 |   8.73 ± 00.33 |
|                    Rust |  1.83 ± 00.01 |   2.68 ± 00.07 |  48.76 ± 01.50 |
|                       C |  1.86 ± 00.04 |   2.14 ± 00.05 |  48.09 ± 01.55 |
|                 Crystal |  2.37 ± 00.01 |   5.17 ± 00.04 |  88.05 ± 02.53 |
|                     LDC |  2.44 ± 00.01 |  11.10 ± 00.05 |  76.92 ± 02.46 |
|                    Ruby |  2.62 ± 00.06 |  73.34 ± 00.10 |  69.10 ± 01.93 |
|                    Java |  3.03 ± 00.02 | 344.54 ± 25.61 |  86.78 ± 02.26 |
|                     GDC |  3.07 ± 00.01 |  10.92 ± 00.09 |  79.89 ± 01.50 |
|                   Scala |  3.21 ± 00.03 | 155.31 ± 13.03 |  90.48 ± 02.22 |
|                 V Clang |  3.22 ± 00.06 |   2.37 ± 00.09 |  81.83 ± 01.78 |
|                  Kotlin |  3.26 ± 00.02 | 357.84 ± 27.08 |  93.46 ± 02.22 |
|                 Nim GCC |  3.30 ± 00.01 |   7.63 ± 00.05 |  83.50 ± 01.96 |
|       Perl MIME::Base64 |  3.31 ± 00.04 |   7.24 ± 00.06 |  86.95 ± 01.40 |
|                 Node.js |  3.46 ± 00.02 | 117.50 ± 01.27 |  98.04 ± 02.77 |
|               Nim Clang |  3.74 ± 00.10 |   7.96 ± 00.05 |  95.89 ± 03.85 |
|                   V GCC |  3.93 ± 00.08 |   1.83 ± 00.06 |  98.10 ± 01.55 |
|                     PHP |  3.94 ± 00.09 |  15.54 ± 00.16 |  99.44 ± 03.91 |
|           C++ libcrypto |  4.04 ± 00.04 |   5.97 ± 00.11 | 105.75 ± 01.26 |
|            C# .NET Core |  4.65 ± 00.05 |  33.15 ± 00.11 | 116.96 ± 01.24 |
|                      Go |  5.11 ± 00.05 |  14.29 ± 01.91 | 105.45 ± 01.79 |
|                     DMD |  5.74 ± 00.02 |  11.56 ± 00.05 | 152.88 ± 03.89 |
|                     Tcl |  5.75 ± 00.05 |   5.12 ± 00.03 | 146.01 ± 01.62 |
|                    PyPy |  6.57 ± 00.01 |  92.13 ± 00.34 | 171.16 ± 03.00 |
|                  Python |  6.77 ± 00.08 |  10.29 ± 00.07 | 166.07 ± 03.10 |
|                  GCC Go |  6.91 ± 00.08 |  44.07 ± 01.81 | 150.48 ± 02.51 |
|                 C# Mono |  8.84 ± 00.08 |  39.83 ± 00.13 | 225.02 ± 01.92 |
|         TruffleRuby JVM |  9.83 ± 00.29 | 584.80 ± 20.41 | 270.75 ± 12.72 |
|                   Julia | 10.33 ± 00.14 | 238.74 ± 06.61 | 265.49 ± 07.55 |
|                   JRuby | 16.08 ± 00.36 | 208.83 ± 07.07 | 426.74 ± 10.46 |
| Perl MIME::Base64::Perl | 28.45 ± 00.29 |   8.95 ± 00.08 | 723.18 ± 13.87 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |     Memory, MiB |        Energy, J |
| :-------------------- | ------------: | --------------: | ---------------: |
|     Rust Serde Custom |  0.19 ± 00.00 |  108.54 ± 00.07 |     5.14 ± 00.06 |
|      Rust Serde Typed |  0.20 ± 00.00 |  120.33 ± 00.13 |     5.74 ± 00.73 |
|              DMD fast |  0.25 ± 00.00 |  228.69 ± 00.13 |     5.92 ± 00.05 |
|          C++ simdjson |  0.31 ± 00.00 |  496.82 ± 00.92 |     8.29 ± 00.46 |
|             C++ gason |  0.32 ± 00.00 |  313.18 ± 00.09 |     8.28 ± 00.42 |
|         C++ RapidJSON |  0.36 ± 00.00 |  345.17 ± 00.09 |    11.32 ± 00.29 |
|                  Java |  0.49 ± 00.01 |  569.25 ± 24.45 |    14.19 ± 00.74 |
|                 Scala |  0.57 ± 00.00 |  461.18 ± 04.73 |    19.27 ± 00.86 |
|           Julia JSON3 |  0.83 ± 00.01 |  834.65 ± 00.68 |    22.72 ± 01.15 |
|     C++ RapidJSON SAX |  0.89 ± 00.00 |  110.19 ± 00.04 |    23.35 ± 00.63 |
|           Go jsoniter |  0.98 ± 00.01 |  225.83 ± 00.13 |    26.63 ± 00.76 |
|               Node.js |  0.98 ± 00.00 |  293.57 ± 03.03 |    37.35 ± 01.44 |
|    Rust Serde Untyped |  1.04 ± 00.08 |  916.54 ± 00.06 |    27.79 ± 01.44 |
|                  PyPy |  1.07 ± 00.00 |  405.69 ± 00.23 |    29.33 ± 00.85 |
| Perl Cpanel::JSON::XS |  1.29 ± 00.02 |  517.07 ± 00.03 |    35.33 ± 00.53 |
|        Crystal Schema |  1.43 ± 00.01 |  157.33 ± 00.12 |    39.11 ± 01.87 |
|          Crystal Pull |  1.44 ± 00.02 |  128.49 ± 00.03 |    40.26 ± 02.61 |
|               Clojure |  1.60 ± 00.04 | 1542.98 ± 20.03 |    64.27 ± 01.99 |
|                   PHP |  1.65 ± 00.02 |  803.43 ± 00.08 |    42.46 ± 01.94 |
|               Crystal |  1.74 ± 00.01 |  503.61 ± 00.03 |    59.80 ± 02.46 |
|     CPython UltraJSON |  2.06 ± 00.01 |  689.01 ± 01.60 |    66.12 ± 00.57 |
|                    Go |  2.08 ± 00.01 |  400.68 ± 00.14 |    58.15 ± 01.63 |
|                 V GCC |  2.08 ± 00.01 |  591.95 ± 00.05 |    54.50 ± 00.48 |
|               V Clang |  2.08 ± 00.01 |  592.46 ± 00.02 |    54.88 ± 00.66 |
|                Python |  2.33 ± 00.01 |  519.47 ± 00.04 |    63.84 ± 02.25 |
|            C++ json-c |  2.70 ± 00.02 | 1756.22 ± 00.06 |    70.98 ± 01.54 |
|               Nim GCC |  2.99 ± 00.01 |  908.77 ± 00.05 |    78.37 ± 00.66 |
|             Nim Clang |  3.06 ± 00.01 |  909.29 ± 00.04 |    80.44 ± 01.25 |
|          C# .NET Core |  3.15 ± 00.06 |  287.88 ± 00.18 |    84.17 ± 01.90 |
|                  Ruby |  3.53 ± 00.10 |  396.80 ± 00.06 |    91.61 ± 02.74 |
|                   LDC |  3.84 ± 00.01 |  796.27 ± 00.11 |    88.35 ± 00.78 |
|             Ruby YAJL |  3.89 ± 00.07 |  406.18 ± 00.04 |   101.48 ± 02.53 |
|               Haskell |  4.27 ± 00.08 |   10.45 ± 00.16 |   109.23 ± 02.89 |
|                GCC Go |  4.33 ± 00.21 |  530.71 ± 00.60 |   125.14 ± 08.23 |
|                   GDC |  4.82 ± 00.02 |  713.69 ± 00.12 |   125.24 ± 01.42 |
|               Rust jq |  4.91 ± 00.01 |  886.42 ± 01.01 |   128.78 ± 01.18 |
|                 JRuby |  5.34 ± 00.13 | 1870.10 ± 30.67 |   234.33 ± 07.50 |
|               C# Mono |  5.52 ± 00.14 |  329.70 ± 00.08 |   142.39 ± 04.19 |
|             C++ Boost |  5.81 ± 00.10 | 1549.84 ± 00.08 |   152.27 ± 03.34 |
|                   DMD |  7.17 ± 00.01 |  796.85 ± 00.08 |   168.60 ± 01.05 |
|   C# System.Text.Json | 10.59 ± 00.26 |  608.33 ± 00.17 |   273.88 ± 07.94 |
|       Perl JSON::Tiny | 25.31 ± 00.24 |  640.24 ± 00.08 |  624.18 ± 142.93 |
|       TruffleRuby JVM | 28.34 ± 00.63 | 1696.62 ± 12.48 | 1203.36 ± 133.72 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |     Memory, MiB |          Energy, J |
| :-------------------- | -------------: | --------------: | -----------------: |
| Nim Clang Arraymancer |   0.11 ± 00.00 |   60.66 ± 00.33 |       9.04 ± 00.20 |
|   Nim GCC Arraymancer |   0.12 ± 00.00 |   60.78 ± 00.26 |       9.86 ± 00.06 |
|            DMD lubeck |   0.12 ± 00.01 |   65.72 ± 00.10 |       9.34 ± 00.38 |
|    Julia (threads: 8) |   0.12 ± 00.00 |  271.86 ± 00.44 |       8.56 ± 00.12 |
|          Python NumPy |   0.14 ± 00.00 |   87.42 ± 00.13 |       9.29 ± 00.14 |
|             Java ND4J |   0.20 ± 00.02 |  236.76 ± 04.63 |      13.80 ± 00.85 |
|    Julia (threads: 1) |   0.29 ± 00.00 |  270.42 ± 00.28 |      10.82 ± 00.67 |
|                   LDC |   1.95 ± 00.02 |   73.36 ± 00.05 |      59.91 ± 01.22 |
|                   DMD |   2.20 ± 00.02 |   73.37 ± 00.13 |      67.34 ± 01.36 |
|                   GDC |   2.30 ± 00.02 |   77.11 ± 00.09 |      69.25 ± 01.59 |
|                     C |   3.11 ± 00.01 |   70.30 ± 00.05 |      82.29 ± 01.42 |
|                  Rust |   3.16 ± 00.01 |   70.98 ± 00.05 |      84.90 ± 01.40 |
|               Nim GCC |   3.21 ± 00.01 |   78.12 ± 05.91 |      85.28 ± 02.30 |
|             Nim Clang |   3.21 ± 00.01 |   82.48 ± 06.80 |      86.67 ± 01.33 |
|       Julia (no BLAS) |   3.24 ± 00.01 |  252.92 ± 00.17 |      89.80 ± 01.85 |
|               Crystal |   3.32 ± 00.01 |   63.71 ± 00.06 |      95.48 ± 03.41 |
|                 Swift |   3.32 ± 00.01 |  195.64 ± 00.18 |      91.80 ± 00.43 |
|                    Go |   3.35 ± 00.03 |   68.38 ± 07.83 |      92.60 ± 02.73 |
|                GCC Go |   3.39 ± 00.02 |  104.87 ± 01.83 |      92.47 ± 01.79 |
|                Kotlin |   3.44 ± 00.01 |  130.04 ± 00.15 |      98.74 ± 01.21 |
|               V Clang |   3.47 ± 00.01 |   70.98 ± 00.05 |      96.91 ± 03.83 |
|                 Scala |   3.73 ± 00.02 |  158.35 ± 07.78 |      99.87 ± 01.19 |
|                  Java |   3.80 ± 00.02 |  127.61 ± 00.15 |     106.99 ± 01.54 |
|                 V GCC |   4.21 ± 00.01 |   70.47 ± 00.06 |     111.95 ± 00.71 |
|               Node.js |   4.36 ± 00.07 |  104.80 ± 00.13 |     119.85 ± 00.76 |
|                  PyPy |   7.86 ± 00.02 |  131.10 ± 00.11 |     198.70 ± 02.67 |
|          C# .NET Core |   9.18 ± 00.01 |  105.34 ± 00.14 |     242.16 ± 03.19 |
|               C# Mono |  14.72 ± 00.18 |   91.84 ± 00.08 |     390.32 ± 06.61 |
|       TruffleRuby JVM |  55.01 ± 00.45 |  861.45 ± 50.34 |   1261.01 ± 296.90 |
|                  Ruby | 387.43 ± 02.43 |   83.07 ± 00.06 |  8588.18 ± 2880.28 |
|                 JRuby | 511.75 ± 37.27 | 1118.73 ± 60.81 | 12295.28 ± 2308.65 |
|                Python | 575.30 ± 06.78 |   79.04 ± 00.05 | 13655.00 ± 3029.43 |
|                   Tcl | 583.29 ± 04.95 |  280.60 ± 00.11 | 12426.05 ± 4797.48 |
|                  Perl | 658.70 ± 03.72 |  608.42 ± 00.09 | 14585.76 ± 3310.24 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------- | -------------: | -------------: | ---------------: |
|      Crystal |  10.80 ± 00.03 | 230.95 ± 01.17 |   334.84 ± 06.39 |
|          C++ |  17.30 ± 00.29 | 180.31 ± 00.06 |   419.36 ± 10.87 |
|      Nim GCC |  17.70 ± 00.09 | 490.47 ± 13.75 |   459.06 ± 04.38 |
|    Nim Clang |  18.37 ± 00.06 | 487.22 ± 05.70 |   474.89 ± 02.39 |
|           Go |  24.84 ± 00.10 | 358.29 ± 09.37 |   724.58 ± 11.98 |
|        Scala |  25.28 ± 00.30 | 381.02 ± 05.13 |  956.24 ± 336.58 |
|          LDC |  25.35 ± 00.25 | 475.57 ± 26.08 |   699.41 ± 11.02 |
|          DMD |  30.52 ± 00.09 | 440.63 ± 01.11 |   844.29 ± 08.26 |
|          GDC |  32.82 ± 00.13 | 419.08 ± 01.12 |   813.39 ± 06.13 |
|      C# Mono |  38.07 ± 00.25 | 324.17 ± 03.09 |  1165.94 ± 23.72 |
|       GCC Go |  40.35 ± 00.43 | 407.94 ± 15.90 |  1204.43 ± 18.00 |
|         PyPy |  41.90 ± 00.85 | 598.79 ± 49.40 |  1082.30 ± 26.54 |
| C# .NET Core |  43.32 ± 00.30 | 542.44 ± 15.92 |  1110.24 ± 10.24 |
|       Python | 172.09 ± 00.58 | 466.74 ± 00.04 | 4343.91 ± 354.48 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.1.100                         |
| C# .NET Core | 3.4.0-beta4-19562-05 (ff930dec) |
| C# Mono      | 6.6.0.161                       |
| Chez Scheme  | 9.5                             |
| Clang        | 9.0.1                           |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.32.1                          |
| DMD          | v2.090.0                        |
| Elixir       | 1.9.1                           |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 9.2.1                           |
| GCC Go       | 9.2.1                           |
| GDC          | 9.2.1                           |
| Go           | go1.13.5                        |
| Haskell      | 8.8.1                           |
| JRuby        | 9.2.9.0                         |
| Java         | 13.0.1                          |
| Julia        | v"1.3.1"                        |
| Kotlin       | 1.3.61                          |
| LDC          | 1.19.0                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.0.4                           |
| Node.js      | v13.5.0                         |
| OCaml        | 4.09.0                          |
| PHP          | 7.3.12-1                        |
| Perl         | v5.30.0                         |
| PyPy         | 7.3.0-final0 for Python 3.6.9   |
| Python       | 3.7.6                           |
| Racket       | "7.5"                           |
| Ruby         | 2.7.0p0                         |
| Rust         | 1.40.0                          |
| Scala        | 2.13.1                          |
| Swift        | swift-5.1.3-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 19.3.0.2                        |
| V            | 0.1.24 8c59232                  |
| Vala         | 0.46.5                          |

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

For C, C++, Chez Scheme:

 - [libsocket](https://github.com/dermesser/libsocket/) for TCP connectivity between the tests and the test runner

# Contribution

Please follow the criteria specified in the [Overview](#overview). Besides that please ensure that the communication protocol between a test and the test runner is satisfied:

 - The test runner listens on localhost:9001;
 - All messages are sent using TCP sockets closed immediately after the message has been sent;
 - There are two messages sent from a test (it establishes the measurement boundary):
  1. The beginning message having the format *name of the test*/t*process ID* (the process ID is used to measure the memory consumption). Please note that the name of the test couldn't use Tab character as it's a delimiter;
  2. The end message with any content (mostly it's "stop" for consistency).
 - The test runner could be unavailable (if the test is launched as is) and the test should gracefully handle it.
