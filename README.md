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
|          Kotlin |   2.03 ± 00.03 |  39.67 ± 00.08 |      53.42 ± 02.24 |
|           C GCC |   2.18 ± 00.01 |   0.72 ± 00.05 |      57.55 ± 01.68 |
|         Nim GCC |   2.19 ± 00.01 |   1.91 ± 00.04 |      57.38 ± 01.09 |
|        Vala GCC |   2.27 ± 00.02 |   4.91 ± 00.04 |      61.19 ± 04.73 |
|             C++ |   2.42 ± 00.54 |   1.71 ± 00.07 |      62.82 ± 12.76 |
|           OCaml |   2.54 ± 00.01 |   5.17 ± 00.07 |      67.13 ± 01.89 |
|            Java |   2.85 ± 00.20 |  39.02 ± 00.06 |      76.35 ± 04.72 |
|              Go |   2.93 ± 00.01 |   3.50 ± 00.13 |      78.90 ± 01.98 |
|         Crystal |   3.13 ± 00.20 |   3.26 ± 00.05 |      81.82 ± 02.82 |
|           MLton |   3.22 ± 00.01 |   0.75 ± 00.04 |      85.96 ± 01.13 |
|          GCC Go |   3.38 ± 00.66 |  19.19 ± 00.66 |      88.79 ± 15.17 |
|            Rust |   3.42 ± 00.04 |   2.12 ± 00.07 |      89.75 ± 02.20 |
|       Nim Clang |   3.47 ± 00.03 |   2.35 ± 00.06 |      89.14 ± 01.45 |
|             GDC |   3.55 ± 00.04 |   6.47 ± 00.07 |      93.56 ± 01.71 |
|             LDC |   3.57 ± 00.05 |   3.06 ± 00.07 |      91.05 ± 02.14 |
|         C Clang |   3.59 ± 00.01 |   0.71 ± 00.04 |      92.79 ± 01.32 |
|      Vala Clang |   3.66 ± 00.22 |   4.77 ± 00.46 |      97.16 ± 09.69 |
|           Julia |   3.93 ± 00.05 | 168.03 ± 00.95 |     102.06 ± 01.65 |
|           Scala |   4.34 ± 00.10 | 135.57 ± 09.62 |     122.64 ± 05.78 |
|    C# .NET Core |   4.35 ± 00.07 |  29.54 ± 00.08 |     114.67 ± 12.24 |
|             DMD |   4.85 ± 00.01 |   3.63 ± 00.04 |     124.73 ± 01.68 |
|  Haskell MArray |   4.97 ± 00.02 |   5.34 ± 00.08 |     134.41 ± 08.95 |
|    F# .NET Core |   5.26 ± 00.08 |  36.68 ± 00.11 |     130.64 ± 01.91 |
|         Node.js |   5.99 ± 00.05 |  33.71 ± 00.10 |     154.02 ± 02.40 |
|         C# Mono |   6.98 ± 00.12 |  20.55 ± 00.09 |     178.42 ± 05.40 |
|           V GCC |   7.85 ± 00.32 |   0.71 ± 00.04 |     192.62 ± 08.79 |
|         V Clang |   9.48 ± 00.09 |   1.06 ± 00.06 |     233.81 ± 02.86 |
|          Racket |  10.16 ± 00.08 | 107.16 ± 00.63 |     270.47 ± 03.83 |
|          LuaJIT |  11.02 ± 00.17 |   3.01 ± 00.09 |     283.36 ± 07.23 |
|            PyPy |  19.41 ± 00.23 |  90.63 ± 00.40 |     524.35 ± 05.84 |
|     Chez Scheme |  24.14 ± 00.06 |  29.36 ± 00.08 |     605.19 ± 05.35 |
| TruffleRuby JVM |  28.21 ± 00.22 | 762.57 ± 32.60 |    1336.26 ± 14.97 |
|         Haskell |  29.10 ± 00.43 |   5.28 ± 00.09 |     777.61 ± 16.60 |
|           JRuby | 183.90 ± 09.21 | 261.64 ± 19.03 |  4648.57 ± 1237.38 |
|             Lua | 202.86 ± 01.19 |   3.11 ± 00.04 |    5073.38 ± 64.18 |
|            Ruby | 204.66 ± 02.74 |  13.91 ± 00.03 |  4793.80 ± 1591.23 |
|          Elixir | 274.65 ± 03.07 |  50.05 ± 00.74 |  6208.59 ± 1500.25 |
|          Python | 393.82 ± 04.37 |  10.30 ± 00.07 |  9699.05 ± 1591.28 |
|        Tcl (FP) | 488.98 ± 02.77 |   4.32 ± 00.06 |  9479.28 ± 5276.29 |
|            Perl | 767.00 ± 06.46 |   6.39 ± 00.07 | 20027.13 ± 1010.61 |
|        Tcl (OO) | 968.92 ± 05.47 |   4.30 ± 00.07 | 24851.18 ± 2331.96 |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

|        Language |        Time, s |    Memory, MiB |         Energy, J |
| :-------------- | -------------: | -------------: | ----------------: |
|        Vala GCC |  19.26 ± 00.16 |   5.68 ± 00.05 |    492.20 ± 03.77 |
|           C GCC |  20.05 ± 00.31 |   1.62 ± 00.04 |    511.23 ± 10.56 |
|             C++ |  23.00 ± 00.34 |   3.76 ± 00.07 |    576.63 ± 12.26 |
|         Crystal |  25.26 ± 00.13 |   3.65 ± 00.07 |    649.32 ± 08.17 |
|          Kotlin |  27.33 ± 00.49 |  45.63 ± 00.15 |   656.05 ± 152.03 |
|           V GCC |  27.89 ± 00.30 |   2.36 ± 00.06 |    704.07 ± 13.73 |
|            Java |  28.99 ± 02.31 |  45.47 ± 00.23 |   733.88 ± 122.90 |
|            Rust |  29.14 ± 00.21 |   2.38 ± 00.08 |    753.79 ± 13.12 |
|      Vala Clang |  29.46 ± 00.19 |   5.68 ± 00.04 |    753.75 ± 07.68 |
|         C Clang |  30.05 ± 00.13 |   1.64 ± 00.04 |    765.21 ± 10.83 |
|         Nim GCC |  30.49 ± 00.26 |   2.40 ± 00.03 |    734.95 ± 13.88 |
|           Scala |  30.98 ± 00.31 | 143.24 ± 12.40 |    849.71 ± 33.63 |
|       Nim Clang |  31.64 ± 00.12 |   2.84 ± 00.05 |    799.43 ± 11.88 |
|             LDC |  31.68 ± 00.29 |   3.94 ± 00.05 |    791.93 ± 13.84 |
|             GDC |  31.86 ± 00.27 |   7.29 ± 00.08 |   736.87 ± 238.64 |
|           MLton |  32.22 ± 00.28 |   3.82 ± 00.04 |   804.09 ± 110.46 |
|          GCC Go |  32.70 ± 00.22 |  25.88 ± 06.16 |    853.11 ± 05.53 |
|    C# .NET Core |  36.02 ± 00.34 |  30.80 ± 00.07 |    906.80 ± 10.66 |
|         V Clang |  39.14 ± 00.24 |   3.16 ± 00.03 |   1002.32 ± 13.15 |
|           OCaml |  47.83 ± 00.37 |  12.87 ± 03.72 |   1280.93 ± 17.15 |
|              Go |  49.49 ± 00.18 |   4.76 ± 00.10 |   1275.91 ± 13.54 |
|             DMD |  57.37 ± 00.29 |   4.51 ± 00.08 |   1401.47 ± 79.21 |
|         Node.js |  58.52 ± 00.93 |  36.01 ± 00.13 |   1476.36 ± 30.29 |
|           Julia |  77.60 ± 01.18 | 167.65 ± 00.78 |   1993.35 ± 35.75 |
|         C# Mono |  80.88 ± 00.44 |  21.16 ± 00.07 |   2017.67 ± 19.96 |
|  Haskell MArray |  97.96 ± 01.20 |   6.47 ± 00.09 |   2611.83 ± 51.58 |
|            PyPy | 105.96 ± 02.03 |  90.80 ± 00.19 |  2741.97 ± 339.05 |
|          LuaJIT | 109.51 ± 01.26 |   3.87 ± 00.06 |   2660.72 ± 41.35 |
|    F# .NET Core | 154.84 ± 04.56 |  39.59 ± 00.04 |  3873.00 ± 104.37 |
|          Racket | 171.79 ± 02.05 | 107.14 ± 00.64 |   4556.26 ± 79.44 |
| TruffleRuby JVM | 187.81 ± 07.63 | 839.88 ± 40.63 |  5316.59 ± 320.99 |
|     Chez Scheme | 241.02 ± 01.56 |  29.39 ± 00.11 | 5435.48 ± 1833.54 |

## Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |    Memory, MiB |      Energy, J |
| :---------------------- | ------------: | -------------: | -------------: |
|                C aklomp |  0.33 ± 00.00 |   1.91 ± 00.04 |   8.64 ± 00.15 |
|                       C |  1.86 ± 00.01 |   1.88 ± 00.04 |  48.15 ± 00.55 |
|                    Rust |  1.94 ± 00.01 |   2.68 ± 00.06 |  51.67 ± 00.91 |
|                 Crystal |  2.38 ± 00.01 |   5.14 ± 00.05 |  88.24 ± 02.78 |
|                     LDC |  2.44 ± 00.01 |  11.08 ± 00.07 |  78.67 ± 02.19 |
|                    Ruby |  2.63 ± 00.01 |  73.37 ± 00.15 |  69.47 ± 01.04 |
|                    Java |  3.03 ± 00.01 | 346.22 ± 23.05 |  86.17 ± 02.91 |
|                     GDC |  3.08 ± 00.01 |  10.87 ± 00.07 |  80.79 ± 01.38 |
|                   Scala |  3.23 ± 00.02 | 144.28 ± 13.26 |  92.21 ± 02.75 |
|                  Kotlin |  3.26 ± 00.01 | 354.55 ± 32.69 |  93.31 ± 01.64 |
|                 V Clang |  3.28 ± 00.03 |   2.32 ± 00.04 |  83.36 ± 01.08 |
|       Perl MIME::Base64 |  3.33 ± 00.06 |   7.23 ± 00.07 |  87.82 ± 01.99 |
|                 Node.js |  3.45 ± 00.01 | 117.34 ± 01.33 |  98.43 ± 01.15 |
|                 Nim GCC |  3.69 ± 00.01 |   7.60 ± 00.05 |  91.77 ± 01.80 |
|               Nim Clang |  3.72 ± 00.04 |   7.92 ± 00.05 |  95.28 ± 01.85 |
|                   V GCC |  3.91 ± 00.02 |   1.84 ± 00.04 |  91.07 ± 22.31 |
|                     PHP |  3.94 ± 00.01 |  15.41 ± 00.12 | 101.69 ± 01.98 |
|           C++ libcrypto |  4.06 ± 00.01 |   5.52 ± 00.10 | 105.87 ± 01.63 |
|            C# .NET Core |  4.68 ± 00.04 |  32.89 ± 00.05 | 119.70 ± 03.39 |
|                      Go |  4.78 ± 00.06 |  13.32 ± 02.15 | 100.50 ± 02.14 |
|                     Tcl |  5.76 ± 00.01 |   5.08 ± 00.03 | 147.36 ± 01.63 |
|                     DMD |  5.80 ± 00.10 |  11.54 ± 00.05 | 156.96 ± 03.38 |
|                    PyPy |  6.63 ± 00.07 |  91.83 ± 00.27 | 173.72 ± 03.24 |
|                  Python |  6.81 ± 00.06 |  10.29 ± 00.04 | 169.03 ± 02.34 |
|                  GCC Go |  6.93 ± 00.08 |  46.03 ± 06.01 | 153.15 ± 03.71 |
|                 C# Mono |  8.93 ± 00.06 |  39.62 ± 00.12 | 229.07 ± 03.63 |
|         TruffleRuby JVM |  9.87 ± 00.29 | 589.88 ± 40.45 | 274.81 ± 09.66 |
|                   Julia | 10.31 ± 00.05 | 238.37 ± 06.78 | 266.05 ± 04.72 |
|                   JRuby | 16.02 ± 00.08 | 203.94 ± 04.80 | 426.85 ± 06.70 |
| Perl MIME::Base64::Perl | 28.68 ± 00.46 |   8.87 ± 00.05 | 737.21 ± 15.14 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |     Memory, MiB |        Energy, J |
| :-------------------- | ------------: | --------------: | ---------------: |
|     C++ DAW JSON Link |  0.12 ± 00.00 |  109.30 ± 00.09 |     2.88 ± 00.08 |
|              GDC fast |  0.14 ± 00.00 |  231.51 ± 00.09 |     3.68 ± 00.18 |
|     Rust Serde Custom |  0.20 ± 00.00 |  108.51 ± 00.05 |     5.25 ± 00.17 |
|      Rust Serde Typed |  0.20 ± 00.00 |  120.35 ± 00.28 |     5.32 ± 00.09 |
|             C++ gason |  0.32 ± 00.00 |  312.86 ± 00.08 |     8.07 ± 00.24 |
|          C++ simdjson |  0.32 ± 00.00 |  495.92 ± 01.07 |     8.23 ± 00.43 |
|         C++ RapidJSON |  0.37 ± 00.00 |  344.55 ± 00.09 |    11.39 ± 00.18 |
|                  Java |  0.50 ± 00.01 |  568.05 ± 22.55 |    13.85 ± 00.45 |
|                 Scala |  0.57 ± 00.01 |  458.40 ± 03.14 |    18.73 ± 00.86 |
|           Julia JSON3 |  0.84 ± 00.00 |  833.35 ± 00.54 |    22.06 ± 00.33 |
|     C++ RapidJSON SAX |  0.95 ± 00.02 |  109.52 ± 00.06 |    24.18 ± 00.41 |
|               Node.js |  0.97 ± 00.01 |  292.13 ± 01.14 |    34.35 ± 01.21 |
|           Go jsoniter |  0.99 ± 00.00 |  225.74 ± 00.11 |    26.61 ± 00.69 |
|    Rust Serde Untyped |  1.02 ± 00.01 |  916.54 ± 00.07 |    26.81 ± 00.56 |
|                  PyPy |  1.08 ± 00.00 |  405.76 ± 00.32 |    29.01 ± 00.54 |
| Perl Cpanel::JSON::XS |  1.29 ± 00.02 |  517.12 ± 00.04 |    35.50 ± 00.75 |
|          Crystal Pull |  1.45 ± 00.01 |  128.48 ± 00.02 |    38.65 ± 02.04 |
|        Crystal Schema |  1.45 ± 00.03 |  157.25 ± 00.14 |    38.72 ± 01.83 |
|               Clojure |  1.59 ± 00.03 | 1552.36 ± 25.95 |    60.00 ± 01.96 |
|                   PHP |  1.66 ± 00.00 |  803.45 ± 00.09 |    42.91 ± 00.59 |
|               Crystal |  1.72 ± 00.01 |  503.60 ± 00.06 |    54.59 ± 03.27 |
|                    Go |  1.98 ± 00.01 |  209.46 ± 00.19 |    51.36 ± 02.65 |
|                 V GCC |  2.06 ± 00.01 |  591.93 ± 00.05 |    53.57 ± 00.53 |
|     CPython UltraJSON |  2.13 ± 00.01 |  688.58 ± 01.48 |    67.03 ± 01.13 |
|               V Clang |  2.14 ± 00.01 |  592.50 ± 00.05 |    55.61 ± 00.51 |
|           Go jsparser |  2.29 ± 00.03 |  221.28 ± 00.25 |    74.07 ± 01.27 |
|                Python |  2.36 ± 00.01 |  519.48 ± 00.06 |    63.15 ± 01.36 |
|            C++ json-c |  2.70 ± 00.01 | 1755.95 ± 00.04 |    69.74 ± 01.31 |
|                GCC Go |  2.74 ± 00.02 |  206.64 ± 00.40 |    68.64 ± 02.48 |
|               Nim GCC |  3.02 ± 00.01 |  908.82 ± 00.05 |    78.27 ± 01.14 |
|             Nim Clang |  3.11 ± 00.05 |  909.27 ± 00.05 |    80.71 ± 01.91 |
|          C# .NET Core |  3.16 ± 00.04 |  287.75 ± 00.20 |    84.06 ± 02.03 |
|                  Ruby |  3.54 ± 00.10 |  396.79 ± 00.04 |    90.87 ± 01.99 |
|                   LDC |  3.87 ± 00.02 |  796.30 ± 00.12 |    88.03 ± 01.22 |
|             Ruby YAJL |  4.11 ± 00.19 |  406.15 ± 00.04 |   105.06 ± 05.08 |
|               Haskell |  4.21 ± 00.05 |   10.38 ± 00.35 |   106.65 ± 03.00 |
|                   GDC |  4.94 ± 00.24 |  713.68 ± 00.07 |   126.49 ± 07.47 |
|               Rust jq |  4.97 ± 00.03 |  886.91 ± 00.61 |   128.70 ± 01.48 |
|               C# Mono |  5.36 ± 00.11 |  353.88 ± 00.08 |   137.33 ± 03.99 |
|                 JRuby |  5.43 ± 00.07 | 1861.09 ± 51.69 |   231.02 ± 05.25 |
|             C++ Boost |  5.87 ± 00.07 | 1549.62 ± 00.10 |   151.50 ± 02.92 |
|                   DMD |  7.23 ± 00.02 |  796.80 ± 00.10 |   168.23 ± 02.85 |
|   C# System.Text.Json | 10.41 ± 00.17 |  608.35 ± 00.27 |   268.34 ± 05.80 |
|       Perl JSON::Tiny | 25.47 ± 00.17 |  640.26 ± 00.07 |   665.93 ± 06.37 |
|       TruffleRuby JVM | 28.98 ± 00.86 | 1679.96 ± 13.09 | 1283.02 ± 163.73 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |     Memory, MiB |          Energy, J |
| :-------------------- | -------------: | --------------: | -----------------: |
| Nim Clang Arraymancer |   0.11 ± 00.00 |   60.46 ± 00.31 |       9.03 ± 00.24 |
|            LDC lubeck |   0.11 ± 00.00 |   65.25 ± 00.24 |       9.17 ± 00.24 |
|   Nim GCC Arraymancer |   0.12 ± 00.00 |   60.57 ± 00.36 |       9.75 ± 00.29 |
|    Julia (threads: 8) |   0.12 ± 00.00 |  271.21 ± 00.70 |       8.52 ± 00.12 |
|          Python NumPy |   0.14 ± 00.00 |   87.37 ± 00.16 |       9.32 ± 00.12 |
|             Java ND4J |   0.20 ± 00.03 |  237.78 ± 04.29 |      13.81 ± 01.27 |
|    Julia (threads: 1) |   0.29 ± 00.00 |  269.50 ± 00.69 |      10.61 ± 00.33 |
|                   LDC |   1.96 ± 00.00 |   73.36 ± 00.04 |      60.25 ± 01.21 |
|                   DMD |   2.21 ± 00.01 |   73.34 ± 00.11 |      68.43 ± 01.70 |
|                   GDC |   2.31 ± 00.01 |   77.03 ± 00.10 |      69.79 ± 01.35 |
|                     C |   3.12 ± 00.01 |   70.26 ± 00.04 |      83.01 ± 01.85 |
|                  Rust |   3.17 ± 00.01 |   70.95 ± 00.07 |      85.61 ± 01.76 |
|               Nim GCC |   3.21 ± 00.01 |   77.23 ± 04.37 |      85.89 ± 01.84 |
|             Nim Clang |   3.21 ± 00.01 |   78.36 ± 05.96 |      87.26 ± 01.63 |
|       Julia (no BLAS) |   3.24 ± 00.01 |  251.91 ± 00.56 |      90.40 ± 02.08 |
|                 Swift |   3.33 ± 00.01 |  196.29 ± 00.13 |      92.70 ± 01.06 |
|                    Go |   3.34 ± 00.02 |   71.84 ± 07.80 |      92.11 ± 02.03 |
|               Crystal |   3.35 ± 00.07 |   63.65 ± 00.06 |      95.81 ± 02.11 |
|                GCC Go |   3.40 ± 00.01 |  104.99 ± 02.34 |      93.04 ± 01.58 |
|               V Clang |   3.47 ± 00.01 |   70.93 ± 00.02 |      96.25 ± 01.37 |
|                Kotlin |   3.48 ± 00.13 |  129.87 ± 00.22 |     100.17 ± 03.47 |
|                 Scala |   3.72 ± 00.02 |  159.94 ± 09.45 |     100.51 ± 02.17 |
|                  Java |   3.81 ± 00.01 |  127.53 ± 00.17 |     107.72 ± 02.10 |
|                 V GCC |   4.22 ± 00.01 |   70.47 ± 00.04 |     112.74 ± 01.44 |
|               Node.js |   4.33 ± 00.01 |  105.19 ± 00.15 |     119.72 ± 02.00 |
|                  PyPy |   7.86 ± 00.02 |  131.08 ± 00.24 |     198.69 ± 03.55 |
|          C# .NET Core |   9.21 ± 00.09 |  105.09 ± 00.18 |     244.98 ± 01.64 |
|               C# Mono |  14.64 ± 00.20 |   91.63 ± 00.10 |    351.51 ± 123.90 |
|       TruffleRuby JVM |  55.18 ± 00.23 |  886.76 ± 75.42 |    1368.39 ± 13.39 |
|                  Ruby | 400.07 ± 11.53 |   83.07 ± 00.06 |  9642.24 ± 1268.81 |
|                 JRuby | 485.51 ± 37.36 | 1183.10 ± 76.12 | 11515.15 ± 2896.77 |
|                Python | 564.55 ± 05.95 |   79.03 ± 00.04 | 12703.53 ± 4059.95 |
|                   Tcl | 588.47 ± 10.22 |  280.55 ± 00.03 | 13942.99 ± 2867.95 |
|                  Perl | 662.37 ± 08.33 |  608.40 ± 00.07 | 14404.28 ± 3334.17 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |    Memory, MiB |       Energy, J |
| :----------- | -------------: | -------------: | --------------: |
|      Crystal |  10.81 ± 00.02 | 231.62 ± 01.01 |  340.54 ± 07.79 |
|      Nim GCC |  17.63 ± 00.06 | 482.91 ± 08.70 |  457.00 ± 04.97 |
|          C++ |  18.54 ± 00.23 | 179.91 ± 00.04 |  432.92 ± 58.31 |
|    Nim Clang |  18.71 ± 00.15 | 487.21 ± 10.95 |  485.03 ± 06.48 |
|           Go |  24.71 ± 00.10 | 361.37 ± 10.34 |  723.37 ± 08.75 |
|        Scala |  25.34 ± 00.33 | 383.96 ± 04.48 | 1062.79 ± 15.99 |
|          LDC |  25.36 ± 00.24 | 475.56 ± 29.33 | 643.18 ± 165.88 |
|          DMD |  30.30 ± 00.08 | 440.09 ± 03.41 |  830.02 ± 08.48 |
|          GDC |  32.70 ± 00.25 | 422.86 ± 10.66 |  811.39 ± 13.62 |
|       GCC Go |  39.97 ± 00.44 | 412.54 ± 26.98 | 1199.10 ± 17.80 |
|      C# Mono |  41.26 ± 00.32 | 323.06 ± 00.55 | 1241.77 ± 30.48 |
|         PyPy |  41.88 ± 00.93 | 608.13 ± 50.46 | 1089.78 ± 32.33 |
| C# .NET Core |  43.33 ± 00.27 | 551.57 ± 07.72 | 1107.93 ± 11.07 |
|       Python | 171.71 ± 00.49 | 466.69 ± 00.02 | 4430.82 ± 34.70 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.1.101                         |
| C# .NET Core | 3.4.1-beta4-19614-01 (16504609) |
| C# Mono      | 6.8.0.105                       |
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
| Go           | go1.13.7                        |
| Haskell      | 8.8.2                           |
| JRuby        | 9.2.9.0                         |
| Java         | 13.0.2                          |
| Julia        | v"1.3.1"                        |
| Kotlin       | 1.3.61                          |
| LDC          | 1.19.0                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.0.6                           |
| Node.js      | v13.8.0                         |
| OCaml        | 4.09.0                          |
| PHP          | 7.3.12-1                        |
| Perl         | v5.30.0                         |
| PyPy         | 7.3.0-final0 for Python 3.6.9   |
| Python       | 3.7.6                           |
| Racket       | "7.5"                           |
| Ruby         | 2.7.0p0                         |
| Rust         | 1.41.0                          |
| Scala        | 2.13.1                          |
| Swift        | swift-5.1.4-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 19.3.1                          |
| V            | 0.1.25 2ce6b19                  |
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

# Contribution

Please follow the criteria specified in the [Overview](#overview). Besides that please ensure that the communication protocol between a test and the test runner is satisfied:

 - The test runner listens on localhost:9001;
 - All messages are sent using TCP sockets closed immediately after the message has been sent;
 - There are two messages sent from a test (it establishes the measurement boundary):
  1. The beginning message having the format *name of the test*/t*process ID* (the process ID is used to measure the memory consumption). Please note that the name of the test couldn't use Tab character as it's a delimiter;
  2. The end message with any content (mostly it's "stop" for consistency).
 - The test runner could be unavailable (if the test is launched as is) and the test should gracefully handle it.
