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

UPDATE: 2020-04-11

# Test Cases

## Brainfuck v2.0

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck v2.0](brainfuck2)
[Brainfuck v1.0](brainfuck)

### bench.b

|        Language |        Time, s |     Memory, MiB |         Energy, J |
| :-------------- | -------------: | --------------: | ----------------: |
|           OCaml |   1.75 ± 00.03 |    5.19 ± 00.04 |     29.83 ± 00.75 |
|        Vala GCC |   1.83 ± 00.03 |    4.01 ± 00.04 |     29.51 ± 01.78 |
|         Nim GCC |   1.88 ± 00.03 |    1.86 ± 00.06 |     31.75 ± 01.78 |
|             LDC |   1.90 ± 00.02 |    3.02 ± 00.06 |     32.55 ± 01.53 |
|             C++ |   1.92 ± 00.05 |    1.68 ± 00.06 |     32.46 ± 02.32 |
|           C GCC |   1.93 ± 00.02 |    0.71 ± 00.04 |     30.21 ± 00.90 |
|       Nim Clang |   1.97 ± 00.05 |    2.35 ± 00.08 |     32.10 ± 02.14 |
|            Rust |   2.03 ± 00.02 |    2.04 ± 00.10 |     33.86 ± 01.87 |
|             GDC |   2.04 ± 00.02 |    6.50 ± 00.05 |     36.28 ± 01.18 |
|      Vala Clang |   2.09 ± 00.16 |    4.92 ± 00.49 |     33.66 ± 02.50 |
|          GCC Go |   2.17 ± 00.03 |   20.34 ± 00.12 |     38.84 ± 01.15 |
|          Kotlin |   2.23 ± 00.07 |   40.71 ± 00.11 |     39.10 ± 03.49 |
|         C Clang |   2.27 ± 00.04 |    0.72 ± 00.04 |     38.83 ± 02.66 |
|    C# .NET Core |   2.43 ± 00.02 |   29.80 ± 00.07 |     42.74 ± 02.21 |
|            Java |   2.44 ± 00.16 |   39.79 ± 00.13 |     41.58 ± 02.62 |
|           MLton |   2.56 ± 00.01 |    0.74 ± 00.04 |     45.52 ± 00.98 |
|              Go |   2.59 ± 00.07 |    3.22 ± 00.36 |     44.30 ± 02.44 |
|         Crystal |   2.64 ± 00.05 |    3.33 ± 00.04 |     46.73 ± 02.34 |
|    F# .NET Core |   2.66 ± 00.03 |   39.01 ± 00.10 |     48.98 ± 00.88 |
|     Chez Scheme |   2.92 ± 00.02 |   29.18 ± 00.60 |     51.80 ± 01.83 |
|           V GCC |   3.08 ± 00.02 |    0.71 ± 00.04 |     54.67 ± 01.71 |
|           Julia |   3.16 ± 00.04 |  176.70 ± 01.00 |     55.20 ± 02.65 |
|           Scala |   3.72 ± 00.14 |  136.77 ± 06.55 |     74.14 ± 01.79 |
|             DMD |   3.75 ± 00.04 |    3.58 ± 00.05 |     63.12 ± 03.78 |
|         V Clang |   3.77 ± 00.04 |    1.08 ± 00.07 |     67.31 ± 01.09 |
|         C# Mono |   4.08 ± 00.05 |   20.18 ± 00.08 |     73.60 ± 01.98 |
|  Haskell MArray |   4.21 ± 00.10 |    4.80 ± 00.06 |     80.48 ± 03.34 |
|         Node.js |   5.11 ± 00.05 |   33.81 ± 00.07 |     91.19 ± 02.94 |
|          Racket |   7.14 ± 00.28 |  104.44 ± 00.09 |    136.95 ± 08.87 |
|          LuaJIT |   7.38 ± 00.20 |    2.98 ± 00.13 |    135.95 ± 07.41 |
|            PyPy |  13.57 ± 00.26 |  107.93 ± 00.07 |    249.40 ± 16.30 |
|         Haskell |  16.10 ± 00.30 |    4.82 ± 00.08 |    331.43 ± 11.02 |
| TruffleRuby JVM |  32.17 ± 00.76 | 1053.97 ± 62.04 |    968.65 ± 44.60 |
|             Lua |  74.70 ± 01.24 |    3.11 ± 00.04 |   1396.14 ± 43.67 |
|            Ruby |  79.11 ± 01.25 |   13.97 ± 00.05 |   1507.81 ± 61.75 |
|           JRuby | 106.81 ± 05.74 |  428.98 ± 10.60 |   2090.86 ± 92.75 |
|          Elixir | 120.06 ± 01.11 |   52.96 ± 00.40 |   2240.10 ± 37.03 |
|          Python | 235.62 ± 02.72 |    9.80 ± 00.04 |  4350.29 ± 165.56 |
|        Tcl (FP) | 272.00 ± 06.10 |    4.30 ± 00.05 |  5342.98 ± 141.81 |
|            Perl | 377.38 ± 10.95 |    6.25 ± 00.10 |  6597.95 ± 163.66 |
|        Tcl (OO) | 543.60 ± 08.83 |    4.31 ± 00.05 | 8943.59 ± 3797.29 |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

|        Language |        Time, s |    Memory, MiB |         Energy, J |
| :-------------- | -------------: | -------------: | ----------------: |
|        Vala GCC |  14.37 ± 02.07 |   5.83 ± 00.09 |    255.23 ± 73.26 |
|           C GCC |  14.84 ± 01.12 |   1.70 ± 00.04 |   302.57 ± 107.78 |
|      Vala Clang |  16.01 ± 01.14 |   5.87 ± 00.09 |   304.69 ± 112.84 |
|             LDC |  16.13 ± 03.46 |   3.94 ± 00.04 |   319.79 ± 106.42 |
|            Rust |  16.68 ± 01.70 |   2.31 ± 00.08 |   363.40 ± 147.79 |
|           V GCC |  17.02 ± 02.97 |   2.48 ± 00.08 |    313.17 ± 79.68 |
|         Crystal |  17.15 ± 01.88 |   3.75 ± 00.04 |   353.24 ± 139.29 |
|             GDC |  17.42 ± 02.68 |   7.35 ± 00.08 |   351.65 ± 121.05 |
|    C# .NET Core |  19.00 ± 02.68 |  31.21 ± 00.17 |    347.43 ± 95.77 |
|       Nim Clang |  19.33 ± 03.64 |   2.86 ± 00.08 |   370.93 ± 136.12 |
|         C Clang |  19.36 ± 03.21 |   1.71 ± 00.03 |    343.15 ± 76.56 |
|             C++ |  19.89 ± 02.14 |   3.82 ± 00.06 |   413.71 ± 167.01 |
|         V Clang |  20.30 ± 01.63 |   3.21 ± 00.03 |   399.75 ± 119.12 |
|          GCC Go |  23.71 ± 02.04 |  30.97 ± 04.90 |   457.09 ± 150.26 |
|           MLton |  24.41 ± 03.80 |   4.05 ± 00.28 |   459.81 ± 135.02 |
|          Kotlin |  24.53 ± 02.92 |  46.51 ± 00.34 |   464.15 ± 105.23 |
|         Nim GCC |  26.69 ± 02.94 |   2.43 ± 00.05 |   539.59 ± 228.35 |
|           Scala |  26.71 ± 02.50 | 129.09 ± 08.06 |   584.37 ± 211.17 |
|            Java |  27.23 ± 05.57 |  46.12 ± 00.11 |   501.77 ± 160.07 |
|           OCaml |  32.69 ± 03.04 |   8.30 ± 02.92 |   710.01 ± 206.94 |
|              Go |  36.82 ± 05.07 |   4.67 ± 00.37 |   659.80 ± 191.82 |
|         C# Mono |  43.10 ± 03.78 |  20.92 ± 00.16 |   856.79 ± 264.83 |
|     Chez Scheme |  45.31 ± 06.24 |  29.44 ± 00.16 |   849.96 ± 297.84 |
|         Node.js |  46.64 ± 05.86 |  36.53 ± 00.26 |   900.97 ± 252.65 |
|             DMD |  50.34 ± 06.30 |   4.46 ± 00.06 |   945.33 ± 416.41 |
|           Julia |  66.60 ± 07.31 | 175.78 ± 00.80 |  1254.10 ± 376.60 |
|  Haskell MArray |  71.32 ± 09.46 |   6.01 ± 00.07 |  1585.96 ± 499.50 |
|          LuaJIT |  72.63 ± 07.87 |   3.89 ± 00.07 |  1305.08 ± 424.97 |
|            PyPy |  76.79 ± 14.91 | 109.19 ± 00.08 |  1413.59 ± 560.19 |
|    F# .NET Core | 129.79 ± 25.60 |  40.12 ± 00.09 | 2640.61 ± 1109.90 |
|          Racket | 136.04 ± 16.63 | 104.43 ± 00.12 |  2561.10 ± 873.78 |
| TruffleRuby JVM | 165.84 ± 23.36 | 955.04 ± 97.96 |  3267.32 ± 888.73 |

## Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)
|                Language |       Time, s |     Memory, MiB |      Energy, J |
| :---------------------- | ------------: | --------------: | -------------: |
|                C aklomp |  0.16 ± 00.00 |    1.99 ± 00.04 |   3.33 ± 00.05 |
|                       C |  1.47 ± 00.04 |    1.96 ± 00.04 |  22.99 ± 00.78 |
|                    Rust |  1.61 ± 00.04 |    2.62 ± 00.07 |  26.78 ± 01.62 |
|               Nim Clang |  1.70 ± 00.03 |    7.96 ± 00.06 |  30.06 ± 01.30 |
|                 Nim GCC |  2.01 ± 00.04 |    7.49 ± 00.05 |  36.94 ± 01.37 |
|                     LDC |  2.14 ± 00.02 |    4.03 ± 00.06 |  50.63 ± 02.08 |
|                 Crystal |  2.22 ± 00.01 |    5.25 ± 00.04 |  52.85 ± 02.23 |
|                     GDC |  2.23 ± 00.05 |   10.92 ± 00.05 |  40.41 ± 02.00 |
|                    Ruby |  2.31 ± 00.02 |   73.37 ± 00.08 |  40.59 ± 02.02 |
|                    Java |  2.47 ± 00.07 |  343.97 ± 11.45 |  39.26 ± 02.50 |
|                  Kotlin |  2.59 ± 00.06 |  341.57 ± 25.53 |  40.69 ± 01.80 |
|                   Scala |  2.67 ± 00.05 |  150.78 ± 06.78 |  46.34 ± 02.27 |
|       Perl MIME::Base64 |  2.68 ± 00.06 |    7.16 ± 00.09 |  48.38 ± 02.07 |
|                 V Clang |  2.73 ± 00.02 |    2.35 ± 00.06 |  46.18 ± 01.36 |
|                 Node.js |  2.80 ± 00.04 |  123.51 ± 14.21 |  40.82 ± 00.87 |
|                     PHP |  3.00 ± 00.02 |   15.29 ± 00.07 |  50.29 ± 02.28 |
|           C++ libcrypto |  3.15 ± 00.07 |    5.53 ± 00.06 |  59.04 ± 03.59 |
|                   V GCC |  3.59 ± 00.03 |    1.87 ± 00.06 |  61.60 ± 02.65 |
|                  GCC Go |  4.01 ± 00.14 |   50.46 ± 06.86 |  68.51 ± 03.24 |
|                     DMD |  4.17 ± 00.07 |   11.58 ± 00.04 |  76.46 ± 01.92 |
|            C# .NET Core |  4.25 ± 00.14 |   33.03 ± 00.14 |  74.62 ± 03.97 |
|                     Tcl |  4.34 ± 00.04 |    5.10 ± 00.05 |  74.89 ± 01.91 |
|                      Go |  4.81 ± 00.75 |    9.22 ± 00.41 |  33.58 ± 03.86 |
|                    PyPy |  4.93 ± 00.11 |  109.41 ± 00.44 |  81.84 ± 04.18 |
|                  Python |  5.48 ± 00.07 |    9.85 ± 00.07 |  97.10 ± 06.38 |
|         TruffleRuby JVM |  6.91 ± 00.08 | 828.43 ± 116.10 | 127.29 ± 02.11 |
|                 C# Mono |  7.07 ± 00.07 |   39.65 ± 00.11 | 117.17 ± 04.55 |
|                   Julia |  7.98 ± 00.14 |  253.24 ± 10.94 | 139.62 ± 03.73 |
|                   JRuby | 10.56 ± 00.46 |  358.59 ± 30.87 | 191.17 ± 08.83 |
| Perl MIME::Base64::Perl | 16.68 ± 00.33 |    8.88 ± 00.11 | 301.17 ± 14.44 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |     Memory, MiB |      Energy, J |
| :-------------------- | ------------: | --------------: | -------------: |
|     C++ DAW JSON Link |  0.09 ± 00.00 |  109.29 ± 00.05 |   1.61 ± 00.04 |
|              GDC fast |  0.12 ± 00.00 |  231.49 ± 00.11 |   1.75 ± 00.05 |
|     Rust Serde Custom |  0.15 ± 00.00 |  108.42 ± 00.08 |   2.29 ± 00.12 |
|      Rust Serde Typed |  0.16 ± 00.00 |  120.12 ± 00.39 |   2.65 ± 00.09 |
|             C++ gason |  0.20 ± 00.00 |  312.77 ± 00.09 |   3.26 ± 00.05 |
|          C++ simdjson |  0.21 ± 00.00 |  493.81 ± 03.17 |   3.15 ± 00.11 |
|         C++ RapidJSON |  0.25 ± 00.00 |  344.54 ± 00.10 |   4.22 ± 00.10 |
|                  Java |  0.43 ± 00.01 |  539.89 ± 09.47 |   7.83 ± 00.29 |
|                 Scala |  0.46 ± 00.01 |  482.99 ± 03.15 |   9.49 ± 00.31 |
|     C++ RapidJSON SAX |  0.59 ± 00.01 |  109.50 ± 00.05 |   9.01 ± 00.35 |
|           Julia JSON3 |  0.59 ± 00.01 |  823.30 ± 51.27 |  10.20 ± 00.25 |
|    Rust Serde Untyped |  0.69 ± 00.01 |  916.46 ± 00.10 |  10.85 ± 00.36 |
|           Go jsoniter |  0.77 ± 00.01 |  226.44 ± 00.19 |  13.24 ± 00.32 |
|          Crystal Pull |  0.85 ± 00.03 |  128.54 ± 00.06 |  14.18 ± 00.65 |
|        Crystal Schema |  0.86 ± 00.02 |  157.23 ± 00.12 |  14.64 ± 00.50 |
| Perl Cpanel::JSON::XS |  0.86 ± 00.01 |  517.03 ± 00.08 |  13.76 ± 00.43 |
|               Node.js |  0.90 ± 00.02 |  291.84 ± 03.18 |  16.99 ± 00.93 |
|                  PyPy |  0.90 ± 00.02 |  406.07 ± 00.99 |  14.20 ± 00.53 |
|               Crystal |  1.14 ± 00.02 |  503.61 ± 00.03 |  20.71 ± 00.87 |
|                    Go |  1.24 ± 00.02 |  209.51 ± 00.23 |  20.49 ± 00.65 |
|                   PHP |  1.25 ± 00.01 |  803.23 ± 00.10 |  18.39 ± 00.37 |
|               Clojure |  1.30 ± 00.06 | 2166.97 ± 34.78 |  30.50 ± 00.73 |
|               V Clang |  1.55 ± 00.01 |  592.49 ± 00.07 |  22.03 ± 00.48 |
|                 V GCC |  1.56 ± 00.01 |  591.99 ± 00.09 |  22.15 ± 00.65 |
|                GCC Go |  1.66 ± 00.02 |  206.86 ± 00.62 |  28.33 ± 00.74 |
|               Haskell |  1.83 ± 00.02 |   10.09 ± 00.07 |  31.58 ± 01.52 |
|            C++ json-c |  1.87 ± 00.02 | 1755.90 ± 00.07 |  28.29 ± 00.76 |
|          C# .NET Core |  1.92 ± 00.03 |  286.67 ± 00.30 |  35.11 ± 01.45 |
|                Python |  1.97 ± 00.02 |  493.53 ± 00.06 |  22.66 ± 00.32 |
|               Nim GCC |  2.05 ± 00.02 |  908.81 ± 00.03 |  30.57 ± 01.19 |
|             Nim Clang |  2.09 ± 00.02 |  909.27 ± 00.06 |  31.58 ± 01.42 |
|     CPython UltraJSON |  2.18 ± 00.03 |  661.06 ± 02.30 |  21.83 ± 00.36 |
|             Ruby YAJL |  2.56 ± 00.03 |  406.17 ± 00.09 |  33.98 ± 01.16 |
|                  Ruby |  2.58 ± 00.04 |  396.78 ± 00.06 |  31.74 ± 00.79 |
|               C# Mono |  2.70 ± 00.07 |  329.02 ± 00.08 |  48.83 ± 04.09 |
|                   LDC |  2.71 ± 00.03 |  796.17 ± 00.08 |  43.70 ± 01.11 |
|                   GDC |  3.07 ± 00.03 |  713.67 ± 00.09 |  51.85 ± 01.96 |
|                 JRuby |  3.96 ± 00.06 | 1956.55 ± 33.53 | 116.22 ± 01.91 |
|             C++ Boost |  4.09 ± 00.03 | 1549.59 ± 00.11 |  59.72 ± 03.17 |
|               Rust jq |  4.20 ± 00.05 |  885.56 ± 00.73 |  55.46 ± 01.68 |
|                   DMD |  5.21 ± 00.07 |  796.85 ± 00.07 |  83.63 ± 01.53 |
|   C# System.Text.Json |  7.01 ± 00.12 |  606.11 ± 00.12 | 129.93 ± 04.02 |
|       Perl JSON::Tiny | 11.90 ± 00.20 |  640.16 ± 00.11 | 223.13 ± 05.47 |
|       TruffleRuby JVM | 18.10 ± 00.71 | 2084.77 ± 54.27 | 474.20 ± 12.99 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |      Memory, MiB |        Energy, J |
| :-------------------- | -------------: | ---------------: | ---------------: |
| Nim Clang Arraymancer |   0.14 ± 00.01 |    56.34 ± 00.23 |     7.80 ± 00.38 |
|            LDC lubeck |   0.14 ± 00.00 |    61.59 ± 00.22 |     7.92 ± 00.27 |
|   Nim GCC Arraymancer |   0.15 ± 00.02 |    56.54 ± 00.32 |     8.59 ± 01.03 |
|          Python NumPy |   0.17 ± 00.02 |    84.30 ± 00.14 |     7.25 ± 00.26 |
|    Julia (threads: 8) |   0.18 ± 00.00 |   285.76 ± 00.50 |     9.39 ± 00.33 |
|             Java ND4J |   0.21 ± 00.01 |   236.01 ± 03.28 |    12.02 ± 00.38 |
|    Julia (threads: 1) |   0.57 ± 00.01 |   285.79 ± 00.57 |    10.11 ± 00.66 |
|                   LDC |   2.46 ± 00.01 |    73.33 ± 00.05 |    21.97 ± 00.30 |
|                   GDC |   2.60 ± 00.02 |    77.03 ± 00.07 |    24.98 ± 00.38 |
|                   DMD |   2.64 ± 00.01 |    73.36 ± 00.11 |    24.44 ± 00.40 |
|                     C |   3.86 ± 00.01 |    70.33 ± 00.07 |    45.83 ± 00.96 |
|                  Rust |   3.87 ± 00.01 |    70.94 ± 00.08 |    42.78 ± 00.82 |
|               Nim GCC |   3.95 ± 00.02 |    80.11 ± 06.47 |    42.80 ± 01.18 |
|             Nim Clang |   3.99 ± 00.03 |    81.38 ± 05.45 |    43.41 ± 00.75 |
|       Julia (no BLAS) |   4.04 ± 00.03 |   275.15 ± 00.54 |    45.24 ± 00.76 |
|                GCC Go |   4.12 ± 00.01 |   105.03 ± 00.17 |    54.15 ± 01.37 |
|                 Swift |   4.12 ± 00.01 |   205.87 ± 06.43 |    58.62 ± 00.91 |
|                    Go |   4.16 ± 00.03 |    76.94 ± 00.15 |    47.09 ± 01.10 |
|               Crystal |   4.17 ± 00.01 |    63.76 ± 00.04 |    49.71 ± 01.40 |
|               Node.js |   4.22 ± 00.01 |   105.77 ± 00.13 |    55.03 ± 00.76 |
|                  Java |   4.22 ± 00.02 |   125.32 ± 00.13 |    61.06 ± 01.12 |
|                 Scala |   4.22 ± 00.03 |   169.72 ± 05.64 |    40.99 ± 00.62 |
|                Kotlin |   4.25 ± 00.26 |   125.97 ± 00.21 |    54.82 ± 12.55 |
|                  PyPy |   6.58 ± 00.02 |   132.82 ± 00.10 |   102.97 ± 02.16 |
|          C# .NET Core |   7.44 ± 00.01 |   105.39 ± 00.13 |   117.47 ± 02.36 |
|               C# Mono |  11.60 ± 00.06 |    91.31 ± 00.08 |   183.41 ± 04.07 |
|                 V GCC |  12.24 ± 00.01 |    70.55 ± 00.07 |   153.38 ± 02.70 |
|               V Clang |  12.26 ± 00.01 |    70.99 ± 00.07 |   134.32 ± 02.64 |
|       TruffleRuby JVM |  63.37 ± 01.50 |   889.51 ± 50.86 |   299.76 ± 06.27 |
|                  Ruby | 220.83 ± 03.02 |    83.14 ± 00.06 |  3959.80 ± 47.62 |
|                Python | 256.07 ± 04.93 |    78.60 ± 00.07 |  4726.42 ± 99.04 |
|                   Tcl | 363.30 ± 01.86 |   280.55 ± 00.06 |  6027.51 ± 89.05 |
|                  Perl | 491.12 ± 03.31 |   608.32 ± 00.07 | 3585.54 ± 838.91 |
|                 JRuby | 592.33 ± 12.40 | 1001.51 ± 117.26 | 6301.73 ± 265.54 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |     Memory, MiB |       Energy, J |
| :----------- | -------------: | --------------: | --------------: |
|      Crystal |   7.86 ± 00.11 |  223.18 ± 07.32 |   96.30 ± 01.05 |
|      Nim GCC |  13.90 ± 00.14 |  485.47 ± 14.00 |  136.98 ± 02.07 |
|    Nim Clang |  14.35 ± 00.14 |  487.39 ± 13.94 |  141.91 ± 01.56 |
|          C++ |  18.16 ± 00.07 |  179.86 ± 00.08 |  145.31 ± 02.04 |
|        Scala |  20.87 ± 00.68 | 751.23 ± 305.92 |  410.83 ± 23.21 |
|           Go |  21.95 ± 00.07 |  350.07 ± 19.55 |  246.13 ± 01.99 |
|          LDC |  23.84 ± 00.45 |  466.86 ± 22.54 |  292.24 ± 18.01 |
|          DMD |  27.64 ± 00.21 |  455.49 ± 29.59 |  360.27 ± 17.22 |
|      C# Mono |  27.77 ± 00.21 |  334.16 ± 00.93 |  365.95 ± 03.30 |
|          GDC |  30.49 ± 00.41 |  418.99 ± 01.29 |  282.25 ± 04.39 |
|         PyPy |  33.60 ± 00.27 |  641.33 ± 74.65 |  356.16 ± 04.55 |
|       GCC Go |  34.64 ± 00.36 |  415.51 ± 28.93 |  442.35 ± 03.00 |
| C# .NET Core |  40.71 ± 00.22 |  530.66 ± 16.09 |  390.45 ± 03.41 |
|       Python | 124.32 ± 00.83 |  407.26 ± 00.05 | 1267.41 ± 11.68 |

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
| DMD          | v2.091.0                        |
| Elixir       | 1.9.1                           |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 9.3.0                           |
| GCC Go       | 9.3.0                           |
| GDC          | 9.3.0                           |
| Go           | go1.14.2                        |
| Haskell      | 8.10.1                          |
| JRuby        | 9.2.11.1                        |
| Java         | 14                              |
| Julia        | v"1.4.0"                        |
| Kotlin       | 1.3.71                          |
| LDC          | 1.20.1                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.2.0                           |
| Node.js      | v13.12.0                        |
| OCaml        | 4.10.0                          |
| PHP          | 7.3.15-3                        |
| Perl         | v5.30.0                         |
| PyPy         | 7.3.1-final0 for Python 3.6.9   |
| Python       | 3.8.2                           |
| Racket       | "7.6"                           |
| Ruby         | 2.7.1p83                        |
| Rust         | 1.42.0                          |
| Scala        | 2.13.1                          |
| Swift        | swift-5.2.1-RELEASE             |
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
