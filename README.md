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

UPDATE: 2020-04-01

# Test Cases

## Brainfuck v2.0

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck v2.0](brainfuck2)
[Brainfuck v1.0](brainfuck)

### bench.b

|        Language |        Time, s |     Memory, MiB |         Energy, J |
| :-------------- | -------------: | --------------: | ----------------: |
|           OCaml |   1.76 ± 00.06 |    5.19 ± 00.07 |     31.05 ± 01.53 |
|        Vala GCC |   1.84 ± 00.02 |    4.18 ± 00.36 |     29.38 ± 02.03 |
|         Nim GCC |   1.90 ± 00.02 |    1.91 ± 00.06 |     31.11 ± 01.48 |
|             LDC |   1.90 ± 00.01 |    3.02 ± 00.05 |     32.87 ± 01.71 |
|           C GCC |   1.91 ± 00.02 |    0.72 ± 00.04 |     32.26 ± 02.14 |
|             C++ |   1.93 ± 00.03 |    1.65 ± 00.05 |     32.80 ± 01.68 |
|       Nim Clang |   1.94 ± 00.02 |    2.34 ± 00.09 |     33.10 ± 01.89 |
|      Vala Clang |   2.03 ± 00.03 |    5.03 ± 00.36 |     33.05 ± 02.37 |
|             GDC |   2.08 ± 00.07 |    6.51 ± 00.06 |     37.93 ± 02.11 |
|            Rust |   2.13 ± 00.30 |    2.06 ± 00.08 |     35.19 ± 04.14 |
|          GCC Go |   2.18 ± 00.04 |   20.32 ± 00.11 |     39.42 ± 00.97 |
|          Kotlin |   2.19 ± 00.07 |   40.70 ± 00.10 |     36.98 ± 02.65 |
|         C Clang |   2.29 ± 00.02 |    0.72 ± 00.05 |     40.02 ± 02.77 |
|    C# .NET Core |   2.44 ± 00.01 |   29.78 ± 00.10 |     41.83 ± 02.01 |
|            Java |   2.53 ± 00.13 |   39.81 ± 00.13 |     43.29 ± 03.63 |
|              Go |   2.55 ± 00.02 |    3.10 ± 00.33 |     44.80 ± 02.57 |
|           MLton |   2.56 ± 00.03 |    0.74 ± 00.04 |     46.34 ± 01.33 |
|         Crystal |   2.66 ± 00.03 |    3.27 ± 00.03 |     46.34 ± 03.49 |
|    F# .NET Core |   2.66 ± 00.08 |   38.99 ± 00.09 |     49.67 ± 01.40 |
|           V GCC |   3.14 ± 00.05 |    0.73 ± 00.03 |     54.90 ± 01.89 |
|           Julia |   3.18 ± 00.07 |  177.31 ± 00.80 |     56.13 ± 02.51 |
|           Scala |   3.69 ± 00.05 |  133.57 ± 08.69 |     75.03 ± 02.09 |
|             DMD |   3.76 ± 00.04 |    3.54 ± 00.10 |     63.08 ± 03.44 |
|         V Clang |   3.80 ± 00.06 |    1.05 ± 00.06 |     66.01 ± 02.54 |
|         C# Mono |   4.11 ± 00.07 |   20.08 ± 00.10 |     74.33 ± 02.76 |
|  Haskell MArray |   4.22 ± 00.12 |    4.81 ± 00.06 |     81.50 ± 03.85 |
|         Node.js |   5.09 ± 00.09 |   33.79 ± 00.10 |     92.05 ± 03.12 |
|          Racket |   7.35 ± 00.34 |  104.48 ± 00.08 |    131.19 ± 12.86 |
|          LuaJIT |   7.42 ± 00.32 |    2.96 ± 00.08 |    137.49 ± 10.69 |
|     Chez Scheme |  13.57 ± 00.31 |   29.38 ± 00.05 |    242.42 ± 11.22 |
|            PyPy |  13.88 ± 00.59 |  108.47 ± 00.46 |    246.84 ± 19.18 |
|         Haskell |  16.35 ± 00.47 |    4.79 ± 00.08 |    323.15 ± 16.39 |
| TruffleRuby JVM |  32.43 ± 00.51 | 1028.85 ± 58.19 |    925.38 ± 34.14 |
|             Lua |  75.70 ± 01.32 |    3.08 ± 00.06 |   1344.92 ± 46.21 |
|            Ruby |  76.93 ± 01.92 |   13.94 ± 00.06 |   1444.58 ± 82.39 |
|           JRuby | 104.38 ± 04.36 |  433.38 ± 06.72 |  2062.81 ± 151.42 |
|          Elixir | 120.84 ± 02.30 |   52.69 ± 00.71 |   2279.63 ± 49.97 |
|          Python | 237.95 ± 02.09 |    9.73 ± 00.06 |   4461.22 ± 69.40 |
|        Tcl (FP) | 275.33 ± 07.40 |    4.30 ± 00.06 |  5231.04 ± 388.72 |
|            Perl | 376.12 ± 05.64 |    6.37 ± 00.06 | 6560.64 ± 1463.84 |
|        Tcl (OO) | 547.87 ± 11.87 |    4.26 ± 00.05 | 10824.32 ± 660.09 |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

|        Language |        Time, s |     Memory, MiB |        Energy, J |
| :-------------- | -------------: | --------------: | ---------------: |
|        Vala GCC |  13.56 ± 00.12 |    5.92 ± 00.11 |   234.50 ± 05.84 |
|           C GCC |  14.14 ± 00.21 |    1.72 ± 00.04 |   274.25 ± 05.72 |
|             LDC |  14.55 ± 00.44 |    3.95 ± 00.03 |   263.01 ± 10.88 |
|      Vala Clang |  15.47 ± 00.23 |    5.89 ± 00.11 |   275.53 ± 07.35 |
|           V GCC |  15.88 ± 00.51 |    2.77 ± 00.13 |   289.85 ± 27.03 |
|            Rust |  16.07 ± 00.45 |    2.30 ± 00.05 |   285.80 ± 14.90 |
|             GDC |  16.20 ± 00.22 |    7.36 ± 00.07 |   293.08 ± 08.58 |
|         Crystal |  16.23 ± 00.41 |    3.75 ± 00.05 |   291.70 ± 11.33 |
|       Nim Clang |  17.58 ± 00.33 |    2.89 ± 00.08 |   316.65 ± 09.77 |
|    C# .NET Core |  18.07 ± 00.27 |   31.18 ± 00.13 |   320.31 ± 09.81 |
|         C Clang |  18.19 ± 00.17 |    1.70 ± 00.04 |   320.78 ± 15.13 |
|             C++ |  18.60 ± 00.48 |    3.81 ± 00.08 |   337.62 ± 14.00 |
|         V Clang |  19.29 ± 00.61 |    3.24 ± 00.03 |   349.03 ± 21.04 |
|          Kotlin |  21.51 ± 01.70 |   46.54 ± 00.27 |   380.27 ± 25.82 |
|           MLton |  23.08 ± 00.62 |    4.31 ± 00.41 |   415.60 ± 23.40 |
|          GCC Go |  23.32 ± 00.91 |   28.66 ± 06.02 |   402.99 ± 32.41 |
|            Java |  23.97 ± 01.37 |   46.11 ± 00.20 |   421.10 ± 24.21 |
|         Nim GCC |  24.69 ± 00.42 |    2.42 ± 00.05 |   447.74 ± 31.85 |
|           Scala |  25.39 ± 00.45 |  135.67 ± 10.54 |   487.97 ± 13.18 |
|           OCaml |  32.13 ± 01.08 |    8.11 ± 01.21 |   613.09 ± 42.51 |
|              Go |  37.98 ± 00.77 |    4.55 ± 00.38 |   689.28 ± 24.27 |
|         C# Mono |  42.01 ± 00.91 |   20.80 ± 00.15 |   763.75 ± 26.18 |
|         Node.js |  45.09 ± 00.86 |   36.60 ± 00.16 |   802.26 ± 29.11 |
|             DMD |  46.70 ± 01.02 |    4.41 ± 00.06 |   748.49 ± 70.60 |
|           Julia |  63.07 ± 00.97 |  177.35 ± 00.86 |  1178.52 ± 23.74 |
|  Haskell MArray |  67.41 ± 01.95 |    6.04 ± 00.08 |  1299.44 ± 86.01 |
|          LuaJIT |  69.24 ± 00.40 |    3.88 ± 00.08 |  1163.16 ± 64.26 |
|            PyPy |  71.81 ± 00.67 |  109.67 ± 00.20 |  1329.68 ± 58.14 |
|    F# .NET Core | 117.61 ± 02.33 |   40.05 ± 00.08 |  2157.81 ± 56.19 |
|          Racket | 131.71 ± 03.36 |  104.49 ± 00.10 | 2365.49 ± 137.11 |
| TruffleRuby JVM | 158.06 ± 09.45 | 943.67 ± 102.09 | 2930.64 ± 499.58 |
|     Chez Scheme | 165.15 ± 02.92 |   29.44 ± 00.07 |  3042.71 ± 96.15 |

## Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |     Memory, MiB |      Energy, J |
| :---------------------- | ------------: | --------------: | -------------: |
|                C aklomp |  0.16 ± 00.00 |    2.00 ± 00.04 |   3.35 ± 00.12 |
|                       C |  1.47 ± 00.01 |    1.97 ± 00.03 |  22.89 ± 01.07 |
|                    Rust |  1.64 ± 00.02 |    2.59 ± 00.06 |  25.75 ± 01.70 |
|                   V GCC |  1.68 ± 00.04 |    1.87 ± 00.07 |  29.89 ± 01.14 |
|                 V Clang |  1.68 ± 00.01 |    2.36 ± 00.05 |  27.27 ± 01.37 |
|                     LDC |  2.16 ± 00.03 |    4.06 ± 00.06 |  49.33 ± 01.12 |
|                 Crystal |  2.21 ± 00.01 |    5.19 ± 00.05 |  53.05 ± 01.47 |
|                     GDC |  2.26 ± 00.02 |   10.96 ± 00.08 |  39.19 ± 01.29 |
|                    Ruby |  2.30 ± 00.01 |   73.36 ± 00.12 |  41.24 ± 01.36 |
|                    Java |  2.46 ± 00.08 |  332.26 ± 06.96 |  40.62 ± 02.40 |
|                  Kotlin |  2.58 ± 00.06 |  331.27 ± 00.43 |  40.50 ± 01.66 |
|                   Scala |  2.66 ± 00.06 |  149.27 ± 07.93 |  46.05 ± 01.52 |
|       Perl MIME::Base64 |  2.67 ± 00.06 |    7.24 ± 00.09 |  49.40 ± 01.84 |
|                 Nim GCC |  2.68 ± 00.04 |    7.49 ± 00.06 |  48.23 ± 02.54 |
|               Nim Clang |  2.73 ± 00.06 |    7.95 ± 00.08 |  49.97 ± 01.63 |
|                 Node.js |  2.79 ± 00.02 |  130.90 ± 22.64 |  41.35 ± 01.04 |
|                     PHP |  3.01 ± 00.02 |   15.42 ± 00.11 |  50.73 ± 02.08 |
|           C++ libcrypto |  3.20 ± 00.03 |    5.59 ± 00.07 |  57.99 ± 02.78 |
|            C# .NET Core |  3.99 ± 00.04 |   33.06 ± 00.11 |  69.12 ± 03.61 |
|                  GCC Go |  4.00 ± 00.08 |   49.99 ± 06.78 |  68.64 ± 01.51 |
|                     DMD |  4.21 ± 00.06 |   11.59 ± 00.06 |  75.77 ± 01.57 |
|                     Tcl |  4.39 ± 00.07 |    5.08 ± 00.06 |  75.59 ± 02.00 |
|                    PyPy |  4.91 ± 00.04 |  109.91 ± 00.20 |  85.39 ± 02.76 |
|                  Python |  5.49 ± 00.03 |    9.86 ± 00.05 |  97.94 ± 04.52 |
|                      Go |  5.91 ± 00.48 |   10.71 ± 05.81 |  39.04 ± 02.60 |
|                 C# Mono |  7.11 ± 00.05 |   39.47 ± 00.08 | 117.51 ± 04.59 |
|         TruffleRuby JVM |  7.44 ± 00.42 | 804.19 ± 123.10 | 138.21 ± 07.21 |
|                   Julia |  8.00 ± 00.08 |  251.81 ± 09.85 | 138.17 ± 07.40 |
|                   JRuby | 10.83 ± 00.35 |  367.72 ± 23.61 | 195.74 ± 10.53 |
| Perl MIME::Base64::Perl | 16.64 ± 00.26 |    8.91 ± 00.10 | 301.91 ± 12.87 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |     Memory, MiB |      Energy, J |
| :-------------------- | ------------: | --------------: | -------------: |
|     C++ DAW JSON Link |  0.09 ± 00.00 |  109.32 ± 00.06 |   1.63 ± 00.05 |
|              GDC fast |  0.12 ± 00.00 |  231.46 ± 00.26 |   1.77 ± 00.08 |
|     Rust Serde Custom |  0.15 ± 00.00 |  108.45 ± 00.09 |   2.33 ± 00.12 |
|      Rust Serde Typed |  0.16 ± 00.00 |  120.01 ± 00.37 |   2.69 ± 00.14 |
|             C++ gason |  0.20 ± 00.00 |  312.83 ± 00.07 |   3.31 ± 00.11 |
|          C++ simdjson |  0.22 ± 00.00 |  494.17 ± 01.82 |   3.13 ± 00.11 |
|         C++ RapidJSON |  0.25 ± 00.00 |  344.55 ± 00.07 |   4.28 ± 00.08 |
|                  Java |  0.41 ± 00.01 |  581.94 ± 16.46 |   7.04 ± 00.20 |
|                 Scala |  0.46 ± 00.01 |  474.58 ± 05.08 |   9.31 ± 00.30 |
|     C++ RapidJSON SAX |  0.58 ± 00.01 |  109.49 ± 00.05 |   9.40 ± 00.47 |
|           Julia JSON3 |  0.59 ± 00.01 |  849.11 ± 01.19 |   9.82 ± 00.16 |
|    Rust Serde Untyped |  0.69 ± 00.01 |  916.48 ± 00.09 |  10.92 ± 00.59 |
|           Go jsoniter |  0.75 ± 00.01 |  226.54 ± 00.25 |  12.94 ± 00.32 |
|          Crystal Pull |  0.84 ± 00.03 |  128.49 ± 00.06 |  14.16 ± 00.55 |
|               Node.js |  0.87 ± 00.07 |  293.52 ± 02.46 |  16.44 ± 01.90 |
| Perl Cpanel::JSON::XS |  0.87 ± 00.01 |  517.12 ± 00.08 |  13.40 ± 00.55 |
|        Crystal Schema |  0.88 ± 00.02 |  157.29 ± 00.13 |  14.41 ± 00.68 |
|                  PyPy |  0.89 ± 00.02 |  406.54 ± 00.78 |  14.06 ± 00.48 |
|               Crystal |  1.17 ± 00.01 |  503.67 ± 00.07 |  21.17 ± 00.89 |
|                    Go |  1.25 ± 00.02 |  209.70 ± 00.18 |  20.94 ± 00.89 |
|                   PHP |  1.25 ± 00.01 |  803.42 ± 00.06 |  18.11 ± 00.37 |
|               Clojure |  1.32 ± 00.05 | 2170.26 ± 30.86 |  30.82 ± 01.40 |
|                 V GCC |  1.56 ± 00.01 |  592.00 ± 00.09 |  21.84 ± 00.35 |
|               V Clang |  1.58 ± 00.02 |  592.47 ± 00.07 |  22.12 ± 00.59 |
|                GCC Go |  1.67 ± 00.04 |  207.43 ± 02.48 |  28.38 ± 00.98 |
|               Haskell |  1.83 ± 00.01 |   10.10 ± 00.06 |  31.44 ± 01.38 |
|            C++ json-c |  1.86 ± 00.02 | 1755.90 ± 00.06 |  28.93 ± 00.71 |
|          C# .NET Core |  1.95 ± 00.02 |  286.82 ± 00.16 |  35.08 ± 01.77 |
|                Python |  2.00 ± 00.02 |  493.56 ± 00.06 |  22.67 ± 00.61 |
|               Nim GCC |  2.06 ± 00.02 |  908.81 ± 00.03 |  29.70 ± 01.05 |
|             Nim Clang |  2.12 ± 00.03 |  909.27 ± 00.05 |  30.75 ± 01.07 |
|     CPython UltraJSON |  2.20 ± 00.04 |  661.97 ± 01.87 |  21.98 ± 00.41 |
|                  Ruby |  2.56 ± 00.02 |  396.79 ± 00.07 |  31.42 ± 00.65 |
|             Ruby YAJL |  2.57 ± 00.02 |  406.15 ± 00.05 |  32.88 ± 00.98 |
|               C# Mono |  2.69 ± 00.08 |  328.96 ± 00.09 |  46.10 ± 02.39 |
|                   LDC |  2.69 ± 00.04 |  796.22 ± 00.09 |  43.31 ± 01.72 |
|                   GDC |  3.08 ± 00.04 |  713.60 ± 00.09 |  52.50 ± 01.13 |
|                 JRuby |  3.95 ± 00.06 | 1932.28 ± 34.72 | 115.40 ± 04.12 |
|             C++ Boost |  4.10 ± 00.03 | 1549.59 ± 00.07 |  59.84 ± 03.13 |
|               Rust jq |  4.25 ± 00.05 |  885.87 ± 01.23 |  54.57 ± 01.33 |
|                   DMD |  5.20 ± 00.11 |  796.73 ± 00.10 |  83.09 ± 01.81 |
|   C# System.Text.Json |  6.94 ± 00.06 |  606.03 ± 00.08 | 127.80 ± 03.49 |
|       Perl JSON::Tiny | 11.86 ± 00.15 |  640.30 ± 00.09 | 223.38 ± 06.24 |
|       TruffleRuby JVM | 18.25 ± 00.44 | 2057.42 ± 28.81 | 478.40 ± 10.98 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|              Language |        Time, s |      Memory, MiB |         Energy, J |
| :-------------------- | -------------: | ---------------: | ----------------: |
| Nim Clang Arraymancer |   0.14 ± 00.01 |    56.48 ± 00.26 |      7.83 ± 00.52 |
|            LDC lubeck |   0.14 ± 00.01 |    61.73 ± 00.25 |      8.22 ± 00.57 |
|   Nim GCC Arraymancer |   0.15 ± 00.01 |    56.45 ± 00.28 |      8.60 ± 00.65 |
|          Python NumPy |   0.17 ± 00.02 |    84.32 ± 00.12 |      7.29 ± 00.43 |
|    Julia (threads: 8) |   0.18 ± 00.00 |   286.76 ± 00.23 |      9.37 ± 00.22 |
|             Java ND4J |   0.22 ± 00.02 |   236.98 ± 01.45 |     12.50 ± 00.93 |
|    Julia (threads: 1) |   0.58 ± 00.01 |   286.92 ± 00.14 |      9.83 ± 00.71 |
|                   LDC |   2.46 ± 00.02 |    73.33 ± 00.06 |     22.27 ± 00.81 |
|                   GDC |   2.59 ± 00.03 |    77.05 ± 00.06 |     25.62 ± 00.81 |
|                   DMD |   2.63 ± 00.02 |    73.32 ± 00.04 |     25.23 ± 00.45 |
|                     C |   3.86 ± 00.01 |    70.31 ± 00.05 |     47.03 ± 01.31 |
|                  Rust |   3.88 ± 00.01 |    70.89 ± 00.04 |     43.46 ± 01.28 |
|               Nim GCC |   3.96 ± 00.03 |    78.13 ± 06.73 |     43.68 ± 00.64 |
|             Nim Clang |   4.01 ± 00.01 |    83.26 ± 07.53 |     44.54 ± 00.64 |
|       Julia (no BLAS) |   4.06 ± 00.04 |   276.40 ± 00.13 |     45.69 ± 01.60 |
|                GCC Go |   4.14 ± 00.02 |   105.08 ± 00.20 |     54.80 ± 01.43 |
|               V Clang |   4.15 ± 00.01 |    71.02 ± 00.04 |     50.99 ± 01.50 |
|                 Swift |   4.15 ± 00.05 |   205.81 ± 06.52 |     58.30 ± 01.71 |
|                    Go |   4.17 ± 00.02 |    76.77 ± 00.19 |     56.48 ± 01.40 |
|                Kotlin |   4.17 ± 00.07 |   126.00 ± 00.21 |     54.96 ± 08.24 |
|               Crystal |   4.18 ± 00.02 |    63.81 ± 00.06 |     58.50 ± 01.77 |
|                 V GCC |   4.21 ± 00.01 |    70.59 ± 00.05 |     61.85 ± 01.87 |
|               Node.js |   4.22 ± 00.02 |   105.73 ± 00.19 |     56.43 ± 01.46 |
|                  Java |   4.23 ± 00.02 |   125.42 ± 00.21 |     61.53 ± 01.28 |
|                 Scala |   4.25 ± 00.07 |   168.09 ± 04.95 |     41.10 ± 01.33 |
|                  PyPy |   6.58 ± 00.02 |   132.91 ± 00.26 |    103.84 ± 02.50 |
|          C# .NET Core |   7.44 ± 00.02 |   105.41 ± 00.10 |    116.97 ± 04.08 |
|               C# Mono |  11.60 ± 00.09 |    91.31 ± 00.09 |    185.71 ± 08.80 |
|       TruffleRuby JVM |  62.46 ± 01.67 |   843.95 ± 45.32 |    270.34 ± 04.63 |
|                  Ruby | 183.31 ± 04.54 |    83.06 ± 00.07 |   3107.53 ± 93.40 |
|                Python | 260.26 ± 06.76 |    78.57 ± 00.05 | 4467.45 ± 1234.80 |
|                   Tcl | 369.01 ± 09.13 |   280.55 ± 00.03 |  6157.28 ± 138.80 |
|                 JRuby | 453.11 ± 26.72 | 1054.57 ± 189.88 |  4346.83 ± 240.23 |
|                  Perl | 490.68 ± 06.50 |   608.46 ± 00.10 |   3855.14 ± 97.38 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |     Memory, MiB |       Energy, J |
| :----------- | -------------: | --------------: | --------------: |
|      Crystal |   7.96 ± 00.06 |  231.56 ± 00.25 |   99.28 ± 02.02 |
|      Nim GCC |  13.99 ± 00.06 |  493.77 ± 13.71 |  139.48 ± 01.00 |
|    Nim Clang |  14.97 ± 00.10 |  489.08 ± 12.49 |  147.81 ± 01.84 |
|          C++ |  18.09 ± 00.07 |  179.85 ± 00.04 |  145.66 ± 03.09 |
|        Scala |  21.17 ± 00.83 | 793.54 ± 243.67 |  421.80 ± 26.87 |
|           Go |  22.00 ± 00.07 |  362.04 ± 12.92 |  246.69 ± 01.88 |
|          LDC |  23.87 ± 00.51 |  489.77 ± 42.25 |  290.44 ± 23.06 |
|      C# Mono |  27.87 ± 00.19 |  333.41 ± 01.64 |  367.35 ± 02.58 |
|          DMD |  28.32 ± 00.29 |  447.03 ± 17.86 |  353.51 ± 18.43 |
|          GDC |  30.52 ± 00.33 |  429.33 ± 31.28 |  260.44 ± 73.98 |
|         PyPy |  34.50 ± 01.41 |  638.82 ± 34.26 |  363.59 ± 08.08 |
|       GCC Go |  34.54 ± 00.32 |  410.01 ± 34.01 |  442.07 ± 04.03 |
| C# .NET Core |  41.05 ± 00.35 |  522.83 ± 23.38 |  391.28 ± 07.81 |
|       Python | 125.14 ± 01.05 |  408.57 ± 00.04 | 1279.60 ± 19.20 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.1.102                         |
| C# .NET Core | 3.4.1-beta4-19614-01 (16504609) |
| C# Mono      | 6.8.0.105                       |
| Chez Scheme  | 9.5                             |
| Clang        | 9.0.1                           |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.33.0                          |
| DMD          | v2.091.0                        |
| Elixir       | 1.9.1                           |
| F# .NET Core | 10.7.0.0 for F# 4.7             |
| GCC          | 9.3.0                           |
| GCC Go       | 9.3.0                           |
| GDC          | 9.3.0                           |
| Go           | go1.14.1                        |
| Haskell      | 8.10.1                          |
| JRuby        | 9.2.11.1                        |
| Java         | 14                              |
| Julia        | v"1.4.0"                        |
| Kotlin       | 1.3.71                          |
| LDC          | 1.20.1                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.0.6                           |
| Node.js      | v13.12.0                        |
| OCaml        | 4.10.0                          |
| PHP          | 7.3.15-3                        |
| Perl         | v5.30.0                         |
| PyPy         | 7.3.0-final0 for Python 3.6.9   |
| Python       | 3.8.2                           |
| Racket       | "7.6"                           |
| Ruby         | 2.7.1p83                        |
| Rust         | 1.42.0                          |
| Scala        | 2.13.1                          |
| Swift        | swift-5.2.1-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 20.0.0                          |
| V            | 0.1.25 a3bd19c                  |
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
