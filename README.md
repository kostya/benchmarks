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

UPDATE: 2020-10-15

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck](brainfuck)

### bench.b

|                 Language |        Time, s |    Memory, MiB |         Energy, J |
| :----------------------- | -------------: | -------------: | ----------------: |
|                  C++/g++ |   0.85 ± 00.03 |   1.50 ± 00.02 |     15.90 ± 00.96 |
|                 Vala/gcc |   1.77 ± 00.08 |   0.80 ± 01.68 |     35.38 ± 03.14 |
|                  Nim/gcc |   1.78 ± 00.07 |   1.90 ± 00.06 |     33.50 ± 02.88 |
|                    C/gcc |   1.79 ± 00.09 |   0.54 ± 00.03 |     37.13 ± 02.91 |
|                   D/ldc2 |   1.84 ± 00.06 |   2.97 ± 00.06 |     37.39 ± 02.66 |
|                   Kotlin |   1.87 ± 00.05 |  39.99 ± 00.30 |     33.79 ± 01.96 |
|                    OCaml |   1.88 ± 00.05 |   5.07 ± 00.03 |     35.06 ± 02.84 |
|                Nim/clang |   1.90 ± 00.06 |   2.34 ± 00.06 |     36.03 ± 02.47 |
|                    D/gdc |   1.98 ± 00.08 |   6.33 ± 00.06 |     40.60 ± 03.30 |
|                     Rust |   2.07 ± 00.08 |   2.11 ± 00.09 |     41.40 ± 04.06 |
|               Vala/clang |   2.10 ± 00.19 |   0.81 ± 01.70 |     41.13 ± 06.39 |
|                 Go/gccgo |   2.17 ± 00.09 |  23.35 ± 05.34 |     44.01 ± 02.95 |
|                  C/clang |   2.18 ± 00.07 |   0.54 ± 00.03 |     43.61 ± 03.54 |
|                     Java |   2.30 ± 00.06 |  37.58 ± 00.33 |     43.42 ± 03.33 |
|             C#/.NET Core |   2.44 ± 00.10 |  34.76 ± 00.09 |     45.60 ± 03.47 |
|                       Go |   2.47 ± 00.08 |   3.12 ± 00.38 |     45.88 ± 04.11 |
|                    V/gcc |   2.52 ± 00.08 |   0.54 ± 00.03 |     49.06 ± 04.25 |
|                    MLton |   2.56 ± 00.10 |   0.57 ± 00.03 |     46.92 ± 04.58 |
|                  Crystal |   2.56 ± 00.09 |   3.35 ± 00.05 |     52.16 ± 04.42 |
|             F#/.NET Core |   2.77 ± 00.06 | 122.91 ± 05.96 |     50.98 ± 03.27 |
|                  V/clang |   2.82 ± 00.10 |   0.87 ± 00.00 |     55.62 ± 05.02 |
|              Chez Scheme |   2.97 ± 00.09 |  29.18 ± 00.09 |     54.65 ± 04.24 |
|                    Julia |   3.27 ± 00.12 | 178.79 ± 00.80 |     62.26 ± 06.01 |
|                    Scala |   3.51 ± 00.08 | 128.45 ± 13.47 |     70.17 ± 02.37 |
|                    D/dmd |   3.68 ± 00.17 |   3.57 ± 00.06 |     68.69 ± 05.66 |
|                  C#/Mono |   4.48 ± 00.18 |  20.44 ± 00.09 |     86.56 ± 07.17 |
|         Haskell (MArray) |   4.62 ± 00.20 |   5.42 ± 00.04 |     87.55 ± 08.22 |
|                  Node.js |   5.14 ± 00.23 |  34.52 ± 00.71 |     95.22 ± 08.89 |
|               Lua/luajit |   7.67 ± 00.24 |   2.84 ± 00.08 |    125.20 ± 09.41 |
|                   Racket |   7.93 ± 00.36 | 106.96 ± 00.16 |    146.34 ± 13.29 |
|              Python/pypy |  13.01 ± 00.33 | 109.26 ± 00.23 |    297.96 ± 15.69 |
|                  Haskell |  16.74 ± 00.64 |   5.42 ± 00.07 |    314.45 ± 21.40 |
| Ruby/truffleruby (--jvm) |  19.01 ± 00.50 | 953.83 ± 29.17 |    500.72 ± 22.61 |
|         Ruby/truffleruby |  30.15 ± 14.62 | 522.07 ± 10.19 |   627.32 ± 322.28 |
|                      Lua |  59.25 ± 01.93 |   2.64 ± 00.03 |   1080.71 ± 60.37 |
|             Ruby (--jit) |  61.12 ± 01.70 |  14.32 ± 00.05 |   1155.02 ± 75.43 |
|                     Ruby |  86.27 ± 03.72 |  14.09 ± 00.07 |  1606.24 ± 124.26 |
|               Ruby/jruby | 112.43 ± 07.04 | 444.49 ± 05.95 |  2058.13 ± 280.35 |
|                   Elixir | 115.68 ± 02.71 |  54.15 ± 00.78 |  2358.41 ± 122.22 |
|                   Python | 229.42 ± 07.71 |   9.33 ± 00.06 |  4601.25 ± 224.31 |
|                 Tcl (FP) | 278.55 ± 06.76 |   4.25 ± 00.09 |  5165.32 ± 342.71 |
|                     Perl | 345.53 ± 10.11 |   6.49 ± 00.08 |  6752.61 ± 365.18 |
|                Tcl (OOP) | 547.94 ± 10.49 |   4.29 ± 00.08 | 9798.11 ± 1640.16 |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------------------- | -------------: | -------------: | ---------------: |
|                  C++/g++ |  12.10 ± 00.58 |   3.68 ± 00.05 |   232.38 ± 24.37 |
|                    C/gcc |  12.59 ± 00.44 |   1.61 ± 00.03 |   257.08 ± 21.01 |
|                 Vala/gcc |  12.97 ± 00.68 |   0.00 ± 00.00 |   256.76 ± 28.99 |
|                    D/gdc |  13.81 ± 00.77 |   7.17 ± 00.09 |   274.14 ± 30.54 |
|                    V/gcc |  14.04 ± 00.70 |   2.40 ± 00.07 |   291.89 ± 27.32 |
|                   D/ldc2 |  15.30 ± 00.63 |   3.84 ± 00.06 |   315.46 ± 30.12 |
|               Vala/clang |  15.76 ± 00.67 |   0.00 ± 00.00 |   312.92 ± 27.98 |
|                  Crystal |  15.80 ± 00.64 |   3.77 ± 00.05 |   309.12 ± 28.99 |
|                  C/clang |  17.54 ± 00.67 |   1.58 ± 00.03 |   357.71 ± 30.01 |
|                  Nim/gcc |  17.99 ± 00.61 |   2.41 ± 00.05 |   372.22 ± 29.02 |
|                  V/clang |  18.33 ± 00.78 |   2.88 ± 00.07 |   369.15 ± 31.69 |
|                     Rust |  18.40 ± 00.80 |   2.33 ± 00.12 |   392.54 ± 37.07 |
|             C#/.NET Core |  18.70 ± 00.83 |  35.81 ± 00.16 |   361.66 ± 37.32 |
|                Nim/clang |  19.74 ± 00.75 |   2.86 ± 00.05 |   410.37 ± 31.98 |
|                     Java |  21.34 ± 01.79 |  43.99 ± 00.85 |   441.27 ± 60.57 |
|                   Kotlin |  22.48 ± 00.90 |  45.15 ± 00.28 |   447.66 ± 36.82 |
|                    MLton |  23.60 ± 00.90 |   3.19 ± 00.45 |   467.03 ± 42.50 |
|                    Scala |  23.67 ± 00.55 | 115.50 ± 09.77 |   497.13 ± 27.07 |
|                 Go/gccgo |  25.91 ± 01.04 |  27.21 ± 06.33 |   517.25 ± 50.36 |
|                    OCaml |  29.06 ± 01.08 |  10.51 ± 02.70 |   626.66 ± 50.58 |
|                       Go |  32.90 ± 01.01 |   4.05 ± 00.22 |   682.08 ± 46.19 |
|                    D/dmd |  44.25 ± 01.49 |   4.38 ± 00.09 |   882.83 ± 62.59 |
|              Chez Scheme |  47.44 ± 01.57 |  29.17 ± 00.08 |  1019.36 ± 73.42 |
|                  C#/Mono |  48.09 ± 01.73 |  21.15 ± 00.09 |   916.13 ± 86.38 |
|                  Node.js |  55.56 ± 01.94 |  37.78 ± 00.34 |  1115.90 ± 91.47 |
|         Haskell (MArray) |  63.35 ± 02.37 |   6.53 ± 00.05 | 1355.55 ± 118.14 |
|               Lua/luajit |  65.10 ± 02.43 |   3.69 ± 00.08 |  1378.17 ± 97.40 |
|                    Julia |  65.34 ± 07.72 | 178.11 ± 00.72 | 1221.10 ± 158.93 |
|              Python/pypy |  68.71 ± 02.62 | 110.57 ± 00.19 | 1430.91 ± 123.60 |
| Ruby/truffleruby (--jvm) | 113.52 ± 02.60 | 931.28 ± 47.62 | 2362.68 ± 114.47 |
|             F#/.NET Core | 123.80 ± 00.77 | 128.28 ± 00.24 |  2551.42 ± 41.89 |
|                   Racket | 134.70 ± 03.83 | 106.91 ± 00.09 | 2591.75 ± 178.43 |
|         Ruby/truffleruby | 198.39 ± 33.50 | 526.21 ± 28.83 | 3972.24 ± 760.59 |
|                  Haskell | 216.36 ± 06.27 |   6.54 ± 00.03 | 4599.84 ± 341.65 |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |       Time, s |     Memory, MiB |      Energy, J |
| :------------------------ | ------------: | --------------: | -------------: |
|            C/gcc (aklomp) |  0.15 ± 00.00 |    1.97 ± 00.04 |   3.89 ± 00.37 |
|                   V/clang |  0.85 ± 00.03 |    2.50 ± 00.08 |  15.09 ± 01.15 |
|                     C/gcc |  1.25 ± 00.04 |    1.93 ± 00.07 |  22.97 ± 01.44 |
|                      Rust |  1.29 ± 00.05 |    2.54 ± 00.09 |  28.60 ± 02.43 |
|                     V/gcc |  1.64 ± 00.08 |    1.90 ± 00.06 |  30.81 ± 03.26 |
|                 Nim/clang |  1.66 ± 00.07 |    7.95 ± 00.07 |  33.56 ± 03.15 |
|                   Nim/gcc |  1.68 ± 00.08 |    7.54 ± 00.03 |  32.29 ± 03.24 |
|                   Crystal |  2.03 ± 00.01 |    5.27 ± 00.05 |  49.01 ± 02.01 |
|                      Ruby |  2.16 ± 00.08 |   73.48 ± 00.10 |  45.20 ± 04.12 |
|              Ruby (--jit) |  2.17 ± 00.09 |   73.54 ± 00.11 |  46.02 ± 03.90 |
|                     D/gdc |  2.24 ± 00.09 |   10.74 ± 00.05 |  47.68 ± 04.80 |
|                    D/ldc2 |  2.24 ± 00.03 |   11.01 ± 00.08 |  48.69 ± 02.00 |
|                      Java |  2.29 ± 00.07 |  349.83 ± 30.43 |  49.64 ± 03.24 |
|                    Kotlin |  2.46 ± 00.09 |  354.40 ± 20.32 |  51.46 ± 03.09 |
|                        Go |  2.49 ± 00.01 |    9.62 ± 00.37 |  47.50 ± 02.12 |
|                     Scala |  2.54 ± 00.08 |  140.92 ± 09.78 |  50.36 ± 02.67 |
|       Perl (MIME::Base64) |  2.66 ± 00.12 |    7.26 ± 00.09 |  50.59 ± 05.41 |
|                       PHP |  2.89 ± 00.10 |   15.58 ± 00.12 |  53.70 ± 04.90 |
|                   Node.js |  2.97 ± 00.11 | 1060.52 ± 02.27 |  60.45 ± 04.89 |
|       C++/g++ (libcrypto) |  3.07 ± 00.15 |    5.55 ± 00.07 |  66.35 ± 06.37 |
|                  Go/gccgo |  3.69 ± 00.02 |   29.31 ± 00.33 |  77.21 ± 03.27 |
|                     D/dmd |  3.95 ± 00.14 |   11.66 ± 00.06 |  88.16 ± 04.59 |
|                       Tcl |  4.30 ± 00.15 |    5.07 ± 00.08 |  79.75 ± 08.10 |
|                    Python |  5.39 ± 00.24 |    9.50 ± 00.02 | 111.13 ± 10.54 |
|               Python/pypy |  5.40 ± 00.17 |  110.40 ± 00.17 | 115.28 ± 08.83 |
|              C#/.NET Core |  5.48 ± 00.16 |   73.84 ± 01.38 | 103.67 ± 05.56 |
|                     Julia |  5.85 ± 00.25 |  259.61 ± 00.50 | 125.96 ± 08.85 |
|  Ruby/truffleruby (--jvm) |  6.22 ± 00.37 |  743.78 ± 18.34 | 124.82 ± 13.03 |
|                   C#/Mono |  7.62 ± 00.30 |   39.61 ± 00.10 | 158.67 ± 14.33 |
|                Ruby/jruby | 10.36 ± 00.46 |  360.51 ± 19.92 | 217.37 ± 21.46 |
| Perl (MIME::Base64::Perl) | 16.43 ± 00.82 |    9.02 ± 00.10 | 345.61 ± 44.55 |
|          Ruby/truffleruby | 23.25 ± 00.90 |  616.66 ± 00.62 | 476.58 ± 40.06 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|                        Language |       Time, s |     Memory, MiB |        Energy, J |
| :------------------------------ | ------------: | --------------: | ---------------: |
|    C++/g++ (simdjson On-Demand) |  0.10 ± 00.00 |  169.45 ± 00.17 |     1.58 ± 00.07 |
|                    D/gdc (fast) |  0.10 ± 00.01 |  231.34 ± 00.32 |     2.60 ± 00.27 |
|         C++/g++ (DAW JSON Link) |  0.11 ± 00.00 |  109.23 ± 00.05 |     2.42 ± 00.29 |
|             Rust (Serde Custom) |  0.15 ± 00.01 |  108.47 ± 00.08 |     2.79 ± 00.29 |
|              Rust (Serde Typed) |  0.15 ± 00.01 |  120.19 ± 00.20 |     2.89 ± 00.30 |
|                 C++/g++ (gason) |  0.17 ± 00.01 |  206.35 ± 00.06 |     3.71 ± 00.47 |
|          C++/g++ (simdjson DOM) |  0.17 ± 00.00 |  286.23 ± 00.49 |     3.39 ± 00.15 |
|             C++/g++ (RapidJSON) |  0.22 ± 00.01 |  238.14 ± 00.09 |     4.81 ± 00.62 |
|                            Java |  0.54 ± 00.02 |  327.50 ± 01.47 |    14.71 ± 00.99 |
|         C++/g++ (RapidJSON SAX) |  0.55 ± 00.03 |  109.43 ± 00.05 |    11.62 ± 01.33 |
|                           Scala |  0.61 ± 00.03 |  396.98 ± 06.37 |    15.99 ± 01.23 |
|                         Node.js |  0.68 ± 00.03 |  430.79 ± 00.33 |    16.22 ± 01.38 |
|                   Go (jsoniter) |  0.70 ± 00.02 |  238.82 ± 00.28 |    14.71 ± 00.89 |
|                Crystal (Schema) |  0.80 ± 00.03 |  157.29 ± 00.13 |    15.40 ± 01.30 |
|                     Python/pypy |  0.81 ± 00.04 |  405.43 ± 00.23 |    18.73 ± 02.35 |
|                  Crystal (Pull) |  0.84 ± 00.06 |  128.56 ± 00.06 |    15.19 ± 02.57 |
|                   Julia (JSON3) |  0.87 ± 00.05 |  614.24 ± 20.45 |    18.60 ± 02.46 |
|            Rust (Serde Untyped) |  0.94 ± 00.03 |  948.43 ± 00.11 |    18.88 ± 01.59 |
|         Perl (Cpanel::JSON::XS) |  0.99 ± 00.04 |  524.43 ± 00.07 |    20.01 ± 01.90 |
|                         Crystal |  1.08 ± 00.06 |  503.70 ± 00.08 |    21.97 ± 02.43 |
|                              Go |  1.12 ± 00.05 |  209.46 ± 00.32 |    23.88 ± 02.19 |
|                             PHP |  1.23 ± 00.05 |  803.52 ± 00.14 |    26.92 ± 02.28 |
|                           V/gcc |  1.42 ± 00.07 |  591.91 ± 00.09 |    30.57 ± 02.98 |
|                         V/clang |  1.43 ± 00.05 |  592.41 ± 00.08 |    31.70 ± 02.42 |
|                        Go/gccgo |  1.48 ± 00.08 |  232.10 ± 05.87 |    29.91 ± 03.45 |
|            Nim/gcc (Packedjson) |  1.49 ± 00.07 |  399.36 ± 00.06 |    30.95 ± 03.50 |
|                C++/g++ (json-c) |  1.52 ± 00.06 | 1325.49 ± 00.07 |    33.63 ± 03.74 |
|          Nim/clang (Packedjson) |  1.53 ± 00.07 |  399.85 ± 00.05 |    31.72 ± 03.71 |
|                         Clojure |  1.61 ± 00.08 | 1020.39 ± 16.00 |    41.27 ± 02.62 |
|             CPython (UltraJSON) |  1.68 ± 00.05 |  662.54 ± 02.86 |    36.56 ± 01.47 |
|                         Haskell |  1.77 ± 00.10 |   10.34 ± 00.48 |    37.49 ± 04.16 |
|                          Python |  1.77 ± 00.06 |  493.07 ± 00.07 |    39.60 ± 02.51 |
|                         Nim/gcc |  1.88 ± 00.09 |  904.85 ± 00.13 |    38.09 ± 03.77 |
|                       Nim/clang |  1.94 ± 00.08 |  905.25 ± 00.13 |    37.82 ± 03.93 |
|                    C#/.NET Core |  2.01 ± 00.12 |  761.27 ± 00.11 |    44.82 ± 04.74 |
|                         C#/Mono |  2.16 ± 00.08 |  462.55 ± 00.15 |    43.34 ± 04.34 |
|                            Ruby |  2.28 ± 00.08 |  385.94 ± 16.27 |    45.44 ± 04.11 |
|                     Ruby (YAJL) |  2.28 ± 00.10 |  408.90 ± 00.18 |    45.57 ± 04.76 |
|                    Ruby (--jit) |  2.29 ± 00.08 |  395.60 ± 09.99 |    49.10 ± 05.21 |
|                           D/gdc |  2.30 ± 00.12 |  713.56 ± 00.07 |    44.22 ± 05.66 |
|                          D/ldc2 |  2.57 ± 00.05 |  789.58 ± 00.12 |    52.42 ± 02.35 |
|                       Rust (jq) |  3.90 ± 00.16 |  886.10 ± 00.75 |    76.39 ± 16.94 |
|                      Ruby/jruby |  3.92 ± 00.18 | 1964.91 ± 39.23 |   112.52 ± 05.56 |
|                 C++/g++ (Boost) |  4.11 ± 00.19 | 1549.66 ± 00.05 |    82.61 ± 09.84 |
|                           D/dmd |  5.03 ± 00.10 |  790.35 ± 00.06 |    99.09 ± 04.89 |
| C#/.NET Core (System.Text.Json) |  6.95 ± 00.26 |  646.91 ± 00.12 |   155.61 ± 11.91 |
|               Perl (JSON::Tiny) | 11.98 ± 00.38 |  647.40 ± 00.10 |   234.12 ± 21.39 |
|        Ruby/truffleruby (--jvm) | 23.63 ± 01.31 | 2141.52 ± 71.40 |   632.38 ± 73.35 |
|                Ruby/truffleruby | 66.10 ± 02.02 | 2902.65 ± 52.00 | 1385.38 ± 112.06 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |        Time, s |     Memory, MiB |         Energy, J |
| :----------------------- | -------------: | --------------: | ----------------: |
|          D/ldc2 (lubeck) |   0.15 ± 00.02 |   58.54 ± 00.19 |      7.76 ± 01.06 |
|  Nim/clang (Arraymancer) |   0.16 ± 00.05 |   57.48 ± 00.20 |      7.94 ± 00.43 |
|    Nim/gcc (Arraymancer) |   0.17 ± 00.05 |   57.49 ± 00.19 |      8.77 ± 00.73 |
|           Python (NumPy) |   0.19 ± 00.03 |   78.90 ± 00.08 |      9.72 ± 01.22 |
|              Java (ND4J) |   0.23 ± 00.04 |  223.47 ± 02.89 |      9.46 ± 01.50 |
|       Julia (threads: 8) |   0.43 ± 00.03 |  276.74 ± 00.31 |     18.94 ± 02.62 |
|       Julia (threads: 1) |   0.71 ± 00.03 |  276.93 ± 00.19 |     14.68 ± 01.48 |
|                   D/ldc2 |   2.02 ± 00.02 |   73.63 ± 00.08 |     43.23 ± 01.64 |
|                    D/dmd |   2.14 ± 00.03 |   74.26 ± 00.05 |     47.53 ± 01.78 |
|                    D/gdc |   2.16 ± 00.04 |   77.33 ± 00.09 |     46.82 ± 02.59 |
|                    C/gcc |   3.32 ± 00.10 |   70.11 ± 00.08 |     72.53 ± 02.94 |
|                     Rust |   3.37 ± 00.06 |   70.86 ± 00.07 |     68.37 ± 02.11 |
|                    Scala |   3.42 ± 00.06 |  148.00 ± 10.18 |     76.83 ± 03.07 |
|                     Java |   3.44 ± 00.26 |  115.22 ± 00.31 |     72.82 ± 05.81 |
|                Nim/clang |   3.51 ± 00.24 |   78.52 ± 04.68 |     71.08 ± 04.13 |
|                  Nim/gcc |   3.53 ± 00.37 |   77.80 ± 04.85 |     69.78 ± 04.83 |
|                 Go/gccgo |   3.60 ± 00.10 |   95.43 ± 03.98 |     73.70 ± 02.20 |
|                       Go |   3.66 ± 00.30 |   77.20 ± 00.14 |     76.49 ± 02.89 |
|          Julia (no BLAS) |   3.68 ± 00.08 |  249.03 ± 00.50 |     75.74 ± 03.36 |
|                  Crystal |   3.77 ± 00.13 |   63.78 ± 00.05 |     84.22 ± 07.04 |
|                    Swift |   3.78 ± 00.31 |  208.62 ± 00.37 |     76.19 ± 03.30 |
|                    V/gcc |   3.79 ± 00.14 |   70.80 ± 00.04 |     80.95 ± 07.89 |
|                  V/clang |   3.84 ± 00.13 |   71.24 ± 00.07 |     82.61 ± 06.96 |
|                  Node.js |   3.94 ± 00.12 |  105.53 ± 00.21 |     80.01 ± 07.02 |
|                   Kotlin |   4.19 ± 00.45 |  115.68 ± 00.33 |     79.85 ± 11.00 |
|              Python/pypy |   6.39 ± 00.23 |  133.48 ± 00.14 |    127.18 ± 12.59 |
|             C#/.NET Core |   7.14 ± 00.22 |  102.34 ± 00.10 |    147.97 ± 10.95 |
|                  C#/Mono |  11.32 ± 00.44 |   89.20 ± 00.09 |    210.87 ± 46.29 |
|         Ruby/truffleruby |  53.08 ± 03.61 |  743.39 ± 02.76 |   1129.45 ± 83.28 |
| Ruby/truffleruby (--jvm) |  74.94 ± 06.34 | 1057.16 ± 37.72 |  1698.31 ± 103.22 |
|             Ruby (--jit) | 221.08 ± 11.22 |   84.24 ± 00.04 |  4224.95 ± 269.05 |
|                     Ruby | 223.84 ± 12.08 |   83.97 ± 00.06 |  4470.52 ± 307.64 |
|                   Python | 246.17 ± 04.64 |   78.47 ± 00.07 |  4791.42 ± 364.39 |
|                      Tcl | 351.67 ± 10.56 |  407.67 ± 00.05 |  7318.24 ± 425.99 |
|                     Perl | 404.90 ± 11.84 |  608.54 ± 00.11 |  8339.54 ± 698.49 |
|               Ruby/jruby | 494.67 ± 20.11 |  939.97 ± 11.56 | 10414.50 ± 307.44 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |      Memory, MiB |        Energy, J |
| :----------- | -------------: | ---------------: | ---------------: |
|      Crystal |   6.99 ± 00.15 |   229.52 ± 00.88 |   168.26 ± 09.03 |
|      Nim/gcc |  11.89 ± 00.83 |   486.86 ± 07.13 |   270.21 ± 25.65 |
|    Nim/clang |  12.09 ± 00.40 |   490.53 ± 16.48 |   275.81 ± 22.95 |
|      C++/g++ |  13.49 ± 00.23 |   178.24 ± 00.04 |   296.23 ± 07.90 |
| C#/.NET Core |  14.97 ± 00.56 | 1447.68 ± 180.64 |   357.07 ± 21.26 |
|        Scala |  18.49 ± 01.17 |   388.93 ± 03.47 |   531.97 ± 53.30 |
|           Go |  18.73 ± 00.14 |   365.98 ± 08.66 |   415.64 ± 09.18 |
|       D/ldc2 |  19.50 ± 01.45 |   469.77 ± 16.15 |   475.34 ± 26.83 |
|        D/gdc |  22.79 ± 00.74 |   351.49 ± 00.10 |   488.14 ± 24.94 |
|        D/dmd |  23.97 ± 00.20 |   440.86 ± 00.63 |   567.34 ± 07.90 |
|      C#/Mono |  25.64 ± 01.16 |   340.45 ± 15.21 |   616.24 ± 45.94 |
|     Go/gccgo |  26.53 ± 00.35 |   386.27 ± 13.65 |   665.59 ± 04.98 |
|  Python/pypy |  29.52 ± 01.86 |   625.70 ± 48.90 |   666.76 ± 37.45 |
|       Python | 106.11 ± 03.57 |   407.52 ± 00.04 | 2358.32 ± 122.02 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 3.1.109                         |
| C#/.NET Core     | 3.4.1-beta4-20127-10 (d8180a5e) |
| C#/Mono          | 6.12.0.90                       |
| C/clang          | 10.0.1                          |
| C/gcc            | 10.2.0                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.1"                        |
| Crystal          | 0.35.1                          |
| D/dmd            | v2.094.0                        |
| D/gdc            | 10.2.0                          |
| D/ldc2           | 1.23.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 10.7.0.0 for F# 4.7             |
| Go               | go1.15.2                        |
| Go/gccgo         | 10.2.0                          |
| Haskell          | 8.10.2                          |
| Java             | 15                              |
| Julia            | v"1.5.2"                        |
| Kotlin           | 1.4.10                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20201002                        |
| Nim              | 1.2.6                           |
| Node.js          | v14.13.1                        |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.10                          |
| Perl             | v5.30.3                         |
| Python           | 3.8.6                           |
| Python/pypy      | 7.3.2-alpha0 for Python 3.7.4   |
| Racket           | "7.8"                           |
| Ruby             | 2.7.2p137                       |
| Ruby/jruby       | 9.2.13.0                        |
| Ruby/truffleruby | 20.2.0                          |
| Rust             | 1.47.0                          |
| Scala            | 2.13.3                          |
| Swift            | swift-5.3-RELEASE               |
| Tcl              | 8.6                             |
| V                | 0.1.29                          |
| Vala             | 0.48.11                         |

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
