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

UPDATE: 2020-09-17

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck](brainfuck)

### bench.b

|                 Language |        Time, s |    Memory, MiB |         Energy, J |
| :----------------------- | -------------: | -------------: | ----------------: |
|                  C++/g++ |   0.85 ± 00.02 |   1.50 ± 00.02 |     16.04 ± 00.76 |
|                 Vala/gcc |   1.78 ± 00.09 |   4.00 ± 00.06 |     34.60 ± 03.50 |
|                  Nim/gcc |   1.80 ± 00.06 |   1.91 ± 00.05 |     33.05 ± 02.43 |
|                    OCaml |   1.84 ± 00.07 |   5.06 ± 00.03 |     37.37 ± 02.81 |
|                    C/gcc |   1.88 ± 00.08 |   0.53 ± 00.03 |     34.00 ± 02.67 |
|                Nim/clang |   1.88 ± 00.09 |   2.39 ± 00.06 |     37.37 ± 03.14 |
|                   Kotlin |   1.88 ± 00.06 |  39.87 ± 00.32 |     33.24 ± 02.62 |
|                   D/ldc2 |   1.90 ± 00.07 |   2.97 ± 00.08 |     35.57 ± 02.91 |
|                    D/gdc |   2.02 ± 00.12 |   6.37 ± 00.06 |     40.01 ± 03.81 |
|               Vala/clang |   2.05 ± 00.21 |   3.97 ± 00.06 |     40.26 ± 04.31 |
|                     Rust |   2.10 ± 00.11 |   2.04 ± 00.07 |     42.06 ± 04.11 |
|                 Go/gccgo |   2.22 ± 00.09 |  22.08 ± 03.85 |     43.29 ± 03.25 |
|                  C/clang |   2.27 ± 00.06 |   0.54 ± 00.03 |     39.13 ± 02.01 |
|                     Java |   2.30 ± 00.11 |  37.91 ± 00.50 |     43.12 ± 04.52 |
|             C#/.NET Core |   2.43 ± 00.10 |  34.76 ± 00.12 |     45.63 ± 04.15 |
|                    MLton |   2.52 ± 00.11 |   0.54 ± 00.03 |     48.88 ± 04.27 |
|                       Go |   2.52 ± 00.09 |   3.16 ± 00.37 |     44.45 ± 03.35 |
|                    V/gcc |   2.56 ± 00.12 |   0.55 ± 00.02 |     47.24 ± 05.37 |
|                  Crystal |   2.63 ± 00.09 |   3.33 ± 00.03 |     50.62 ± 04.68 |
|             F#/.NET Core |   2.82 ± 00.14 | 125.42 ± 00.64 |     54.34 ± 02.48 |
|              Chez Scheme |   2.92 ± 00.09 |  29.17 ± 00.61 |     54.79 ± 04.01 |
|                  V/clang |   2.93 ± 00.14 |   0.87 ± 00.03 |     55.97 ± 05.32 |
|                    Julia |   3.23 ± 00.16 | 179.51 ± 00.54 |     63.40 ± 05.30 |
|                    Scala |   3.59 ± 00.11 | 123.64 ± 11.40 |     71.12 ± 02.20 |
|                    D/dmd |   3.68 ± 00.19 |   3.52 ± 00.08 |     68.34 ± 06.31 |
|                  C#/Mono |   4.51 ± 00.22 |  20.41 ± 00.08 |     87.67 ± 06.43 |
|         Haskell (MArray) |   4.61 ± 00.16 |   5.40 ± 00.03 |     89.03 ± 06.54 |
|                  Node.js |   5.15 ± 00.22 |  33.18 ± 00.57 |     95.99 ± 06.98 |
|               Lua/luajit |   7.43 ± 00.25 |   2.78 ± 00.06 |    137.68 ± 09.30 |
|                   Racket |   8.04 ± 00.33 | 106.90 ± 00.10 |    145.51 ± 08.60 |
|              Python/pypy |  13.51 ± 00.61 | 108.32 ± 00.09 |    269.25 ± 23.87 |
|                  Haskell |  16.25 ± 00.76 |   5.42 ± 00.04 |    339.93 ± 27.37 |
| Ruby/truffleruby (--jvm) |  18.32 ± 00.66 | 923.76 ± 44.85 |    512.43 ± 35.98 |
|         Ruby/truffleruby |  38.02 ± 18.36 | 517.76 ± 12.55 |   735.00 ± 338.56 |
|                      Lua |  59.95 ± 02.44 |   2.63 ± 00.03 |  1103.48 ± 103.01 |
|             Ruby (--jit) |  62.09 ± 02.50 |  14.25 ± 00.05 |   1180.81 ± 79.66 |
|                     Ruby |  87.69 ± 01.72 |  14.00 ± 00.06 |   1519.69 ± 51.60 |
|               Ruby/jruby | 107.03 ± 03.67 | 440.58 ± 06.10 |   2027.44 ± 98.42 |
|                   Elixir | 118.71 ± 03.07 |  53.72 ± 00.55 |  2198.08 ± 120.65 |
|                   Python | 237.45 ± 04.30 |   9.33 ± 00.07 |  4326.33 ± 104.13 |
|                 Tcl (FP) | 275.71 ± 07.44 |   4.30 ± 00.05 |  5319.30 ± 231.72 |
|                     Perl | 353.43 ± 12.86 |   6.40 ± 00.09 |  6464.89 ± 472.86 |
|                Tcl (OOP) | 549.45 ± 12.93 |   4.32 ± 00.07 | 10387.71 ± 461.41 |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |        Time, s |    Memory, MiB |        Energy, J |
| :----------------------- | -------------: | -------------: | ---------------: |
|                  C++/g++ |  13.51 ± 00.18 |   3.70 ± 00.06 |   225.63 ± 04.71 |
|                    C/gcc |  13.58 ± 00.13 |   1.60 ± 00.03 |   230.90 ± 03.96 |
|                 Vala/gcc |  13.73 ± 00.17 |   5.79 ± 00.05 |   234.86 ± 05.03 |
|                    D/gdc |  14.47 ± 00.28 |   7.18 ± 00.03 |   257.45 ± 06.22 |
|                   D/ldc2 |  16.35 ± 00.15 |   3.85 ± 00.06 |   290.30 ± 06.36 |
|               Vala/clang |  16.51 ± 00.20 |   5.83 ± 00.09 |   291.95 ± 05.41 |
|                    V/gcc |  16.57 ± 00.15 |   2.45 ± 00.06 |   292.20 ± 02.72 |
|                  Crystal |  16.67 ± 00.21 |   3.77 ± 00.06 |   289.15 ± 05.47 |
|             C#/.NET Core |  18.65 ± 00.25 |  35.95 ± 00.15 |   319.33 ± 05.31 |
|                  C/clang |  18.97 ± 00.04 |   1.62 ± 00.03 |   302.59 ± 05.45 |
|                  Nim/gcc |  19.10 ± 00.09 |   2.43 ± 00.05 |   330.75 ± 04.39 |
|                     Rust |  19.47 ± 00.27 |   2.33 ± 00.12 |   349.64 ± 06.85 |
|                Nim/clang |  20.97 ± 00.22 |   2.90 ± 00.06 |   373.13 ± 08.92 |
|                  V/clang |  22.70 ± 00.40 |   2.88 ± 00.06 |   414.99 ± 10.29 |
|                     Java |  22.80 ± 01.91 |  43.70 ± 00.64 |   388.42 ± 29.83 |
|                   Kotlin |  24.01 ± 00.39 |  45.46 ± 00.43 |   405.84 ± 08.44 |
|                    MLton |  24.50 ± 00.21 |   4.33 ± 00.33 |   444.62 ± 02.22 |
|                    Scala |  24.95 ± 00.31 | 109.79 ± 09.84 |   471.44 ± 03.20 |
|                 Go/gccgo |  27.16 ± 00.46 |  22.45 ± 00.21 |   486.89 ± 08.64 |
|                    OCaml |  30.30 ± 01.08 |   9.90 ± 01.49 |   607.14 ± 43.37 |
|                       Go |  35.52 ± 00.32 |   4.53 ± 00.33 |   603.33 ± 09.50 |
|              Chez Scheme |  43.36 ± 00.35 |  29.38 ± 00.12 |   796.68 ± 11.54 |
|                    D/dmd |  48.21 ± 00.05 |   4.43 ± 00.04 |   748.05 ± 22.71 |
|                  C#/Mono |  50.16 ± 00.70 |  21.26 ± 00.08 |   888.29 ± 12.05 |
|                  Node.js |  58.96 ± 00.41 |  36.90 ± 00.37 |  1026.73 ± 16.08 |
|                    Julia |  62.48 ± 01.35 | 180.13 ± 00.63 |  1152.92 ± 21.35 |
|         Haskell (MArray) |  66.20 ± 01.55 |   6.63 ± 00.05 |  1267.89 ± 53.65 |
|               Lua/luajit |  70.60 ± 00.19 |   3.74 ± 00.09 |  1168.44 ± 23.33 |
|              Python/pypy |  72.51 ± 01.02 | 109.58 ± 00.11 |  1313.30 ± 23.37 |
| Ruby/truffleruby (--jvm) | 116.73 ± 03.14 | 926.02 ± 46.09 |  2291.17 ± 54.85 |
|             F#/.NET Core | 131.45 ± 05.40 | 128.40 ± 00.14 | 2306.60 ± 324.34 |
|                   Racket | 139.20 ± 02.04 | 106.91 ± 00.12 |  2536.83 ± 45.01 |
|         Ruby/truffleruby | 208.88 ± 40.41 | 541.79 ± 28.87 | 3736.64 ± 759.88 |
|                  Haskell | 227.07 ± 03.35 |   6.64 ± 00.04 |  4302.82 ± 75.20 |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |       Time, s |     Memory, MiB |      Energy, J |
| :------------------------ | ------------: | --------------: | -------------: |
|            C/gcc (aklomp) |  0.16 ± 00.01 |    2.00 ± 00.04 |   3.57 ± 00.11 |
|                   V/clang |  0.92 ± 00.03 |    2.48 ± 00.07 |  15.54 ± 00.44 |
|                     C/gcc |  1.34 ± 00.01 |    1.94 ± 00.05 |  20.74 ± 00.50 |
|                      Rust |  1.39 ± 00.05 |    2.57 ± 00.10 |  24.77 ± 00.98 |
|                     V/gcc |  1.72 ± 00.05 |    1.86 ± 00.10 |  28.76 ± 00.95 |
|                 Nim/clang |  1.74 ± 00.05 |    8.00 ± 00.05 |  30.83 ± 01.00 |
|                   Nim/gcc |  1.77 ± 00.05 |    7.52 ± 00.03 |  30.37 ± 01.06 |
|                   Crystal |  2.06 ± 00.01 |    5.27 ± 00.07 |  50.66 ± 01.64 |
|                      Ruby |  2.16 ± 00.05 |   73.43 ± 00.12 |  38.71 ± 01.62 |
|              Ruby (--jit) |  2.18 ± 00.05 |   73.49 ± 00.16 |  40.01 ± 01.19 |
|                    D/ldc2 |  2.29 ± 00.07 |   11.00 ± 00.08 |  51.75 ± 02.92 |
|                     D/gdc |  2.38 ± 00.06 |   10.81 ± 00.06 |  43.56 ± 01.69 |
|                      Java |  2.48 ± 00.05 |  327.03 ± 31.65 |  44.67 ± 00.71 |
|                        Go |  2.54 ± 00.01 |    9.24 ± 00.23 |  49.35 ± 01.28 |
|                    Kotlin |  2.62 ± 00.04 |  345.21 ± 28.82 |  48.40 ± 00.93 |
|                     Scala |  2.67 ± 00.03 |  144.69 ± 10.52 |  50.85 ± 01.12 |
|       Perl (MIME::Base64) |  2.76 ± 00.09 |    7.35 ± 00.07 |  49.93 ± 03.01 |
|                       PHP |  2.93 ± 00.03 |   15.64 ± 00.13 |  50.80 ± 00.67 |
|                   Node.js |  3.10 ± 00.02 | 1058.35 ± 01.55 |  57.48 ± 00.71 |
|       C++/g++ (libcrypto) |  3.27 ± 00.07 |    5.54 ± 00.06 |  59.85 ± 01.85 |
|                  Go/gccgo |  3.78 ± 00.01 |   29.57 ± 00.57 |  87.98 ± 02.33 |
|                     D/dmd |  4.05 ± 00.09 |   11.59 ± 00.08 |  82.89 ± 03.01 |
|                       Tcl |  4.44 ± 00.10 |    5.08 ± 00.10 |  76.51 ± 01.94 |
|               Python/pypy |  5.06 ± 00.09 |  109.91 ± 00.46 |  83.41 ± 01.55 |
|              C#/.NET Core |  5.54 ± 00.16 |   73.95 ± 02.90 | 101.92 ± 03.50 |
|                    Python |  5.65 ± 00.10 |    9.43 ± 00.07 |  98.51 ± 02.20 |
|                     Julia |  6.28 ± 00.04 |  259.86 ± 00.31 | 110.23 ± 01.43 |
|  Ruby/truffleruby (--jvm) |  6.59 ± 00.54 |  749.51 ± 35.76 | 123.02 ± 11.18 |
|                   C#/Mono |  8.06 ± 00.13 |   39.65 ± 00.10 | 144.45 ± 01.74 |
|                Ruby/jruby | 11.12 ± 00.62 |  348.88 ± 21.91 | 193.90 ± 10.30 |
| Perl (MIME::Base64::Perl) | 17.13 ± 00.52 |    9.01 ± 00.10 | 313.46 ± 09.78 |
|          Ruby/truffleruby | 24.92 ± 00.63 |  617.53 ± 00.94 | 428.99 ± 14.88 |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|                        Language |       Time, s |     Memory, MiB |       Energy, J |
| :------------------------------ | ------------: | --------------: | --------------: |
|         C++/g++ (DAW JSON Link) |  0.09 ± 00.00 |  109.18 ± 00.04 |    2.04 ± 00.11 |
|                    D/gdc (fast) |  0.11 ± 00.00 |  231.32 ± 00.35 |    2.13 ± 00.05 |
|             Rust (Serde Custom) |  0.14 ± 00.01 |  108.42 ± 00.09 |    2.48 ± 00.20 |
|              Rust (Serde Typed) |  0.14 ± 00.01 |  120.10 ± 00.14 |    2.89 ± 00.32 |
|              C++/g++ (simdjson) |  0.17 ± 00.00 |  286.67 ± 00.06 |    3.41 ± 00.18 |
|                 C++/g++ (gason) |  0.18 ± 00.00 |  206.33 ± 00.06 |    3.42 ± 00.07 |
|             C++/g++ (RapidJSON) |  0.22 ± 00.00 |  238.08 ± 00.07 |    4.29 ± 00.08 |
|                            Java |  0.52 ± 00.02 |  327.29 ± 01.92 |   14.15 ± 00.46 |
|         C++/g++ (RapidJSON SAX) |  0.58 ± 00.01 |  109.46 ± 00.07 |   10.22 ± 00.17 |
|                           Scala |  0.59 ± 00.03 |  393.29 ± 08.72 |   15.52 ± 00.85 |
|                         Node.js |  0.69 ± 00.01 |  430.21 ± 00.67 |   15.44 ± 00.22 |
|                   Go (jsoniter) |  0.73 ± 00.01 |  238.61 ± 00.32 |   13.76 ± 00.14 |
|                Crystal (Schema) |  0.82 ± 00.04 |  157.28 ± 00.14 |   14.96 ± 01.12 |
|                     Python/pypy |  0.85 ± 00.02 |  404.72 ± 00.13 |   16.22 ± 00.28 |
|                  Crystal (Pull) |  0.86 ± 00.04 |  128.53 ± 00.05 |   14.65 ± 01.26 |
|                   Julia (JSON3) |  0.90 ± 00.03 |  620.07 ± 23.36 |   16.49 ± 00.36 |
|            Rust (Serde Untyped) |  0.94 ± 00.04 |  948.49 ± 00.08 |   16.44 ± 00.85 |
|         Perl (Cpanel::JSON::XS) |  1.01 ± 00.02 |  524.40 ± 00.07 |   18.54 ± 00.35 |
|                         Crystal |  1.12 ± 00.03 |  503.67 ± 00.08 |   20.69 ± 00.98 |
|                              Go |  1.18 ± 00.03 |  209.52 ± 00.31 |   21.84 ± 00.62 |
|                             PHP |  1.28 ± 00.03 |  803.75 ± 00.19 |   22.98 ± 01.26 |
|                           V/gcc |  1.50 ± 00.03 |  591.98 ± 00.07 |   26.54 ± 00.45 |
|                         V/clang |  1.52 ± 00.02 |  592.39 ± 00.08 |   27.23 ± 00.59 |
|                        Go/gccgo |  1.55 ± 00.04 |  228.35 ± 00.22 |   28.77 ± 00.70 |
|            Nim/gcc (Packedjson) |  1.59 ± 00.08 |  399.37 ± 00.05 |   28.74 ± 00.91 |
|                C++/g++ (json-c) |  1.59 ± 00.02 | 1325.51 ± 00.08 |   29.64 ± 00.40 |
|          Nim/clang (Packedjson) |  1.60 ± 00.04 |  399.87 ± 00.04 |   29.28 ± 00.56 |
|                         Clojure |  1.65 ± 00.04 | 1002.73 ± 35.47 |   42.23 ± 01.34 |
|             CPython (UltraJSON) |  1.72 ± 00.04 |  661.24 ± 02.79 |   28.61 ± 00.51 |
|                          Python |  1.84 ± 00.02 |  493.18 ± 00.06 |   32.91 ± 00.33 |
|                         Haskell |  1.86 ± 00.04 |    9.64 ± 00.06 |   35.53 ± 00.52 |
|                         Nim/gcc |  1.92 ± 00.04 |  904.88 ± 00.12 |   34.73 ± 00.77 |
|                       Nim/clang |  1.94 ± 00.03 |  905.23 ± 00.13 |   35.80 ± 00.46 |
|                    C#/.NET Core |  2.11 ± 00.05 |  761.30 ± 00.09 |   40.45 ± 00.87 |
|                         C#/Mono |  2.23 ± 00.06 |  462.73 ± 00.19 |   42.19 ± 01.71 |
|                            Ruby |  2.30 ± 00.02 |  396.89 ± 00.07 |   41.88 ± 00.63 |
|                     Ruby (YAJL) |  2.34 ± 00.03 |  406.27 ± 00.05 |   42.70 ± 00.61 |
|                    Ruby (--jit) |  2.35 ± 00.02 |  397.06 ± 00.06 |   44.03 ± 00.84 |
|                           D/gdc |  2.38 ± 00.07 |  713.62 ± 00.06 |   41.79 ± 02.21 |
|                          D/ldc2 |  2.66 ± 00.02 |  789.70 ± 00.09 |   48.25 ± 00.47 |
|                      Ruby/jruby |  3.93 ± 00.12 | 1949.68 ± 44.70 |  114.95 ± 06.14 |
|                       Rust (jq) |  3.98 ± 00.12 |  885.83 ± 01.19 |   67.82 ± 02.31 |
|                 C++/g++ (Boost) |  4.24 ± 00.06 | 1549.66 ± 00.05 |   77.49 ± 01.92 |
|                           D/dmd |  5.19 ± 00.06 |  790.24 ± 00.08 |   90.83 ± 01.30 |
| C#/.NET Core (System.Text.Json) |  7.52 ± 00.12 |  647.00 ± 00.15 |  137.35 ± 02.38 |
|               Perl (JSON::Tiny) | 12.17 ± 00.26 |  647.40 ± 00.10 |  232.96 ± 03.24 |
|        Ruby/truffleruby (--jvm) | 24.48 ± 00.98 | 2160.44 ± 66.52 |  651.61 ± 73.72 |
|                Ruby/truffleruby | 68.56 ± 01.89 | 2857.50 ± 96.40 | 1179.87 ± 56.24 |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |        Time, s |     Memory, MiB |         Energy, J |
| :----------------------- | -------------: | --------------: | ----------------: |
|          D/ldc2 (lubeck) |   0.13 ± 00.00 |   58.48 ± 00.12 |      7.79 ± 00.11 |
|  Nim/clang (Arraymancer) |   0.14 ± 00.00 |   57.47 ± 00.15 |      7.95 ± 00.11 |
|    Nim/gcc (Arraymancer) |   0.15 ± 00.00 |   57.49 ± 00.10 |      8.58 ± 00.15 |
|           Python (NumPy) |   0.18 ± 00.00 |   78.84 ± 00.10 |      9.58 ± 00.32 |
|              Java (ND4J) |   0.22 ± 00.03 |  222.81 ± 02.55 |     10.32 ± 01.22 |
|       Julia (threads: 8) |   0.41 ± 00.01 |  276.39 ± 00.28 |     20.43 ± 01.10 |
|       Julia (threads: 1) |   0.73 ± 00.01 |  276.80 ± 00.26 |     13.60 ± 00.38 |
|                   D/ldc2 |   2.15 ± 00.01 |   73.65 ± 00.07 |     33.58 ± 00.46 |
|                    D/gdc |   2.28 ± 00.01 |   77.46 ± 00.09 |     38.29 ± 00.71 |
|                    D/dmd |   2.31 ± 00.01 |   74.23 ± 00.05 |     37.36 ± 00.38 |
|                     Java |   3.63 ± 00.01 |  115.14 ± 00.30 |     57.30 ± 00.55 |
|                    C/gcc |   3.72 ± 00.01 |   70.11 ± 00.04 |     53.21 ± 01.17 |
|                     Rust |   3.73 ± 00.01 |   70.95 ± 00.06 |     53.13 ± 00.86 |
|                    Scala |   3.74 ± 00.07 |  154.62 ± 10.00 |     60.83 ± 01.35 |
|                  Nim/gcc |   3.82 ± 00.01 |   76.84 ± 05.13 |     55.11 ± 02.26 |
|                Nim/clang |   3.84 ± 00.02 |   81.55 ± 05.92 |     55.14 ± 02.15 |
|                 Go/gccgo |   3.94 ± 00.02 |   94.16 ± 00.23 |     59.08 ± 01.48 |
|                       Go |   3.95 ± 00.02 |   77.04 ± 00.12 |     60.18 ± 01.21 |
|                    V/gcc |   3.97 ± 00.02 |   70.77 ± 00.08 |     61.80 ± 01.07 |
|                    Swift |   3.99 ± 00.01 |  205.28 ± 00.09 |     61.52 ± 01.03 |
|                  V/clang |   4.00 ± 00.02 |   71.24 ± 00.08 |     71.81 ± 01.16 |
|                  Crystal |   4.01 ± 00.02 |   63.82 ± 00.06 |     72.19 ± 00.99 |
|                   Kotlin |   4.01 ± 00.17 |  115.80 ± 00.38 |     66.68 ± 04.64 |
|          Julia (no BLAS) |   4.02 ± 00.01 |  249.03 ± 00.59 |     62.08 ± 01.87 |
|                  Node.js |   4.12 ± 00.02 |  104.38 ± 00.12 |     70.84 ± 02.14 |
|              Python/pypy |   6.50 ± 00.08 |  132.95 ± 00.17 |    117.80 ± 02.19 |
|             C#/.NET Core |   7.34 ± 00.07 |  102.51 ± 00.18 |    134.32 ± 03.83 |
|                  C#/Mono |  11.55 ± 00.15 |   89.35 ± 00.08 |    205.29 ± 03.96 |
|         Ruby/truffleruby |  53.29 ± 00.38 |  744.92 ± 01.85 |   1021.14 ± 12.30 |
| Ruby/truffleruby (--jvm) |  75.59 ± 01.07 | 1055.51 ± 38.61 |  1352.84 ± 111.96 |
|             Ruby (--jit) | 216.65 ± 03.88 |   84.21 ± 00.08 |  4051.27 ± 542.30 |
|                     Ruby | 223.59 ± 04.05 |   83.99 ± 00.04 |  4339.66 ± 141.54 |
|                   Python | 248.38 ± 04.51 |   78.48 ± 00.06 |  4795.00 ± 180.85 |
|                      Tcl | 361.32 ± 03.89 |  407.67 ± 00.05 |   6832.12 ± 75.69 |
|                     Perl | 424.07 ± 06.26 |  608.54 ± 00.12 |  6939.78 ± 105.77 |
|               Ruby/jruby | 516.99 ± 15.96 |  941.17 ± 14.71 | 9169.67 ± 2069.52 |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |        Time, s |     Memory, MiB |       Energy, J |
| :----------- | -------------: | --------------: | --------------: |
|      Crystal |   7.14 ± 00.03 |  229.19 ± 01.42 |  149.77 ± 04.20 |
|      Nim/gcc |  12.08 ± 00.07 |  484.33 ± 09.21 |  228.45 ± 03.85 |
|    Nim/clang |  12.41 ± 00.05 |  488.02 ± 17.29 |  232.55 ± 03.66 |
| C#/.NET Core |  15.46 ± 00.22 | 1406.35 ± 86.75 |  323.96 ± 09.17 |
|      C++/g++ |  16.57 ± 00.29 |  178.27 ± 00.06 |  253.26 ± 05.60 |
|        Scala |  18.10 ± 00.40 |  389.45 ± 03.37 |  549.26 ± 11.45 |
|       D/ldc2 |  19.22 ± 00.27 |  471.91 ± 15.59 |  450.57 ± 19.14 |
|           Go |  19.25 ± 00.06 |  365.69 ± 10.06 |  391.77 ± 04.40 |
|        D/gdc |  23.44 ± 00.70 |  358.38 ± 21.57 |  368.62 ± 66.60 |
|        D/dmd |  24.26 ± 00.19 |  474.23 ± 18.78 |  513.66 ± 10.01 |
|      C#/Mono |  25.70 ± 00.43 |  344.07 ± 20.11 |  558.55 ± 09.15 |
|     Go/gccgo |  27.14 ± 00.23 |  393.12 ± 24.20 |  633.27 ± 04.13 |
|  Python/pypy |  30.17 ± 00.23 |  637.55 ± 77.21 |  541.21 ± 05.08 |
|       Python | 109.03 ± 00.56 |  406.44 ± 00.07 | 1892.34 ± 25.92 |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 3.1.108                         |
| C#/.NET Core     | 3.4.1-beta4-20127-10 (d8180a5e) |
| C#/Mono          | 6.12.0.90                       |
| C/clang          | 10.0.1                          |
| C/gcc            | 10.2.0                          |
| Chez Scheme      | 9.5                             |
| Clojure          | "1.10.1"                        |
| Crystal          | 0.35.1                          |
| D/dmd            | v2.093.1                        |
| D/gdc            | 10.2.0                          |
| D/ldc2           | 1.23.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 10.7.0.0 for F# 4.7             |
| Go               | go1.15.2                        |
| Go/gccgo         | 10.2.0                          |
| Haskell          | 8.10.2                          |
| Java             | 15                              |
| Julia            | v"1.5.1"                        |
| Kotlin           | 1.4.10                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20200817                        |
| Nim              | 1.2.6                           |
| Node.js          | v14.10.1                        |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.9                           |
| Perl             | v5.30.3                         |
| Python           | 3.8.5                           |
| Python/pypy      | 7.3.1-final0 for Python 3.6.9   |
| Racket           | "7.8"                           |
| Ruby             | 2.7.1p83                        |
| Ruby/jruby       | 9.2.13.0                        |
| Ruby/truffleruby | 20.2.0                          |
| Rust             | 1.46.0                          |
| Scala            | 2.13.3                          |
| Swift            | swift-5.2.5-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.1.29                          |
| Vala             | 0.48.10                         |

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
