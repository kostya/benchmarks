Some benchmarks of different languages
--------------------------------------

The benchmarks follow the criteria:

  - They are written as the average software developer would write them, i.e.

    - The algorithms are implemented as cited in public sources;
    - The libraries are used as described in the tutorials, documentation and examples;
    - Used data structures are idiomatic.

  - The used algorithms are similar between the languages (reference implementations), variants are acceptable if the reference implementation exists.
  - All final binaries are releases (optimized for performance if possible) as debug performance may vary too much depending on the compiler.
  - JIT warming up is applied when necessary, and the actual measurements are taken only after the test signals the runner with the TCP request.

# UPDATE 

2019-12-13

# Brainfuck v2.0

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck v2.0](brainfuck2)
[Brainfuck v1.0](brainfuck)

### bench.b

|        Language |        Time, s |    Memory, MiB |         Energy, J |
| :-------------- | -------------: | -------------: | ----------------: |
|          Kotlin |   2.02 ± 00.03 |  39.50 ± 00.17 |     51.10 ± 01.88 |
|         Nim GCC |   2.21 ± 00.04 |   1.79 ± 00.05 |     55.55 ± 01.38 |
|             C++ |   2.22 ± 00.06 |   3.50 ± 00.06 |     59.26 ± 09.25 |
|           OCaml |   2.52 ± 00.01 |   4.78 ± 00.18 |     64.83 ± 02.00 |
|          GCC Go |   2.81 ± 00.33 |  18.83 ± 00.36 |     74.35 ± 07.35 |
|            Java |   2.86 ± 00.16 |  38.47 ± 00.11 |     72.61 ± 03.70 |
|              Go |   2.97 ± 00.01 |   3.51 ± 00.17 |     77.73 ± 00.53 |
|         Crystal |   3.07 ± 00.03 |   3.12 ± 00.08 |     83.40 ± 11.22 |
|           MLton |   3.21 ± 00.01 |   0.74 ± 00.05 |     83.22 ± 00.44 |
|            Rust |   3.40 ± 00.07 |   2.11 ± 00.05 |     91.57 ± 11.73 |
|       Nim Clang |   3.45 ± 00.04 |   2.27 ± 00.06 |     86.81 ± 01.75 |
|             GDC |   3.56 ± 00.06 |   6.31 ± 00.07 |     91.91 ± 01.48 |
|             LDC |   3.57 ± 00.04 |   2.97 ± 00.04 |     89.26 ± 02.04 |
|           Julia |   4.01 ± 00.22 | 166.84 ± 00.88 |    104.61 ± 12.76 |
|    C# .NET Core |   4.31 ± 00.01 |  28.82 ± 00.11 |    109.03 ± 01.56 |
|           Scala |   4.37 ± 00.25 | 142.42 ± 10.24 |    119.58 ± 02.00 |
|             DMD |   4.54 ± 00.01 |   3.57 ± 00.04 |    117.29 ± 00.59 |
|  Haskell MArray |   5.20 ± 00.02 |   5.29 ± 00.10 |    141.23 ± 01.88 |
|    F# .NET Core |   5.48 ± 00.02 |  36.21 ± 00.15 |    135.06 ± 01.88 |
|         Node.js |   5.78 ± 00.01 |  32.74 ± 00.11 |    148.46 ± 01.78 |
|         C# Mono |   7.27 ± 00.35 |  19.94 ± 00.20 |    181.94 ± 06.84 |
|           V GCC |   7.66 ± 00.23 |   0.71 ± 00.04 |    186.48 ± 05.67 |
|         V Clang |   9.53 ± 00.24 |   1.08 ± 00.06 |    236.65 ± 13.94 |
|          Racket |   9.61 ± 00.12 | 105.32 ± 00.62 |    256.96 ± 03.61 |
|          LuaJIT |  10.94 ± 00.15 |   2.60 ± 00.10 |    279.02 ± 04.46 |
|            PyPy |  20.07 ± 00.94 |  90.46 ± 00.13 |    543.63 ± 25.17 |
|     Chez Scheme |  24.19 ± 00.11 |  29.59 ± 00.05 |    604.36 ± 06.09 |
| TruffleRuby JVM |  28.32 ± 00.17 | 786.52 ± 44.77 |   1342.99 ± 17.96 |
|         Haskell |  28.51 ± 00.10 |   5.22 ± 00.04 |    763.91 ± 07.25 |
|           JRuby | 181.07 ± 12.19 | 239.61 ± 11.02 |  5031.09 ± 330.46 |
|            Ruby | 191.85 ± 01.71 |  14.09 ± 00.08 |   4979.95 ± 66.37 |
|             Lua | 202.02 ± 01.19 |   2.69 ± 00.07 |   5077.08 ± 57.01 |
|          Elixir | 279.17 ± 02.11 |  49.86 ± 00.83 |  7122.67 ± 112.16 |
|          Python | 390.84 ± 03.28 |   9.84 ± 00.06 |  10251.58 ± 98.10 |
|        Tcl (FP) | 489.97 ± 03.65 |   4.33 ± 00.04 | 12772.69 ± 138.71 |
|            Perl | 764.59 ± 05.10 |   6.33 ± 00.06 | 20322.41 ± 246.97 |
|        Tcl (OO) | 972.11 ± 06.77 |   4.29 ± 00.06 | 26029.12 ± 296.08 |

### mandel.b

[Mandel in Brainfuck](brainfuck2/mandel.b)

|        Language |        Time, s |    Memory, MiB |        Energy, J |
| :-------------- | -------------: | -------------: | ---------------: |
|             C++ |  21.25 ± 00.05 |   4.03 ± 00.09 |   549.86 ± 05.61 |
|           V GCC |  25.02 ± 00.06 |   2.34 ± 00.05 |   644.13 ± 05.25 |
|         Crystal |  25.33 ± 00.15 |   3.66 ± 00.06 |   657.11 ± 07.67 |
|         Nim GCC |  25.56 ± 00.07 |   2.32 ± 00.05 |   633.31 ± 05.47 |
|          Kotlin |  27.52 ± 00.47 |  45.50 ± 00.24 |   723.07 ± 12.79 |
|            Java |  28.41 ± 00.67 |  44.65 ± 00.39 |   762.49 ± 18.30 |
|            Rust |  29.17 ± 00.12 |   2.38 ± 00.05 |   760.46 ± 10.09 |
|           Scala |  30.85 ± 00.15 | 139.33 ± 06.72 |   833.64 ± 13.59 |
|             LDC |  31.50 ± 00.13 |   3.75 ± 00.07 |   789.88 ± 10.18 |
|       Nim Clang |  31.54 ± 00.14 |   2.80 ± 00.08 |   799.38 ± 10.49 |
|             GDC |  31.79 ± 00.25 |   7.21 ± 00.05 |   819.28 ± 10.61 |
|           MLton |  32.18 ± 00.09 |   3.54 ± 00.04 |   843.68 ± 11.64 |
|          GCC Go |  32.38 ± 00.15 |  26.03 ± 06.16 |   854.05 ± 10.55 |
|    C# .NET Core |  36.28 ± 00.12 |  30.66 ± 00.11 |   918.48 ± 10.54 |
|         V Clang |  39.32 ± 00.46 |   2.86 ± 00.05 |  1012.79 ± 17.00 |
|              Go |  45.19 ± 00.12 |   4.74 ± 00.03 |  1201.12 ± 09.97 |
|           OCaml |  47.61 ± 00.13 |   7.55 ± 00.10 |  1286.73 ± 14.53 |
|             DMD |  55.75 ± 00.20 |   4.37 ± 00.04 |  1413.72 ± 12.76 |
|         Node.js |  60.19 ± 00.15 |  35.68 ± 00.20 |  1520.83 ± 16.45 |
|         C# Mono |  71.48 ± 00.44 |  20.31 ± 00.09 |  1851.52 ± 23.80 |
|           Julia |  77.24 ± 00.44 | 165.72 ± 01.12 |  2002.31 ± 18.20 |
|  Haskell MArray |  98.61 ± 00.33 |   6.50 ± 00.08 |  2652.74 ± 30.50 |
|            PyPy | 106.01 ± 01.46 |  91.16 ± 00.20 |  2926.25 ± 43.11 |
|          LuaJIT | 108.73 ± 00.95 |   3.52 ± 00.07 |  2657.96 ± 43.69 |
|    F# .NET Core | 153.26 ± 00.50 |  37.74 ± 00.08 |  3903.34 ± 25.56 |
|          Racket | 170.28 ± 01.81 | 105.35 ± 00.67 |  4558.36 ± 85.98 |
| TruffleRuby JVM | 184.74 ± 03.19 | 835.52 ± 31.05 |  5318.34 ± 92.08 |
|     Chez Scheme | 245.69 ± 00.53 |  29.59 ± 00.08 |  6399.08 ± 64.01 |

# Base64

Testing large blob base64 encoding/decoding into newly allocated buffers.

[Base64](base64)

|                Language |       Time, s |    Memory, MiB |      Energy, J |
| :---------------------- | ------------: | -------------: | -------------: |
|                C aklomp |  0.37 ± 00.00 |   2.07 ± 00.04 |   8.14 ± 00.10 |
|                    Rust |  1.82 ± 00.00 |   2.65 ± 00.06 |  47.57 ± 00.70 |
|                       C |  1.85 ± 00.00 |   2.08 ± 00.04 |  46.60 ± 00.80 |
|                 Crystal |  2.32 ± 00.02 |   5.98 ± 00.05 |  82.56 ± 02.71 |
|                     LDC |  2.48 ± 00.00 |  11.17 ± 00.05 |  72.13 ± 01.56 |
|                    Ruby |  2.71 ± 00.01 |  75.87 ± 03.93 |  69.55 ± 00.63 |
|                   V GCC |  2.74 ± 00.05 |   1.70 ± 00.04 |  69.32 ± 02.30 |
|                     GDC |  2.80 ± 00.00 |  10.88 ± 00.07 |  73.22 ± 01.01 |
|                 V Clang |  2.83 ± 00.01 |   2.23 ± 00.07 |  73.15 ± 01.64 |
|                    Java |  3.05 ± 00.02 | 349.63 ± 29.13 |  84.31 ± 01.49 |
|                   Scala |  3.22 ± 00.04 | 155.74 ± 12.76 |  89.03 ± 01.89 |
|               Nim Clang |  3.27 ± 00.00 |   7.91 ± 00.08 |  86.26 ± 01.20 |
|                  Kotlin |  3.28 ± 00.01 | 350.43 ± 18.84 |  90.68 ± 01.87 |
|       Perl MIME::Base64 |  3.30 ± 00.01 |   7.19 ± 00.05 |  86.11 ± 01.46 |
|                 Nim GCC |  3.30 ± 00.01 |   7.57 ± 00.03 |  83.19 ± 01.34 |
|                 Node.js |  3.49 ± 00.11 | 113.69 ± 25.65 |  96.70 ± 03.70 |
|                     PHP |  3.94 ± 00.07 |  15.39 ± 00.13 | 100.01 ± 01.62 |
|           C++ libcrypto |  4.03 ± 00.02 |   5.93 ± 00.08 | 104.34 ± 01.92 |
|            C# .NET Core |  4.94 ± 00.20 |  32.52 ± 00.13 | 124.23 ± 04.39 |
|                      Go |  5.14 ± 00.08 |  15.64 ± 02.52 | 107.76 ± 02.45 |
|                     Tcl |  5.72 ± 00.01 |   5.11 ± 00.02 | 144.71 ± 01.65 |
|                     DMD |  5.76 ± 00.01 |  11.54 ± 00.07 | 153.39 ± 02.69 |
|                    PyPy |  6.47 ± 00.01 |  91.23 ± 00.12 | 170.13 ± 02.13 |
|                  Python |  6.84 ± 00.01 |  10.20 ± 00.09 | 169.84 ± 02.41 |
|                  GCC Go |  6.89 ± 00.05 |  43.36 ± 02.02 | 150.68 ± 03.18 |
|                 C# Mono |  8.83 ± 00.05 |  38.44 ± 00.12 | 227.60 ± 03.68 |
|         TruffleRuby JVM |  9.83 ± 00.23 | 598.84 ± 51.53 | 270.29 ± 08.51 |
|                   Julia | 10.79 ± 00.05 | 245.57 ± 08.17 | 276.67 ± 04.55 |
|                   JRuby | 16.02 ± 00.08 | 217.77 ± 11.82 | 428.63 ± 06.78 |
| Perl MIME::Base64::Perl | 28.43 ± 00.11 |   8.90 ± 00.06 | 730.30 ± 09.59 |

# Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|              Language |       Time, s |      Memory, MiB |        Energy, J |
| :-------------------- | ------------: | ---------------: | ---------------: |
|              GDC fast |  0.16 ± 00.00 |    94.51 ± 01.35 |     2.69 ± 00.11 |
|     Rust Serde custom |  0.22 ± 00.00 |   105.07 ± 00.04 |     5.27 ± 00.88 |
|      Rust Serde typed |  0.22 ± 00.00 |   115.49 ± 02.68 |     3.32 ± 01.15 |
|     C++ RapidJSON SAX |  0.27 ± 00.00 |     3.57 ± 00.05 |     5.44 ± 00.07 |
|          C++ simdjson |  0.30 ± 00.00 |   253.08 ± 00.66 |     5.55 ± 00.06 |
|         C++ RapidJSON |  0.44 ± 00.00 |   124.06 ± 00.05 |    10.85 ± 00.07 |
|             C++ gason |  0.51 ± 00.00 |   295.99 ± 00.64 |    10.78 ± 00.14 |
|                  Java |  0.58 ± 00.02 |   254.34 ± 12.30 |    15.48 ± 00.36 |
|                 Scala |  0.92 ± 00.39 |   228.96 ± 14.91 |    19.62 ± 02.75 |
|               Node.js |  1.24 ± 00.01 |   286.35 ± 00.29 |    39.28 ± 01.29 |
|    Rust Serde Untyped |  1.25 ± 00.01 |   916.54 ± 00.05 |    31.08 ± 00.40 |
|           Go jsoniter |  1.33 ± 00.01 |    57.09 ± 02.84 |    33.05 ± 00.45 |
|        Crystal Schema |  1.51 ± 00.01 |   156.97 ± 00.08 |    38.68 ± 00.59 |
| Perl Cpanel::JSON::XS |  1.59 ± 00.02 |   495.48 ± 13.88 |    40.25 ± 01.68 |
|                  PyPy |  1.62 ± 00.01 |   401.70 ± 00.91 |    42.41 ± 00.32 |
|                   PHP |  1.74 ± 00.01 |   777.91 ± 02.11 |    44.26 ± 00.24 |
|               Clojure |  1.79 ± 00.12 | 1249.33 ± 168.10 |    58.92 ± 06.49 |
|               Crystal |  1.80 ± 00.01 |   491.18 ± 01.74 |    51.53 ± 01.81 |
|     CPython UltraJSON |  2.06 ± 00.01 |   660.85 ± 02.87 |    54.34 ± 00.59 |
|                 Julia |  2.11 ± 00.01 |  1867.22 ± 00.30 |    54.28 ± 01.12 |
|                    Go |  2.16 ± 00.01 |   275.00 ± 00.29 |    57.19 ± 00.73 |
|                 V GCC |  2.16 ± 00.01 |   591.92 ± 00.06 |    55.87 ± 00.48 |
|               V Clang |  2.16 ± 00.01 |   592.39 ± 00.03 |    55.89 ± 00.54 |
|            C++ json-c |  2.94 ± 00.02 |  1756.27 ± 00.08 |    74.58 ± 01.09 |
|                Python |  2.96 ± 00.05 |   518.99 ± 00.05 |    78.00 ± 01.98 |
|          Crystal Pull |  3.06 ± 00.03 |     4.87 ± 00.03 |    83.68 ± 03.22 |
|                GCC Go |  3.06 ± 00.03 |   259.14 ± 01.73 |    78.88 ± 01.73 |
|               Nim GCC |  3.14 ± 00.05 |   692.31 ± 02.65 |    81.11 ± 01.11 |
|             Nim Clang |  3.22 ± 00.01 |   694.07 ± 00.02 |    82.67 ± 00.63 |
|          C# .NET Core |  3.49 ± 00.06 |   335.08 ± 03.59 |    91.60 ± 01.89 |
|                  Ruby |  3.60 ± 00.03 |   464.26 ± 00.59 |    92.26 ± 01.86 |
|                   LDC |  4.12 ± 00.01 |   789.63 ± 00.07 |    91.90 ± 00.99 |
|             Ruby YAJL |  4.23 ± 00.11 |   464.64 ± 00.22 |   107.83 ± 02.78 |
|               Haskell |  4.33 ± 00.03 |    10.32 ± 00.07 |   109.12 ± 01.87 |
|               Rust jq |  4.88 ± 00.01 |   868.81 ± 05.43 |   125.88 ± 00.83 |
|                   GDC |  4.95 ± 00.01 |   717.01 ± 00.03 |   126.16 ± 02.41 |
|                 JRuby |  5.64 ± 00.17 |  1837.30 ± 36.30 |   240.32 ± 04.94 |
|               C# Mono |  6.15 ± 00.25 |   310.65 ± 06.95 |   157.46 ± 06.37 |
|             C++ Boost |  6.35 ± 00.07 |  1550.01 ± 00.07 |   165.33 ± 02.93 |
|                   DMD |  7.54 ± 00.06 |   790.07 ± 00.10 |   175.26 ± 03.06 |
|       Perl JSON::Tiny | 25.99 ± 00.21 |   639.48 ± 00.51 |   687.22 ± 10.68 |
|       TruffleRuby JVM | 31.03 ± 00.91 |  1705.95 ± 18.43 | 1362.97 ± 189.19 |

# Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|           Language |        Time, s |     Memory, MiB |         Energy, J |
| :----------------- | -------------: | --------------: | ----------------: |
|         LDC lubeck |   0.13 ± 00.06 |   31.40 ± 17.08 |      6.60 ± 01.98 |
| Julia (threads: 8) |   0.13 ± 00.00 |  271.34 ± 00.18 |      8.81 ± 00.12 |
|       Python NumPy |   0.16 ± 00.00 |   85.21 ± 00.09 |      9.49 ± 00.14 |
| Julia (threads: 1) |   0.31 ± 00.01 |  269.76 ± 00.10 |      8.10 ± 01.43 |
|                LDC |   1.90 ± 00.01 |   73.34 ± 00.04 |     58.01 ± 00.94 |
|                DMD |   2.16 ± 00.01 |   73.27 ± 00.05 |     65.96 ± 01.70 |
|                GDC |   2.26 ± 00.01 |   76.95 ± 00.05 |     67.24 ± 01.38 |
|                  C |   3.11 ± 00.01 |   70.21 ± 00.03 |     83.16 ± 01.49 |
|               Rust |   3.17 ± 00.01 |   70.96 ± 00.05 |     84.25 ± 01.93 |
|            Nim GCC |   3.21 ± 00.01 |   75.66 ± 05.58 |     85.36 ± 02.02 |
|          Nim Clang |   3.21 ± 00.01 |   77.20 ± 05.22 |     88.02 ± 01.89 |
|    Julia (no BLAS) |   3.25 ± 00.02 |  252.49 ± 00.39 |     88.92 ± 01.60 |
|            Crystal |   3.33 ± 00.01 |   63.70 ± 00.05 |     94.51 ± 01.92 |
|                 Go |   3.33 ± 00.02 |   65.64 ± 07.63 |     91.48 ± 02.00 |
|              Swift |   3.35 ± 00.05 |  195.84 ± 00.12 |     93.17 ± 00.93 |
|             GCC Go |   3.40 ± 00.01 |  103.28 ± 00.54 |     92.30 ± 01.74 |
|            V Clang |   3.48 ± 00.01 |   70.88 ± 00.04 |     95.23 ± 00.58 |
|             Kotlin |   3.48 ± 00.13 |  129.95 ± 00.11 |     99.86 ± 03.98 |
|              Scala |   3.74 ± 00.02 |  160.69 ± 08.30 |     99.45 ± 02.14 |
|               Java |   3.82 ± 00.03 |  126.51 ± 00.28 |    107.13 ± 02.43 |
|              V GCC |   4.21 ± 00.01 |   70.37 ± 00.04 |    111.66 ± 00.41 |
|            Node.js |   4.33 ± 00.01 |  104.46 ± 00.17 |    118.40 ± 02.00 |
|               PyPy |   7.85 ± 00.02 |  130.68 ± 00.15 |    199.37 ± 03.13 |
|       C# .NET Core |   9.20 ± 00.04 |  104.65 ± 00.14 |    245.58 ± 04.69 |
|            C# Mono |  14.63 ± 00.11 |   92.56 ± 00.12 |    391.00 ± 05.94 |
|    TruffleRuby JVM |  54.93 ± 00.35 |  868.08 ± 44.42 |   1369.98 ± 08.16 |
|               Ruby | 375.27 ± 03.08 |   83.19 ± 00.06 |   9561.87 ± 93.22 |
|              JRuby | 493.21 ± 26.77 | 1178.86 ± 70.70 | 12711.33 ± 602.40 |
|             Python | 561.80 ± 05.88 |   79.04 ± 00.05 | 14332.15 ± 149.68 |
|                Tcl | 579.99 ± 03.84 |  280.54 ± 00.04 | 14821.26 ± 119.03 |
|               Perl | 658.61 ± 11.11 |  608.39 ± 00.04 | 16095.76 ± 277.50 |

# Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |       Time, s |    Memory, MiB |       Energy, J |
| :----------- | ------------: | -------------: | --------------: |
|      Crystal |  10.61 ± 00.03 | 230.98 ± 01.42 |  315.04 ± 05.51 |
|      Nim GCC |  17.97 ± 00.05 | 488.74 ± 12.08 |  466.46 ± 02.45 |
|    Nim Clang |  18.39 ± 00.05 | 486.58 ± 05.07 |  481.98 ± 06.01 |
|          C++ |  18.41 ± 00.20 | 180.20 ± 00.07 |  448.23 ± 07.22 |
|           Go |  24.78 ± 00.09 | 368.25 ± 16.48 |  722.28 ± 10.52 |
|        Scala |  25.54 ± 00.39 | 386.10 ± 06.89 | 1063.59 ± 21.75 |
|          LDC |  25.86 ± 00.09 | 466.93 ± 10.38 |  727.94 ± 06.65 |
|          DMD |  31.26 ± 00.07 | 477.36 ± 16.73 |  888.16 ± 10.52 |
|          GDC |  32.73 ± 00.13 | 419.37 ± 00.04 |  823.79 ± 09.78 |
|      C# Mono |  37.46 ± 00.14 | 323.23 ± 02.32 | 1153.25 ± 15.56 |
|       GCC Go |  40.11 ± 00.50 | 416.71 ± 32.67 | 1210.13 ± 15.52 |
|         PyPy |  42.66 ± 01.13 | 648.11 ± 57.22 | 1101.84 ± 35.78 |
| C# .NET Core |  43.42 ± 00.26 | 541.12 ± 16.22 | 1128.20 ± 14.77 |
|       Python | 173.19 ± 00.81 | 464.74 ± 00.05 | 4508.53 ± 56.21 |

# Environment

CPU: Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz

Base Docker image: Debian GNU/Linux bullseye/sid

| Language     | Version                         |
| ------------ | ------------------------------- |
| .NET Core    | 3.0.101                         |
| C# .NET Core | 3.3.1-beta4-19462-11 (66a912c9) |
| C# Mono      | 6.4.0.198                       |
| Chez Scheme  | 9.5                             |
| Clang        | 9.0.0                           |
| Clojure      | "1.10.1"                        |
| Crystal      | 0.31.1                          |
| DMD          | v2.089.0                        |
| Elixir       | 1.9.1                           |
| F# .NET Core | 10.6.0.0 for F# 4.7             |
| GCC          | 9.2.1                           |
| GCC Go       | 9.2.1                           |
| GDC          | 9.2.1                           |
| Go           | go1.13.5                        |
| Haskell      | 8.8.1                           |
| JRuby        | 9.2.9.0                         |
| Java         | 13.0.1                          |
| Julia        | v"1.3.0"                        |
| Kotlin       | 1.3.61                          |
| LDC          | 1.18.0                          |
| Lua          | Lua 5.3                         |
| LuaJIT       | LuaJIT 2.1.0-beta3              |
| MLton        | 20180207                        |
| Nim          | 1.0.4                           |
| Node.js      | v13.3.0                         |
| OCaml        | 4.09.0                          |
| PHP          | 7.3.12-1                        |
| Perl         | v5.30.0                         |
| PyPy         | 7.2.0-final0 for Python 3.6.9   |
| Python       | 3.7.5                           |
| Racket       | "7.5"                           |
| Ruby         | 2.6.5p114                       |
| Rust         | 1.39.0                          |
| Scala        | 2.13.1                          |
| Swift        | swift-5.1.2-RELEASE             |
| Tcl          | 8.6                             |
| TruffleRuby  | 19.3.0                          |
| V            | 0.1.23 6d5e9f8                  |

# Testing

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

## Manual

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

For Haskell:

 - [network-simple](http://hackage.haskell.org/package/network-simple) for TCP connectivity between the tests and the test runner

For C, C++, Chez Scheme:

 - [libsocket](https://github.com/dermesser/libsocket/) for TCP connectivity between the tests and the test runner
