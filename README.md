# Table of Content

<!-- toc-begin -->
* [Overview](#overview)
  * [Measurements](#measurements)
* [Test Cases](#test-cases)
  * [Brainfuck](#brainfuck)
    * [bench.b](#benchb)
    * [mandel.b](#mandelb)
  * [Base64](#base64)
  * [Json](#json)
  * [Matmul](#matmul)
  * [Primes](#primes)
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
  * [README update](#readme-update)
  * [Docker image update](#docker-image-update)
<!-- toc-end -->

# Overview

The benchmarks follow the criteria:

  - They are written as the average software developer would write them, i.e.

    - The algorithms are implemented as cited in public sources;
    - The libraries are used as described in the tutorials, documentation and examples;
    - The used data structures are idiomatic.

  - The used algorithms are similar between the languages (as the reference implementations), variants are acceptable if the reference implementation exists.
  - All final binaries are releases (optimized for performance if possible) as debug performance may vary too much depending on the compiler.

My other benchmarks: [jit-benchmarks](https://github.com/kostya/jit-benchmarks), [crystal-benchmarks-game](https://github.com/kostya/crystal-benchmarks-game)

## Measurements

The measured values are:

 - time spent for the benchmark execution (loading required data and code self-testing are not measured);
 - memory consumption of the benchmark process, reported as `base` + `increase`, where `base` is the RSS before the benchmark and `increase` is the peak increase of the RSS during the benchmark;
 - energy consumption of the CPU package during the benchmark: PP0 (cores) + PP1 (uncores like GPU) + DRAM. Currently, only Intel CPU are supported via the powercap interface.

All values are presented as: `median`<sub>±`median absolute deviation`</sub>.

UPDATE: 2022-05-30

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|                 Language |                  Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | -------------------------: |
|  Racket (Syntax Objects) |   1.294<sub>±0.001</sub> |   104.83<sub>±00.98</sub> + 0.00<sub>±00.00</sub> |     47.02<sub>±00.02</sub> |
|                  C++/g++ |   1.317<sub>±0.000</sub> |     1.65<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     52.77<sub>±00.08</sub> |
|                     Rust |   1.589<sub>±0.001</sub> |     2.12<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     63.57<sub>±00.08</sub> |
|                   D/ldc2 |   1.655<sub>±0.000</sub> |     3.21<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     66.25<sub>±00.07</sub> |
|                     Java |   1.655<sub>±0.001</sub> |    37.33<sub>±00.46</sub> + 0.90<sub>±00.37</sub> |     64.58<sub>±00.05</sub> |
|                    D/gdc |   1.660<sub>±0.000</sub> |     7.49<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     67.55<sub>±00.71</sub> |
|                  C/clang |   1.663<sub>±0.000</sub> |     0.69<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     64.54<sub>±00.06</sub> |
|                    C/gcc |   1.667<sub>±0.000</sub> |     0.71<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     64.71<sub>±00.06</sub> |
|               Kotlin/JVM |   1.677<sub>±0.006</sub> |    40.74<sub>±00.11</sub> + 0.42<sub>±00.16</sub> |     65.50<sub>±00.18</sub> |
|              C++/clang++ |   1.691<sub>±0.000</sub> |     1.48<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     64.32<sub>±00.73</sub> |
|                      Zig |   1.747<sub>±0.000</sub> |     0.70<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     68.20<sub>±00.38</sub> |
|             F#/.NET Core |   1.896<sub>±0.000</sub> |    37.86<sub>±00.09</sub> + 0.54<sub>±00.00</sub> |     76.35<sub>±00.41</sub> |
|             C#/.NET Core |   1.902<sub>±0.000</sub> |    33.85<sub>±00.29</sub> + 0.00<sub>±00.00</sub> |     76.27<sub>±00.19</sub> |
|                       Go |   1.904<sub>±0.000</sub> |     2.79<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     72.13<sub>±00.49</sub> |
|                    OCaml |   1.912<sub>±0.000</sub> |     2.71<sub>±00.02</sub> + 2.30<sub>±00.03</sub> |     85.13<sub>±00.36</sub> |
|              Chez Scheme |   1.923<sub>±0.001</sub> |    24.91<sub>±00.08</sub> + 4.40<sub>±00.02</sub> |     79.48<sub>±00.05</sub> |
|                  Nim/gcc |   1.976<sub>±0.000</sub> |     2.02<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     78.82<sub>±00.74</sub> |
|                   Racket |   2.012<sub>±0.010</sub> |    93.21<sub>±00.51</sub> + 0.00<sub>±00.00</sub> |     81.10<sub>±00.77</sub> |
|                 Vala/gcc |   2.090<sub>±0.001</sub> |     4.25<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     77.06<sub>±00.85</sub> |
|                Nim/clang |   2.100<sub>±0.002</sub> |     2.43<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     81.52<sub>±00.15</sub> |
|                 Go/gccgo |   2.172<sub>±0.000</sub> |    24.55<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |     86.43<sub>±00.27</sub> |
|               Vala/clang |   2.262<sub>±0.000</sub> |     4.26<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     82.35<sub>±00.19</sub> |
|                    V/gcc |   2.340<sub>±0.000</sub> |     0.66<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     87.35<sub>±00.63</sub> |
|                  Crystal |   2.384<sub>±0.000</sub> |     3.46<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     95.11<sub>±00.44</sub> |
|                    MLton |   2.608<sub>±0.000</sub> |     0.80<sub>±00.03</sub> + 1.01<sub>±00.03</sub> |    102.87<sub>±00.28</sub> |
|                  C#/Mono |   2.685<sub>±0.001</sub> |    24.99<sub>±00.13</sub> + 0.00<sub>±00.00</sub> |    107.58<sub>±00.08</sub> |
|                  V/clang |   2.967<sub>±0.019</sub> |     0.66<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    115.67<sub>±00.92</sub> |
|         Haskell (MArray) |   3.165<sub>±0.001</sub> |     4.18<sub>±00.04</sub> + 3.97<sub>±00.00</sub> |    131.78<sub>±00.77</sub> |
|                    Scala |   3.222<sub>±0.023</sub> |  66.15<sub>±00.25</sub> + 141.06<sub>±00.32</sub> |    133.08<sub>±00.82</sub> |
|                    D/dmd |   3.267<sub>±0.000</sub> |     3.76<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    122.89<sub>±00.09</sub> |
|                    Julia |   3.565<sub>±0.001</sub> |   208.86<sub>±00.13</sub> + 0.99<sub>±00.03</sub> |    134.35<sub>±00.18</sub> |
|                  Node.js |   4.016<sub>±0.002</sub> |    37.68<sub>±00.02</sub> + 0.85<sub>±00.08</sub> |    161.03<sub>±00.35</sub> |
|                    Swift |   5.415<sub>±0.000</sub> |    13.76<sub>±00.14</sub> + 0.00<sub>±00.00</sub> |    200.89<sub>±02.89</sub> |
|               Lua/luajit |   6.492<sub>±0.008</sub> |     2.41<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    259.77<sub>±00.54</sub> |
|         Ruby/truffleruby |   7.352<sub>±0.037</sub> | 291.61<sub>±00.77</sub> + 396.22<sub>±08.35</sub> |    347.59<sub>±02.47</sub> |
| Ruby/truffleruby (--jvm) |   8.767<sub>±0.078</sub> | 376.87<sub>±16.30</sub> + 458.33<sub>±29.58</sub> |    430.85<sub>±06.14</sub> |
|              Python/pypy |  12.207<sub>±0.065</sub> |   65.14<sub>±00.02</sub> + 29.65<sub>±00.03</sub> |    512.74<sub>±00.94</sub> |
|                  Haskell |  13.368<sub>±0.019</sub> |     4.31<sub>±00.04</sub> + 3.97<sub>±00.00</sub> |    595.87<sub>±04.15</sub> |
|             Ruby (--jit) |  39.635<sub>±0.156</sub> |   270.95<sub>±00.02</sub> + 0.02<sub>±00.00</sub> |   1618.92<sub>±08.59</sub> |
|                   Elixir |  43.913<sub>±0.156</sub> |    70.50<sub>±00.34</sub> + 0.00<sub>±00.00</sub> |   2011.98<sub>±10.43</sub> |
|                      Lua |  50.617<sub>±0.073</sub> |     2.30<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1941.25<sub>±23.16</sub> |
|               Ruby/jruby |  78.965<sub>±0.799</sub> |  183.82<sub>±00.75</sub> + 83.58<sub>±00.83</sub> |   3500.74<sub>±39.03</sub> |
|                     Ruby |  85.918<sub>±0.486</sub> |    14.41<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3615.58<sub>±32.78</sub> |
|                   Python | 185.860<sub>±1.077</sub> |    10.63<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   7415.67<sub>±57.06</sub> |
|                 Tcl (FP) | 239.705<sub>±1.971</sub> |     4.56<sub>±00.03</sub> + 0.00<sub>±00.00</sub> | 10232.99<sub>±120.09</sub> |
|                     Perl | 324.279<sub>±3.019</sub> |     7.06<sub>±00.05</sub> + 0.00<sub>±00.00</sub> | 12967.03<sub>±121.22</sub> |
|                Tcl (OOP) | 492.812<sub>±2.162</sub> |     4.55<sub>±00.08</sub> + 0.00<sub>±00.00</sub> | 20801.83<sub>±165.07</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                  C++/g++ |  10.461<sub>±0.014</sub> |     1.65<sub>±00.00</sub> + 2.34<sub>±00.05</sub> |   429.24<sub>±01.97</sub> |
|  Racket (Syntax Objects) |  14.090<sub>±0.116</sub> |  105.24<sub>±00.45</sub> + 72.57<sub>±00.64</sub> |   563.20<sub>±06.84</sub> |
|                    C/gcc |  14.203<sub>±0.011</sub> |     0.68<sub>±00.03</sub> + 1.05<sub>±00.02</sub> |   561.03<sub>±02.60</sub> |
|               Kotlin/JVM |  14.337<sub>±0.030</sub> |    40.77<sub>±00.09</sub> + 1.00<sub>±00.14</sub> |   580.64<sub>±03.91</sub> |
|                    V/gcc |  14.519<sub>±0.017</sub> |     0.72<sub>±00.00</sub> + 2.08<sub>±00.05</sub> |   589.11<sub>±05.55</sub> |
|              C++/clang++ |  14.656<sub>±0.009</sub> |     1.50<sub>±00.02</sub> + 2.09<sub>±00.02</sub> |   588.07<sub>±01.37</sub> |
|                     Rust |  14.823<sub>±0.008</sub> |     2.17<sub>±00.04</sub> + 0.25<sub>±00.00</sub> |   593.32<sub>±00.71</sub> |
|                   D/ldc2 |  14.864<sub>±0.019</sub> |     3.20<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   598.33<sub>±02.87</sub> |
|                    D/gdc |  15.138<sub>±0.015</sub> |     7.47<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   640.93<sub>±01.15</sub> |
|                  C/clang |  15.431<sub>±0.010</sub> |     0.69<sub>±00.03</sub> + 1.05<sub>±00.04</sub> |   644.66<sub>±01.57</sub> |
|                      Zig |  16.222<sub>±0.009</sub> |     0.70<sub>±00.00</sub> + 1.61<sub>±00.01</sub> |   669.85<sub>±04.37</sub> |
|                       Go |  17.065<sub>±0.259</sub> |     3.34<sub>±00.11</sub> + 1.26<sub>±00.00</sub> |   674.90<sub>±08.80</sub> |
|                    Swift |  18.068<sub>±0.088</sub> |    14.62<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   734.16<sub>±06.70</sub> |
|                     Java |  18.283<sub>±0.012</sub> |    37.81<sub>±00.06</sub> + 1.29<sub>±00.13</sub> |   732.18<sub>±00.83</sub> |
|                 Vala/gcc |  18.498<sub>±0.004</sub> |     4.09<sub>±00.05</sub> + 2.04<sub>±00.02</sub> |   694.48<sub>±01.06</sub> |
|             C#/.NET Core |  18.529<sub>±0.008</sub> |    34.13<sub>±00.07</sub> + 0.83<sub>±00.01</sub> |   751.34<sub>±04.79</sub> |
|                  Crystal |  19.010<sub>±0.215</sub> |     3.46<sub>±00.01</sub> + 0.38<sub>±00.02</sub> |   778.36<sub>±08.01</sub> |
|                Nim/clang |  19.309<sub>±0.305</sub> |     2.43<sub>±00.04</sub> + 0.57<sub>±00.00</sub> |   784.15<sub>±16.13</sub> |
|               Vala/clang |  19.913<sub>±0.003</sub> |     4.08<sub>±00.02</sub> + 2.05<sub>±00.06</sub> |   770.85<sub>±00.76</sub> |
|                 Go/gccgo |  20.039<sub>±0.013</sub> |    24.59<sub>±00.05</sub> + 1.27<sub>±00.00</sub> |   845.96<sub>±02.65</sub> |
|                  Nim/gcc |  21.108<sub>±0.569</sub> |     1.95<sub>±00.01</sub> + 0.51<sub>±00.00</sub> |   854.87<sub>±21.62</sub> |
|                    Scala |  21.395<sub>±0.016</sub> |  66.24<sub>±00.05</sub> + 141.19<sub>±00.08</sub> |   883.73<sub>±05.55</sub> |
|                  V/clang |  22.592<sub>±0.064</sub> |     0.66<sub>±00.00</sub> + 1.89<sub>±00.03</sub> |   931.66<sub>±02.36</sub> |
|                    OCaml |  26.203<sub>±0.017</sub> |     3.48<sub>±00.02</sub> + 8.08<sub>±03.91</sub> |  1247.53<sub>±12.44</sub> |
|              Chez Scheme |  26.997<sub>±0.052</sub> |    25.38<sub>±00.05</sub> + 3.92<sub>±00.01</sub> |  1180.81<sub>±03.40</sub> |
|                  C#/Mono |  31.357<sub>±0.018</sub> |    25.04<sub>±00.06</sub> + 0.83<sub>±00.00</sub> |  1331.72<sub>±01.53</sub> |
|                  Node.js |  31.704<sub>±0.292</sub> |    37.65<sub>±00.06</sub> + 5.37<sub>±00.02</sub> |  1317.45<sub>±17.77</sub> |
|               Lua/luajit |  35.127<sub>±0.069</sub> |     2.42<sub>±00.01</sub> + 0.50<sub>±00.00</sub> |  1408.43<sub>±04.07</sub> |
|                   Racket |  36.927<sub>±0.067</sub> |    92.66<sub>±00.27</sub> + 0.00<sub>±00.00</sub> |  1636.94<sub>±08.74</sub> |
|                    D/dmd |  37.471<sub>±0.015</sub> |     3.76<sub>±00.01</sub> + 0.77<sub>±00.00</sub> |  1377.33<sub>±08.82</sub> |
|             F#/.NET Core |  38.358<sub>±0.052</sub> |    37.99<sub>±00.04</sub> + 2.08<sub>±00.00</sub> |  1587.43<sub>±03.20</sub> |
|                    MLton |  44.833<sub>±0.049</sub> |     1.59<sub>±00.04</sub> + 4.11<sub>±00.00</sub> |  1945.45<sub>±09.66</sub> |
|              Python/pypy |  48.373<sub>±0.180</sub> |   64.76<sub>±00.06</sub> + 30.07<sub>±00.03</sub> |  2068.90<sub>±24.04</sub> |
|         Haskell (MArray) |  49.631<sub>±0.622</sub> |     4.12<sub>±00.02</sub> + 5.83<sub>±00.00</sub> |  2120.11<sub>±20.32</sub> |
|                    Julia |  64.532<sub>±0.078</sub> |   209.49<sub>±00.10</sub> + 0.97<sub>±00.08</sub> |  2486.31<sub>±13.12</sub> |
| Ruby/truffleruby (--jvm) |  78.552<sub>±1.319</sub> | 398.37<sub>±13.36</sub> + 440.08<sub>±26.60</sub> |  3295.51<sub>±37.63</sub> |
|         Ruby/truffleruby |  83.372<sub>±0.453</sub> | 291.66<sub>±00.26</sub> + 251.66<sub>±23.69</sub> |  3567.05<sub>±19.14</sub> |
|                  Haskell | 186.042<sub>±2.425</sub> |    4.34<sub>±00.04</sub> + 29.24<sub>±00.00</sub> | 8142.94<sub>±109.17</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|          C/clang (aklomp) |  0.100<sub>±0.001</sub> |     2.04<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.73<sub>±00.07</sub> |
|            C/gcc (aklomp) |  0.101<sub>±0.000</sub> |     2.09<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   4.80<sub>±00.05</sub> |
|                      Rust |  0.969<sub>±0.000</sub> |     2.58<sub>±00.03</sub> + 0.01<sub>±00.00</sub> |  39.56<sub>±00.22</sub> |
|                   V/clang |  0.995<sub>±0.001</sub> |     1.91<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  37.66<sub>±00.14</sub> |
|                   C/clang |  0.997<sub>±0.000</sub> |     2.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  37.14<sub>±00.36</sub> |
|                     C/gcc |  1.012<sub>±0.000</sub> |     2.05<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  37.95<sub>±00.09</sub> |
|                 Nim/clang |  1.024<sub>±0.001</sub> |     2.82<sub>±00.03</sub> + 4.44<sub>±00.00</sub> |  41.08<sub>±00.18</sub> |
|                    D/ldc2 |  1.074<sub>±0.003</sub> |     3.58<sub>±00.03</sub> + 3.58<sub>±00.00</sub> |  45.02<sub>±00.50</sub> |
|                   Nim/gcc |  1.081<sub>±0.002</sub> |     2.31<sub>±00.03</sub> + 4.44<sub>±00.06</sub> |  42.48<sub>±00.56</sub> |
|                     V/gcc |  1.092<sub>±0.000</sub> |     1.63<sub>±00.01</sub> + 0.44<sub>±00.04</sub> |  42.03<sub>±00.17</sub> |
|                   Crystal |  1.154<sub>±0.002</sub> |     3.95<sub>±00.01</sub> + 1.17<sub>±00.02</sub> |  51.11<sub>±00.15</sub> |
|              Ruby (--jit) |  1.326<sub>±0.001</sub> |  271.35<sub>±00.02</sub> + 66.67<sub>±00.01</sub> |  52.24<sub>±00.23</sub> |
|                      Ruby |  1.336<sub>±0.002</sub> |   14.83<sub>±00.01</sub> + 58.68<sub>±00.04</sub> |  52.41<sub>±00.27</sub> |
|                      Java |  1.511<sub>±0.008</sub> |  38.37<sub>±00.06</sub> + 258.46<sub>±28.01</sub> |  61.39<sub>±00.49</sub> |
|                     Scala |  1.562<sub>±0.005</sub> |  64.37<sub>±00.18</sub> + 315.74<sub>±04.03</sub> |  67.26<sub>±01.06</sub> |
|                  Vala/gcc |  1.563<sub>±0.001</sub> |     5.49<sub>±00.03</sub> + 0.44<sub>±00.08</sub> |  61.08<sub>±00.51</sub> |
|                Vala/clang |  1.563<sub>±0.001</sub> |     5.60<sub>±00.04</sub> + 0.30<sub>±00.03</sub> |  60.93<sub>±00.56</sub> |
|                Kotlin/JVM |  1.635<sub>±0.007</sub> |  40.46<sub>±00.12</sub> + 263.10<sub>±18.51</sub> |  69.46<sub>±01.12</sub> |
|   C++/clang++ (libcrypto) |  1.725<sub>±0.005</sub> |     4.46<sub>±00.05</sub> + 0.68<sub>±00.06</sub> |  68.58<sub>±00.43</sub> |
|       C++/g++ (libcrypto) |  1.726<sub>±0.004</sub> |     5.12<sub>±00.09</sub> + 0.68<sub>±00.06</sub> |  68.44<sub>±00.24</sub> |
|       Perl (MIME::Base64) |  1.779<sub>±0.014</sub> |    14.86<sub>±00.04</sub> + 0.07<sub>±00.00</sub> |  70.08<sub>±00.39</sub> |
|                        Go |  1.852<sub>±0.001</sub> |     4.07<sub>±00.06</sub> + 4.57<sub>±00.38</sub> |  78.91<sub>±00.38</sub> |
|                       PHP |  2.015<sub>±0.022</sub> |    17.09<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |  81.94<sub>±00.93</sub> |
|                    Python |  2.115<sub>±0.002</sub> |    10.41<sub>±00.06</sub> + 0.18<sub>±00.00</sub> |  78.44<sub>±00.28</sub> |
|                   Node.js |  2.234<sub>±0.003</sub> |   36.96<sub>±00.03</sub> + 39.88<sub>±00.01</sub> |  90.04<sub>±00.67</sub> |
|                     D/gdc |  2.645<sub>±0.001</sub> |     7.62<sub>±00.02</sub> + 3.91<sub>±00.00</sub> | 114.17<sub>±00.89</sub> |
|                     D/dmd |  2.736<sub>±0.001</sub> |     4.04<sub>±00.02</sub> + 3.36<sub>±00.00</sub> | 118.22<sub>±00.86</sub> |
|                       Zig |  3.062<sub>±0.001</sub> |     1.47<sub>±00.01</sub> + 0.31<sub>±00.00</sub> | 122.30<sub>±00.82</sub> |
|              F#/.NET Core |  3.130<sub>±0.065</sub> |   38.63<sub>±00.03</sub> + 18.63<sub>±01.03</sub> | 117.75<sub>±01.18</sub> |
|               Python/pypy |  3.580<sub>±0.002</sub> |   65.49<sub>±00.10</sub> + 30.62<sub>±00.15</sub> | 155.88<sub>±01.19</sub> |
|                  Go/gccgo |  3.716<sub>±0.007</sub> |    25.42<sub>±00.09</sub> + 7.60<sub>±00.21</sub> | 167.47<sub>±00.66</sub> |
|              C#/.NET Core |  3.748<sub>±0.081</sub> |   34.49<sub>±00.06</sub> + 20.71<sub>±03.36</sub> | 137.05<sub>±01.81</sub> |
|                       Tcl |  3.993<sub>±0.005</sub> |     5.09<sub>±00.02</sub> + 0.16<sub>±00.00</sub> | 160.32<sub>±00.37</sub> |
|                   C#/Mono |  4.698<sub>±0.019</sub> |   25.75<sub>±00.07</sub> + 18.71<sub>±00.09</sub> | 194.78<sub>±00.66</sub> |
|  Ruby/truffleruby (--jvm) |  5.090<sub>±0.008</sub> | 343.44<sub>±03.17</sub> + 242.66<sub>±37.11</sub> | 247.93<sub>±01.44</sub> |
|                     Julia |  5.095<sub>±0.007</sub> |  228.19<sub>±00.26</sub> + 62.22<sub>±00.04</sub> | 190.73<sub>±02.14</sub> |
|                Ruby/jruby | 10.597<sub>±0.088</sub> |  182.58<sub>±00.94</sub> + 77.53<sub>±03.02</sub> | 422.40<sub>±02.95</sub> |
| Perl (MIME::Base64::Perl) | 13.988<sub>±0.057</sub> |    16.16<sub>±00.09</sub> + 0.22<sub>±00.03</sub> | 575.68<sub>±03.73</sub> |
|          Ruby/truffleruby | 17.438<sub>±0.022</sub> | 281.42<sub>±00.63</sub> + 235.47<sub>±06.16</sub> | 701.31<sub>±05.07</sub> |

## Json

Testing parsing and simple calculating of values from a big JSON file.

Few notes:

 - gason mutates input strings;
 - simdjson requires input strings with batch of trailing zeros: a special zero padding for SIMD instructions;
 - DAW JSON Link "NoCheck" skips some JSON structure correctness checks;
 - DAW JSON Link, gason, default (not "Precise") RapidJSON, and D implementations except Mir-based
have some inaccuracies in number parsing:
   - [DAW JSON Link's number parsing issue](https://github.com/beached/daw_json_link/issues/226)
   - [gason's number parsing issue](https://github.com/vivkin/gason/issues/35)
   - [D stdlib number parsing issue](https://issues.dlang.org/show_bug.cgi?id=20967)

[Json](json)

|                            Language |                 Time, s |                                        Memory, MiB |                Energy, J |
| :---------------------------------- | ----------------------: | -------------------------------------------------: | -----------------------: |
|        C++/g++ (simdjson On-Demand) |  0.067<sub>±0.000</sub> |   113.45<sub>±00.09</sub> + 59.81<sub>±00.00</sub> |    2.78<sub>±00.03</sub> |
|    C++/clang++ (simdjson On-Demand) |  0.068<sub>±0.000</sub> |   112.55<sub>±00.07</sub> + 60.36<sub>±00.06</sub> |    2.83<sub>±00.01</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.077<sub>±0.000</sub> |    112.44<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    3.14<sub>±00.01</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.079<sub>±0.000</sub> |    113.19<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    3.16<sub>±00.02</sub> |
|             C++/g++ (DAW JSON Link) |  0.087<sub>±0.000</sub> |    113.22<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |    3.58<sub>±00.03</sub> |
|         C++/clang++ (DAW JSON Link) |  0.089<sub>±0.000</sub> |    112.44<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    3.67<sub>±00.04</sub> |
|                 Rust (Serde Custom) |  0.107<sub>±0.000</sub> |    111.99<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    4.56<sub>±00.02</sub> |
|                  Rust (Serde Typed) |  0.112<sub>±0.000</sub> |   111.97<sub>±00.07</sub> + 12.03<sub>±00.00</sub> |    4.72<sub>±00.01</sub> |
|                     C++/g++ (gason) |  0.133<sub>±0.000</sub> |   113.14<sub>±00.05</sub> + 96.80<sub>±00.06</sub> |    5.28<sub>±00.03</sub> |
|          C++/clang++ (simdjson DOM) |  0.136<sub>±0.001</sub> |  112.48<sub>±00.03</sub> + 177.15<sub>±00.06</sub> |    5.79<sub>±00.03</sub> |
|              C++/g++ (simdjson DOM) |  0.138<sub>±0.000</sub> |  113.01<sub>±00.09</sub> + 176.39<sub>±00.19</sub> |    5.88<sub>±00.01</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.146<sub>±0.000</sub> |   112.73<sub>±00.04</sub> + 61.27<sub>±00.03</sub> |    6.09<sub>±00.02</sub> |
|                 C++/clang++ (gason) |  0.149<sub>±0.000</sub> |   112.48<sub>±00.03</sub> + 96.97<sub>±00.06</sub> |    6.02<sub>±00.01</sub> |
|                 C++/g++ (RapidJSON) |  0.167<sub>±0.000</sub> |  113.26<sub>±00.05</sub> + 126.54<sub>±00.42</sub> |    6.93<sub>±00.04</sub> |
|             C++/clang++ (RapidJSON) |  0.221<sub>±0.000</sub> |  112.45<sub>±00.00</sub> + 129.02<sub>±00.04</sub> |    8.98<sub>±00.04</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.231<sub>±0.000</sub> |   112.72<sub>±00.05</sub> + 80.80<sub>±00.00</sub> |    9.50<sub>±00.02</sub> |
|         C++/g++ (RapidJSON Precise) |  0.246<sub>±0.000</sub> |  113.24<sub>±00.03</sub> + 128.82<sub>±00.06</sub> |   10.41<sub>±00.10</sub> |
|                C++/g++ (Boost.JSON) |  0.394<sub>±0.000</sub> |  113.22<sub>±00.03</sub> + 435.88<sub>±00.04</sub> |   16.44<sub>±00.13</sub> |
|                   Nim/clang (jsony) |  0.399<sub>±0.000</sub> |   112.16<sub>±00.02</sub> + 42.34<sub>±00.03</sub> |   16.95<sub>±00.22</sub> |
|             C++/g++ (RapidJSON SAX) |  0.404<sub>±0.001</sub> |    112.96<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   16.68<sub>±00.07</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.410<sub>±0.000</sub> |  112.47<sub>±00.03</sub> + 129.01<sub>±00.02</sub> |   16.63<sub>±00.05</sub> |
|            C++/clang++ (Boost.JSON) |  0.413<sub>±0.000</sub> |  112.44<sub>±00.00</sub> + 436.25<sub>±00.06</sub> |   17.23<sub>±00.20</sub> |
|                     Nim/gcc (jsony) |  0.422<sub>±0.000</sub> |   111.72<sub>±00.03</sub> + 42.28<sub>±00.00</sub> |   18.09<sub>±00.12</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.450<sub>±0.001</sub> |    112.96<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   19.34<sub>±00.09</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.496<sub>±0.000</sub> |    194.69<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   19.89<sub>±00.05</sub> |
|                       Go (jsoniter) |  0.519<sub>±0.002</sub> |    230.86<sub>±00.02</sub> + 1.07<sub>±00.04</sub> |   21.80<sub>±00.07</sub> |
|                             Node.js |  0.548<sub>±0.003</sub> |  147.87<sub>±00.07</sub> + 188.65<sub>±00.28</sub> |   25.00<sub>±00.14</sub> |
|                     Java (DSL-JSON) |  0.621<sub>±0.015</sub> |  258.17<sub>±00.07</sub> + 138.46<sub>±00.77</sub> |   32.18<sub>±00.99</sub> |
|                Rust (Serde Untyped) |  0.633<sub>±0.001</sub> |  111.90<sub>±00.03</sub> + 840.05<sub>±00.03</sub> |   25.50<sub>±00.07</sub> |
|                             V/clang |  0.635<sub>±0.001</sub> |  111.23<sub>±00.05</sub> + 496.21<sub>±00.00</sub> |   26.63<sub>±00.29</sub> |
|                               V/gcc |  0.635<sub>±0.001</sub> |  111.25<sub>±00.02</sub> + 496.12<sub>±00.03</sub> |   26.51<sub>±00.25</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.658<sub>±0.001</sub> |    194.71<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   26.74<sub>±00.12</sub> |
|                         Python/pypy |  0.678<sub>±0.001</sub> |  285.59<sub>±00.03</sub> + 124.88<sub>±00.00</sub> |   29.23<sub>±00.13</sub> |
|                      Crystal (Pull) |  0.688<sub>±0.002</sub> |   113.86<sub>±00.02</sub> + 18.18<sub>±00.02</sub> |   30.53<sub>±00.29</sub> |
|                    Crystal (Schema) |  0.694<sub>±0.002</sub> |   113.84<sub>±00.00</sub> + 48.57<sub>±00.04</sub> |   30.58<sub>±00.36</sub> |
|                       Julia (JSON3) |  0.744<sub>±0.004</sub> |  378.93<sub>±00.36</sub> + 378.85<sub>±00.20</sub> |   30.54<sub>±00.32</sub> |
|     C#/.NET Core (System.Text.Json) |  0.759<sub>±0.002</sub> |  479.31<sub>±00.03</sub> + 138.58<sub>±00.03</sub> |   33.20<sub>±00.31</sub> |
|                                 Zig |  0.769<sub>±0.001</sub> |   110.75<sub>±00.01</sub> + 12.18<sub>±00.00</sub> |   30.36<sub>±00.06</sub> |
|             Perl (Cpanel::JSON::XS) |  0.833<sub>±0.005</sub> |  125.42<sub>±00.05</sub> + 402.78<sub>±00.06</sub> |   34.78<sub>±00.29</sub> |
|                                  Go |  0.855<sub>±0.001</sub> |   117.10<sub>±00.06</sub> + 79.79<sub>±00.11</sub> |   35.57<sub>±00.01</sub> |
|              Nim/clang (Packedjson) |  0.929<sub>±0.000</sub> |  112.50<sub>±00.02</sub> + 294.16<sub>±00.00</sub> |   38.21<sub>±00.10</sub> |
|                             Crystal |  0.937<sub>±0.010</sub> |  113.88<sub>±00.01</sub> + 392.00<sub>±00.01</sub> |   42.07<sub>±00.33</sub> |
|                                 PHP |  0.984<sub>±0.001</sub> |  126.43<sub>±00.10</sub> + 682.01<sub>±00.00</sub> |   41.13<sub>±00.11</sub> |
|                Nim/gcc (Packedjson) |  1.010<sub>±0.000</sub> |  112.00<sub>±00.04</sub> + 294.16<sub>±00.00</sub> |   41.38<sub>±00.17</sub> |
|                C++/clang++ (json-c) |  1.203<sub>±0.007</sub> | 112.72<sub>±00.03</sub> + 1216.17<sub>±00.04</sub> |   50.96<sub>±00.27</sub> |
|                    C++/g++ (json-c) |  1.208<sub>±0.009</sub> | 113.28<sub>±00.10</sub> + 1215.96<sub>±00.03</sub> |   50.54<sub>±00.57</sub> |
|                             Clojure |  1.217<sub>±0.014</sub> |  450.12<sub>±06.93</sub> + 535.88<sub>±48.40</sub> |   64.53<sub>±00.76</sub> |
|              C++/clang++ (Nlohmann) |  1.253<sub>±0.002</sub> |  112.62<sub>±00.04</sub> + 360.14<sub>±00.02</sub> |   53.05<sub>±00.28</sub> |
|                            Go/gccgo |  1.263<sub>±0.003</sub> |   139.06<sub>±00.07</sub> + 83.52<sub>±00.15</sub> |   52.29<sub>±00.14</sub> |
|                        C#/.NET Core |  1.294<sub>±0.016</sub> |  487.04<sub>±00.06</sub> + 294.45<sub>±00.04</sub> |   56.51<sub>±00.63</sub> |
|                           Nim/clang |  1.315<sub>±0.002</sub> |  112.34<sub>±00.12</sub> + 925.29<sub>±00.10</sub> |   54.34<sub>±00.27</sub> |
|                             Nim/gcc |  1.349<sub>±0.002</sub> |  111.95<sub>±00.09</sub> + 919.68<sub>±00.06</sub> |   56.49<sub>±00.36</sub> |
|                 CPython (UltraJSON) |  1.440<sub>±0.003</sub> |  121.97<sub>±00.01</sub> + 543.94<sub>±02.33</sub> |   53.47<sub>±00.22</sub> |
|                        Ruby (--jit) |  1.465<sub>±0.002</sub> |  381.01<sub>±00.02</sub> + 262.87<sub>±00.01</sub> |   61.51<sub>±00.41</sub> |
|                                Ruby |  1.471<sub>±0.007</sub> |  124.42<sub>±00.03</sub> + 262.85<sub>±00.01</sub> |   62.65<sub>±00.34</sub> |
|                  C++/g++ (Nlohmann) |  1.476<sub>±0.005</sub> |  113.25<sub>±00.04</sub> + 447.92<sub>±00.04</sub> |   60.35<sub>±00.21</sub> |
|                              Python |  1.520<sub>±0.003</sub> |  120.41<sub>±00.01</sub> + 374.96<sub>±00.00</sub> |   59.77<sub>±00.14</sub> |
|     F#/.NET Core (System.Text.Json) |  1.802<sub>±0.005</sub> |  486.98<sub>±00.15</sub> + 384.58<sub>±12.24</sub> |   79.04<sub>±00.74</sub> |
|                     Scala (uPickle) |  1.849<sub>±0.011</sub> |  291.27<sub>±00.18</sub> + 665.30<sub>±48.68</sub> |   90.05<sub>±00.57</sub> |
|                         Ruby (YAJL) |  1.983<sub>±0.025</sub> |  124.21<sub>±00.01</sub> + 282.40<sub>±00.00</sub> |   83.42<sub>±01.05</sub> |
|                              D/ldc2 |  2.085<sub>±0.003</sub> |  113.15<sub>±00.02</sub> + 680.09<sub>±00.02</sub> |   86.06<sub>±00.41</sub> |
|                             C#/Mono |  2.293<sub>±0.021</sub> |   252.49<sub>±00.14</sub> + 31.58<sub>±00.02</sub> |   96.48<sub>±01.08</sub> |
|                             Haskell |  2.688<sub>±0.004</sub> |  115.95<sub>±00.03</sub> + 716.91<sub>±00.26</sub> |  108.80<sub>±00.08</sub> |
|                           Rust (jq) |  2.791<sub>±0.006</sub> |  113.65<sub>±00.07</sub> + 778.76<sub>±00.90</sub> |  115.54<sub>±00.54</sub> |
|                          Ruby/jruby |  3.113<sub>±0.027</sub> | 442.08<sub>±03.76</sub> + 933.59<sub>±104.90</sub> |  157.63<sub>±03.53</sub> |
|    C++/clang++ (Boost.PropertyTree) |  3.126<sub>±0.003</sub> | 194.87<sub>±00.02</sub> + 1232.87<sub>±00.01</sub> |  129.76<sub>±00.70</sub> |
|        C++/g++ (Boost.PropertyTree) |  3.338<sub>±0.006</sub> | 113.14<sub>±00.03</sub> + 1440.12<sub>±00.02</sub> |  135.06<sub>±00.77</sub> |
|                               D/gdc |  3.681<sub>±0.009</sub> |  117.28<sub>±00.03</sub> + 680.72<sub>±00.08</sub> |  157.65<sub>±00.82</sub> |
|                            Vala/gcc |  3.897<sub>±0.009</sub> |  114.69<sub>±00.01</sub> + 940.84<sub>±00.03</sub> |  165.64<sub>±00.87</sub> |
|                          Vala/clang |  4.031<sub>±0.009</sub> |  114.69<sub>±00.03</sub> + 876.74<sub>±00.02</sub> |  170.91<sub>±00.96</sub> |
|                               D/dmd |  4.524<sub>±0.007</sub> |  113.67<sub>±00.02</sub> + 680.09<sub>±00.02</sub> |  182.17<sub>±00.86</sub> |
|                   Perl (JSON::Tiny) |  9.756<sub>±0.035</sub> |  125.79<sub>±00.04</sub> + 528.88<sub>±00.06</sub> |  422.50<sub>±03.92</sub> |
|            Ruby/truffleruby (--jvm) | 16.470<sub>±0.253</sub> | 461.41<sub>±25.16</sub> + 2492.61<sub>±30.69</sub> | 1069.49<sub>±16.47</sub> |
|                    Ruby/truffleruby | 22.345<sub>±0.910</sub> | 417.31<sub>±01.29</sub> + 2336.16<sub>±85.83</sub> | 1080.54<sub>±36.02</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.046<sub>±0.000</sub> |    6.57<sub>±00.04</sub> + 54.95<sub>±00.10</sub> |      4.66<sub>±00.03</sub> |
|    Nim/gcc (Arraymancer) |    0.066<sub>±0.002</sub> |    5.52<sub>±00.05</sub> + 54.73<sub>±00.11</sub> |      5.46<sub>±00.13</sub> |
|      C++/clang++ (Eigen) |    0.068<sub>±0.003</sub> |   38.90<sub>±02.59</sub> + 51.51<sub>±02.58</sub> |      5.53<sub>±00.15</sub> |
|           Python (NumPy) |    0.070<sub>±0.000</sub> |   28.41<sub>±00.12</sub> + 54.96<sub>±00.04</sub> |      6.49<sub>±00.04</sub> |
|          C++/g++ (Eigen) |    0.070<sub>±0.002</sub> |   29.55<sub>±08.72</sub> + 60.24<sub>±08.76</sub> |      5.13<sub>±00.13</sub> |
|  Nim/clang (Arraymancer) |    0.072<sub>±0.001</sub> |    6.34<sub>±00.04</sub> + 54.76<sub>±00.08</sub> |      6.07<sub>±00.07</sub> |
|              Java (ND4J) |    0.081<sub>±0.005</sub> |  104.24<sub>±01.83</sub> + 92.25<sub>±00.16</sub> |      6.35<sub>±00.31</sub> |
|           Rust (ndarray) |    0.090<sub>±0.000</sub> |    2.63<sub>±00.04</sub> + 68.48<sub>±00.00</sub> |      6.00<sub>±00.03</sub> |
|       Julia (threads: 4) |    0.191<sub>±0.000</sub> |  235.35<sub>±00.21</sub> + 49.23<sub>±00.90</sub> |     15.54<sub>±00.02</sub> |
|       Julia (threads: 1) |    0.510<sub>±0.000</sub> |  235.65<sub>±00.18</sub> + 49.17<sub>±00.66</sub> |     21.51<sub>±00.03</sub> |
|          Julia (no BLAS) |    1.046<sub>±0.010</sub> |  225.72<sub>±00.30</sub> + 51.62<sub>±00.01</sub> |     46.15<sub>±00.39</sub> |
|                   D/ldc2 |    1.716<sub>±0.001</sub> |    3.52<sub>±00.03</sub> + 70.39<sub>±00.00</sub> |     63.31<sub>±00.11</sub> |
|                    D/gdc |    1.868<sub>±0.001</sub> |    7.55<sub>±00.04</sub> + 70.43<sub>±00.03</sub> |     73.08<sub>±00.12</sub> |
|                    D/dmd |    1.875<sub>±0.002</sub> |    3.63<sub>±00.04</sub> + 70.41<sub>±00.00</sub> |     71.06<sub>±00.11</sub> |
|                    C/gcc |    3.029<sub>±0.000</sub> |    1.32<sub>±00.00</sub> + 68.80<sub>±00.04</sub> |    109.29<sub>±00.43</sub> |
|               Vala/clang |    3.060<sub>±0.000</sub> |    4.10<sub>±00.03</sub> + 69.79<sub>±00.08</sub> |    107.55<sub>±00.90</sub> |
|                     Rust |    3.065<sub>±0.000</sub> |    2.45<sub>±00.05</sub> + 68.57<sub>±00.00</sub> |    105.15<sub>±00.17</sub> |
|                  C/clang |    3.067<sub>±0.000</sub> |    1.38<sub>±00.01</sub> + 68.77<sub>±00.02</sub> |    105.98<sub>±00.38</sub> |
|                      Zig |    3.085<sub>±0.001</sub> |    1.34<sub>±00.03</sub> + 68.89<sub>±00.00</sub> |    109.27<sub>±00.08</sub> |
|                  Nim/gcc |    3.088<sub>±0.001</sub> |    2.43<sub>±00.06</sub> + 67.19<sub>±00.67</sub> |    111.11<sub>±00.30</sub> |
|                    Swift |    3.094<sub>±0.000</sub> |    6.49<sub>±00.02</sub> + 68.88<sub>±00.01</sub> |    108.15<sub>±00.40</sub> |
|                     Java |    3.095<sub>±0.000</sub> |   38.11<sub>±00.07</sub> + 68.41<sub>±00.13</sub> |    107.24<sub>±00.38</sub> |
|                 Vala/gcc |    3.125<sub>±0.001</sub> |    4.13<sub>±00.02</sub> + 69.79<sub>±00.07</sub> |    114.53<sub>±00.06</sub> |
|                Nim/clang |    3.136<sub>±0.001</sub> |    2.88<sub>±00.04</sub> + 70.44<sub>±00.06</sub> |    107.79<sub>±00.13</sub> |
|                    V/gcc |    3.146<sub>±0.000</sub> |    1.87<sub>±00.06</sub> + 68.84<sub>±00.00</sub> |    117.55<sub>±01.78</sub> |
|                  V/clang |    3.147<sub>±0.000</sub> |    2.22<sub>±00.07</sub> + 68.84<sub>±00.00</sub> |    115.64<sub>±00.13</sub> |
|                       Go |    3.152<sub>±0.000</sub> |    3.63<sub>±00.04</sub> + 73.18<sub>±00.09</sub> |    113.80<sub>±00.18</sub> |
|                 Go/gccgo |    3.159<sub>±0.000</sub> |   24.91<sub>±00.09</sub> + 73.53<sub>±00.12</sub> |    112.23<sub>±00.10</sub> |
|                  Crystal |    3.161<sub>±0.001</sub> |    4.25<sub>±00.02</sub> + 59.67<sub>±00.03</sub> |    115.88<sub>±00.10</sub> |
|                  Node.js |    3.213<sub>±0.001</sub> |   41.96<sub>±00.13</sub> + 71.22<sub>±00.19</sub> |    129.55<sub>±00.09</sub> |
|              Python/pypy |    3.270<sub>±0.001</sub> |   65.78<sub>±00.05</sub> + 68.63<sub>±00.01</sub> |    136.14<sub>±00.12</sub> |
|                    Scala |    3.322<sub>±0.007</sub> |  65.01<sub>±00.11</sub> + 133.87<sub>±00.15</sub> |    118.54<sub>±00.41</sub> |
|             C#/.NET Core |    4.874<sub>±0.001</sub> |   34.67<sub>±00.10</sub> + 69.34<sub>±00.00</sub> |    196.06<sub>±00.16</sub> |
|               Kotlin/JVM |    6.937<sub>±0.001</sub> |   39.71<sub>±00.10</sub> + 68.16<sub>±00.22</sub> |    271.50<sub>±00.34</sub> |
|                  C#/Mono |    7.413<sub>±0.001</sub> |   25.56<sub>±00.12</sub> + 69.54<sub>±00.03</sub> |    299.15<sub>±00.74</sub> |
|         Ruby/truffleruby |   14.604<sub>±1.175</sub> | 351.93<sub>±01.70</sub> + 465.25<sub>±18.48</sub> |    553.17<sub>±37.67</sub> |
| Ruby/truffleruby (--jvm) |   21.393<sub>±0.051</sub> | 437.31<sub>±33.21</sub> + 961.78<sub>±75.62</sub> |    804.61<sub>±05.21</sub> |
|                     Ruby |  180.926<sub>±2.288</sub> |   15.46<sub>±00.03</sub> + 68.58<sub>±00.00</sub> |  7927.39<sub>±113.83</sub> |
|             Ruby (--jit) |  182.285<sub>±0.371</sub> |  272.05<sub>±00.05</sub> + 68.84<sub>±00.00</sub> |   7995.18<sub>±94.11</sub> |
|                     Perl |  221.133<sub>±0.748</sub> |   9.56<sub>±00.04</sub> + 599.55<sub>±00.02</sub> |   9001.25<sub>±37.66</sub> |
|                   Python |  261.473<sub>±0.827</sub> |   10.58<sub>±00.05</sub> + 68.84<sub>±00.00</sub> |  10445.40<sub>±55.25</sub> |
|                      Tcl |  298.785<sub>±5.053</sub> |   7.51<sub>±00.04</sub> + 400.15<sub>±00.03</sub> | 12660.24<sub>±177.21</sub> |
|               Ruby/jruby | 404.829<sub>±24.918</sub> | 266.19<sub>±01.83</sub> + 779.12<sub>±54.18</sub> | 16982.36<sub>±869.82</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                      Zig | 0.059<sub>±0.000</sub> |    0.75<sub>±00.00</sub> + 49.41<sub>±00.04</sub> |   2.44<sub>±00.01</sub> |
|                     Rust | 0.093<sub>±0.000</sub> |    2.19<sub>±00.05</sub> + 77.27<sub>±00.00</sub> |   3.78<sub>±00.01</sub> |
|                  Crystal | 0.123<sub>±0.000</sub> |    3.48<sub>±00.02</sub> + 89.43<sub>±00.64</sub> |   5.54<sub>±00.06</sub> |
|                  C++/g++ | 0.132<sub>±0.000</sub> |    3.54<sub>±00.07</sub> + 85.27<sub>±00.26</sub> |   5.36<sub>±00.04</sub> |
|              C++/clang++ | 0.143<sub>±0.000</sub> |    3.08<sub>±00.08</sub> + 75.41<sub>±00.00</sub> |   5.63<sub>±00.05</sub> |
|                  V/clang | 0.148<sub>±0.000</sub> |   0.72<sub>±00.00</sub> + 262.80<sub>±00.57</sub> |   6.15<sub>±00.06</sub> |
|                    V/gcc | 0.150<sub>±0.000</sub> |   0.71<sub>±00.06</sub> + 259.28<sub>±00.48</sub> |   6.28<sub>±00.04</sub> |
|                     Java | 0.155<sub>±0.002</sub> |  36.96<sub>±00.11</sub> + 144.23<sub>±05.61</sub> |   8.63<sub>±00.15</sub> |
|                  Node.js | 0.256<sub>±0.003</sub> |  36.96<sub>±00.11</sub> + 181.19<sub>±02.81</sub> |  12.80<sub>±00.06</sub> |
|               Lua/luajit | 0.337<sub>±0.001</sub> |   2.49<sub>±00.03</sub> + 156.75<sub>±00.38</sub> |  12.87<sub>±00.03</sub> |
|                    Scala | 0.391<sub>±0.005</sub> |  66.10<sub>±00.30</sub> + 258.71<sub>±02.41</sub> |  22.50<sub>±00.24</sub> |
|                    Julia | 0.557<sub>±0.001</sub> | 229.65<sub>±00.18</sub> + 366.99<sub>±02.17</sub> |  22.00<sub>±00.09</sub> |
|              Python/pypy | 0.880<sub>±0.002</sub> |  64.77<sub>±00.07</sub> + 249.36<sub>±00.15</sub> |  34.55<sub>±00.17</sub> |
|             Ruby (--jit) | 1.443<sub>±0.005</sub> | 270.96<sub>±00.02</sub> + 146.80<sub>±00.04</sub> |  58.79<sub>±00.46</sub> |
|                      Lua | 1.480<sub>±0.005</sub> |   2.32<sub>±00.01</sub> + 283.97<sub>±00.87</sub> |  57.87<sub>±00.19</sub> |
|         Ruby/truffleruby | 1.563<sub>±0.012</sub> | 287.18<sub>±00.42</sub> + 334.66<sub>±24.08</sub> |  80.19<sub>±00.54</sub> |
|               Ruby/jruby | 1.883<sub>±0.026</sub> | 182.58<sub>±00.29</sub> + 370.42<sub>±55.17</sub> | 101.51<sub>±01.01</sub> |
| Ruby/truffleruby (--jvm) | 1.895<sub>±0.018</sub> | 348.72<sub>±09.17</sub> + 526.76<sub>±97.23</sub> | 123.08<sub>±01.27</sub> |
|                     Ruby | 2.032<sub>±0.004</sub> |  14.36<sub>±00.02</sub> + 146.75<sub>±00.00</sub> |  82.09<sub>±00.60</sub> |
|                   Python | 4.806<sub>±0.058</sub> |  10.41<sub>±00.06</sub> + 234.92<sub>±00.64</sub> | 190.06<sub>±02.19</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 6.0.203                         |
| C#/.NET Core     | 4.1.0-5.22128.4 (5d10d428)      |
| C#/Mono          | 6.8.0.105                       |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.11.1"                        |
| Crystal          | 1.4.1                           |
| D/dmd            | v2.100.0                        |
| D/gdc            | 12.1.0                          |
| D/ldc2           | 1.29.0                          |
| Elixir           | 1.12.2                          |
| F#/.NET Core     | 12.0.1.0 for F# 6.0             |
| Go               | go1.18.2                        |
| Go/gccgo         | 12.1.0                          |
| Haskell          | 9.2.3                           |
| Java             | 18.0.1.1                        |
| Julia            | v"1.7.3"                        |
| Kotlin           | 1.6.21                          |
| Lua              | 5.4.4                           |
| Lua/luajit       | 2.1.0-beta3                     |
| MLton            | 20210117                        |
| Nim              | 1.6.6                           |
| Node.js          | v18.2.0                         |
| OCaml            | 4.14.0                          |
| PHP              | 8.1.5                           |
| Perl             | v5.34.0                         |
| Python           | 3.10.4                          |
| Python/pypy      | 7.3.9-final0 for Python 3.9.12  |
| Racket           | "8.5"                           |
| Ruby             | 3.1.2p20                        |
| Ruby/jruby       | 9.3.4.0                         |
| Ruby/truffleruby | 22.1.0                          |
| Rust             | 1.61.0                          |
| Scala            | 3.1.2                           |
| Swift            | 5.6.1                           |
| Tcl              | 8.6                             |
| V                | 0.2.4 a3c0a9b                   |
| Vala             | 0.56.1                          |
| Zig              | 0.9.1                           |
| clang/clang++    | 13.0.1                          |
| gcc/g++          | 12.1.0                          |

## Using Docker

Build the image:

    $ docker build docker/ -t benchmarks

Run the image:

    $ docker run -it --rm -v $(pwd):/src benchmarks <cmd>

where `<cmd>` is:

 - `versions` (print installed language versions);
 - `shell` (start the shell);
 - `brainfuck bench` (build and run Brainfuck bench.b benchmarks);
 - `brainfuck mandel` (build and run Brainfuck mandel.b benchmarks);
 - `base64` (build and run Base64 benchmarks);
 - `json` (build and run Json benchmarks);
 - `matmul` (build and run Matmul benchmarks);
 - `primes` (build and run Primes benchmarks);

Please note that the actual measurements provided in the project are taken semi-manually (via `shell`) as the full update takes days and could have occassional issues in Docker.

There is a `./run.sh` that could be used to simplify Docker usage:

 - `./run.sh build` (build the image);
 - `./run.sh make versions` (run the image with the `versions` command);
 - `sudo ./run.sh shell` (run the image with the `shell' command, sudo is required to read energy levels).

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

For Haskell:

 - [network](http://hackage.haskell.org/package/network) for
TCP connectivity between the tests and the test runner.
 - [raw-strings-qq](http://hackage.haskell.org/package/raw-strings-qq) for
raw string literals used in tests.

For Perl:

 - [cpanminus](https://metacpan.org/pod/App::cpanminus) for installing
modules from CPAN (Debian package `cpanminus`).

For Vala:

 - [JSON-GLib](https://wiki.gnome.org/Projects/JsonGlib) for JSON tests
 (Debian package `libjson-glib-dev`).

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

## README update

TOC is regenerated using [git-markdown-toc](https://github.com/ildar-shaimordanov/git-markdown-toc):

```
./run.sh toc
```

## Docker image update

Debian packages are pinned and updated with the script
(first, please ensure that the image is fine with the linter):

```
./run.sh lint
./run.sh update_apt
```
