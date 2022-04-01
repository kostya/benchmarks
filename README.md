<!-- md-toc-begin -->
# Table of Content
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

## Measurements

The measured values are:

 - time spent for the benchmark execution (loading required data and code self-testing are not measured);
 - memory consumption of the benchmark process, reported as `base` + `increase`, where `base` is the RSS before the benchmark and `increase` is the peak increase of the RSS during the benchmark;
 - energy consumption of the CPU package during the benchmark: PP0 (cores) + PP1 (uncores like GPU) + DRAM.

All values are presented as: `median`<sub>±`median absolute deviation`</sub>.

UPDATE: 2022-02-18

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|  Racket (Syntax Objects) |   0.848<sub>±0.013</sub> |   112.71<sub>±00.74</sub> + 0.00<sub>±00.00</sub> |    18.86<sub>±01.01</sub> |
|                  C++/g++ |   1.143<sub>±0.063</sub> |     1.68<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    20.79<sub>±02.04</sub> |
|                    C/gcc |   1.319<sub>±0.006</sub> |     0.73<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    28.21<sub>±00.90</sub> |
|                       Go |   1.367<sub>±0.044</sub> |     3.35<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    26.07<sub>±02.29</sub> |
|              C++/clang++ |   1.407<sub>±0.054</sub> |     1.53<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    28.82<sub>±02.69</sub> |
|                   D/ldc2 |   1.420<sub>±0.018</sub> |     3.14<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    29.74<sub>±00.87</sub> |
|                     Rust |   1.444<sub>±0.021</sub> |     2.16<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    24.06<sub>±00.58</sub> |
|                 Vala/gcc |   1.453<sub>±0.021</sub> |     4.33<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    27.56<sub>±01.83</sub> |
|                    D/gdc |   1.470<sub>±0.031</sub> |     6.86<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    31.01<sub>±00.88</sub> |
|                  Nim/gcc |   1.489<sub>±0.052</sub> |     1.92<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |    28.67<sub>±02.70</sub> |
|                  C/clang |   1.526<sub>±0.018</sub> |     0.68<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    26.22<sub>±00.52</sub> |
|               Vala/clang |   1.581<sub>±0.014</sub> |     4.37<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    30.19<sub>±00.52</sub> |
|                     Java |   1.648<sub>±0.017</sub> |    37.17<sub>±00.23</sub> + 1.56<sub>±00.36</sub> |    36.73<sub>±00.64</sub> |
|                 Go/gccgo |   1.678<sub>±0.094</sub> |    22.37<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |    33.14<sub>±01.98</sub> |
|                      Zig |   1.682<sub>±0.048</sub> |     0.72<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    33.60<sub>±03.41</sub> |
|                Nim/clang |   1.768<sub>±0.065</sub> |     2.38<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    31.90<sub>±02.28</sub> |
|               Kotlin/JVM |   1.797<sub>±0.036</sub> |    40.57<sub>±00.29</sub> + 1.19<sub>±00.60</sub> |    33.54<sub>±01.29</sub> |
|                    OCaml |   1.956<sub>±0.033</sub> |     2.85<sub>±00.05</sub> + 2.27<sub>±00.03</sub> |    42.49<sub>±02.50</sub> |
|                    MLton |   2.039<sub>±0.044</sub> |     1.47<sub>±00.07</sub> + 0.25<sub>±00.00</sub> |    44.69<sub>±02.30</sub> |
|                   Racket |   2.131<sub>±0.137</sub> |    95.76<sub>±00.26</sub> + 0.00<sub>±00.00</sub> |    42.11<sub>±03.49</sub> |
|             C#/.NET Core |   2.256<sub>±0.063</sub> |    34.20<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    43.70<sub>±03.79</sub> |
|                  V/clang |   2.288<sub>±0.096</sub> |     0.74<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    49.66<sub>±02.87</sub> |
|                    Julia |   2.392<sub>±0.097</sub> |   207.12<sub>±00.32</sub> + 0.70<sub>±00.03</sub> |    35.68<sub>±01.69</sub> |
|                  Crystal |   2.433<sub>±0.068</sub> |     3.52<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    43.41<sub>±02.12</sub> |
|             F#/.NET Core |   2.551<sub>±0.068</sub> |    37.80<sub>±00.08</sub> + 0.61<sub>±00.00</sub> |    45.74<sub>±02.08</sub> |
|                  C#/Mono |   2.803<sub>±0.104</sub> |    20.40<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    57.16<sub>±05.19</sub> |
|                    Scala |   2.988<sub>±0.055</sub> |  75.72<sub>±00.09</sub> + 207.00<sub>±30.15</sub> |    60.22<sub>±04.14</sub> |
|              Chez Scheme |   3.022<sub>±0.122</sub> |    24.83<sub>±00.04</sub> + 4.43<sub>±00.08</sub> |    55.62<sub>±02.53</sub> |
|                    D/dmd |   3.731<sub>±0.086</sub> |     3.76<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    65.93<sub>±01.93</sub> |
|                  Node.js |   4.569<sub>±0.094</sub> |    35.26<sub>±00.04</sub> + 1.10<sub>±00.09</sub> |    78.53<sub>±01.73</sub> |
|         Haskell (MArray) |   4.822<sub>±0.141</sub> |     3.85<sub>±00.07</sub> + 1.11<sub>±00.00</sub> |    87.24<sub>±02.20</sub> |
|                    Swift |   5.678<sub>±0.081</sub> |    13.17<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   128.69<sub>±02.71</sub> |
|               Lua/luajit |   7.852<sub>±0.387</sub> |     2.42<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   142.46<sub>±12.07</sub> |
| Ruby/truffleruby (--jvm) |   7.983<sub>±0.381</sub> | 338.00<sub>±02.50</sub> + 562.25<sub>±53.63</sub> |   282.72<sub>±06.68</sub> |
|         Ruby/truffleruby |   9.986<sub>±0.467</sub> | 278.40<sub>±01.31</sub> + 707.98<sub>±60.03</sub> |   264.21<sub>±20.52</sub> |
|              Python/pypy |  13.383<sub>±0.487</sub> |   63.75<sub>±00.14</sub> + 46.92<sub>±00.05</sub> |   273.75<sub>±15.52</sub> |
|                  Haskell |  14.413<sub>±0.224</sub> |     3.95<sub>±00.12</sub> + 0.82<sub>±00.17</sub> |   343.68<sub>±08.44</sub> |
|             Ruby (--jit) |  42.420<sub>±1.098</sub> |   271.26<sub>±00.04</sub> + 0.02<sub>±00.00</sub> |   759.89<sub>±21.19</sub> |
|                      Lua |  53.184<sub>±1.056</sub> |     2.28<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   929.47<sub>±10.98</sub> |
|                   Elixir |  59.667<sub>±1.157</sub> |    76.02<sub>±01.32</sub> + 0.00<sub>±00.00</sub> |  1077.70<sub>±71.88</sub> |
|                     Ruby |  91.057<sub>±2.093</sub> |    14.72<sub>±00.03</sub> + 0.00<sub>±00.00</sub> | 1799.94<sub>±149.19</sub> |
|               Ruby/jruby | 108.798<sub>±1.292</sub> | 185.06<sub>±01.75</sub> + 145.62<sub>±31.24</sub> | 2173.96<sub>±119.87</sub> |
|                   Python | 166.013<sub>±6.278</sub> |    10.45<sub>±00.01</sub> + 0.00<sub>±00.00</sub> | 3395.94<sub>±151.28</sub> |
|                 Tcl (FP) | 238.309<sub>±4.381</sub> |     4.31<sub>±00.05</sub> + 0.00<sub>±00.00</sub> | 5134.52<sub>±121.99</sub> |
|                     Perl | 319.914<sub>±2.781</sub> |     6.59<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  5786.86<sub>±30.09</sub> |
|                Tcl (OOP) | 481.582<sub>±5.443</sub> |     4.35<sub>±00.06</sub> + 0.00<sub>±00.00</sub> | 8798.96<sub>±234.20</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)
|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                  C++/g++ |  10.950<sub>±0.251</sub> |     3.48<sub>±00.06</sub> + 0.52<sub>±00.00</sub> |   218.96<sub>±08.02</sub> |
|                    C/gcc |  12.523<sub>±0.183</sub> |     0.71<sub>±00.03</sub> + 1.01<sub>±00.02</sub> |   257.16<sub>±05.69</sub> |
|                    V/gcc |  13.707<sub>±0.426</sub> |     1.79<sub>±00.04</sub> + 1.03<sub>±00.00</sub> |   285.62<sub>±08.07</sub> |
|                     Rust |  13.731<sub>±0.171</sub> |     2.12<sub>±00.07</sub> + 0.28<sub>±00.03</sub> |   235.48<sub>±05.28</sub> |
|              C++/clang++ |  14.174<sub>±0.325</sub> |     1.52<sub>±00.03</sub> + 2.12<sub>±00.02</sub> |   299.19<sub>±09.15</sub> |
|                 Vala/gcc |  14.663<sub>±0.363</sub> |     4.18<sub>±00.03</sub> + 1.86<sub>±00.08</sub> |   291.60<sub>±26.98</sub> |
|                   D/ldc2 |  14.976<sub>±0.486</sub> |     3.25<sub>±00.04</sub> + 0.77<sub>±00.00</sub> |   283.84<sub>±27.12</sub> |
|                  C/clang |  15.470<sub>±0.404</sub> |     0.73<sub>±00.00</sub> + 0.98<sub>±00.01</sub> |   323.58<sub>±06.07</sub> |
|                    D/gdc |  16.506<sub>±0.701</sub> |     6.85<sub>±00.04</sub> + 0.56<sub>±00.04</sub> |   337.21<sub>±15.95</sub> |
|  Racket (Syntax Objects) |  16.950<sub>±0.862</sub> |  112.16<sub>±00.33</sub> + 68.29<sub>±00.23</sub> |   361.35<sub>±27.86</sub> |
|               Kotlin/JVM |  17.402<sub>±0.433</sub> |    40.71<sub>±00.16</sub> + 1.10<sub>±00.29</sub> |   337.89<sub>±11.57</sub> |
|                       Go |  17.836<sub>±0.743</sub> |     3.22<sub>±00.02</sub> + 1.26<sub>±00.00</sub> |   324.48<sub>±21.86</sub> |
|                  Nim/gcc |  18.680<sub>±0.826</sub> |     1.97<sub>±00.03</sub> + 0.51<sub>±00.00</sub> |   372.68<sub>±27.56</sub> |
|                      Zig |  18.718<sub>±0.372</sub> |     1.54<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |   337.39<sub>±20.87</sub> |
|               Vala/clang |  18.810<sub>±0.517</sub> |     4.16<sub>±00.07</sub> + 1.86<sub>±00.09</sub> |   401.28<sub>±40.20</sub> |
|                  Crystal |  19.709<sub>±0.364</sub> |     3.45<sub>±00.03</sub> + 0.43<sub>±00.02</sub> |   347.61<sub>±10.56</sub> |
|                    Scala |  20.175<sub>±0.496</sub> |  77.13<sub>±00.16</sub> + 126.70<sub>±00.21</sub> |   413.26<sub>±22.62</sub> |
|                     Java |  20.628<sub>±0.997</sub> |    37.38<sub>±00.53</sub> + 1.93<sub>±00.45</sub> |   424.03<sub>±32.83</sub> |
|                Nim/clang |  20.684<sub>±0.372</sub> |     2.44<sub>±00.05</sub> + 0.54<sub>±00.03</sub> |   432.57<sub>±15.62</sub> |
|                 Go/gccgo |  21.434<sub>±0.736</sub> |    22.44<sub>±00.13</sub> + 1.28<sub>±00.01</sub> |   441.50<sub>±28.10</sub> |
|                    Swift |  21.689<sub>±0.758</sub> |    13.93<sub>±00.17</sub> + 0.00<sub>±00.00</sub> |   475.67<sub>±23.43</sub> |
|                  V/clang |  22.521<sub>±0.469</sub> |     1.76<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   466.48<sub>±25.32</sub> |
|             C#/.NET Core |  23.313<sub>±0.767</sub> |    33.81<sub>±00.10</sub> + 1.14<sub>±00.00</sub> |   475.38<sub>±43.97</sub> |
|                    OCaml |  34.780<sub>±1.291</sub> |     4.10<sub>±00.04</sub> + 8.86<sub>±01.05</sub> |   683.02<sub>±52.59</sub> |
|                   Racket |  39.128<sub>±1.913</sub> |    95.71<sub>±00.25</sub> + 0.26<sub>±00.00</sub> |   839.39<sub>±90.78</sub> |
|              Chez Scheme |  42.220<sub>±1.691</sub> |    25.69<sub>±00.06</sub> + 3.68<sub>±00.02</sub> |   813.73<sub>±36.13</sub> |
|                    D/dmd |  43.995<sub>±1.064</sub> |     3.81<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |   902.73<sub>±36.53</sub> |
|               Lua/luajit |  44.276<sub>±1.444</sub> |     2.43<sub>±00.04</sub> + 0.44<sub>±00.00</sub> |   936.01<sub>±28.17</sub> |
|             F#/.NET Core |  44.921<sub>±1.647</sub> |    37.84<sub>±00.07</sub> + 2.15<sub>±00.00</sub> |   872.03<sub>±63.91</sub> |
|                  C#/Mono |  45.631<sub>±0.283</sub> |    20.30<sub>±00.07</sub> + 0.89<sub>±00.00</sub> |   795.13<sub>±10.84</sub> |
|                    MLton |  47.547<sub>±1.556</sub> |     1.59<sub>±00.09</sub> + 4.11<sub>±00.00</sub> |  1030.05<sub>±69.26</sub> |
|                  Node.js |  50.279<sub>±1.020</sub> |    35.20<sub>±00.05</sub> + 5.20<sub>±00.00</sub> |   942.47<sub>±48.01</sub> |
|                    Julia |  51.288<sub>±0.979</sub> |   208.13<sub>±00.12</sub> + 0.65<sub>±00.01</sub> |  1124.20<sub>±40.28</sub> |
|              Python/pypy |  63.640<sub>±1.713</sub> |   63.78<sub>±00.14</sub> + 47.77<sub>±00.01</sub> | 1295.30<sub>±103.67</sub> |
|         Haskell (MArray) |  69.589<sub>±0.591</sub> |     3.79<sub>±00.08</sub> + 2.63<sub>±00.03</sub> |  1285.05<sub>±07.56</sub> |
| Ruby/truffleruby (--jvm) | 123.667<sub>±3.258</sub> | 335.56<sub>±07.72</sub> + 548.28<sub>±93.60</sub> |  2637.61<sub>±76.37</sub> |
|         Ruby/truffleruby | 137.962<sub>±3.223</sub> | 280.17<sub>±03.67</sub> + 714.62<sub>±46.79</sub> | 2642.24<sub>±170.32</sub> |
|                  Haskell | 221.219<sub>±2.389</sub> |    3.92<sub>±00.06</sub> + 26.16<sub>±00.00</sub> | 5035.28<sub>±152.83</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|          C/clang (aklomp) |  0.137<sub>±0.004</sub> |     2.05<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   3.42<sub>±00.15</sub> |
|            C/gcc (aklomp) |  0.140<sub>±0.006</sub> |     2.03<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   3.54<sub>±00.10</sub> |
|                      Rust |  1.106<sub>±0.044</sub> |     2.58<sub>±00.07</sub> + 0.01<sub>±00.01</sub> |  24.75<sub>±01.65</sub> |
|                     C/gcc |  1.207<sub>±0.053</sub> |     2.01<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  23.13<sub>±01.89</sub> |
|                   C/clang |  1.226<sub>±0.045</sub> |     2.00<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  24.68<sub>±02.34</sub> |
|                   V/clang |  1.272<sub>±0.035</sub> |     1.93<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  26.59<sub>±02.32</sub> |
|                   Nim/gcc |  1.372<sub>±0.012</sub> |     2.31<sub>±00.09</sub> + 4.12<sub>±00.03</sub> |  23.48<sub>±00.43</sub> |
|                 Nim/clang |  1.507<sub>±0.063</sub> |     2.82<sub>±00.07</sub> + 4.38<sub>±00.03</sub> |  33.97<sub>±02.72</sub> |
|                    D/ldc2 |  1.867<sub>±0.024</sub> |     3.47<sub>±00.02</sub> + 3.63<sub>±00.00</sub> |  33.14<sub>±00.30</sub> |
|              Ruby (--jit) |  2.078<sub>±0.046</sub> |  271.68<sub>±00.05</sub> + 67.07<sub>±00.04</sub> |  46.75<sub>±03.33</sub> |
|                      Ruby |  2.099<sub>±0.033</sub> |   15.13<sub>±00.05</sub> + 58.73<sub>±00.00</sub> |  46.06<sub>±01.90</sub> |
|                Vala/clang |  2.187<sub>±0.023</sub> |     5.54<sub>±00.03</sub> + 0.44<sub>±00.04</sub> |  44.67<sub>±01.14</sub> |
|                      Java |  2.205<sub>±0.062</sub> |  38.96<sub>±00.16</sub> + 262.42<sub>±04.83</sub> |  41.97<sub>±01.95</sub> |
|                  Vala/gcc |  2.361<sub>±0.103</sub> |     5.58<sub>±00.05</sub> + 0.46<sub>±00.03</sub> |  54.23<sub>±04.60</sub> |
|   C++/clang++ (libcrypto) |  2.370<sub>±0.072</sub> |     5.12<sub>±00.03</sub> + 0.08<sub>±00.00</sub> |  52.52<sub>±03.27</sub> |
|       C++/g++ (libcrypto) |  2.391<sub>±0.115</sub> |     5.72<sub>±00.04</sub> + 0.08<sub>±00.00</sub> |  51.61<sub>±04.25</sub> |
|                   Crystal |  2.419<sub>±0.010</sub> |     3.92<sub>±00.03</sub> + 1.17<sub>±00.01</sub> |  48.52<sub>±00.35</sub> |
|                     Scala |  2.435<sub>±0.023</sub> |  74.49<sub>±00.26</sub> + 240.30<sub>±16.40</sub> |  46.46<sub>±01.14</sub> |
|                Kotlin/JVM |  2.451<sub>±0.048</sub> |  41.52<sub>±00.68</sub> + 241.74<sub>±00.39</sub> |  57.94<sub>±02.46</sub> |
|                        Go |  2.547<sub>±0.007</sub> |     4.40<sub>±00.04</sub> + 5.56<sub>±00.23</sub> |  50.55<sub>±00.71</sub> |
|       Perl (MIME::Base64) |  2.825<sub>±0.094</sub> |    14.33<sub>±00.04</sub> + 0.07<sub>±00.06</sub> |  55.10<sub>±04.96</sub> |
|                       PHP |  2.958<sub>±0.047</sub> |    16.74<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |  63.91<sub>±04.43</sub> |
|                   Node.js |  3.058<sub>±0.046</sub> |   34.98<sub>±00.05</sub> + 36.73<sub>±00.09</sub> |  59.94<sub>±01.24</sub> |
|                  Go/gccgo |  3.176<sub>±0.009</sub> |    23.43<sub>±00.19</sub> + 8.72<sub>±00.28</sub> |  64.46<sub>±00.47</sub> |
|                     D/gdc |  3.455<sub>±0.130</sub> |     7.12<sub>±00.05</sub> + 3.41<sub>±00.01</sub> |  66.42<sub>±06.31</sub> |
|                     D/dmd |  3.706<sub>±0.078</sub> |     3.94<sub>±00.03</sub> + 3.62<sub>±00.00</sub> |  70.75<sub>±03.68</sub> |
|                    Python |  4.077<sub>±0.137</sub> |    10.16<sub>±00.04</sub> + 0.18<sub>±00.00</sub> |  79.47<sub>±08.79</sub> |
|                       Zig |  4.106<sub>±0.040</sub> |     1.50<sub>±00.02</sub> + 0.37<sub>±00.00</sub> |  93.20<sub>±04.91</sub> |
|               Python/pypy |  4.727<sub>±0.123</sub> |   63.59<sub>±00.10</sub> + 47.59<sub>±00.08</sub> | 106.54<sub>±05.62</sub> |
|                     Julia |  4.848<sub>±0.060</sub> |  226.41<sub>±00.43</sub> + 52.16<sub>±09.93</sub> | 113.62<sub>±03.55</sub> |
|              F#/.NET Core |  5.150<sub>±0.021</sub> |   38.55<sub>±00.03</sub> + 39.05<sub>±06.54</sub> |  88.05<sub>±00.95</sub> |
|              C#/.NET Core |  5.230<sub>±0.038</sub> |   34.58<sub>±00.05</sub> + 32.37<sub>±03.19</sub> |  89.02<sub>±01.14</sub> |
|                       Tcl |  5.931<sub>±0.092</sub> |     4.88<sub>±00.02</sub> + 0.18<sub>±00.03</sub> | 104.96<sub>±02.94</sub> |
|  Ruby/truffleruby (--jvm) |  6.189<sub>±0.098</sub> | 343.99<sub>±06.43</sub> + 262.94<sub>±42.01</sub> | 133.48<sub>±06.65</sub> |
|                   C#/Mono |  7.444<sub>±0.281</sub> |   20.87<sub>±00.06</sub> + 18.47<sub>±00.05</sub> | 168.84<sub>±12.40</sub> |
|                Ruby/jruby |  7.554<sub>±0.145</sub> | 183.80<sub>±02.99</sub> + 176.76<sub>±03.83</sub> | 160.35<sub>±09.12</sub> |
| Perl (MIME::Base64::Perl) | 14.333<sub>±0.442</sub> |    15.62<sub>±00.05</sub> + 0.27<sub>±00.04</sub> | 303.59<sub>±08.81</sub> |
|          Ruby/truffleruby | 18.930<sub>±0.680</sub> | 280.37<sub>±02.60</sub> + 285.21<sub>±20.17</sub> | 366.82<sub>±27.92</sub> |

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

|                            Language |                 Time, s |                                         Memory, MiB |               Energy, J |
| :---------------------------------- | ----------------------: | --------------------------------------------------: | ----------------------: |
|        C++/g++ (simdjson On-Demand) |  0.090<sub>±0.001</sub> |    113.41<sub>±00.06</sub> + 59.81<sub>±00.00</sub> |   1.93<sub>±00.08</sub> |
|    C++/clang++ (simdjson On-Demand) |  0.095<sub>±0.002</sub> |    113.07<sub>±00.06</sub> + 59.81<sub>±00.00</sub> |   2.11<sub>±00.18</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.096<sub>±0.003</sub> |     113.18<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   2.02<sub>±00.11</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.103<sub>±0.005</sub> |     112.49<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   2.21<sub>±00.25</sub> |
|             C++/g++ (DAW JSON Link) |  0.104<sub>±0.003</sub> |     113.25<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   2.65<sub>±00.18</sub> |
|         C++/clang++ (DAW JSON Link) |  0.123<sub>±0.002</sub> |     112.52<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |   2.56<sub>±00.27</sub> |
|          C++/clang++ (simdjson DOM) |  0.135<sub>±0.005</sub> |   113.03<sub>±00.07</sub> + 176.60<sub>±00.26</sub> |   3.19<sub>±00.20</sub> |
|              C++/g++ (simdjson DOM) |  0.136<sub>±0.003</sub> |   113.44<sub>±00.01</sub> + 176.60<sub>±00.00</sub> |   3.01<sub>±00.10</sub> |
|                 Rust (Serde Custom) |  0.144<sub>±0.004</sub> |     112.00<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |   2.97<sub>±00.26</sub> |
|                  Rust (Serde Typed) |  0.147<sub>±0.005</sub> |    111.93<sub>±00.09</sub> + 11.67<sub>±00.13</sub> |   3.14<sub>±00.34</sub> |
|                     C++/g++ (gason) |  0.165<sub>±0.006</sub> |    113.15<sub>±00.06</sub> + 96.80<sub>±00.03</sub> |   3.27<sub>±00.20</sub> |
|                 C++/clang++ (gason) |  0.191<sub>±0.002</sub> |    112.49<sub>±00.05</sub> + 96.97<sub>±00.00</sub> |   3.69<sub>±00.09</sub> |
|                 C++/g++ (RapidJSON) |  0.213<sub>±0.003</sub> |   113.23<sub>±00.05</sub> + 128.82<sub>±00.02</sub> |   4.11<sub>±00.24</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.247<sub>±0.008</sub> |    112.83<sub>±00.04</sub> + 61.35<sub>±00.00</sub> |   5.01<sub>±00.42</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.258<sub>±0.008</sub> |    112.79<sub>±00.06</sub> + 16.25<sub>±00.00</sub> |   4.96<sub>±00.43</sub> |
|             C++/clang++ (RapidJSON) |  0.260<sub>±0.009</sub> |   112.53<sub>±00.09</sub> + 129.00<sub>±00.01</sub> |   5.90<sub>±00.40</sub> |
|         C++/g++ (RapidJSON Precise) |  0.270<sub>±0.005</sub> |   113.25<sub>±00.04</sub> + 128.85<sub>±00.04</sub> |   6.17<sub>±00.23</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.417<sub>±0.009</sub> |   112.58<sub>±00.06</sub> + 129.00<sub>±00.00</sub> |   7.63<sub>±00.24</sub> |
|             C++/g++ (RapidJSON SAX) |  0.473<sub>±0.021</sub> |     112.93<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  10.24<sub>±00.69</sub> |
|                C++/g++ (Boost.JSON) |  0.500<sub>±0.016</sub> |   113.21<sub>±00.04</sub> + 435.85<sub>±00.03</sub> |  12.05<sub>±00.83</sub> |
|                   Nim/clang (jsony) |  0.533<sub>±0.014</sub> |    112.14<sub>±00.02</sub> + 42.31<sub>±00.03</sub> |   9.99<sub>±00.44</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.539<sub>±0.014</sub> |     194.79<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |   9.65<sub>±00.71</sub> |
|                     Nim/gcc (jsony) |  0.556<sub>±0.029</sub> |    111.61<sub>±00.05</sub> + 42.34<sub>±00.06</sub> |  12.71<sub>±00.92</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.559<sub>±0.015</sub> |     113.00<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   9.78<sub>±00.23</sub> |
|            C++/clang++ (Boost.JSON) |  0.565<sub>±0.019</sub> |   112.55<sub>±00.04</sub> + 436.25<sub>±00.00</sub> |  11.17<sub>±00.75</sub> |
|                             Node.js |  0.650<sub>±0.026</sub> |   145.06<sub>±00.07</sub> + 184.66<sub>±00.33</sub> |  16.07<sub>±00.95</sub> |
|                       Go (jsoniter) |  0.657<sub>±0.016</sub> |     231.09<sub>±00.09</sub> + 1.43<sub>±00.09</sub> |  13.75<sub>±00.74</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.665<sub>±0.029</sub> |     194.77<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  11.86<sub>±00.41</sub> |
|                     Java (DSL-JSON) |  0.737<sub>±0.010</sub> |   263.00<sub>±00.49</sub> + 210.85<sub>±11.03</sub> |  18.70<sub>±00.72</sub> |
|                         Python/pypy |  0.780<sub>±0.032</sub> |   283.42<sub>±00.15</sub> + 123.29<sub>±00.00</sub> |  16.10<sub>±02.00</sub> |
|                Rust (Serde Untyped) |  0.831<sub>±0.040</sub> |   111.96<sub>±00.06</sub> + 839.98<sub>±00.00</sub> |  17.55<sub>±01.77</sub> |
|                               V/gcc |  0.844<sub>±0.029</sub> |   111.24<sub>±00.02</sub> + 496.21<sub>±00.06</sub> |  19.19<sub>±01.14</sub> |
|                             V/clang |  0.884<sub>±0.020</sub> |   111.23<sub>±00.03</sub> + 496.21<sub>±00.06</sub> |  17.92<sub>±01.90</sub> |
|                                 Zig |  0.924<sub>±0.038</sub> |    110.80<sub>±00.02</sub> + 12.18<sub>±00.00</sub> |  19.69<sub>±01.71</sub> |
|                       Julia (JSON3) |  0.924<sub>±0.032</sub> |   462.42<sub>±00.29</sub> + 257.55<sub>±00.49</sub> |  17.61<sub>±00.72</sub> |
|     C#/.NET Core (System.Text.Json) |  0.985<sub>±0.030</sub> |   478.82<sub>±00.04</sub> + 138.78<sub>±00.00</sub> |  18.62<sub>±01.95</sub> |
|              Nim/clang (Packedjson) |  0.999<sub>±0.045</sub> |   112.74<sub>±00.06</sub> + 293.91<sub>±00.00</sub> |  22.41<sub>±01.07</sub> |
|             Perl (Cpanel::JSON::XS) |  1.004<sub>±0.038</sub> |   124.90<sub>±00.03</sub> + 402.77<sub>±00.00</sub> |  22.90<sub>±01.36</sub> |
|                      Crystal (Pull) |  1.035<sub>±0.029</sub> |    113.89<sub>±00.05</sub> + 18.18<sub>±00.02</sub> |  20.58<sub>±00.30</sub> |
|                                  Go |  1.083<sub>±0.047</sub> |    117.23<sub>±00.09</sub> + 83.28<sub>±00.25</sub> |  20.20<sub>±01.63</sub> |
|                    Crystal (Schema) |  1.084<sub>±0.047</sub> |    113.86<sub>±00.08</sub> + 48.61<sub>±00.00</sub> |  20.99<sub>±01.20</sub> |
|                Nim/gcc (Packedjson) |  1.137<sub>±0.035</sub> |   112.18<sub>±00.08</sub> + 293.91<sub>±00.00</sub> |  25.53<sub>±01.02</sub> |
|                                 PHP |  1.192<sub>±0.046</sub> |   125.95<sub>±00.06</sub> + 682.09<sub>±00.00</sub> |  25.15<sub>±01.84</sub> |
|                             Crystal |  1.342<sub>±0.037</sub> |   113.88<sub>±00.04</sub> + 392.07<sub>±00.09</sub> |  25.04<sub>±02.04</sub> |
|                           Nim/clang |  1.456<sub>±0.039</sub> |   112.70<sub>±00.04</sub> + 924.80<sub>±00.03</sub> |  31.77<sub>±02.54</sub> |
|                             Clojure |  1.536<sub>±0.065</sub> |   450.20<sub>±03.25</sub> + 499.05<sub>±09.78</sub> |  40.77<sub>±02.65</sub> |
|                    C++/g++ (json-c) |  1.557<sub>±0.052</sub> |  113.30<sub>±00.12</sub> + 1215.96<sub>±00.00</sub> |  35.02<sub>±02.95</sub> |
|              C++/clang++ (Nlohmann) |  1.602<sub>±0.055</sub> |   112.66<sub>±00.05</sub> + 360.15<sub>±00.03</sub> |  36.01<sub>±03.19</sub> |
|                C++/clang++ (json-c) |  1.617<sub>±0.021</sub> |  112.70<sub>±00.01</sub> + 1216.09<sub>±00.02</sub> |  30.62<sub>±01.36</sub> |
|                             Nim/gcc |  1.642<sub>±0.030</sub> |   112.18<sub>±00.09</sub> + 919.39<sub>±00.03</sub> |  30.55<sub>±01.62</sub> |
|                  C++/g++ (Nlohmann) |  1.680<sub>±0.077</sub> |   113.21<sub>±00.12</sub> + 447.96<sub>±00.08</sub> |  36.12<sub>±01.71</sub> |
|                              Python |  1.703<sub>±0.054</sub> |   120.14<sub>±00.02</sub> + 377.33<sub>±00.00</sub> |  39.86<sub>±01.82</sub> |
|                            Go/gccgo |  1.705<sub>±0.020</sub> |    137.43<sub>±00.26</sub> + 83.45<sub>±00.04</sub> |  31.40<sub>±00.86</sub> |
|                 CPython (UltraJSON) |  1.724<sub>±0.021</sub> |   121.89<sub>±00.03</sub> + 547.29<sub>±01.16</sub> |  37.53<sub>±01.69</sub> |
|                        C#/.NET Core |  1.749<sub>±0.034</sub> |   486.57<sub>±00.21</sub> + 294.18<sub>±00.01</sub> |  31.46<sub>±01.37</sub> |
|                        Ruby (--jit) |  1.925<sub>±0.066</sub> |   381.21<sub>±00.03</sub> + 262.95<sub>±00.00</sub> |  43.85<sub>±02.16</sub> |
|                                Ruby |  1.970<sub>±0.076</sub> |   124.65<sub>±00.05</sub> + 262.92<sub>±00.01</sub> |  41.77<sub>±04.36</sub> |
|     F#/.NET Core (System.Text.Json) |  2.348<sub>±0.099</sub> |   486.03<sub>±00.07</sub> + 452.20<sub>±01.80</sub> |  45.45<sub>±04.39</sub> |
|                             C#/Mono |  2.377<sub>±0.153</sub> |     476.10<sub>±00.10</sub> + 0.19<sub>±00.01</sub> |  47.15<sub>±04.55</sub> |
|                              D/ldc2 |  2.442<sub>±0.051</sub> |   113.13<sub>±00.09</sub> + 680.13<sub>±00.03</sub> |  49.10<sub>±02.48</sub> |
|                     Scala (uPickle) |  2.569<sub>±0.085</sub> |   303.83<sub>±00.12</sub> + 714.35<sub>±50.07</sub> |  53.86<sub>±02.62</sub> |
|                         Ruby (YAJL) |  2.629<sub>±0.043</sub> |   124.57<sub>±00.03</sub> + 279.79<sub>±00.00</sub> |  49.00<sub>±02.29</sub> |
|                               D/gdc |  2.899<sub>±0.035</sub> |   116.72<sub>±00.02</sub> + 600.57<sub>±00.01</sub> |  64.11<sub>±02.12</sub> |
|                             Haskell |  3.490<sub>±0.107</sub> |   115.97<sub>±00.03</sub> + 715.06<sub>±00.12</sub> |  77.20<sub>±05.47</sub> |
|                           Rust (jq) |  3.699<sub>±0.127</sub> |   113.83<sub>±00.01</sub> + 778.38<sub>±00.26</sub> |  80.25<sub>±02.99</sub> |
|    C++/clang++ (Boost.PropertyTree) |  3.867<sub>±0.111</sub> |  194.94<sub>±00.05</sub> + 1232.86<sub>±00.03</sub> |  79.39<sub>±05.54</sub> |
|                          Ruby/jruby |  3.867<sub>±0.072</sub> | 459.34<sub>±07.57</sub> + 1182.21<sub>±110.62</sub> |  95.92<sub>±07.95</sub> |
|        C++/g++ (Boost.PropertyTree) |  4.057<sub>±0.156</sub> |  113.12<sub>±00.02</sub> + 1440.14<sub>±00.03</sub> |  82.94<sub>±07.71</sub> |
|                          Vala/clang |  5.012<sub>±0.164</sub> |   114.73<sub>±00.03</sub> + 932.45<sub>±00.02</sub> |  97.95<sub>±08.40</sub> |
|                            Vala/gcc |  5.109<sub>±0.192</sub> |   114.72<sub>±00.02</sub> + 996.94<sub>±00.04</sub> | 101.13<sub>±09.80</sub> |
|                               D/dmd |  5.449<sub>±0.092</sub> |   113.78<sub>±00.02</sub> + 680.12<sub>±00.03</sub> |  99.31<sub>±06.83</sub> |
|                   Perl (JSON::Tiny) | 12.389<sub>±0.059</sub> |   125.54<sub>±00.06</sub> + 528.63<sub>±00.01</sub> | 219.19<sub>±06.26</sub> |
|            Ruby/truffleruby (--jvm) | 20.231<sub>±0.636</sub> |  518.25<sub>±33.14</sub> + 1531.78<sub>±51.59</sub> | 658.38<sub>±20.97</sub> |
|                    Ruby/truffleruby | 31.058<sub>±1.111</sub> |  432.55<sub>±05.15</sub> + 2239.55<sub>±42.82</sub> | 741.89<sub>±33.73</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                        Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | -------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.082<sub>±0.003</sub> |     7.09<sub>±00.02</sub> + 55.94<sub>±00.12</sub> |      2.93<sub>±00.74</sub> |
|          C++/g++ (Eigen) |    0.091<sub>±0.007</sub> |    23.69<sub>±12.86</sub> + 66.30<sub>±12.89</sub> |      2.80<sub>±00.29</sub> |
|           Python (NumPy) |    0.108<sub>±0.006</sub> |    28.38<sub>±00.04</sub> + 57.59<sub>±00.04</sub> |      4.37<sub>±00.58</sub> |
|      C++/clang++ (Eigen) |    0.118<sub>±0.016</sub> |    15.25<sub>±10.81</sub> + 75.25<sub>±10.75</sub> |      3.49<sub>±00.41</sub> |
|              Java (ND4J) |    0.118<sub>±0.008</sub> |   137.43<sub>±02.38</sub> + 90.75<sub>±00.03</sub> |      3.93<sub>±00.56</sub> |
|  Nim/clang (Arraymancer) |    0.125<sub>±0.021</sub> |     6.58<sub>±00.08</sub> + 55.68<sub>±00.31</sub> |      4.04<sub>±00.40</sub> |
|           Rust (ndarray) |    0.129<sub>±0.005</sub> |     3.10<sub>±00.06</sub> + 67.97<sub>±00.00</sub> |      4.65<sub>±00.37</sub> |
|    Nim/gcc (Arraymancer) |    0.140<sub>±0.007</sub> |     5.70<sub>±00.11</sub> + 55.84<sub>±00.47</sub> |      4.42<sub>±00.80</sub> |
|       Julia (threads: 8) |    0.154<sub>±0.015</sub> |   249.46<sub>±00.31</sub> + 43.40<sub>±00.07</sub> |      5.32<sub>±00.65</sub> |
|       Julia (threads: 1) |    0.206<sub>±0.004</sub> |   249.49<sub>±00.27</sub> + 41.87<sub>±00.03</sub> |      4.01<sub>±00.11</sub> |
|          Julia (no BLAS) |    1.274<sub>±0.020</sub> |   223.95<sub>±00.24</sub> + 51.44<sub>±00.03</sub> |     29.94<sub>±03.39</sub> |
|                   D/ldc2 |    2.058<sub>±0.047</sub> |     3.57<sub>±00.03</sub> + 70.26<sub>±00.13</sub> |     43.34<sub>±01.51</sub> |
|                    D/dmd |    2.201<sub>±0.030</sub> |     3.85<sub>±00.14</sub> + 70.27<sub>±00.13</sub> |     46.81<sub>±01.61</sub> |
|                    D/gdc |    2.245<sub>±0.047</sub> |     7.14<sub>±00.02</sub> + 70.04<sub>±00.02</sub> |     45.11<sub>±00.81</sub> |
|                  Nim/gcc |    3.420<sub>±0.045</sub> |     2.70<sub>±00.04</sub> + 65.87<sub>±00.13</sub> |     77.88<sub>±02.61</sub> |
|                    C/gcc |    3.456<sub>±0.085</sub> |     1.71<sub>±00.32</sub> + 68.51<sub>±00.23</sub> |     74.79<sub>±02.64</sub> |
|                  C/clang |    3.474<sub>±0.090</sub> |     1.76<sub>±00.35</sub> + 68.38<sub>±00.35</sub> |     70.66<sub>±01.70</sub> |
|                Nim/clang |    3.483<sub>±0.032</sub> |     3.09<sub>±00.14</sub> + 81.73<sub>±04.25</sub> |     72.13<sub>±01.80</sub> |
|                     Rust |    3.515<sub>±0.042</sub> |     2.71<sub>±00.17</sub> + 68.32<sub>±00.25</sub> |     73.23<sub>±01.49</sub> |
|                      Zig |    3.536<sub>±0.038</sub> |     1.73<sub>±00.06</sub> + 68.58<sub>±00.00</sub> |     74.47<sub>±02.16</sub> |
|                 Vala/gcc |    3.594<sub>±0.063</sub> |     5.80<sub>±00.09</sub> + 68.32<sub>±00.00</sub> |     61.46<sub>±01.34</sub> |
|                     Java |    3.607<sub>±0.030</sub> |    38.92<sub>±00.12</sub> + 80.58<sub>±00.29</sub> |     73.25<sub>±00.95</sub> |
|                    Swift |    3.611<sub>±0.096</sub> |     7.84<sub>±00.15</sub> + 68.91<sub>±00.02</sub> |     73.49<sub>±01.95</sub> |
|                 Go/gccgo |    3.645<sub>±0.080</sub> |    22.57<sub>±00.15</sub> + 73.47<sub>±00.14</sub> |     78.64<sub>±02.77</sub> |
|                       Go |    3.661<sub>±0.090</sub> |     3.54<sub>±00.06</sub> + 73.31<sub>±00.26</sub> |     83.72<sub>±02.83</sub> |
|               Vala/clang |    3.711<sub>±0.091</sub> |     5.79<sub>±00.06</sub> + 68.32<sub>±00.00</sub> |     59.58<sub>±03.05</sub> |
|                  V/clang |    3.726<sub>±0.071</sub> |     2.31<sub>±00.02</sub> + 68.84<sub>±00.00</sub> |     84.50<sub>±02.34</sub> |
|                    Scala |    3.790<sub>±0.045</sub> |   75.11<sub>±00.21</sub> + 144.23<sub>±00.43</sub> |     79.63<sub>±01.18</sub> |
|                  Crystal |    3.818<sub>±0.101</sub> |     4.30<sub>±00.04</sub> + 59.67<sub>±00.01</sub> |     83.75<sub>±06.03</sub> |
|                    V/gcc |    3.856<sub>±0.057</sub> |     1.94<sub>±00.06</sub> + 68.84<sub>±00.00</sub> |     77.54<sub>±04.86</sub> |
|               Kotlin/JVM |    4.037<sub>±0.033</sub> |    40.09<sub>±00.14</sub> + 80.13<sub>±00.28</sub> |     78.12<sub>±01.49</sub> |
|                  Node.js |    4.059<sub>±0.055</sub> |    40.39<sub>±00.07</sub> + 70.68<sub>±00.13</sub> |     77.87<sub>±05.83</sub> |
|              Python/pypy |    6.603<sub>±0.058</sub> |    64.86<sub>±00.07</sub> + 69.14<sub>±00.05</sub> |    117.64<sub>±03.61</sub> |
|             C#/.NET Core |    7.545<sub>±0.085</sub> |    33.17<sub>±00.05</sub> + 69.46<sub>±00.00</sub> |    128.54<sub>±03.88</sub> |
|                  C#/Mono |   11.652<sub>±0.080</sub> |    20.27<sub>±00.04</sub> + 69.01<sub>±00.01</sub> |    214.67<sub>±21.08</sub> |
|         Ruby/truffleruby |   25.218<sub>±0.727</sub> | 315.64<sub>±07.64</sub> + 877.79<sub>±215.04</sub> |    548.71<sub>±51.22</sub> |
| Ruby/truffleruby (--jvm) |   44.597<sub>±0.947</sub> | 392.42<sub>±17.45</sub> + 745.43<sub>±116.26</sub> |   1007.27<sub>±37.75</sub> |
|             Ruby (--jit) |  202.034<sub>±6.199</sub> |   272.42<sub>±00.02</sub> + 68.84<sub>±00.00</sub> |  3820.61<sub>±263.34</sub> |
|                     Ruby |  222.467<sub>±6.667</sub> |    15.83<sub>±00.06</sub> + 68.58<sub>±00.00</sub> |  4145.66<sub>±315.71</sub> |
|                   Python |  238.462<sub>±7.000</sub> |    10.52<sub>±00.02</sub> + 68.58<sub>±00.00</sub> |  4496.58<sub>±343.38</sub> |
|                      Tcl |  321.774<sub>±7.723</sub> |    7.21<sub>±00.01</sub> + 400.44<sub>±00.00</sub> |  6297.82<sub>±475.82</sub> |
|                     Perl |  402.410<sub>±4.590</sub> |    9.04<sub>±00.03</sub> + 599.72<sub>±00.08</sub> |  8259.27<sub>±234.35</sub> |
|               Ruby/jruby | 535.502<sub>±17.283</sub> | 260.31<sub>±03.16</sub> + 1172.21<sub>±31.18</sub> | 10768.71<sub>±199.71</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |              Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ---------------------: |
|                      Zig | 0.074<sub>±0.001</sub> |    1.42<sub>±00.02</sub> + 55.56<sub>±02.12</sub> |  1.59<sub>±00.07</sub> |
|                     Rust | 0.114<sub>±0.003</sub> |    2.35<sub>±00.09</sub> + 77.02<sub>±00.00</sub> |  2.40<sub>±00.14</sub> |
|                     Java | 0.163<sub>±0.003</sub> |  38.34<sub>±00.21</sub> + 102.58<sub>±00.41</sub> |  4.87<sub>±00.05</sub> |
|                  Crystal | 0.170<sub>±0.002</sub> |    3.53<sub>±00.08</sub> + 88.55<sub>±00.03</sub> |  3.73<sub>±00.04</sub> |
|              C++/clang++ | 0.174<sub>±0.002</sub> |    3.16<sub>±00.05</sub> + 75.41<sub>±00.01</sub> |  3.79<sub>±00.20</sub> |
|                  C++/g++ | 0.184<sub>±0.008</sub> |    3.52<sub>±00.03</sub> + 84.80<sub>±00.00</sub> |  3.64<sub>±00.26</sub> |
|                  V/clang | 0.187<sub>±0.003</sub> |   1.72<sub>±00.04</sub> + 270.91<sub>±03.22</sub> |  3.60<sub>±00.11</sub> |
|                    V/gcc | 0.189<sub>±0.009</sub> |   1.75<sub>±00.05</sub> + 263.56<sub>±04.06</sub> |  3.82<sub>±00.40</sub> |
|                  Node.js | 0.270<sub>±0.007</sub> |  34.45<sub>±00.08</sub> + 176.28<sub>±01.54</sub> |  7.12<sub>±00.08</sub> |
|               Lua/luajit | 0.404<sub>±0.015</sub> |   2.56<sub>±00.06</sub> + 156.05<sub>±00.57</sub> |  8.21<sub>±00.68</sub> |
|                    Scala | 0.416<sub>±0.005</sub> |  75.49<sub>±00.09</sub> + 230.85<sub>±01.07</sub> | 13.05<sub>±00.56</sub> |
|              Python/pypy | 0.836<sub>±0.018</sub> |  63.07<sub>±00.16</sub> + 251.34<sub>±00.04</sub> | 18.52<sub>±00.48</sub> |
|                    Julia | 1.047<sub>±0.019</sub> | 228.75<sub>±00.49</sub> + 333.23<sub>±00.60</sub> | 11.13<sub>±00.71</sub> |
|                      Lua | 1.656<sub>±0.021</sub> |   2.27<sub>±00.04</sub> + 284.17<sub>±00.48</sub> | 34.45<sub>±00.71</sub> |
|             Ruby (--jit) | 1.851<sub>±0.018</sub> | 271.23<sub>±00.02</sub> + 147.76<sub>±00.00</sub> | 31.36<sub>±00.73</sub> |
|         Ruby/truffleruby | 1.959<sub>±0.047</sub> | 276.10<sub>±03.31</sub> + 418.65<sub>±56.56</sub> | 52.68<sub>±05.10</sub> |
| Ruby/truffleruby (--jvm) | 2.089<sub>±0.069</sub> | 334.89<sub>±04.28</sub> + 472.94<sub>±77.31</sub> | 76.26<sub>±01.68</sub> |
|                     Ruby | 2.356<sub>±0.101</sub> |  14.69<sub>±00.06</sub> + 147.65<sub>±00.00</sub> | 46.89<sub>±05.81</sub> |
|               Ruby/jruby | 2.852<sub>±0.054</sub> | 182.88<sub>±01.82</sub> + 431.31<sub>±33.87</sub> | 69.85<sub>±06.73</sub> |
|                   Python | 4.629<sub>±0.151</sub> |  10.32<sub>±00.04</sub> + 235.70<sub>±00.90</sub> | 95.65<sub>±04.32</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 6.0.102                         |
| C#/.NET Core     | 4.0.1-1.22053.6 (9942dc95)      |
| C#/Mono          | 6.12.0.122                      |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.3"                        |
| Crystal          | 1.3.2                           |
| D/dmd            | v2.098.1                        |
| D/gdc            | 11.2.0                          |
| D/ldc2           | 1.28.1                          |
| Elixir           | 1.12.2                          |
| F#/.NET Core     | 12.0.0.0 for F# 6.0             |
| Go               | go1.17.7                        |
| Go/gccgo         | 11.2.0                          |
| Haskell          | 9.0.1                           |
| Java             | 17.0.2                          |
| Julia            | v"1.7.2"                        |
| Kotlin           | 1.6.10                          |
| Lua              | 5.4.4                           |
| Lua/luajit       | 2.1.0-beta3                     |
| MLton            | 20210117                        |
| Nim              | 1.6.4                           |
| Node.js          | v17.5.0                         |
| OCaml            | 4.13.1                          |
| PHP              | 8.1.2                           |
| Perl             | v5.34.0                         |
| Python           | 3.9.10                          |
| Python/pypy      | 7.3.7-final0 for Python 3.8.12  |
| Racket           | "8.4"                           |
| Ruby             | 3.1.0p0                         |
| Ruby/jruby       | 9.3.3.0                         |
| Ruby/truffleruby | 22.0.0.2                        |
| Rust             | 1.58.1                          |
| Scala            | 3.1.1                           |
| Swift            | 5.5.3                           |
| Tcl              | 8.6                             |
| V                | 0.2.4 efdbe9d                   |
| Vala             | 0.54.6                          |
| Zig              | 0.9.0                           |
| clang/clang++    | 13.0.1                          |
| gcc/g++          | 11.2.0                          |

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

There is a `Makefile` that could be used to simplify Docker usage:

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
