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
 - energy consumption of the CPU package during the benchmark: PP0 (cores) + PP1 (uncores like GPU) + DRAM.

All values are presented as: `median`<sub>±`median absolute deviation`</sub>.

UPDATE: 2022-04-05

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|                 Language |                   Time, s |                                        Memory, MiB |                   Energy, J |
| :----------------------- | ------------------------: | -------------------------------------------------: | --------------------------: |
|  Racket (Syntax Objects) |    1.011<sub>±0.097</sub> |    113.88<sub>±00.22</sub> + 0.00<sub>±00.00</sub> |      24.37<sub>±02.29</sub> |
|                  C++/g++ |    1.328<sub>±0.057</sub> |      1.65<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |      29.46<sub>±03.64</sub> |
|                  Nim/gcc |    1.540<sub>±0.052</sub> |      1.92<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |      33.70<sub>±05.18</sub> |
|                       Go |    1.553<sub>±0.107</sub> |      2.69<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |      40.06<sub>±05.62</sub> |
|                   D/ldc2 |    1.581<sub>±0.064</sub> |      3.11<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |      34.15<sub>±04.05</sub> |
|                    C/gcc |    1.596<sub>±0.037</sub> |      0.69<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |      37.11<sub>±05.43</sub> |
|              C++/clang++ |    1.600<sub>±0.046</sub> |      1.54<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |      37.64<sub>±03.13</sub> |
|               Kotlin/JVM |    1.645<sub>±0.089</sub> |     40.62<sub>±00.18</sub> + 0.32<sub>±00.06</sub> |      39.08<sub>±06.58</sub> |
|                 Vala/gcc |    1.694<sub>±0.136</sub> |      4.24<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |      41.70<sub>±03.03</sub> |
|                     Rust |    1.695<sub>±0.047</sub> |      2.13<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |      34.48<sub>±05.25</sub> |
|                    D/gdc |    1.699<sub>±0.119</sub> |      7.35<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |      36.09<sub>±04.06</sub> |
|                     Java |    1.731<sub>±0.113</sub> |     36.86<sub>±00.31</sub> + 1.36<sub>±00.13</sub> |      42.22<sub>±02.92</sub> |
|               Vala/clang |    1.849<sub>±0.103</sub> |      4.28<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |      45.56<sub>±03.55</sub> |
|                  C/clang |    1.869<sub>±0.153</sub> |      0.66<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |      50.55<sub>±04.01</sub> |
|                      Zig |    1.968<sub>±0.091</sub> |      0.73<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |      47.79<sub>±04.08</sub> |
|                Nim/clang |    2.010<sub>±0.102</sub> |      2.35<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |      51.21<sub>±06.20</sub> |
|                    OCaml |    2.171<sub>±0.127</sub> |      2.87<sub>±00.06</sub> + 2.25<sub>±00.03</sub> |      50.71<sub>±03.76</sub> |
|                    V/gcc |    2.202<sub>±0.140</sub> |      0.66<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |      49.36<sub>±05.44</sub> |
|                 Go/gccgo |    2.252<sub>±0.171</sub> |     24.53<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |      55.11<sub>±03.29</sub> |
|                    MLton |    2.333<sub>±0.161</sub> |      1.55<sub>±00.06</sub> + 0.25<sub>±00.00</sub> |      55.62<sub>±06.66</sub> |
|             C#/.NET Core |    2.507<sub>±0.173</sub> |     33.90<sub>±00.19</sub> + 0.03<sub>±00.03</sub> |      59.00<sub>±07.71</sub> |
|                   Racket |    2.656<sub>±0.262</sub> |     96.06<sub>±00.60</sub> + 0.00<sub>±00.00</sub> |      61.00<sub>±05.90</sub> |
|                  Crystal |    2.704<sub>±0.259</sub> |      3.49<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |      67.41<sub>±06.07</sub> |
|                    Julia |    2.707<sub>±0.198</sub> |    207.11<sub>±00.20</sub> + 0.52<sub>±00.04</sub> |      59.26<sub>±11.01</sub> |
|             F#/.NET Core |    2.999<sub>±0.146</sub> |     38.27<sub>±00.01</sub> + 0.54<sub>±00.00</sub> |      70.68<sub>±09.66</sub> |
|                  V/clang |    3.180<sub>±0.306</sub> |      0.69<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |      75.56<sub>±13.69</sub> |
|                  C#/Mono |    3.304<sub>±0.267</sub> |     20.39<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |      81.06<sub>±14.53</sub> |
|                    Scala |    3.459<sub>±0.242</sub> |   74.91<sub>±00.17</sub> + 176.15<sub>±00.39</sub> |      85.96<sub>±10.30</sub> |
|              Chez Scheme |    3.482<sub>±0.194</sub> |     24.80<sub>±00.03</sub> + 4.46<sub>±00.02</sub> |      74.04<sub>±08.16</sub> |
|                    D/dmd |    4.240<sub>±0.222</sub> |      3.68<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |      91.05<sub>±09.71</sub> |
|                  Node.js |    5.129<sub>±0.151</sub> |     36.56<sub>±00.04</sub> + 1.21<sub>±00.12</sub> |     122.66<sub>±17.70</sub> |
|         Haskell (MArray) |    5.505<sub>±0.240</sub> |      3.93<sub>±00.03</sub> + 1.11<sub>±00.00</sub> |     145.13<sub>±06.01</sub> |
|                    Swift |    6.951<sub>±0.543</sub> |     13.63<sub>±00.13</sub> + 0.00<sub>±00.00</sub> |     165.75<sub>±16.22</sub> |
|               Lua/luajit |    9.030<sub>±0.465</sub> |      2.39<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     216.01<sub>±23.40</sub> |
| Ruby/truffleruby (--jvm) |   11.454<sub>±1.583</sub> |  347.94<sub>±04.53</sub> + 540.62<sub>±28.68</sub> |     341.08<sub>±37.04</sub> |
|         Ruby/truffleruby |   12.226<sub>±1.522</sub> | 280.21<sub>±02.09</sub> + 748.83<sub>±183.46</sub> |     289.25<sub>±15.78</sub> |
|              Python/pypy |   16.909<sub>±1.115</sub> |    67.23<sub>±00.03</sub> + 45.17<sub>±00.07</sub> |     434.07<sub>±75.31</sub> |
|                  Haskell |   17.024<sub>±0.480</sub> |      4.12<sub>±00.03</sub> + 0.82<sub>±00.00</sub> |     453.91<sub>±40.49</sub> |
|             Ruby (--jit) |   50.055<sub>±4.216</sub> |    271.04<sub>±00.03</sub> + 0.02<sub>±00.00</sub> |   1187.01<sub>±115.63</sub> |
|                      Lua |   62.420<sub>±3.088</sub> |      2.24<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1473.42<sub>±183.53</sub> |
|                   Elixir |   68.931<sub>±3.954</sub> |     76.99<sub>±00.66</sub> + 0.00<sub>±00.00</sub> |   1659.67<sub>±157.02</sub> |
|                     Ruby |  103.270<sub>±9.509</sub> |     14.54<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   2470.41<sub>±191.63</sub> |
|               Ruby/jruby |  122.164<sub>±8.631</sub> |   188.03<sub>±02.15</sub> + 97.38<sub>±13.71</sub> |   2952.67<sub>±305.64</sub> |
|                   Python | 193.369<sub>±11.274</sub> |     10.23<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4545.79<sub>±363.17</sub> |
|                 Tcl (FP) | 283.398<sub>±20.971</sub> |      4.47<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   6972.13<sub>±417.92</sub> |
|                     Perl | 373.937<sub>±24.031</sub> |      7.07<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   8436.17<sub>±854.20</sub> |
|                Tcl (OOP) | 561.155<sub>±31.859</sub> |      4.45<sub>±00.04</sub> + 0.00<sub>±00.00</sub> | 13389.49<sub>±1068.41</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                  C++/g++ |  14.327<sub>±0.642</sub> |     3.45<sub>±00.09</sub> + 0.52<sub>±00.00</sub> |   366.85<sub>±30.85</sub> |
|                    C/gcc |  15.437<sub>±0.360</sub> |     0.68<sub>±00.03</sub> + 0.98<sub>±00.02</sub> |   402.16<sub>±27.19</sub> |
|                     Rust |  16.519<sub>±0.248</sub> |     2.26<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |   423.82<sub>±14.51</sub> |
|                   D/ldc2 |  16.683<sub>±0.610</sub> |     3.14<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   464.32<sub>±20.98</sub> |
|              C++/clang++ |  17.157<sub>±0.293</sub> |     1.53<sub>±00.05</sub> + 2.03<sub>±00.06</sub> |   463.00<sub>±15.43</sub> |
|                 Vala/gcc |  17.415<sub>±0.360</sub> |     4.12<sub>±00.02</sub> + 1.89<sub>±00.06</sub> |   437.74<sub>±19.51</sub> |
|                    D/gdc |  17.698<sub>±0.752</sub> |     7.36<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   401.17<sub>±16.08</sub> |
|                    V/gcc |  18.011<sub>±0.592</sub> |     1.62<sub>±00.16</sub> + 1.03<sub>±00.00</sub> |   447.08<sub>±15.85</sub> |
|               Kotlin/JVM |  18.981<sub>±0.534</sub> |    40.41<sub>±00.15</sub> + 0.97<sub>±00.16</sub> |   521.30<sub>±19.86</sub> |
|  Racket (Syntax Objects) |  20.434<sub>±0.741</sub> |  113.96<sub>±00.35</sub> + 69.35<sub>±00.52</sub> |   571.06<sub>±43.82</sub> |
|               Vala/clang |  20.912<sub>±0.578</sub> |     4.11<sub>±00.07</sub> + 1.96<sub>±00.11</sub> |   562.34<sub>±30.07</sub> |
|                  C/clang |  21.311<sub>±0.169</sub> |     0.71<sub>±00.00</sub> + 0.99<sub>±00.02</sub> |   548.33<sub>±22.73</sub> |
|                       Go |  21.366<sub>±0.361</sub> |     3.37<sub>±00.02</sub> + 1.26<sub>±00.00</sub> |   540.51<sub>±18.16</sub> |
|                  Crystal |  21.573<sub>±0.471</sub> |     3.49<sub>±00.02</sub> + 0.43<sub>±00.01</sub> |   600.92<sub>±24.38</sub> |
|                      Zig |  22.023<sub>±0.780</sub> |     1.50<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   576.80<sub>±22.73</sub> |
|                  Nim/gcc |  22.558<sub>±0.720</sub> |     1.93<sub>±00.06</sub> + 0.51<sub>±00.00</sub> |   571.71<sub>±28.56</sub> |
|                    Scala |  24.524<sub>±0.691</sub> |  75.12<sub>±00.11</sub> + 127.66<sub>±00.20</sub> |   637.82<sub>±39.98</sub> |
|                     Java |  24.985<sub>±0.863</sub> |    37.07<sub>±00.43</sub> + 2.02<sub>±00.48</sub> |   652.14<sub>±34.74</sub> |
|                Nim/clang |  26.895<sub>±0.627</sub> |     2.38<sub>±00.03</sub> + 0.54<sub>±00.03</sub> |   678.90<sub>±28.15</sub> |
|                    Swift |  27.927<sub>±0.256</sub> |    14.57<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |   712.38<sub>±12.51</sub> |
|             C#/.NET Core |  28.393<sub>±0.600</sub> |    33.92<sub>±00.05</sub> + 1.08<sub>±00.07</sub> |   753.14<sub>±34.28</sub> |
|                 Go/gccgo |  29.874<sub>±0.451</sub> |    24.56<sub>±00.05</sub> + 1.27<sub>±00.01</sub> |   750.58<sub>±37.75</sub> |
|                  V/clang |  30.913<sub>±0.696</sub> |     1.75<sub>±00.07</sub> + 0.77<sub>±00.00</sub> |   814.93<sub>±30.77</sub> |
|                    OCaml |  36.121<sub>±0.650</sub> |     4.05<sub>±00.05</sub> + 8.87<sub>±00.67</sub> |   936.73<sub>±36.22</sub> |
|                   Racket |  53.497<sub>±3.174</sub> |    97.60<sub>±00.59</sub> + 0.00<sub>±00.00</sub> |  1261.29<sub>±60.59</sub> |
|             F#/.NET Core |  54.494<sub>±1.072</sub> |    38.32<sub>±00.02</sub> + 2.09<sub>±00.00</sub> |  1379.30<sub>±15.93</sub> |
|                  C#/Mono |  54.782<sub>±1.113</sub> |    20.45<sub>±00.03</sub> + 0.88<sub>±00.00</sub> |  1386.87<sub>±37.55</sub> |
|              Chez Scheme |  54.820<sub>±3.482</sub> |    25.63<sub>±00.09</sub> + 3.67<sub>±00.01</sub> |  1327.41<sub>±61.37</sub> |
|                    D/dmd |  56.102<sub>±1.342</sub> |     3.68<sub>±00.06</sub> + 0.77<sub>±00.00</sub> |  1460.07<sub>±33.01</sub> |
|               Lua/luajit |  57.185<sub>±1.582</sub> |     2.40<sub>±00.03</sub> + 0.44<sub>±00.00</sub> |  1400.62<sub>±46.39</sub> |
|                    MLton |  59.369<sub>±2.219</sub> |     1.57<sub>±00.04</sub> + 4.11<sub>±00.00</sub> |  1468.63<sub>±25.28</sub> |
|                  Node.js |  61.555<sub>±1.985</sub> |    36.59<sub>±00.01</sub> + 5.77<sub>±00.27</sub> |  1565.65<sub>±53.59</sub> |
|                    Julia |  66.425<sub>±1.651</sub> |   207.29<sub>±00.33</sub> + 0.67<sub>±00.03</sub> |  1594.78<sub>±34.84</sub> |
|         Haskell (MArray) |  80.475<sub>±0.907</sub> |     4.00<sub>±00.09</sub> + 2.63<sub>±00.00</sub> |  2035.84<sub>±53.03</sub> |
|              Python/pypy |  81.882<sub>±1.597</sub> |   66.80<sub>±00.11</sub> + 46.35<sub>±00.23</sub> |  2067.69<sub>±49.20</sub> |
| Ruby/truffleruby (--jvm) | 162.026<sub>±4.443</sub> | 345.92<sub>±03.42</sub> + 528.44<sub>±71.86</sub> | 4106.21<sub>±202.03</sub> |
|         Ruby/truffleruby | 176.393<sub>±9.865</sub> | 280.18<sub>±01.63</sub> + 678.28<sub>±38.77</sub> | 4260.70<sub>±135.13</sub> |
|                  Haskell | 271.744<sub>±2.637</sub> |    4.09<sub>±00.04</sub> + 26.16<sub>±00.00</sub> |  6885.28<sub>±29.20</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|          C/clang (aklomp) |  0.162<sub>±0.008</sub> |     1.99<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   4.91<sub>±00.81</sub> |
|            C/gcc (aklomp) |  0.175<sub>±0.003</sub> |     2.01<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   5.10<sub>±00.66</sub> |
|                      Rust |  1.378<sub>±0.020</sub> |     2.54<sub>±00.06</sub> + 0.08<sub>±00.02</sub> |  39.05<sub>±03.70</sub> |
|                     C/gcc |  1.531<sub>±0.055</sub> |     2.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  46.84<sub>±02.07</sub> |
|                   C/clang |  1.541<sub>±0.051</sub> |     1.93<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  37.50<sub>±02.73</sub> |
|                   Nim/gcc |  1.622<sub>±0.094</sub> |     2.33<sub>±00.06</sub> + 4.41<sub>±00.03</sub> |  42.99<sub>±06.17</sub> |
|                   V/clang |  1.698<sub>±0.070</sub> |     1.82<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  43.94<sub>±07.12</sub> |
|                 Nim/clang |  1.889<sub>±0.061</sub> |     2.75<sub>±00.04</sub> + 4.38<sub>±00.00</sub> |  52.36<sub>±02.59</sub> |
|                     V/gcc |  1.997<sub>±0.057</sub> |     1.91<sub>±00.29</sub> + 0.15<sub>±00.15</sub> |  49.78<sub>±01.87</sub> |
|                    D/ldc2 |  2.324<sub>±0.145</sub> |     3.52<sub>±00.02</sub> + 3.63<sub>±00.00</sub> |  60.20<sub>±04.48</sub> |
|                      Java |  2.575<sub>±0.096</sub> |  37.35<sub>±00.24</sub> + 185.08<sub>±00.62</sub> |  72.17<sub>±01.81</sub> |
|                Vala/clang |  2.606<sub>±0.096</sub> |     5.57<sub>±00.03</sub> + 0.37<sub>±00.07</sub> |  67.16<sub>±04.53</sub> |
|                  Vala/gcc |  2.630<sub>±0.114</sub> |     5.57<sub>±00.03</sub> + 0.49<sub>±00.16</sub> |  65.13<sub>±06.43</sub> |
|                Kotlin/JVM |  2.685<sub>±0.108</sub> |  41.09<sub>±00.64</sub> + 241.23<sub>±01.17</sub> |  77.65<sub>±03.81</sub> |
|                     Scala |  2.714<sub>±0.115</sub> |  74.16<sub>±00.21</sub> + 166.92<sub>±00.18</sub> |  77.22<sub>±05.41</sub> |
|              Ruby (--jit) |  2.757<sub>±0.092</sub> |  271.46<sub>±00.02</sub> + 66.51<sub>±00.05</sub> |  64.87<sub>±07.20</sub> |
|                   Crystal |  2.828<sub>±0.053</sub> |     3.93<sub>±00.02</sub> + 1.18<sub>±00.02</sub> |  95.24<sub>±03.02</sub> |
|   C++/clang++ (libcrypto) |  2.930<sub>±0.050</sub> |     5.06<sub>±00.03</sub> + 0.08<sub>±00.00</sub> |  75.05<sub>±04.36</sub> |
|       C++/g++ (libcrypto) |  2.991<sub>±0.125</sub> |     5.73<sub>±00.06</sub> + 0.08<sub>±00.00</sub> |  74.53<sub>±09.28</sub> |
|                      Ruby |  3.066<sub>±0.087</sub> |   14.92<sub>±00.04</sub> + 58.41<sub>±00.13</sub> |  76.22<sub>±04.28</sub> |
|                        Go |  3.161<sub>±0.048</sub> |     4.33<sub>±00.15</sub> + 4.31<sub>±00.21</sub> |  92.55<sub>±01.63</sub> |
|       Perl (MIME::Base64) |  3.251<sub>±0.069</sub> |    14.82<sub>±00.09</sub> + 0.01<sub>±00.00</sub> |  91.09<sub>±04.73</sub> |
|                   Node.js |  3.580<sub>±0.081</sub> |   37.05<sub>±00.02</sub> + 35.92<sub>±00.03</sub> | 104.98<sub>±03.17</sub> |
|                       PHP |  3.864<sub>±0.174</sub> |    16.79<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  97.47<sub>±05.63</sub> |
|                     D/gdc |  3.986<sub>±0.091</sub> |     7.50<sub>±00.02</sub> + 3.63<sub>±00.00</sub> | 102.86<sub>±05.00</sub> |
|                     D/dmd |  4.555<sub>±0.167</sub> |     3.92<sub>±00.03</sub> + 3.63<sub>±00.00</sub> | 114.69<sub>±05.94</sub> |
|                       Zig |  4.644<sub>±0.156</sub> |     1.46<sub>±00.02</sub> + 0.31<sub>±00.00</sub> | 121.76<sub>±05.53</sub> |
|                    Python |  4.913<sub>±0.161</sub> |    10.02<sub>±00.03</sub> + 0.18<sub>±00.00</sub> | 119.05<sub>±06.92</sub> |
|                  Go/gccgo |  5.103<sub>±0.029</sub> |   25.71<sub>±00.04</sub> + 14.68<sub>±02.94</sub> | 155.11<sub>±02.76</sub> |
|                     Julia |  5.914<sub>±0.122</sub> |  226.20<sub>±00.59</sub> + 48.68<sub>±06.30</sub> | 154.51<sub>±08.92</sub> |
|               Python/pypy |  6.148<sub>±0.224</sub> |   67.18<sub>±00.11</sub> + 45.68<sub>±00.02</sub> | 153.10<sub>±08.04</sub> |
|              C#/.NET Core |  6.901<sub>±0.085</sub> |   34.69<sub>±00.02</sub> + 47.81<sub>±05.29</sub> | 199.81<sub>±03.73</sub> |
|              F#/.NET Core |  7.008<sub>±0.060</sub> |   39.00<sub>±00.07</sub> + 48.22<sub>±03.29</sub> | 194.86<sub>±03.38</sub> |
|                       Tcl |  7.190<sub>±0.243</sub> |     5.02<sub>±00.02</sub> + 0.18<sub>±00.00</sub> | 177.81<sub>±20.24</sub> |
|  Ruby/truffleruby (--jvm) |  7.337<sub>±0.296</sub> | 341.19<sub>±08.71</sub> + 176.25<sub>±38.94</sub> | 202.83<sub>±11.66</sub> |
|                Ruby/jruby |  9.093<sub>±0.415</sub> |  187.15<sub>±01.64</sub> + 93.80<sub>±10.12</sub> | 237.37<sub>±06.68</sub> |
|                   C#/Mono |  9.137<sub>±0.086</sub> |   21.03<sub>±00.02</sub> + 18.43<sub>±00.03</sub> | 234.56<sub>±06.33</sub> |
| Perl (MIME::Base64::Perl) | 17.220<sub>±0.490</sub> |    16.12<sub>±00.04</sub> + 0.27<sub>±00.01</sub> | 455.93<sub>±18.96</sub> |
|          Ruby/truffleruby | 23.410<sub>±1.302</sub> | 276.76<sub>±01.79</sub> + 280.98<sub>±18.35</sub> | 588.06<sub>±36.14</sub> |

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
|        C++/g++ (simdjson On-Demand) |  0.107<sub>±0.003</sub> |    113.41<sub>±00.03</sub> + 59.81<sub>±00.00</sub> |   3.23<sub>±00.40</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.114<sub>±0.009</sub> |     113.19<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.02<sub>±00.44</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.119<sub>±0.018</sub> |     112.49<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   2.45<sub>±00.51</sub> |
|    C++/clang++ (simdjson On-Demand) |  0.120<sub>±0.009</sub> |    112.98<sub>±00.06</sub> + 59.81<sub>±00.00</sub> |   2.88<sub>±00.68</sub> |
|             C++/g++ (DAW JSON Link) |  0.136<sub>±0.004</sub> |     113.15<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.75<sub>±00.72</sub> |
|         C++/clang++ (DAW JSON Link) |  0.155<sub>±0.032</sub> |     112.50<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.74<sub>±00.51</sub> |
|              C++/g++ (simdjson DOM) |  0.172<sub>±0.003</sub> |   113.36<sub>±00.07</sub> + 176.60<sub>±00.00</sub> |   4.89<sub>±01.06</sub> |
|                 Rust (Serde Custom) |  0.173<sub>±0.013</sub> |     111.84<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.93<sub>±00.57</sub> |
|          C++/clang++ (simdjson DOM) |  0.173<sub>±0.014</sub> |   112.74<sub>±00.28</sub> + 176.85<sub>±00.25</sub> |   5.09<sub>±00.49</sub> |
|                  Rust (Serde Typed) |  0.184<sub>±0.006</sub> |    111.81<sub>±00.06</sub> + 11.80<sub>±00.16</sub> |   3.62<sub>±00.42</sub> |
|                     C++/g++ (gason) |  0.184<sub>±0.008</sub> |    113.10<sub>±00.04</sub> + 96.83<sub>±00.04</sub> |   5.49<sub>±00.68</sub> |
|                 C++/clang++ (gason) |  0.224<sub>±0.009</sub> |    112.46<sub>±00.06</sub> + 96.97<sub>±00.00</sub> |   5.91<sub>±00.94</sub> |
|                 C++/g++ (RapidJSON) |  0.237<sub>±0.012</sub> |   113.19<sub>±00.04</sub> + 128.87<sub>±00.04</sub> |   6.61<sub>±00.50</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.295<sub>±0.009</sub> |    112.76<sub>±00.06</sub> + 61.36<sub>±00.00</sub> |   7.98<sub>±01.05</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.300<sub>±0.016</sub> |    112.73<sub>±00.02</sub> + 16.26<sub>±00.00</sub> |   7.98<sub>±00.70</sub> |
|             C++/clang++ (RapidJSON) |  0.319<sub>±0.008</sub> |   112.46<sub>±00.03</sub> + 129.02<sub>±00.03</sub> |   9.02<sub>±01.40</sub> |
|         C++/g++ (RapidJSON Precise) |  0.345<sub>±0.019</sub> |   113.15<sub>±00.05</sub> + 128.87<sub>±00.04</sub> |  10.06<sub>±01.00</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.486<sub>±0.024</sub> |   112.46<sub>±00.01</sub> + 129.05<sub>±00.03</sub> |  12.16<sub>±01.51</sub> |
|             C++/g++ (RapidJSON SAX) |  0.594<sub>±0.023</sub> |     112.92<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  14.72<sub>±00.82</sub> |
|                C++/g++ (Boost.JSON) |  0.620<sub>±0.019</sub> |   113.29<sub>±00.04</sub> + 435.82<sub>±00.06</sub> |  19.52<sub>±02.19</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.641<sub>±0.007</sub> |     194.70<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  18.25<sub>±00.27</sub> |
|                   Nim/clang (jsony) |  0.650<sub>±0.037</sub> |    112.10<sub>±00.04</sub> + 42.34<sub>±00.06</sub> |  15.65<sub>±01.40</sub> |
|            C++/clang++ (Boost.JSON) |  0.658<sub>±0.031</sub> |   112.46<sub>±00.02</sub> + 436.31<sub>±00.06</sub> |  17.97<sub>±01.39</sub> |
|                     Nim/gcc (jsony) |  0.660<sub>±0.038</sub> |    111.67<sub>±00.04</sub> + 42.28<sub>±00.00</sub> |  16.16<sub>±01.51</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.694<sub>±0.025</sub> |     112.97<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  16.53<sub>±02.22</sub> |
|                             Node.js |  0.757<sub>±0.029</sub> |   146.31<sub>±00.02</sub> + 187.39<sub>±00.22</sub> |  23.18<sub>±01.75</sub> |
|                       Go (jsoniter) |  0.789<sub>±0.019</sub> |     231.05<sub>±00.09</sub> + 1.13<sub>±00.13</sub> |  20.67<sub>±01.75</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.840<sub>±0.069</sub> |     194.73<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  19.21<sub>±02.43</sub> |
|                     Java (DSL-JSON) |  0.840<sub>±0.018</sub> |   259.38<sub>±00.52</sub> + 184.87<sub>±03.04</sub> |  26.22<sub>±03.35</sub> |
|                Rust (Serde Untyped) |  0.962<sub>±0.048</sub> |   111.82<sub>±00.05</sub> + 840.02<sub>±00.02</sub> |  25.09<sub>±03.50</sub> |
|                         Python/pypy |  0.969<sub>±0.042</sub> |   286.74<sub>±00.27</sub> + 121.37<sub>±00.01</sub> |  27.58<sub>±02.66</sub> |
|                               V/gcc |  0.995<sub>±0.079</sub> |   111.19<sub>±00.01</sub> + 496.09<sub>±00.00</sub> |  26.18<sub>±03.30</sub> |
|                             V/clang |  1.062<sub>±0.076</sub> |   111.18<sub>±00.01</sub> + 496.18<sub>±00.03</sub> |  28.19<sub>±03.03</sub> |
|     C#/.NET Core (System.Text.Json) |  1.100<sub>±0.047</sub> |   478.79<sub>±00.09</sub> + 138.79<sub>±00.00</sub> |  30.61<sub>±01.85</sub> |
|                       Julia (JSON3) |  1.105<sub>±0.038</sub> |   443.31<sub>±00.57</sub> + 263.37<sub>±01.80</sub> |  30.82<sub>±02.48</sub> |
|              Nim/clang (Packedjson) |  1.221<sub>±0.049</sub> |   112.44<sub>±00.02</sub> + 294.16<sub>±00.00</sub> |  31.59<sub>±01.79</sub> |
|                                 Zig |  1.231<sub>±0.037</sub> |    110.78<sub>±00.01</sub> + 12.18<sub>±00.00</sub> |  33.01<sub>±02.73</sub> |
|                      Crystal (Pull) |  1.246<sub>±0.021</sub> |    113.87<sub>±00.03</sub> + 18.18<sub>±00.01</sub> |  30.60<sub>±03.69</sub> |
|                                  Go |  1.277<sub>±0.034</sub> |    117.08<sub>±00.05</sub> + 79.95<sub>±00.04</sub> |  36.54<sub>±02.43</sub> |
|             Perl (Cpanel::JSON::XS) |  1.277<sub>±0.111</sub> |   125.33<sub>±00.04</sub> + 402.83<sub>±00.00</sub> |  32.31<sub>±05.03</sub> |
|                    Crystal (Schema) |  1.290<sub>±0.059</sub> |    113.88<sub>±00.01</sub> + 48.63<sub>±00.03</sub> |  30.12<sub>±02.78</sub> |
|                Nim/gcc (Packedjson) |  1.420<sub>±0.091</sub> |   111.97<sub>±00.03</sub> + 294.16<sub>±00.00</sub> |  32.64<sub>±03.89</sub> |
|                                 PHP |  1.457<sub>±0.033</sub> |   126.10<sub>±00.03</sub> + 682.09<sub>±00.00</sub> |  37.28<sub>±02.23</sub> |
|                             Crystal |  1.512<sub>±0.024</sub> |   113.88<sub>±00.02</sub> + 392.02<sub>±00.01</sub> |  40.55<sub>±01.30</sub> |
|                           Nim/clang |  1.744<sub>±0.110</sub> |   112.42<sub>±00.01</sub> + 925.06<sub>±00.03</sub> |  44.84<sub>±11.78</sub> |
|                             Clojure |  1.750<sub>±0.074</sub> |   440.18<sub>±07.17</sub> + 477.47<sub>±08.46</sub> |  52.64<sub>±02.25</sub> |
|                C++/clang++ (json-c) |  1.877<sub>±0.064</sub> |  112.69<sub>±00.07</sub> + 1216.12<sub>±00.02</sub> |  48.37<sub>±05.31</sub> |
|                    C++/g++ (json-c) |  1.889<sub>±0.067</sub> |  113.28<sub>±00.01</sub> + 1215.96<sub>±00.00</sub> |  50.51<sub>±03.20</sub> |
|              C++/clang++ (Nlohmann) |  1.934<sub>±0.157</sub> |   112.59<sub>±00.02</sub> + 360.19<sub>±00.05</sub> |  52.37<sub>±07.02</sub> |
|                  C++/g++ (Nlohmann) |  1.967<sub>±0.075</sub> |   113.28<sub>±00.03</sub> + 447.94<sub>±00.03</sub> |  56.16<sub>±02.56</sub> |
|                        C#/.NET Core |  1.999<sub>±0.045</sub> |   486.82<sub>±00.07</sub> + 294.20<sub>±00.00</sub> |  52.88<sub>±04.45</sub> |
|                             Nim/gcc |  2.013<sub>±0.094</sub> |   111.96<sub>±00.03</sub> + 919.62<sub>±00.00</sub> |  50.43<sub>±02.79</sub> |
|                            Go/gccgo |  2.031<sub>±0.130</sub> |    139.20<sub>±00.09</sub> + 83.48<sub>±00.12</sub> |  56.16<sub>±01.78</sub> |
|                              Python |  2.072<sub>±0.072</sub> |   119.90<sub>±00.02</sub> + 377.32<sub>±00.00</sub> |  59.05<sub>±04.17</sub> |
|                 CPython (UltraJSON) |  2.079<sub>±0.036</sub> |   121.63<sub>±00.03</sub> + 549.01<sub>±01.16</sub> |  58.88<sub>±03.31</sub> |
|                                Ruby |  2.281<sub>±0.065</sub> |   124.58<sub>±00.03</sub> + 262.83<sub>±00.00</sub> |  69.45<sub>±01.83</sub> |
|                        Ruby (--jit) |  2.319<sub>±0.086</sub> |   381.16<sub>±00.03</sub> + 262.86<sub>±00.01</sub> |  62.90<sub>±04.72</sub> |
|                             C#/Mono |  2.699<sub>±0.091</sub> |     476.27<sub>±00.10</sub> + 0.19<sub>±00.02</sub> |  71.62<sub>±07.17</sub> |
|     F#/.NET Core (System.Text.Json) |  2.709<sub>±0.146</sub> |   486.73<sub>±00.14</sub> + 456.74<sub>±01.62</sub> |  73.24<sub>±05.16</sub> |
|                              D/ldc2 |  2.905<sub>±0.155</sub> |   113.05<sub>±00.05</sub> + 680.08<sub>±00.03</sub> |  82.67<sub>±03.83</sub> |
|                     Scala (uPickle) |  2.910<sub>±0.114</sub> |   299.48<sub>±00.17</sub> + 589.33<sub>±14.73</sub> |  81.15<sub>±03.81</sub> |
|                         Ruby (YAJL) |  3.135<sub>±0.119</sub> |   124.38<sub>±00.03</sub> + 282.45<sub>±00.00</sub> |  79.00<sub>±06.88</sub> |
|                             Haskell |  4.215<sub>±0.136</sub> |   115.59<sub>±00.18</sub> + 715.40<sub>±00.22</sub> | 115.04<sub>±06.11</sub> |
|    C++/clang++ (Boost.PropertyTree) |  4.454<sub>±0.049</sub> |  194.88<sub>±00.04</sub> + 1232.84<sub>±00.06</sub> | 122.71<sub>±06.29</sub> |
|                           Rust (jq) |  4.506<sub>±0.213</sub> |   113.75<sub>±00.01</sub> + 779.15<sub>±00.77</sub> | 118.68<sub>±12.42</sub> |
|                          Ruby/jruby |  4.517<sub>±0.295</sub> |   446.44<sub>±02.39</sub> + 872.79<sub>±25.79</sub> | 132.47<sub>±07.12</sub> |
|        C++/g++ (Boost.PropertyTree) |  4.727<sub>±0.090</sub> |  113.11<sub>±00.05</sub> + 1440.12<sub>±00.02</sub> | 126.68<sub>±04.47</sub> |
|                               D/dmd |  5.702<sub>±0.250</sub> |   113.61<sub>±00.04</sub> + 680.12<sub>±00.04</sub> | 151.01<sub>±11.46</sub> |
|                               D/gdc |  5.765<sub>±0.187</sub> |   117.19<sub>±00.02</sub> + 680.78<sub>±00.08</sub> | 171.44<sub>±07.90</sub> |
|                          Vala/clang |  5.930<sub>±0.108</sub> |   114.77<sub>±00.01</sub> + 876.66<sub>±00.09</sub> | 156.93<sub>±08.01</sub> |
|                            Vala/gcc |  6.389<sub>±0.385</sub> |   114.69<sub>±00.05</sub> + 940.91<sub>±00.16</sub> | 175.00<sub>±15.75</sub> |
|                   Perl (JSON::Tiny) | 15.127<sub>±1.107</sub> |   125.89<sub>±00.12</sub> + 528.64<sub>±00.12</sub> | 370.65<sub>±30.01</sub> |
|            Ruby/truffleruby (--jvm) | 29.035<sub>±4.217</sub> | 506.97<sub>±24.95</sub> + 1717.25<sub>±124.69</sub> | 677.70<sub>±95.05</sub> |
|                    Ruby/truffleruby | 44.241<sub>±4.551</sub> |  427.86<sub>±04.62</sub> + 2251.62<sub>±33.95</sub> | 967.29<sub>±71.44</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                         Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | --------------------------------------------------: | -------------------------: |
|              Java (ND4J) |    0.117<sub>±0.009</sub> |    116.55<sub>±02.89</sub> + 90.78<sub>±00.01</sub> |      4.77<sub>±00.39</sub> |
|           Rust (ndarray) |    0.150<sub>±0.015</sub> |      3.09<sub>±00.06</sub> + 67.97<sub>±00.00</sub> |      4.65<sub>±00.64</sub> |
|       Julia (threads: 8) |    0.155<sub>±0.008</sub> |    249.12<sub>±00.17</sub> + 43.29<sub>±00.02</sub> |      5.97<sub>±00.70</sub> |
|  Nim/clang (Arraymancer) |    0.191<sub>±0.031</sub> |      6.64<sub>±00.14</sub> + 55.78<sub>±00.44</sub> |      6.98<sub>±02.13</sub> |
|       Julia (threads: 1) |    0.242<sub>±0.012</sub> |    249.45<sub>±00.19</sub> + 41.92<sub>±00.05</sub> |      5.98<sub>±00.61</sub> |
|    Nim/gcc (Arraymancer) |    0.278<sub>±0.035</sub> |      5.69<sub>±00.05</sub> + 56.29<sub>±00.23</sub> |     10.05<sub>±00.82</sub> |
|          D/ldc2 (lubeck) |    0.286<sub>±0.053</sub> |      6.99<sub>±00.06</sub> + 56.30<sub>±00.15</sub> |     10.13<sub>±01.37</sub> |
|          C++/g++ (Eigen) |    0.332<sub>±0.028</sub> |      4.24<sub>±00.27</sub> + 85.72<sub>±00.27</sub> |     12.72<sub>±00.24</sub> |
|           Python (NumPy) |    0.334<sub>±0.023</sub> |     28.15<sub>±00.04</sub> + 57.77<sub>±00.02</sub> |     13.15<sub>±01.27</sub> |
|      C++/clang++ (Eigen) |    0.363<sub>±0.033</sub> |      4.49<sub>±00.08</sub> + 86.00<sub>±00.03</sub> |     13.48<sub>±03.00</sub> |
|          Julia (no BLAS) |    1.549<sub>±0.016</sub> |    224.54<sub>±00.25</sub> + 51.44<sub>±00.03</sub> |     45.45<sub>±05.36</sub> |
|                   D/ldc2 |    2.493<sub>±0.153</sub> |      3.72<sub>±00.03</sub> + 70.13<sub>±00.00</sub> |     71.22<sub>±12.50</sub> |
|                    D/dmd |    2.710<sub>±0.097</sub> |      3.69<sub>±00.14</sub> + 70.28<sub>±00.12</sub> |     72.59<sub>±04.13</sub> |
|                    D/gdc |    2.722<sub>±0.115</sub> |      7.71<sub>±00.13</sub> + 70.24<sub>±00.13</sub> |     67.76<sub>±12.08</sub> |
|               Vala/clang |    4.134<sub>±0.232</sub> |      5.78<sub>±00.14</sub> + 68.32<sub>±00.00</sub> |    110.51<sub>±16.45</sub> |
|                    C/gcc |    4.145<sub>±0.229</sub> |      2.06<sub>±00.05</sub> + 68.06<sub>±00.00</sub> |    117.97<sub>±11.91</sub> |
|                     Java |    4.150<sub>±0.067</sub> |     37.95<sub>±00.25</sub> + 81.48<sub>±00.24</sub> |    125.62<sub>±08.05</sub> |
|                  Nim/gcc |    4.158<sub>±0.076</sub> |      2.65<sub>±00.04</sub> + 80.69<sub>±03.61</sub> |    116.87<sub>±00.76</sub> |
|                      Zig |    4.185<sub>±0.209</sub> |      1.73<sub>±00.03</sub> + 68.58<sub>±00.00</sub> |    115.16<sub>±18.06</sub> |
|                  C/clang |    4.188<sub>±0.168</sub> |      2.07<sub>±00.03</sub> + 68.06<sub>±00.13</sub> |    109.95<sub>±06.75</sub> |
|                       Go |    4.204<sub>±0.074</sub> |      3.57<sub>±00.08</sub> + 73.06<sub>±00.09</sub> |    131.02<sub>±07.18</sub> |
|                Nim/clang |    4.245<sub>±0.133</sub> |      3.11<sub>±00.02</sub> + 77.47<sub>±03.22</sub> |    118.62<sub>±08.83</sub> |
|                 Go/gccgo |    4.252<sub>±0.066</sub> |     24.99<sub>±00.16</sub> + 73.46<sub>±00.11</sub> |    130.01<sub>±08.05</sub> |
|                    Swift |    4.371<sub>±0.245</sub> |      6.32<sub>±00.02</sub> + 68.92<sub>±00.01</sub> |    110.87<sub>±17.14</sub> |
|                     Rust |    4.388<sub>±0.345</sub> |      2.69<sub>±00.08</sub> + 68.32<sub>±00.00</sub> |    113.52<sub>±18.78</sub> |
|                 Vala/gcc |    4.466<sub>±0.155</sub> |      5.65<sub>±00.07</sub> + 68.32<sub>±00.00</sub> |    123.33<sub>±05.18</sub> |
|                  V/clang |    4.510<sub>±0.097</sub> |      2.30<sub>±00.07</sub> + 68.84<sub>±00.00</sub> |    119.77<sub>±05.32</sub> |
|                    V/gcc |    4.512<sub>±0.094</sub> |      1.94<sub>±00.02</sub> + 68.84<sub>±00.00</sub> |    117.35<sub>±09.06</sub> |
|                  Crystal |    4.564<sub>±0.215</sub> |      4.26<sub>±00.02</sub> + 59.67<sub>±00.01</sub> |    124.74<sub>±06.60</sub> |
|                    Scala |    4.626<sub>±0.319</sub> |    74.32<sub>±00.32</sub> + 146.71<sub>±03.45</sub> |    130.22<sub>±13.45</sub> |
|                  Node.js |    4.768<sub>±0.428</sub> |     41.72<sub>±00.08</sub> + 70.72<sub>±00.13</sub> |    125.94<sub>±21.48</sub> |
|              Python/pypy |    7.763<sub>±0.203</sub> |     67.08<sub>±00.14</sub> + 69.11<sub>±00.07</sub> |    194.22<sub>±08.08</sub> |
|               Kotlin/JVM |    7.816<sub>±1.705</sub> |     39.84<sub>±00.11</sub> + 80.27<sub>±00.33</sub> |    207.44<sub>±63.75</sub> |
|             C#/.NET Core |    8.697<sub>±0.219</sub> |     34.63<sub>±00.12</sub> + 69.39<sub>±00.05</sub> |    222.21<sub>±22.74</sub> |
|                  C#/Mono |   13.592<sub>±0.646</sub> |     20.42<sub>±00.04</sub> + 69.01<sub>±00.02</sub> |    367.41<sub>±31.99</sub> |
|         Ruby/truffleruby |   30.741<sub>±2.113</sub> | 329.32<sub>±17.21</sub> + 1016.19<sub>±371.12</sub> |   717.20<sub>±127.90</sub> |
| Ruby/truffleruby (--jvm) |   51.371<sub>±1.132</sub> |  400.60<sub>±12.24</sub> + 638.52<sub>±151.81</sub> |  1294.26<sub>±132.95</sub> |
|             Ruby (--jit) |  246.812<sub>±6.744</sub> |    272.22<sub>±00.01</sub> + 68.84<sub>±00.00</sub> |  6126.62<sub>±145.10</sub> |
|                     Ruby |  258.210<sub>±7.250</sub> |     15.64<sub>±00.04</sub> + 68.58<sub>±00.00</sub> |  6630.94<sub>±231.12</sub> |
|                   Python |  277.581<sub>±2.827</sub> |     10.33<sub>±00.03</sub> + 68.58<sub>±00.00</sub> |   6803.39<sub>±91.67</sub> |
|                      Tcl |  385.609<sub>±3.409</sub> |     7.37<sub>±00.02</sub> + 400.39<sub>±00.02</sub> |   9718.19<sub>±92.19</sub> |
|                     Perl |  472.250<sub>±5.819</sub> |     9.57<sub>±00.03</sub> + 599.61<sub>±00.05</sub> |  12162.42<sub>±90.60</sub> |
|               Ruby/jruby | 686.602<sub>±38.075</sub> |   264.80<sub>±08.62</sub> + 672.79<sub>±76.03</sub> | 16661.14<sub>±977.64</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                      Zig | 0.084<sub>±0.003</sub> |    1.40<sub>±00.03</sub> + 55.92<sub>±01.96</sub> |   2.33<sub>±00.49</sub> |
|                     Rust | 0.131<sub>±0.008</sub> |    2.38<sub>±00.05</sub> + 77.02<sub>±00.02</sub> |   3.65<sub>±00.78</sub> |
|                  C++/g++ | 0.179<sub>±0.006</sub> |    3.49<sub>±00.05</sub> + 84.79<sub>±00.14</sub> |   5.12<sub>±00.83</sub> |
|              C++/clang++ | 0.209<sub>±0.006</sub> |    3.12<sub>±00.03</sub> + 75.41<sub>±00.00</sub> |   5.31<sub>±01.22</sub> |
|                     Java | 0.209<sub>±0.020</sub> |  36.87<sub>±00.10</sub> + 103.22<sub>±02.12</sub> |   6.85<sub>±00.77</sub> |
|                  V/clang | 0.214<sub>±0.010</sub> |   1.63<sub>±00.08</sub> + 264.86<sub>±02.00</sub> |   6.56<sub>±00.49</sub> |
|                  Crystal | 0.215<sub>±0.022</sub> |    3.47<sub>±00.02</sub> + 88.60<sub>±00.34</sub> |   5.17<sub>±00.64</sub> |
|                    V/gcc | 0.230<sub>±0.010</sub> |   1.57<sub>±00.03</sub> + 271.94<sub>±03.48</sub> |   6.43<sub>±00.94</sub> |
|                  Node.js | 0.338<sub>±0.020</sub> |  35.80<sub>±00.06</sub> + 181.38<sub>±04.29</sub> |  10.28<sub>±02.11</sub> |
|               Lua/luajit | 0.524<sub>±0.013</sub> |   2.50<sub>±00.04</sub> + 156.16<sub>±00.74</sub> |  12.73<sub>±02.48</sub> |
|                    Scala | 0.542<sub>±0.043</sub> |  74.46<sub>±00.15</sub> + 231.71<sub>±02.82</sub> |  18.38<sub>±01.29</sub> |
|                    Julia | 0.852<sub>±0.053</sub> | 228.82<sub>±00.27</sub> + 329.10<sub>±00.85</sub> |  20.14<sub>±02.90</sub> |
|              Python/pypy | 1.018<sub>±0.034</sub> |  66.47<sub>±00.09</sub> + 248.32<sub>±00.06</sub> |  29.34<sub>±04.15</sub> |
|                      Lua | 2.136<sub>±0.114</sub> |   2.21<sub>±00.02</sub> + 284.55<sub>±00.36</sub> |  49.83<sub>±09.35</sub> |
|             Ruby (--jit) | 2.196<sub>±0.067</sub> | 271.10<sub>±00.01</sub> + 146.92<sub>±00.00</sub> |  56.95<sub>±06.47</sub> |
|         Ruby/truffleruby | 2.587<sub>±0.158</sub> | 278.28<sub>±02.19</sub> + 387.69<sub>±16.37</sub> |  71.86<sub>±02.98</sub> |
| Ruby/truffleruby (--jvm) | 2.810<sub>±0.286</sub> | 344.56<sub>±03.96</sub> + 520.57<sub>±39.74</sub> |  89.94<sub>±05.85</sub> |
|                     Ruby | 2.889<sub>±0.080</sub> |  14.53<sub>±00.03</sub> + 146.75<sub>±00.00</sub> |  71.19<sub>±03.90</sub> |
|               Ruby/jruby | 3.219<sub>±0.270</sub> | 189.27<sub>±01.76</sub> + 394.60<sub>±12.90</sub> |  86.92<sub>±07.55</sub> |
|                   Python | 5.620<sub>±0.174</sub> |  10.16<sub>±00.14</sub> + 236.34<sub>±00.26</sub> | 150.33<sub>±10.30</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 6.0.201                         |
| C#/.NET Core     | 4.1.0-5.22116.13 (dbffaa4a)     |
| C#/Mono          | 6.12.0.122                      |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.11.0"                        |
| Crystal          | 1.3.2                           |
| D/dmd            | v2.099.0                        |
| D/gdc            | 12.0.1                          |
| D/ldc2           | 1.28.1                          |
| Elixir           | 1.12.2                          |
| F#/.NET Core     | 12.0.1.0 for F# 6.0             |
| Go               | go1.18                          |
| Go/gccgo         | 12.0.1                          |
| Haskell          | 9.0.1                           |
| Java             | 18                              |
| Julia            | v"1.7.2"                        |
| Kotlin           | 1.6.20                          |
| Lua              | 5.4.4                           |
| Lua/luajit       | 2.1.0-beta3                     |
| MLton            | 20210117                        |
| Nim              | 1.6.4                           |
| Node.js          | v17.8.0                         |
| OCaml            | 4.13.1                          |
| PHP              | 8.1.2                           |
| Perl             | v5.34.0                         |
| Python           | 3.9.12                          |
| Python/pypy      | 7.3.9-final0 for Python 3.9.12  |
| Racket           | "8.4"                           |
| Ruby             | 3.1.1p18                        |
| Ruby/jruby       | 9.3.4.0                         |
| Ruby/truffleruby | 22.0.0.2                        |
| Rust             | 1.59.0                          |
| Scala            | 3.1.1                           |
| Swift            | 5.6                             |
| Tcl              | 8.6                             |
| V                | 0.2.4 509367b                   |
| Vala             | 0.56.0                          |
| Zig              | 0.9.1                           |
| clang/clang++    | 13.0.1                          |
| gcc/g++          | 12.0.1                          |

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
 - `make shell` (run the image with the `shell' command).

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

## README update

TOC is regenerated using [git-markdown-toc](https://github.com/ildar-shaimordanov/git-markdown-toc):

```
make toc
```

## Docker image update

Debian packages are pinned and updated with the script
(first, please ensure that the image is fine with the linter):

```
make lint
make update_apt
```
