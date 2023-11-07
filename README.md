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

UPDATE: 2023-10-16

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|               Language |                  Time, s |                                       Memory, MiB |                  Energy, J |
| :--------------------- | -----------------------: | ------------------------------------------------: | -------------------------: |
|         Scala (Staged) |   0.500<sub>±0.011</sub> |  214.99<sub>±04.56</sub> + 21.33<sub>±01.86</sub> |     28.38<sub>±02.06</sub> |
|        Racket (Staged) |   1.300<sub>±0.000</sub> |   100.79<sub>±00.40</sub> + 0.00<sub>±00.00</sub> |     47.18<sub>±00.05</sub> |
|                C++/g++ |   1.323<sub>±0.000</sub> |     1.84<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     52.88<sub>±00.06</sub> |
|                   Java |   1.582<sub>±0.000</sub> |    39.81<sub>±00.08</sub> + 1.19<sub>±00.04</sub> |     61.57<sub>±00.09</sub> |
|                Nim/gcc |   1.612<sub>±0.000</sub> |     0.92<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     64.40<sub>±00.05</sub> |
|            C++/clang++ |   1.642<sub>±0.000</sub> |     1.62<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     63.90<sub>±00.13</sub> |
|                  V/gcc |   1.649<sub>±0.002</sub> |     1.84<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     63.31<sub>±00.62</sub> |
|                   Rust |   1.658<sub>±0.000</sub> |     0.92<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     64.18<sub>±00.04</sub> |
|                  D/gdc |   1.658<sub>±0.000</sub> |     6.34<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     68.09<sub>±00.23</sub> |
|                 D/ldc2 |   1.664<sub>±0.000</sub> |     1.39<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     66.39<sub>±00.05</sub> |
|                  C/gcc |   1.667<sub>±0.000</sub> |     0.85<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     64.65<sub>±00.11</sub> |
|                C/clang |   1.674<sub>±0.000</sub> |     0.89<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     64.93<sub>±00.16</sub> |
|             Kotlin/JVM |   1.689<sub>±0.000</sub> |    43.17<sub>±00.06</sub> + 0.79<sub>±00.27</sub> |     65.90<sub>±00.17</sub> |
|                    Zig |   1.790<sub>±0.001</sub> |     0.92<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     70.28<sub>±00.46</sub> |
|                     Go |   1.900<sub>±0.000</sub> |     3.08<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     73.52<sub>±00.12</sub> |
|            Chez Scheme |   1.932<sub>±0.001</sub> |    24.74<sub>±00.03</sub> + 4.47<sub>±00.11</sub> |     79.80<sub>±00.06</sub> |
|           F#/.NET Core |   1.959<sub>±0.003</sub> |    36.38<sub>±00.11</sub> + 0.58<sub>±00.00</sub> |     80.39<sub>±00.23</sub> |
|           C#/.NET Core |   1.979<sub>±0.000</sub> |    31.68<sub>±00.15</sub> + 0.20<sub>±00.00</sub> |     81.21<sub>±00.02</sub> |
|                  OCaml |   1.997<sub>±0.001</sub> |     3.24<sub>±00.04</sub> + 2.40<sub>±00.03</sub> |     91.15<sub>±00.11</sub> |
|                 Racket |   2.027<sub>±0.022</sub> |   93.25<sub>±00.28</sub> + 20.64<sub>±00.63</sub> |     81.62<sub>±00.91</sub> |
|              Nim/clang |   2.050<sub>±0.001</sub> |     1.14<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     80.91<sub>±00.74</sub> |
|               Vala/gcc |   2.083<sub>±0.000</sub> |     4.56<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     78.05<sub>±00.05</sub> |
|             Vala/clang |   2.259<sub>±0.000</sub> |     4.58<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     84.70<sub>±00.11</sub> |
|               Go/gccgo |   2.331<sub>±0.000</sub> |    23.56<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     94.42<sub>±00.99</sub> |
|                Crystal |   2.345<sub>±0.001</sub> |     2.95<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     93.56<sub>±00.08</sub> |
|                V/clang |   2.511<sub>±0.038</sub> |     1.84<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    106.19<sub>±00.94</sub> |
|                  MLton |   2.608<sub>±0.000</sub> |     1.61<sub>±00.05</sub> + 0.25<sub>±00.00</sub> |    104.24<sub>±01.38</sub> |
|                C#/Mono |   2.685<sub>±0.000</sub> |    25.41<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    108.49<sub>±01.15</sub> |
|                  Julia |   2.858<sub>±0.002</sub> |   249.20<sub>±00.09</sub> + 0.35<sub>±00.03</sub> |    110.50<sub>±00.26</sub> |
|                  D/dmd |   3.128<sub>±0.001</sub> |     3.40<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    117.91<sub>±00.51</sub> |
|                  Scala |   3.277<sub>±0.004</sub> |  72.46<sub>±00.13</sub> + 186.43<sub>±00.26</sub> |    138.10<sub>±00.61</sub> |
|       Haskell (MArray) |   3.861<sub>±0.001</sub> |     5.81<sub>±00.03</sub> + 4.84<sub>±00.00</sub> |    157.16<sub>±01.03</sub> |
|                Node.js |   4.160<sub>±0.004</sub> |    38.00<sub>±00.02</sub> + 3.94<sub>±00.03</sub> |    166.25<sub>±01.10</sub> |
|           Haskell (FP) |   4.183<sub>±0.002</sub> |     5.99<sub>±00.02</sub> + 4.99<sub>±00.03</sub> |    177.11<sub>±00.22</sub> |
|                  Swift |   5.479<sub>±0.000</sub> |    16.14<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |    205.37<sub>±00.21</sub> |
|       Ruby/truffleruby |   5.733<sub>±0.233</sub> | 224.91<sub>±04.25</sub> + 647.13<sub>±70.73</sub> |    278.60<sub>±11.01</sub> |
|             Lua/luajit |   6.489<sub>±0.005</sub> |     2.44<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    259.08<sub>±00.13</sub> |
| Ruby/truffleruby (JVM) |   6.637<sub>±0.177</sub> | 398.30<sub>±18.36</sub> + 602.85<sub>±86.95</sub> |    317.74<sub>±07.84</sub> |
|            Python/pypy |  12.214<sub>±0.008</sub> |   59.50<sub>±00.13</sub> + 29.66<sub>±00.18</sub> |    510.88<sub>±01.26</sub> |
|                  Idris |  15.676<sub>±0.024</sub> |    20.65<sub>±00.06</sub> + 8.82<sub>±00.05</sub> |    686.28<sub>±01.00</sub> |
|                 Elixir |  23.946<sub>±0.021</sub> |    70.45<sub>±00.70</sub> + 0.00<sub>±00.00</sub> |    942.72<sub>±02.80</sub> |
|                    Lua |  51.056<sub>±0.422</sub> |     2.23<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   1973.41<sub>±15.02</sub> |
|           Ruby (--jit) |  54.186<sub>±0.425</sub> |    16.16<sub>±00.02</sub> + 1.79<sub>±00.01</sub> |   2302.73<sub>±18.18</sub> |
|                    PHP |  55.973<sub>±0.024</sub> |    17.81<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   2181.36<sub>±02.17</sub> |
|                   Ruby |  78.429<sub>±0.569</sub> |    14.95<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   3302.47<sub>±21.59</sub> |
|             Ruby/jruby |  85.643<sub>±1.154</sub> | 197.30<sub>±04.27</sub> + 212.10<sub>±11.71</sub> |   3804.72<sub>±62.43</sub> |
|                 Python | 119.026<sub>±0.454</sub> |    10.14<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   4666.45<sub>±30.38</sub> |
|               Tcl (FP) | 270.164<sub>±2.350</sub> |     3.93<sub>±00.04</sub> + 0.00<sub>±00.00</sub> | 11304.53<sub>±116.31</sub> |
|                   Perl | 316.357<sub>±2.409</sub> |     7.05<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  12793.56<sub>±88.59</sub> |
|              Tcl (OOP) | 530.477<sub>±4.818</sub> |     3.93<sub>±00.03</sub> + 0.00<sub>±00.00</sub> | 22275.06<sub>±348.47</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|               Language |                 Time, s |                                       Memory, MiB |                Energy, J |
| :--------------------- | ----------------------: | ------------------------------------------------: | -----------------------: |
|                C++/g++ | 10.158<sub>±0.026</sub> |     1.80<sub>±00.02</sub> + 2.29<sub>±00.04</sub> |  414.88<sub>±04.38</sub> |
|                   Java | 14.106<sub>±0.016</sub> |    39.81<sub>±00.09</sub> + 2.27<sub>±00.07</sub> |  561.52<sub>±04.28</sub> |
|                  C/gcc | 14.185<sub>±0.004</sub> |     0.86<sub>±00.01</sub> + 0.83<sub>±00.05</sub> |  563.74<sub>±02.93</sub> |
|        Racket (Staged) | 14.379<sub>±0.428</sub> |  100.46<sub>±00.16</sub> + 76.88<sub>±02.04</sub> |  572.08<sub>±14.26</sub> |
|             Kotlin/JVM | 14.396<sub>±0.014</sub> |    42.93<sub>±00.21</sub> + 2.18<sub>±00.22</sub> |  592.87<sub>±01.55</sub> |
|                   Rust | 15.057<sub>±0.014</sub> |     0.90<sub>±00.01</sub> + 1.10<sub>±00.02</sub> |  594.38<sub>±02.79</sub> |
|         Scala (Staged) | 15.065<sub>±0.646</sub> | 216.61<sub>±02.00</sub> + 103.20<sub>±07.08</sub> |  742.04<sub>±18.57</sub> |
|                  D/gdc | 15.070<sub>±0.006</sub> |     6.27<sub>±00.02</sub> + 1.44<sub>±00.02</sub> |  636.33<sub>±01.52</sub> |
|                    Zig | 15.107<sub>±0.007</sub> |     0.90<sub>±00.02</sub> + 1.41<sub>±00.01</sub> |  622.38<sub>±01.04</sub> |
|                C/clang | 15.113<sub>±0.005</sub> |     0.86<sub>±00.01</sub> + 0.88<sub>±00.03</sub> |  648.78<sub>±04.04</sub> |
|            C++/clang++ | 15.135<sub>±0.011</sub> |     1.59<sub>±00.02</sub> + 1.96<sub>±00.01</sub> |  621.55<sub>±02.98</sub> |
|                 D/ldc2 | 15.319<sub>±0.005</sub> |     3.01<sub>±00.03</sub> + 0.79<sub>±00.02</sub> |  613.95<sub>±00.68</sub> |
|                     Go | 15.413<sub>±0.141</sub> |     3.04<sub>±00.07</sub> + 1.26<sub>±00.00</sub> |  616.56<sub>±06.53</sub> |
|           C#/.NET Core | 16.331<sub>±0.009</sub> |    31.53<sub>±00.09</sub> + 0.88<sub>±00.00</sub> |  691.29<sub>±02.05</sub> |
|                Crystal | 16.589<sub>±0.353</sub> |     2.87<sub>±00.04</sub> + 0.72<sub>±00.04</sub> |  689.62<sub>±13.84</sub> |
|               Vala/gcc | 18.058<sub>±0.007</sub> |     4.40<sub>±00.04</sub> + 1.19<sub>±00.03</sub> |  698.11<sub>±00.81</sub> |
|                  V/gcc | 18.122<sub>±0.068</sub> |     1.80<sub>±00.02</sub> + 1.16<sub>±00.01</sub> |  702.84<sub>±01.94</sub> |
|                  Swift | 18.141<sub>±0.044</sub> |    15.91<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  745.20<sub>±01.74</sub> |
|                Nim/gcc | 18.170<sub>±0.031</sub> |     2.00<sub>±00.01</sub> + 1.29<sub>±00.00</sub> |  768.06<sub>±03.91</sub> |
|             Vala/clang | 19.705<sub>±0.003</sub> |     4.39<sub>±00.04</sub> + 1.21<sub>±00.02</sub> |  763.62<sub>±00.28</sub> |
|              Nim/clang | 19.914<sub>±0.585</sub> |     2.26<sub>±00.04</sub> + 1.29<sub>±00.00</sub> |  819.31<sub>±20.88</sub> |
|               Go/gccgo | 20.479<sub>±0.021</sub> |    23.90<sub>±00.09</sub> + 1.28<sub>±00.01</sub> |  867.17<sub>±00.59</sub> |
|                  Scala | 20.991<sub>±0.024</sub> |  72.57<sub>±00.12</sub> + 137.46<sub>±00.42</sub> |  885.00<sub>±01.03</sub> |
|                V/clang | 22.603<sub>±0.245</sub> |     1.83<sub>±00.01</sub> + 1.14<sub>±00.02</sub> |  985.37<sub>±15.29</sub> |
|                  OCaml | 26.617<sub>±0.004</sub> |     3.97<sub>±00.01</sub> + 3.47<sub>±00.06</sub> | 1284.03<sub>±06.98</sub> |
|            Chez Scheme | 27.772<sub>±0.070</sub> |    25.50<sub>±00.05</sub> + 3.68<sub>±00.01</sub> | 1221.71<sub>±03.21</sub> |
|                Node.js | 28.483<sub>±0.392</sub> |    38.93<sub>±00.06</sub> + 6.70<sub>±00.12</sub> | 1209.63<sub>±15.82</sub> |
|                  Julia | 30.379<sub>±0.066</sub> |   250.43<sub>±00.03</sub> + 0.41<sub>±00.01</sub> | 1160.03<sub>±01.94</sub> |
|                C#/Mono | 31.446<sub>±0.014</sub> |    25.27<sub>±00.06</sub> + 0.82<sub>±00.00</sub> | 1337.71<sub>±01.79</sub> |
|           F#/.NET Core | 34.935<sub>±0.027</sub> |    36.02<sub>±00.12</sub> + 2.14<sub>±00.02</sub> | 1480.47<sub>±04.19</sub> |
|             Lua/luajit | 35.086<sub>±0.061</sub> |     2.42<sub>±00.02</sub> + 0.43<sub>±00.00</sub> | 1406.88<sub>±05.61</sub> |
|                 Racket | 37.036<sub>±0.513</sub> |   92.86<sub>±00.08</sub> + 22.57<sub>±00.15</sub> | 1640.19<sub>±13.64</sub> |
|       Haskell (MArray) | 37.162<sub>±0.014</sub> |     5.61<sub>±00.01</sub> + 5.82<sub>±00.00</sub> | 1523.81<sub>±05.62</sub> |
|                  D/dmd | 38.423<sub>±0.006</sub> |     3.25<sub>±00.04</sub> + 0.86<sub>±00.03</sub> | 1412.70<sub>±09.18</sub> |
|                  MLton | 44.897<sub>±0.052</sub> |     1.65<sub>±00.03</sub> + 4.11<sub>±00.00</sub> | 1924.64<sub>±09.32</sub> |
|            Python/pypy | 48.246<sub>±0.092</sub> |   59.57<sub>±00.07</sub> + 30.36<sub>±00.08</sub> | 2077.33<sub>±19.41</sub> |
|       Ruby/truffleruby | 48.288<sub>±0.912</sub> | 222.51<sub>±09.39</sub> + 557.98<sub>±63.76</sub> | 2344.82<sub>±60.98</sub> |
| Ruby/truffleruby (JVM) | 54.733<sub>±0.355</sub> | 388.41<sub>±10.42</sub> + 460.00<sub>±43.85</sub> | 2492.14<sub>±26.26</sub> |
|                  Idris | 67.241<sub>±0.176</sub> |    21.86<sub>±00.06</sub> + 9.54<sub>±00.00</sub> | 2893.54<sub>±10.95</sub> |
|           Haskell (FP) | 80.301<sub>±0.045</sub> |    5.75<sub>±00.03</sub> + 75.70<sub>±00.00</sub> | 3338.80<sub>±16.07</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|          C/clang (aklomp) |  0.096<sub>±0.000</sub> |     2.07<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.59<sub>±00.07</sub> |
|            C/gcc (aklomp) |  0.099<sub>±0.000</sub> |     2.08<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   4.69<sub>±00.05</sub> |
|                       PHP |  0.106<sub>±0.000</sub> |    18.51<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.98<sub>±00.02</sub> |
|                        Go |  0.306<sub>±0.006</sub> |     6.29<sub>±00.05</sub> + 4.62<sub>±00.35</sub> |  14.18<sub>±00.25</sub> |
|                      Rust |  0.956<sub>±0.000</sub> |     2.31<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  39.05<sub>±00.21</sub> |
|                   C/clang |  0.997<sub>±0.000</sub> |     1.98<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  36.78<sub>±00.05</sub> |
|                    D/ldc2 |  1.095<sub>±0.009</sub> |     3.58<sub>±00.04</sub> + 3.41<sub>±00.00</sub> |  46.71<sub>±00.59</sub> |
|                     C/gcc |  1.098<sub>±0.001</sub> |     2.00<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  40.28<sub>±00.05</sub> |
|                   Crystal |  1.102<sub>±0.001</sub> |     3.57<sub>±00.02</sub> + 1.27<sub>±00.03</sub> |  45.33<sub>±00.27</sub> |
|                 Nim/clang |  1.471<sub>±0.001</sub> |     1.92<sub>±00.03</sub> + 5.83<sub>±00.06</sub> |  58.38<sub>±00.37</sub> |
|                      Java |  1.508<sub>±0.002</sub> |  40.71<sub>±00.09</sub> + 209.37<sub>±12.91</sub> |  60.15<sub>±00.52</sub> |
|                   V/clang |  1.563<sub>±0.000</sub> |  2.36<sub>±00.04</sub> + 2388.26<sub>±00.77</sub> |  59.06<sub>±00.12</sub> |
|                   Nim/gcc |  1.575<sub>±0.001</sub> |     1.66<sub>±00.02</sub> + 5.25<sub>±00.03</sub> |  64.12<sub>±00.20</sub> |
|                     Scala |  1.578<sub>±0.001</sub> |  68.33<sub>±00.19</sub> + 318.32<sub>±05.49</sub> |  64.35<sub>±00.22</sub> |
|                     V/gcc |  1.584<sub>±0.001</sub> |  2.32<sub>±00.04</sub> + 2386.10<sub>±00.91</sub> |  58.12<sub>±00.34</sub> |
|                Kotlin/JVM |  1.635<sub>±0.009</sub> |  43.84<sub>±00.04</sub> + 250.75<sub>±01.85</sub> |  66.17<sub>±00.82</sub> |
|                  Vala/gcc |  1.644<sub>±0.002</sub> |     5.69<sub>±00.01</sub> + 0.01<sub>±00.00</sub> |  62.83<sub>±00.12</sub> |
|                Vala/clang |  1.645<sub>±0.001</sub> |     5.61<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  63.55<sub>±00.56</sub> |
|              Ruby (--jit) |  1.670<sub>±0.002</sub> |   16.79<sub>±00.02</sub> + 41.32<sub>±02.06</sub> |  65.08<sub>±00.30</sub> |
|                      Ruby |  1.678<sub>±0.001</sub> |   15.57<sub>±00.03</sub> + 40.68<sub>±00.33</sub> |  65.41<sub>±00.13</sub> |
|   C++/clang++ (libcrypto) |  1.717<sub>±0.002</sub> |     5.03<sub>±00.05</sub> + 0.68<sub>±00.04</sub> |  69.64<sub>±00.50</sub> |
|       C++/g++ (libcrypto) |  1.717<sub>±0.003</sub> |     5.54<sub>±00.08</sub> + 0.70<sub>±00.03</sub> |  69.18<sub>±00.27</sub> |
|                   Node.js |  1.729<sub>±0.005</sub> |   38.73<sub>±00.03</sub> + 37.05<sub>±00.10</sub> |  71.17<sub>±00.74</sub> |
|                        Go |  1.784<sub>±0.002</sub> |     4.17<sub>±00.04</sub> + 4.48<sub>±00.39</sub> |  75.34<sub>±00.29</sub> |
|       Perl (MIME::Base64) |  1.880<sub>±0.004</sub> |    14.81<sub>±00.06</sub> + 0.12<sub>±00.05</sub> |  74.45<sub>±00.50</sub> |
|              F#/.NET Core |  2.339<sub>±0.035</sub> |   36.42<sub>±00.08</sub> + 43.54<sub>±01.89</sub> |  87.52<sub>±01.18</sub> |
|                     D/gdc |  2.383<sub>±0.001</sub> |     7.29<sub>±00.04</sub> + 3.35<sub>±00.00</sub> | 105.29<sub>±00.61</sub> |
|              C#/.NET Core |  2.605<sub>±0.016</sub> |   31.59<sub>±00.09</sub> + 50.83<sub>±09.00</sub> |  95.85<sub>±00.78</sub> |
|                     D/dmd |  2.733<sub>±0.001</sub> |     3.53<sub>±00.04</sub> + 3.35<sub>±00.00</sub> | 119.71<sub>±00.60</sub> |
|                    Python |  3.186<sub>±0.013</sub> |    10.23<sub>±00.02</sub> + 0.09<sub>±00.00</sub> | 125.50<sub>±00.65</sub> |
|                       Zig |  3.196<sub>±0.003</sub> |     1.52<sub>±00.04</sub> + 0.00<sub>±00.00</sub> | 125.26<sub>±00.49</sub> |
|               Python/pypy |  3.580<sub>±0.001</sub> |   59.43<sub>±00.07</sub> + 31.58<sub>±00.10</sub> | 157.31<sub>±00.19</sub> |
|                       Tcl |  3.624<sub>±0.003</sub> |     5.05<sub>±00.04</sub> + 0.00<sub>±00.00</sub> | 148.02<sub>±00.31</sub> |
|                  Go/gccgo |  3.674<sub>±0.001</sub> |    24.81<sub>±00.12</sub> + 8.08<sub>±00.25</sub> | 166.56<sub>±00.23</sub> |
|                    Racket |  3.903<sub>±0.036</sub> |   91.21<sub>±00.25</sub> + 19.37<sub>±00.50</sub> | 155.49<sub>±01.84</sub> |
|    Ruby/truffleruby (JVM) |  3.967<sub>±0.036</sub> | 391.81<sub>±10.87</sub> + 298.87<sub>±50.79</sub> | 201.52<sub>±02.09</sub> |
|                   C#/Mono |  4.775<sub>±0.001</sub> |   26.15<sub>±00.06</sub> + 18.66<sub>±00.07</sub> | 196.42<sub>±00.82</sub> |
|                     Julia |  4.827<sub>±0.001</sub> |  266.49<sub>±00.12</sub> + 43.51<sub>±00.12</sub> | 181.97<sub>±00.63</sub> |
|                Ruby/jruby | 10.655<sub>±0.215</sub> |  190.12<sub>±01.70</sub> + 91.63<sub>±06.55</sub> | 428.25<sub>±09.08</sub> |
|          Ruby/truffleruby | 12.337<sub>±0.012</sub> | 226.13<sub>±03.32</sub> + 492.67<sub>±49.16</sub> | 546.38<sub>±03.47</sub> |
| Perl (MIME::Base64::Perl) | 13.588<sub>±0.098</sub> |    16.08<sub>±00.04</sub> + 0.28<sub>±00.09</sub> | 574.28<sub>±06.82</sub> |

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
|    C++/clang++ (simdjson On-Demand) |  0.059<sub>±0.000</sub> |    112.62<sub>±00.29</sub> + 59.80<sub>±00.25</sub> |   2.48<sub>±00.01</sub> |
|        C++/g++ (simdjson On-Demand) |  0.060<sub>±0.000</sub> |    113.51<sub>±00.06</sub> + 59.81<sub>±00.00</sub> |   2.56<sub>±00.02</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.083<sub>±0.000</sub> |     112.36<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |   3.38<sub>±00.02</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.083<sub>±0.000</sub> |     113.16<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.41<sub>±00.03</sub> |
|         C++/clang++ (DAW JSON Link) |  0.084<sub>±0.000</sub> |     112.42<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   3.53<sub>±00.01</sub> |
|             C++/g++ (DAW JSON Link) |  0.087<sub>±0.000</sub> |     113.11<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |   3.66<sub>±00.02</sub> |
|                 Rust (Serde Custom) |  0.096<sub>±0.000</sub> |     111.31<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   4.11<sub>±00.04</sub> |
|                  Rust (Serde Typed) |  0.104<sub>±0.000</sub> |    111.67<sub>±00.03</sub> + 11.77<sub>±00.00</sub> |   4.43<sub>±00.01</sub> |
|          C++/clang++ (simdjson DOM) |  0.123<sub>±0.000</sub> |   112.83<sub>±00.13</sub> + 176.09<sub>±00.13</sub> |   5.33<sub>±00.03</sub> |
|              C++/g++ (simdjson DOM) |  0.128<sub>±0.000</sub> |   113.51<sub>±00.04</sub> + 176.60<sub>±00.00</sub> |   5.60<sub>±00.07</sub> |
|                 C++/clang++ (gason) |  0.138<sub>±0.000</sub> |    112.37<sub>±00.04</sub> + 96.97<sub>±00.06</sub> |   5.62<sub>±00.03</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.138<sub>±0.000</sub> |    112.85<sub>±00.03</sub> + 61.10<sub>±00.00</sub> |   5.78<sub>±00.06</sub> |
|                     C++/g++ (gason) |  0.138<sub>±0.000</sub> |    113.13<sub>±00.05</sub> + 96.93<sub>±00.05</sub> |   5.50<sub>±00.02</sub> |
|                 C++/g++ (RapidJSON) |  0.157<sub>±0.000</sub> |   113.11<sub>±00.02</sub> + 128.90<sub>±00.02</sub> |   6.70<sub>±00.06</sub> |
|              Scala (jsoniter-scala) |  0.158<sub>±0.002</sub> |    291.61<sub>±00.13</sub> + 16.65<sub>±00.51</sub> |   8.50<sub>±00.11</sub> |
|                   Go (rjson custom) |  0.196<sub>±0.000</sub> |     115.18<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   7.63<sub>±00.02</sub> |
|                                 Zig |  0.221<sub>±0.000</sub> |    110.91<sub>±00.01</sub> + 39.02<sub>±00.03</sub> |   9.87<sub>±00.04</sub> |
|                          Go (rjson) |  0.221<sub>±0.000</sub> |    115.23<sub>±00.07</sub> + 68.03<sub>±00.00</sub> |   8.68<sub>±00.02</sub> |
|                          Go (Sonic) |  0.225<sub>±0.002</sub> |   123.66<sub>±00.07</sub> + 111.83<sub>±00.23</sub> |   9.79<sub>±00.15</sub> |
|             C++/clang++ (RapidJSON) |  0.226<sub>±0.000</sub> |   112.42<sub>±00.03</sub> + 128.97<sub>±00.03</sub> |   9.46<sub>±00.06</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.231<sub>±0.000</sub> |    112.87<sub>±00.02</sub> + 80.70<sub>±00.00</sub> |   9.62<sub>±00.05</sub> |
|         C++/g++ (RapidJSON Precise) |  0.233<sub>±0.000</sub> |   113.17<sub>±00.05</sub> + 128.33<sub>±00.59</sub> |  10.02<sub>±00.06</sub> |
|                  Go (goccy/go-json) |  0.269<sub>±0.000</sub> |   115.61<sub>±00.15</sub> + 112.20<sub>±00.14</sub> |  10.66<sub>±00.04</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.302<sub>±0.000</sub> |   112.46<sub>±00.04</sub> + 128.97<sub>±00.03</sub> |  13.00<sub>±00.06</sub> |
|                        C/gcc (yajl) |  0.374<sub>±0.001</sub> |     110.83<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  16.15<sub>±00.07</sub> |
|                      C/clang (yajl) |  0.374<sub>±0.000</sub> |     110.89<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  16.29<sub>±00.09</sub> |
|                C++/g++ (Boost.JSON) |  0.380<sub>±0.000</sub> |   113.27<sub>±00.01</sub> + 308.09<sub>±00.05</sub> |  15.91<sub>±00.04</sub> |
|             C++/g++ (RapidJSON SAX) |  0.388<sub>±0.000</sub> |     112.91<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  16.51<sub>±00.02</sub> |
|                   Nim/clang (jsony) |  0.391<sub>±0.000</sub> |   111.38<sub>±00.02</sub> + 146.18<sub>±00.03</sub> |  16.73<sub>±00.07</sub> |
|            C++/clang++ (Boost.JSON) |  0.395<sub>±0.001</sub> |   112.52<sub>±00.03</sub> + 308.18<sub>±00.00</sub> |  16.62<sub>±00.04</sub> |
|                     Nim/gcc (jsony) |  0.407<sub>±0.000</sub> |   111.10<sub>±00.01</sub> + 156.78<sub>±01.00</sub> |  17.43<sub>±00.16</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.445<sub>±0.001</sub> |     112.97<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  19.60<sub>±00.06</sub> |
|                       Go (jsoniter) |  0.520<sub>±0.001</sub> |     227.75<sub>±00.08</sub> + 1.78<sub>±00.21</sub> |  22.21<sub>±00.23</sub> |
|                Rust (Serde Untyped) |  0.547<sub>±0.001</sub> |   111.65<sub>±00.01</sub> + 839.98<sub>±00.00</sub> |  23.09<sub>±00.15</sub> |
|     C#/.NET Core (System.Text.Json) |  0.573<sub>±0.001</sub> |   488.19<sub>±00.29</sub> + 139.73<sub>±00.02</sub> |  25.40<sub>±00.12</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.585<sub>±0.000</sub> |     194.66<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  23.44<sub>±00.03</sub> |
|                       Julia (JSON3) |  0.615<sub>±0.003</sub> |   462.74<sub>±00.18</sub> + 224.58<sub>±01.29</sub> |  26.04<sub>±00.29</sub> |
|                     Java (DSL-JSON) |  0.633<sub>±0.022</sub> |   262.57<sub>±00.10</sub> + 198.23<sub>±00.93</sub> |  32.50<sub>±00.48</sub> |
|                             V/clang |  0.634<sub>±0.000</sub> |   111.43<sub>±00.01</sub> + 496.21<sub>±00.06</sub> |  26.61<sub>±00.19</sub> |
|                               V/gcc |  0.638<sub>±0.000</sub> |   111.40<sub>±00.03</sub> + 496.21<sub>±00.00</sub> |  26.68<sub>±00.26</sub> |
|                             Node.js |  0.646<sub>±0.004</sub> |   150.07<sub>±00.04</sub> + 196.56<sub>±01.49</sub> |  31.69<sub>±00.18</sub> |
|                      Crystal (Pull) |  0.648<sub>±0.002</sub> |    113.25<sub>±00.01</sub> + 18.39<sub>±00.03</sub> |  29.04<sub>±00.18</sub> |
|                         Python/pypy |  0.668<sub>±0.001</sub> |   279.67<sub>±00.01</sub> + 125.79<sub>±00.07</sub> |  28.63<sub>±00.11</sub> |
|                    Crystal (Schema) |  0.669<sub>±0.002</sub> |    113.24<sub>±00.04</sub> + 48.84<sub>±00.06</sub> |  30.10<sub>±00.21</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.695<sub>±0.001</sub> |     194.65<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  28.76<sub>±00.07</sub> |
|                Nim/gcc (Packedjson) |  0.791<sub>±0.001</sub> |   111.83<sub>±00.02</sub> + 294.16<sub>±00.00</sub> |  32.60<sub>±00.13</sub> |
|             Perl (Cpanel::JSON::XS) |  0.796<sub>±0.008</sub> |   125.50<sub>±00.07</sub> + 402.87<sub>±00.02</sub> |  33.22<sub>±00.36</sub> |
|                                 PHP |  0.844<sub>±0.000</sub> |   127.86<sub>±00.21</sub> + 517.86<sub>±00.00</sub> |  36.27<sub>±00.19</sub> |
|                                  Go |  0.887<sub>±0.002</sub> |   115.35<sub>±00.10</sub> + 104.95<sub>±00.22</sub> |  36.93<sub>±00.10</sub> |
|              Nim/clang (Packedjson) |  0.899<sub>±0.002</sub> |   112.14<sub>±00.01</sub> + 294.16<sub>±00.00</sub> |  36.99<sub>±00.21</sub> |
|                             Crystal |  0.950<sub>±0.011</sub> |   113.24<sub>±00.01</sub> + 392.50<sub>±00.00</sub> |  41.49<sub>±00.82</sub> |
|                        C#/.NET Core |  1.084<sub>±0.012</sub> |   496.41<sub>±00.12</sub> + 271.70<sub>±00.01</sub> |  48.39<sub>±00.47</sub> |
|                             Nim/gcc |  1.116<sub>±0.002</sub> |  111.86<sub>±00.03</sub> + 1001.34<sub>±00.00</sub> |  46.65<sub>±00.27</sub> |
|                C++/clang++ (json-c) |  1.163<sub>±0.004</sub> |  112.67<sub>±00.02</sub> + 1216.08<sub>±00.00</sub> |  48.79<sub>±00.78</sub> |
|                    C++/g++ (json-c) |  1.164<sub>±0.005</sub> |  113.30<sub>±00.06</sub> + 1216.02<sub>±00.06</sub> |  49.22<sub>±00.81</sub> |
|                             Clojure |  1.246<sub>±0.014</sub> |   460.37<sub>±07.51</sub> + 546.75<sub>±16.98</sub> |  66.52<sub>±01.33</sub> |
|                           Nim/clang |  1.277<sub>±0.001</sub> |   112.14<sub>±00.01</sub> + 999.02<sub>±00.00</sub> |  52.22<sub>±00.36</sub> |
|                            Go/gccgo |  1.286<sub>±0.001</sub> |    138.88<sub>±00.10</sub> + 83.51<sub>±00.03</sub> |  55.05<sub>±00.31</sub> |
|                 CPython (UltraJSON) |  1.337<sub>±0.001</sub> |   122.63<sub>±00.05</sub> + 495.71<sub>±02.58</sub> |  49.85<sub>±00.19</sub> |
|              C++/clang++ (Nlohmann) |  1.343<sub>±0.006</sub> |   112.63<sub>±00.05</sub> + 360.14<sub>±00.00</sub> |  57.01<sub>±00.32</sub> |
|                              Python |  1.391<sub>±0.002</sub> |   120.04<sub>±00.02</sub> + 326.36<sub>±00.04</sub> |  55.39<sub>±00.55</sub> |
|                                Ruby |  1.452<sub>±0.002</sub> |   125.12<sub>±00.04</sub> + 261.61<sub>±00.07</sub> |  61.45<sub>±00.35</sub> |
|                        Ruby (--jit) |  1.539<sub>±0.008</sub> |   126.19<sub>±00.04</sub> + 263.91<sub>±00.01</sub> |  65.79<sub>±00.58</sub> |
|     F#/.NET Core (System.Text.Json) |  1.576<sub>±0.004</sub> |   498.64<sub>±00.26</sub> + 229.56<sub>±04.48</sub> |  69.97<sub>±00.45</sub> |
|                  C++/g++ (Nlohmann) |  1.611<sub>±0.005</sub> |   113.26<sub>±00.07</sub> + 448.03<sub>±00.03</sub> |  66.24<sub>±00.72</sub> |
|                             C#/Mono |  1.802<sub>±0.028</sub> |    253.15<sub>±00.17</sub> + 31.50<sub>±00.00</sub> |  78.74<sub>±01.13</sub> |
|                         Ruby (YAJL) |  1.808<sub>±0.004</sub> |   125.06<sub>±00.04</sub> + 276.09<sub>±00.04</sub> |  75.84<sub>±00.58</sub> |
|                              D/ldc2 |  2.044<sub>±0.006</sub> |   112.68<sub>±00.01</sub> + 680.39<sub>±00.04</sub> |  85.05<sub>±00.13</sub> |
|                             Haskell |  2.257<sub>±0.006</sub> |   117.34<sub>±00.01</sub> + 725.38<sub>±00.23</sub> |  94.48<sub>±00.60</sub> |
|                           Rust (jq) |  2.596<sub>±0.001</sub> |   113.53<sub>±00.04</sub> + 903.76<sub>±01.16</sub> | 108.96<sub>±00.28</sub> |
|                          Ruby/jruby |  2.986<sub>±0.024</sub> |   456.73<sub>±06.29</sub> + 913.88<sub>±75.59</sub> | 154.96<sub>±02.12</sub> |
|        C++/g++ (Boost.PropertyTree) |  3.065<sub>±0.003</sub> |  113.12<sub>±00.04</sub> + 1440.12<sub>±00.00</sub> | 128.54<sub>±01.53</sub> |
|    C++/clang++ (Boost.PropertyTree) |  3.092<sub>±0.005</sub> |  194.90<sub>±00.04</sub> + 1232.84<sub>±00.00</sub> | 128.35<sub>±00.98</sub> |
|                          Vala/clang |  3.326<sub>±0.012</sub> |   114.98<sub>±00.01</sub> + 980.04<sub>±00.01</sub> | 144.32<sub>±01.14</sub> |
|                            Vala/gcc |  3.327<sub>±0.003</sub> |   114.98<sub>±00.08</sub> + 980.04<sub>±00.01</sub> | 143.32<sub>±01.85</sub> |
|                               D/gdc |  3.612<sub>±0.016</sub> |   116.49<sub>±00.05</sub> + 681.07<sub>±00.10</sub> | 155.27<sub>±00.75</sub> |
|                              Racket |  3.779<sub>±0.036</sub> |   220.86<sub>±00.19</sub> + 263.51<sub>±25.06</sub> | 156.87<sub>±01.63</sub> |
|                               D/dmd |  4.453<sub>±0.006</sub> |   113.07<sub>±00.03</sub> + 680.43<sub>±00.05</sub> | 179.34<sub>±00.49</sub> |
|                   Perl (JSON::Tiny) |  9.657<sub>±0.113</sub> |   126.14<sub>±00.09</sub> + 528.61<sub>±00.05</sub> | 423.18<sub>±04.31</sub> |
|                    Ruby/truffleruby | 10.672<sub>±0.121</sub> |  466.47<sub>±23.50</sub> + 1900.14<sub>±89.47</sub> | 619.64<sub>±07.46</sub> |
|              Ruby/truffleruby (JVM) | 10.982<sub>±0.124</sub> | 498.64<sub>±10.98</sub> + 2558.62<sub>±197.73</sub> | 687.46<sub>±08.96</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                Language |                   Time, s |                                         Memory, MiB |                  Energy, J |
| :---------------------- | ------------------------: | --------------------------------------------------: | -------------------------: |
|         D/ldc2 (lubeck) |    0.042<sub>±0.000</sub> |      6.08<sub>±00.05</sub> + 57.76<sub>±00.05</sub> |      4.44<sub>±00.02</sub> |
|     V/gcc (VSL + CBLAS) |    0.046<sub>±0.000</sub> |      6.62<sub>±00.02</sub> + 58.28<sub>±00.00</sub> |      4.65<sub>±00.03</sub> |
|   V/clang (VSL + CBLAS) |    0.046<sub>±0.000</sub> |      6.66<sub>±00.01</sub> + 58.29<sub>±00.00</sub> |      4.65<sub>±00.03</sub> |
|   Nim/gcc (Arraymancer) |    0.063<sub>±0.004</sub> |      5.46<sub>±00.06</sub> + 57.49<sub>±00.12</sub> |      5.32<sub>±00.22</sub> |
|          Python (NumPy) |    0.064<sub>±0.000</sub> |     31.77<sub>±00.13</sub> + 58.54<sub>±00.10</sub> |      6.19<sub>±00.02</sub> |
|         C++/g++ (Eigen) |    0.065<sub>±0.003</sub> |     28.29<sub>±10.15</sub> + 61.41<sub>±10.18</sub> |      4.90<sub>±00.13</sub> |
|     C++/clang++ (Eigen) |    0.065<sub>±0.003</sub> |     30.17<sub>±09.25</sub> + 60.15<sub>±09.15</sub> |      5.48<sub>±00.18</sub> |
| Nim/clang (Arraymancer) |    0.068<sub>±0.001</sub> |      6.04<sub>±00.07</sub> + 57.60<sub>±00.18</sub> |      5.90<sub>±00.06</sub> |
|             Java (ND4J) |    0.076<sub>±0.001</sub> |    112.18<sub>±02.20</sub> + 92.14<sub>±00.02</sub> |      5.98<sub>±00.05</sub> |
|      Julia (threads: 2) |    0.083<sub>±0.000</sub> |    283.67<sub>±00.03</sub> + 57.16<sub>±00.06</sub> |      5.25<sub>±00.02</sub> |
|          Rust (ndarray) |    0.085<sub>±0.000</sub> |      2.37<sub>±00.05</sub> + 68.47<sub>±00.00</sub> |      5.99<sub>±00.04</sub> |
|      Julia (threads: 1) |    0.133<sub>±0.000</sub> |    283.77<sub>±00.18</sub> + 56.76<sub>±00.06</sub> |      6.62<sub>±00.04</sub> |
|           V/clang (VSL) |    0.272<sub>±0.004</sub> |      7.33<sub>±00.07</sub> + 51.56<sub>±00.00</sub> |     19.00<sub>±00.23</sub> |
|             V/gcc (VSL) |    0.505<sub>±0.002</sub> |      7.05<sub>±00.07</sub> + 51.82<sub>±00.00</sub> |     36.74<sub>±00.18</sub> |
|         Julia (no BLAS) |    1.018<sub>±0.004</sub> |    267.67<sub>±00.06</sub> + 51.50<sub>±00.00</sub> |     45.38<sub>±00.67</sub> |
|                  D/ldc2 |    1.717<sub>±0.001</sub> |      3.25<sub>±00.01</sub> + 70.47<sub>±00.02</sub> |     63.31<sub>±00.18</sub> |
|                   D/gdc |    1.869<sub>±0.001</sub> |      7.30<sub>±00.03</sub> + 70.16<sub>±00.01</sub> |     73.12<sub>±00.03</sub> |
|                   D/dmd |    1.882<sub>±0.002</sub> |      3.17<sub>±00.02</sub> + 70.49<sub>±00.02</sub> |     70.93<sub>±00.15</sub> |
|                   C/gcc |    3.026<sub>±0.001</sub> |      1.50<sub>±00.05</sub> + 68.66<sub>±00.02</sub> |    111.93<sub>±00.58</sub> |
|                   V/gcc |    3.038<sub>±0.001</sub> |      2.49<sub>±00.06</sub> + 68.58<sub>±00.00</sub> |    112.89<sub>±00.08</sub> |
|              Vala/clang |    3.056<sub>±0.000</sub> |      5.48<sub>±00.04</sub> + 68.32<sub>±00.00</sub> |    104.92<sub>±00.17</sub> |
|                 V/clang |    3.058<sub>±0.000</sub> |      2.79<sub>±00.05</sub> + 68.58<sub>±00.00</sub> |    104.95<sub>±00.09</sub> |
|                 C/clang |    3.060<sub>±0.000</sub> |      1.49<sub>±00.01</sub> + 68.70<sub>±00.00</sub> |    104.77<sub>±00.04</sub> |
|                    Rust |    3.062<sub>±0.000</sub> |      2.10<sub>±00.02</sub> + 68.57<sub>±00.00</sub> |    105.83<sub>±00.47</sub> |
|                     Zig |    3.070<sub>±0.001</sub> |      1.79<sub>±00.05</sub> + 68.58<sub>±00.00</sub> |    108.93<sub>±00.04</sub> |
|                   Swift |    3.090<sub>±0.000</sub> |      7.91<sub>±00.03</sub> + 68.75<sub>±00.01</sub> |    110.70<sub>±00.57</sub> |
|                 Nim/gcc |    3.093<sub>±0.001</sub> |      2.54<sub>±00.04</sub> + 57.75<sub>±00.00</sub> |    114.70<sub>±00.43</sub> |
|               Nim/clang |    3.117<sub>±0.001</sub> |      2.79<sub>±00.01</sub> + 64.97<sub>±03.74</sub> |    107.58<sub>±00.51</sub> |
|                Vala/gcc |    3.121<sub>±0.000</sub> |      5.32<sub>±00.20</sub> + 68.32<sub>±00.00</sub> |    114.54<sub>±00.07</sub> |
|                      Go |    3.150<sub>±0.001</sub> |      3.62<sub>±00.19</sub> + 72.15<sub>±00.13</sub> |    115.35<sub>±00.13</sub> |
|                Go/gccgo |    3.157<sub>±0.001</sub> |     24.18<sub>±00.07</sub> + 73.45<sub>±00.04</sub> |    112.37<sub>±00.18</sub> |
|                 Crystal |    3.159<sub>±0.000</sub> |      3.52<sub>±00.04</sub> + 60.05<sub>±00.06</sub> |    115.85<sub>±00.21</sub> |
|                    Java |    3.175<sub>±0.001</sub> |     40.65<sub>±00.08</sub> + 69.13<sub>±00.15</sub> |    124.54<sub>±00.09</sub> |
|              Kotlin/JVM |    3.207<sub>±0.005</sub> |     41.86<sub>±00.02</sub> + 68.71<sub>±00.41</sub> |    130.79<sub>±00.32</sub> |
|                 Node.js |    3.215<sub>±0.004</sub> |     44.73<sub>±00.03</sub> + 72.69<sub>±00.11</sub> |    129.83<sub>±00.12</sub> |
|             Python/pypy |    3.275<sub>±0.001</sub> |     60.40<sub>±00.21</sub> + 68.76<sub>±00.17</sub> |    136.34<sub>±00.07</sub> |
|                   Scala |    3.313<sub>±0.003</sub> |    68.84<sub>±00.22</sub> + 160.83<sub>±00.27</sub> |    121.22<sub>±00.21</sub> |
|            C#/.NET Core |    4.382<sub>±0.001</sub> |     33.31<sub>±00.06</sub> + 69.05<sub>±00.00</sub> |    176.43<sub>±00.07</sub> |
|                 C#/Mono |    7.411<sub>±0.000</sub> |     26.02<sub>±00.06</sub> + 69.47<sub>±00.00</sub> |    304.12<sub>±02.00</sub> |
|        Ruby/truffleruby |   19.501<sub>±1.622</sub> |   424.02<sub>±38.01</sub> + 560.75<sub>±63.85</sub> |    702.98<sub>±52.89</sub> |
|  Ruby/truffleruby (JVM) |   24.995<sub>±0.199</sub> |   442.91<sub>±19.85</sub> + 454.12<sub>±55.17</sub> |    894.29<sub>±06.62</sub> |
|            Ruby (--jit) |  152.114<sub>±0.134</sub> |     17.91<sub>±00.07</sub> + 69.69<sub>±00.04</sub> |   6574.42<sub>±25.26</sub> |
|                    Ruby |  178.566<sub>±0.786</sub> |     15.79<sub>±00.04</sub> + 69.15<sub>±00.05</sub> |   7779.16<sub>±40.92</sub> |
|                    Perl |  223.173<sub>±1.482</sub> |     9.54<sub>±00.08</sub> + 599.64<sub>±00.09</sub> |   9122.71<sub>±66.22</sub> |
|                  Python |  238.744<sub>±0.471</sub> |     10.51<sub>±00.02</sub> + 68.84<sub>±00.00</sub> |   9445.43<sub>±16.40</sub> |
|                     Tcl |  332.625<sub>±1.772</sub> |     7.31<sub>±00.05</sub> + 400.44<sub>±00.00</sub> |  13948.04<sub>±55.34</sub> |
|              Ruby/jruby | 445.418<sub>±12.396</sub> | 276.55<sub>±04.29</sub> + 1023.72<sub>±133.69</sub> | 18883.42<sub>±466.58</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

Notes:

 - All languages but V and Python use unordered hashmaps (V and Python don't provide those out of box, and
 their hashmaps use keys in the insertion order);
 - The results are always sorted (could be unstable or stable though).

[Primes](primes)

|               Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :--------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                    Zig | 0.058<sub>±0.000</sub> |    0.90<sub>±00.03</sub> + 50.20<sub>±00.26</sub> |   2.44<sub>±00.04</sub> |
|                C++/g++ | 0.068<sub>±0.000</sub> |    3.61<sub>±00.01</sub> + 79.45<sub>±00.38</sub> |   2.63<sub>±00.02</sub> |
|            C++/clang++ | 0.070<sub>±0.000</sub> |    3.05<sub>±00.00</sub> + 59.19<sub>±00.13</sub> |   2.62<sub>±00.04</sub> |
|                V/clang | 0.106<sub>±0.000</sub> |   1.86<sub>±00.01</sub> + 211.02<sub>±00.55</sub> |   4.27<sub>±00.05</sub> |
|                  V/gcc | 0.108<sub>±0.000</sub> |   1.83<sub>±00.02</sub> + 207.57<sub>±00.39</sub> |   4.40<sub>±00.03</sub> |
|                   Rust | 0.125<sub>±0.000</sub> |    2.00<sub>±00.05</sub> + 72.94<sub>±00.00</sub> |   4.85<sub>±00.01</sub> |
|                Crystal | 0.146<sub>±0.000</sub> |    3.71<sub>±00.02</sub> + 88.43<sub>±00.00</sub> |   5.94<sub>±00.07</sub> |
|                   Java | 0.157<sub>±0.003</sub> |  39.69<sub>±00.06</sub> + 150.56<sub>±01.21</sub> |   8.72<sub>±00.13</sub> |
|                  Scala | 0.226<sub>±0.005</sub> |  73.14<sub>±00.21</sub> + 210.71<sub>±00.50</sub> |  12.73<sub>±00.18</sub> |
|                Node.js | 0.256<sub>±0.002</sub> |  38.33<sub>±00.02</sub> + 151.44<sub>±00.14</sub> |  12.60<sub>±00.07</sub> |
|              Nim/clang | 0.296<sub>±0.000</sub> |   2.01<sub>±00.02</sub> + 601.22<sub>±01.55</sub> |  11.45<sub>±00.02</sub> |
|                Nim/gcc | 0.298<sub>±0.001</sub> |   1.74<sub>±00.05</sub> + 615.91<sub>±00.00</sub> |  11.25<sub>±00.07</sub> |
|             Lua/luajit | 0.334<sub>±0.000</sub> |   2.58<sub>±00.06</sub> + 157.98<sub>±00.68</sub> |  13.11<sub>±00.13</sub> |
|                  Julia | 0.675<sub>±0.002</sub> | 267.69<sub>±00.07</sub> + 342.19<sub>±00.25</sub> |  26.19<sub>±00.26</sub> |
|                 Racket | 0.730<sub>±0.003</sub> | 102.20<sub>±00.12</sub> + 237.07<sub>±00.09</sub> |  29.01<sub>±00.31</sub> |
|            Python/pypy | 0.793<sub>±0.003</sub> |  59.10<sub>±00.02</sub> + 249.23<sub>±00.08</sub> |  31.40<sub>±00.13</sub> |
|       Ruby/truffleruby | 0.912<sub>±0.014</sub> | 225.50<sub>±01.85</sub> + 779.85<sub>±19.22</sub> |  61.61<sub>±01.20</sub> |
| Ruby/truffleruby (JVM) | 1.359<sub>±0.037</sub> | 388.78<sub>±12.63</sub> + 567.15<sub>±29.85</sub> |  88.28<sub>±02.40</sub> |
|                    Lua | 1.487<sub>±0.003</sub> |   2.25<sub>±00.02</sub> + 284.27<sub>±00.80</sub> |  58.31<sub>±00.46</sub> |
|           Ruby (--jit) | 1.775<sub>±0.020</sub> |  15.95<sub>±00.02</sub> + 172.01<sub>±01.28</sub> |  80.44<sub>±00.60</sub> |
|                   Ruby | 1.887<sub>±0.003</sub> |  14.91<sub>±00.02</sub> + 142.89<sub>±00.03</sub> |  77.16<sub>±00.57</sub> |
|             Ruby/jruby | 2.484<sub>±0.033</sub> | 192.08<sub>±00.89</sub> + 464.73<sub>±41.25</sub> | 134.20<sub>±02.35</sub> |
|                 Python | 3.101<sub>±0.013</sub> |  10.14<sub>±00.04</sub> + 181.56<sub>±00.77</sub> | 125.18<sub>±00.55</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 7.0.402                         |
| C#/.NET Core     | 4.7.0-3.23416.9 (43b0b05c)      |
| C#/Mono          | 6.12.0.200                      |
| Chez Scheme      | 9.5.8                           |
| Clojure          | "1.11.1"                        |
| Crystal          | 1.10.1                          |
| D/dmd            | v2.105.2                        |
| D/gdc            | 13.2.0                          |
| D/ldc2           | 1.34.0                          |
| Elixir           | 1.14.0                          |
| F#/.NET Core     | 12.7.0.0 for F# 7.0             |
| Go               | go1.21.3                        |
| Go/gccgo         | 13.2.0                          |
| Haskell          | 9.4.7                           |
| Idris 2          | 0.6.0                           |
| Java             | 21                              |
| Julia            | v"1.9.3"                        |
| Kotlin           | 1.9.10                          |
| Lua              | 5.4.4                           |
| Lua/luajit       | 2.1.1696562864                  |
| MLton            | 20210117                        |
| Nim              | 2.0.0                           |
| Node.js          | v20.8.1                         |
| OCaml            | 5.1.0                           |
| PHP              | 8.2.10                          |
| Perl             | v5.36.0                         |
| Python           | 3.11.6                          |
| Python/pypy      | 7.3.13-final0 for Python 3.10.13 |
| Racket           | "8.10"                          |
| Ruby             | 3.2.2p53                        |
| Ruby/jruby       | 9.4.3.0                         |
| Ruby/truffleruby | 23.1.0                          |
| Rust             | 1.73.0                          |
| Scala            | 3.3.1                           |
| Swift            | 5.9                             |
| Tcl              | 8.6                             |
| V                | 0.4.2 2332c17                   |
| Vala             | 0.56.13                         |
| Zig              | 0.11.0                          |
| clang/clang++    | 16.0.6 (15)                     |
| gcc/g++          | 13.2.0                          |

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
