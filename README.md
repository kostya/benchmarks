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

UPDATE: 2024-06-06

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
|         Scala (Staged) |   0.390<sub>±0.015</sub> |  222.77<sub>±03.17</sub> + 15.42<sub>±01.67</sub> |     24.89<sub>±00.74</sub> |
|        Racket (Staged) |   0.893<sub>±0.001</sub> |   101.38<sub>±00.97</sub> + 0.00<sub>±00.00</sub> |     34.61<sub>±00.07</sub> |
|                   Rust |   1.011<sub>±0.000</sub> |     1.05<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     43.04<sub>±00.28</sub> |
|                  V/gcc |   1.060<sub>±0.000</sub> |     1.92<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     43.67<sub>±00.15</sub> |
|                  C/gcc |   1.111<sub>±0.001</sub> |     1.01<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     47.01<sub>±00.65</sub> |
|                C++/g++ |   1.113<sub>±0.001</sub> |     1.98<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     46.04<sub>±00.34</sub> |
|                  D/gdc |   1.120<sub>±0.000</sub> |     6.39<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     49.26<sub>±00.57</sub> |
|            C++/clang++ |   1.125<sub>±0.001</sub> |     1.74<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     46.90<sub>±00.51</sub> |
|                C/clang |   1.140<sub>±0.001</sub> |     1.02<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     48.57<sub>±00.37</sub> |
|                 D/ldc2 |   1.163<sub>±0.003</sub> |     2.32<sub>±00.80</sub> + 0.00<sub>±00.00</sub> |     49.08<sub>±00.30</sub> |
|                Nim/gcc |   1.168<sub>±0.001</sub> |     1.01<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     49.66<sub>±00.25</sub> |
|                   Java |   1.193<sub>±0.000</sub> |    39.79<sub>±00.09</sub> + 1.55<sub>±00.06</sub> |     49.18<sub>±00.26</sub> |
|               Vala/gcc |   1.210<sub>±0.001</sub> |     4.50<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     50.69<sub>±00.11</sub> |
|             Kotlin/JVM |   1.222<sub>±0.002</sub> |    43.53<sub>±00.14</sub> + 0.52<sub>±00.26</sub> |     50.93<sub>±00.10</sub> |
|             Vala/clang |   1.238<sub>±0.006</sub> |     4.52<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     52.25<sub>±01.41</sub> |
|                     Go |   1.247<sub>±0.000</sub> |     3.55<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     51.47<sub>±00.33</sub> |
|                    Zig |   1.265<sub>±0.000</sub> |     1.03<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     52.54<sub>±00.44</sub> |
|           C#/.NET Core |   1.367<sub>±0.001</sub> |    32.66<sub>±00.13</sub> + 0.15<sub>±00.02</sub> |     58.23<sub>±00.27</sub> |
|               Go/gccgo |   1.481<sub>±0.002</sub> |    23.56<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     62.86<sub>±00.67</sub> |
|              Nim/clang |   1.565<sub>±0.000</sub> |     1.29<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     64.41<sub>±00.46</sub> |
|           F#/.NET Core |   1.574<sub>±0.004</sub> |    37.41<sub>±00.18</sub> + 0.30<sub>±00.00</sub> |     67.12<sub>±00.40</sub> |
|                Crystal |   1.597<sub>±0.008</sub> |     3.00<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     67.57<sub>±00.71</sub> |
|                  OCaml |   1.638<sub>±0.005</sub> |     3.01<sub>±00.05</sub> + 2.81<sub>±00.06</sub> |     76.01<sub>±00.28</sub> |
|                  Julia |   1.638<sub>±0.003</sub> |   256.65<sub>±00.11</sub> + 0.40<sub>±00.03</sub> |     69.57<sub>±00.34</sub> |
|            Chez Scheme |   1.714<sub>±0.004</sub> |    24.77<sub>±00.02</sub> + 4.46<sub>±00.12</sub> |     72.48<sub>±00.24</sub> |
|                 Racket |   1.756<sub>±0.028</sub> |   113.87<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |     71.30<sub>±01.18</sub> |
|                V/clang |   1.973<sub>±0.018</sub> |     1.96<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     85.72<sub>±01.29</sub> |
|                C#/Mono |   2.054<sub>±0.011</sub> |    25.60<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     87.72<sub>±01.03</sub> |
|                  MLton |   2.088<sub>±0.027</sub> |     1.77<sub>±00.04</sub> + 0.25<sub>±00.00</sub> |     86.67<sub>±01.18</sub> |
|                  Scala |   2.779<sub>±0.003</sub> |  72.03<sub>±00.11</sub> + 249.04<sub>±00.15</sub> |    121.64<sub>±00.71</sub> |
|                Node.js |   2.920<sub>±0.028</sub> |    44.53<sub>±00.02</sub> + 4.37<sub>±00.00</sub> |    123.76<sub>±01.03</sub> |
|       Haskell (MArray) |   3.095<sub>±0.007</sub> |     3.17<sub>±00.04</sub> + 4.92<sub>±00.04</sub> |    130.42<sub>±01.60</sub> |
|                  D/dmd |   3.326<sub>±0.001</sub> |     3.39<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    124.72<sub>±00.18</sub> |
|           Haskell (FP) |   3.624<sub>±0.002</sub> |     3.27<sub>±00.05</sub> + 4.89<sub>±00.04</sub> |    154.06<sub>±00.80</sub> |
| Ruby/truffleruby (JVM) |   4.996<sub>±0.396</sub> | 374.17<sub>±10.09</sub> + 496.92<sub>±17.09</sub> |    243.84<sub>±22.97</sub> |
|       Ruby/truffleruby |   5.193<sub>±0.358</sub> | 203.03<sub>±01.33</sub> + 550.75<sub>±59.29</sub> |    253.87<sub>±16.16</sub> |
|                  Swift |   5.656<sub>±0.005</sub> |    16.69<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    211.92<sub>±00.58</sub> |
|             Lua/luajit |   5.881<sub>±0.025</sub> |     2.54<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    241.87<sub>±05.05</sub> |
|            Python/pypy |   9.544<sub>±0.044</sub> |   58.79<sub>±00.12</sub> + 29.96<sub>±00.01</sub> |    422.77<sub>±04.07</sub> |
|                  Idris |  15.129<sub>±0.051</sub> |    20.71<sub>±00.03</sub> + 8.83<sub>±00.04</sub> |    661.53<sub>±10.66</sub> |
|                 Elixir |  20.395<sub>±0.043</sub> |    70.50<sub>±00.79</sub> + 0.00<sub>±00.00</sub> |    807.25<sub>±05.02</sub> |
|           Ruby (--jit) |  31.817<sub>±0.055</sub> |    21.59<sub>±00.04</sub> + 4.76<sub>±00.01</sub> |   1311.22<sub>±06.23</sub> |
|                    PHP |  34.492<sub>±0.160</sub> |    17.78<sub>±00.18</sub> + 0.00<sub>±00.00</sub> |   1476.88<sub>±31.23</sub> |
|                    Lua |  36.833<sub>±0.101</sub> |     2.28<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   1512.87<sub>±19.03</sub> |
|                   Ruby |  68.430<sub>±0.445</sub> |    11.43<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   3008.91<sub>±26.07</sub> |
|                 Python |  69.075<sub>±0.780</sub> |    11.17<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   3039.83<sub>±50.68</sub> |
|             Ruby/jruby |  81.684<sub>±1.546</sub> | 203.50<sub>±04.69</sub> + 220.84<sub>±20.47</sub> |   3624.22<sub>±70.81</sub> |
|               Tcl (FP) | 192.447<sub>±0.637</sub> |     4.06<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   8478.25<sub>±83.42</sub> |
|                   Perl | 225.331<sub>±1.241</sub> |     6.90<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  10000.48<sub>±94.38</sub> |
|              Tcl (OOP) | 384.789<sub>±1.841</sub> |     4.06<sub>±00.01</sub> + 0.00<sub>±00.00</sub> | 17091.07<sub>±181.64</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|               Language |                 Time, s |                                       Memory, MiB |                Energy, J |
| :--------------------- | ----------------------: | ------------------------------------------------: | -----------------------: |
|         Scala (Staged) |  7.639<sub>±0.120</sub> | 224.42<sub>±01.90</sub> + 102.36<sub>±04.76</sub> |  459.89<sub>±14.99</sub> |
|                C++/g++ |  9.755<sub>±0.030</sub> |     1.97<sub>±00.03</sub> + 2.29<sub>±00.11</sub> |  393.99<sub>±02.68</sub> |
|           C#/.NET Core | 12.048<sub>±0.028</sub> |    32.72<sub>±00.06</sub> + 1.27<sub>±00.00</sub> |  482.24<sub>±01.22</sub> |
|                   Java | 12.450<sub>±0.060</sub> |    39.92<sub>±00.06</sub> + 2.44<sub>±00.04</sub> |  489.83<sub>±01.92</sub> |
|                  C/gcc | 12.513<sub>±0.013</sub> |     0.99<sub>±00.02</sub> + 0.84<sub>±00.06</sub> |  505.46<sub>±02.16</sub> |
|             Kotlin/JVM | 13.082<sub>±0.033</sub> |    43.53<sub>±00.17</sub> + 2.30<sub>±00.33</sub> |  547.94<sub>±03.79</sub> |
|           F#/.NET Core | 13.115<sub>±0.017</sub> |    37.55<sub>±00.07</sub> + 2.12<sub>±00.04</sub> |  526.18<sub>±01.55</sub> |
|                C/clang | 13.293<sub>±0.031</sub> |     0.97<sub>±00.03</sub> + 0.75<sub>±00.03</sub> |  572.78<sub>±05.53</sub> |
|                  V/gcc | 13.591<sub>±0.007</sub> |     1.91<sub>±00.03</sub> + 1.18<sub>±00.04</sub> |  547.49<sub>±03.65</sub> |
|            C++/clang++ | 13.898<sub>±0.024</sub> |     1.72<sub>±00.05</sub> + 1.88<sub>±00.03</sub> |  569.56<sub>±02.36</sub> |
|        Racket (Staged) | 13.986<sub>±0.059</sub> |   99.66<sub>±00.50</sub> + 77.13<sub>±01.67</sub> |  556.40<sub>±01.66</sub> |
|                Crystal | 14.103<sub>±0.018</sub> |     2.96<sub>±00.03</sub> + 0.74<sub>±00.04</sub> |  598.53<sub>±02.62</sub> |
|                     Go | 14.182<sub>±0.009</sub> |     3.56<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  562.34<sub>±00.49</sub> |
|                   Rust | 14.229<sub>±0.012</sub> |     1.04<sub>±00.01</sub> + 1.20<sub>±00.04</sub> |  564.72<sub>±02.02</sub> |
|                  D/gdc | 14.266<sub>±0.016</sub> |     6.37<sub>±00.06</sub> + 1.49<sub>±00.04</sub> |  601.20<sub>±03.61</sub> |
|                 D/ldc2 | 14.278<sub>±0.013</sub> |     3.05<sub>±00.02</sub> + 0.83<sub>±00.02</sub> |  572.27<sub>±01.12</sub> |
|               Vala/gcc | 14.469<sub>±0.022</sub> |     4.47<sub>±00.04</sub> + 1.24<sub>±00.04</sub> |  574.39<sub>±01.98</sub> |
|                    Zig | 14.481<sub>±0.014</sub> |     1.02<sub>±00.02</sub> + 1.42<sub>±00.06</sub> |  600.92<sub>±02.27</sub> |
|             Vala/clang | 14.845<sub>±0.010</sub> |     4.46<sub>±00.02</sub> + 1.22<sub>±00.03</sub> |  607.68<sub>±02.95</sub> |
|                Nim/gcc | 15.641<sub>±0.030</sub> |     2.07<sub>±00.06</sub> + 1.29<sub>±00.00</sub> |  663.67<sub>±00.73</sub> |
|                  Scala | 16.365<sub>±0.031</sub> |  71.96<sub>±00.25</sub> + 139.77<sub>±00.37</sub> |  735.09<sub>±03.15</sub> |
|                  Swift | 18.421<sub>±0.037</sub> |    16.45<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  762.02<sub>±06.09</sub> |
|               Go/gccgo | 19.091<sub>±0.406</sub> |    23.58<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  796.35<sub>±13.26</sub> |
|              Nim/clang | 19.696<sub>±0.204</sub> |     2.38<sub>±00.01</sub> + 1.29<sub>±00.00</sub> |  806.37<sub>±09.58</sub> |
|                V/clang | 20.895<sub>±0.225</sub> |     1.94<sub>±00.05</sub> + 1.17<sub>±00.00</sub> |  901.27<sub>±12.94</sub> |
|                  OCaml | 24.390<sub>±0.016</sub> |     4.53<sub>±00.08</sub> + 2.82<sub>±00.00</sub> | 1171.79<sub>±06.03</sub> |
|                  Julia | 26.058<sub>±0.070</sub> |   256.43<sub>±00.04</sub> + 0.30<sub>±00.00</sub> | 1021.32<sub>±06.88</sub> |
|            Chez Scheme | 27.698<sub>±0.056</sub> |    25.55<sub>±00.06</sub> + 3.68<sub>±00.01</sub> | 1210.07<sub>±04.32</sub> |
|                C#/Mono | 31.025<sub>±0.011</sub> |    25.61<sub>±00.09</sub> + 0.83<sub>±00.00</sub> | 1289.92<sub>±08.22</sub> |
|                  MLton | 33.842<sub>±0.072</sub> |     1.73<sub>±00.05</sub> + 4.11<sub>±00.00</sub> | 1537.44<sub>±17.63</sub> |
|       Haskell (MArray) | 34.546<sub>±0.022</sub> |     4.08<sub>±00.02</sub> + 5.11<sub>±00.00</sub> | 1400.92<sub>±05.61</sub> |
|                Node.js | 34.598<sub>±0.063</sub> |    44.44<sub>±00.07</sub> + 5.25<sub>±00.00</sub> | 1378.99<sub>±02.63</sub> |
|             Lua/luajit | 34.656<sub>±0.034</sub> |     2.56<sub>±00.04</sub> + 0.38<sub>±00.00</sub> | 1388.13<sub>±02.21</sub> |
|                 Racket | 35.183<sub>±0.294</sub> |   113.90<sub>±00.09</sub> + 1.45<sub>±00.65</sub> | 1567.04<sub>±13.20</sub> |
|                  D/dmd | 37.942<sub>±0.003</sub> |     3.35<sub>±00.03</sub> + 0.87<sub>±00.02</sub> | 1378.02<sub>±00.92</sub> |
|            Python/pypy | 41.463<sub>±0.088</sub> |   58.86<sub>±00.09</sub> + 30.60<sub>±00.03</sub> | 1837.98<sub>±07.63</sub> |
|       Ruby/truffleruby | 48.320<sub>±0.851</sub> | 203.25<sub>±00.90</sub> + 581.44<sub>±30.40</sub> | 2360.50<sub>±62.00</sub> |
| Ruby/truffleruby (JVM) | 50.546<sub>±1.177</sub> | 374.73<sub>±03.49</sub> + 489.39<sub>±89.50</sub> | 2202.33<sub>±46.95</sub> |
|                  Idris | 66.241<sub>±0.299</sub> |    22.03<sub>±00.05</sub> + 9.54<sub>±00.00</sub> | 2848.93<sub>±05.21</sub> |
|           Haskell (FP) | 77.093<sub>±0.126</sub> |    4.19<sub>±00.02</sub> + 75.74<sub>±00.03</sub> | 3195.67<sub>±11.41</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)


|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.098<sub>±0.000</sub> |     2.20<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   4.60<sub>±00.03</sub> |
|          C/clang (aklomp) |  0.099<sub>±0.000</sub> |     2.10<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   4.68<sub>±00.05</sub> |
|                       PHP |  0.105<sub>±0.000</sub> |    18.56<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   4.76<sub>±00.07</sub> |
|              Go (base64x) |  0.267<sub>±0.001</sub> |     6.61<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  12.80<sub>±00.11</sub> |
|                       Zig |  0.698<sub>±0.000</sub> |     1.66<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  26.49<sub>±00.19</sub> |
|                      Rust |  0.849<sub>±0.000</sub> |     2.42<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  35.03<sub>±00.11</sub> |
|                   Node.js |  0.913<sub>±0.001</sub> |   43.17<sub>±00.03</sub> + 40.43<sub>±00.21</sub> |  40.38<sub>±00.37</sub> |
|                   C/clang |  0.997<sub>±0.000</sub> |     2.15<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  36.62<sub>±00.05</sub> |
|                     C/gcc |  1.101<sub>±0.016</sub> |     2.10<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  40.24<sub>±00.60</sub> |
|                 Nim/clang |  1.103<sub>±0.001</sub> |     2.06<sub>±00.02</sub> + 5.79<sub>±00.03</sub> |  44.95<sub>±00.27</sub> |
|                   Crystal |  1.107<sub>±0.003</sub> |     3.61<sub>±00.04</sub> + 1.26<sub>±00.03</sub> |  44.93<sub>±00.29</sub> |
|                    D/ldc2 |  1.175<sub>±0.003</sub> |     3.68<sub>±00.03</sub> + 3.41<sub>±00.00</sub> |  48.49<sub>±00.44</sub> |
|                   Nim/gcc |  1.364<sub>±0.002</sub> |     1.75<sub>±00.04</sub> + 4.96<sub>±00.01</sub> |  55.70<sub>±00.32</sub> |
|              Ruby (--jit) |  1.427<sub>±0.002</sub> |   15.10<sub>±00.16</sub> + 73.37<sub>±00.56</sub> |  55.92<sub>±00.37</sub> |
|                      Java |  1.518<sub>±0.005</sub> |  41.05<sub>±00.10</sub> + 210.65<sub>±19.06</sub> |  60.66<sub>±00.44</sub> |
|                   V/clang |  1.534<sub>±0.001</sub> |  2.43<sub>±00.01</sub> + 2386.75<sub>±01.65</sub> |  57.81<sub>±00.29</sub> |
|                     V/gcc |  1.575<sub>±0.000</sub> |  2.43<sub>±00.03</sub> + 2385.17<sub>±00.74</sub> |  57.34<sub>±00.19</sub> |
|                     Scala |  1.604<sub>±0.001</sub> |  70.48<sub>±00.15</sub> + 276.28<sub>±02.12</sub> |  66.81<sub>±00.54</sub> |
|                        Go |  1.622<sub>±0.002</sub> |     4.34<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  67.66<sub>±00.22</sub> |
|                      Ruby |  1.642<sub>±0.005</sub> |   11.69<sub>±00.04</sub> + 42.50<sub>±00.30</sub> |  64.29<sub>±00.22</sub> |
|                Vala/clang |  1.643<sub>±0.001</sub> |     5.68<sub>±00.03</sub> + 0.10<sub>±00.03</sub> |  63.09<sub>±00.70</sub> |
|                  Vala/gcc |  1.644<sub>±0.001</sub> |     5.66<sub>±00.01</sub> + 0.08<sub>±00.05</sub> |  62.92<sub>±00.40</sub> |
|                Kotlin/JVM |  1.665<sub>±0.003</sub> |  44.48<sub>±00.20</sub> + 314.13<sub>±03.17</sub> |  67.36<sub>±00.44</sub> |
|       C++/g++ (libcrypto) |  1.711<sub>±0.003</sub> |     6.05<sub>±00.16</sub> + 0.76<sub>±00.03</sub> |  68.53<sub>±00.38</sub> |
|   C++/clang++ (libcrypto) |  1.713<sub>±0.002</sub> |     5.51<sub>±00.04</sub> + 0.73<sub>±00.02</sub> |  69.23<sub>±00.37</sub> |
|       Perl (MIME::Base64) |  1.863<sub>±0.044</sub> |    14.57<sub>±00.04</sub> + 0.12<sub>±00.03</sub> |  75.20<sub>±01.07</sub> |
|              F#/.NET Core |  2.038<sub>±0.018</sub> |   38.32<sub>±00.07</sub> + 11.92<sub>±00.65</sub> |  80.42<sub>±00.77</sub> |
|              C#/.NET Core |  2.181<sub>±0.021</sub> |   33.77<sub>±00.03</sub> + 12.54<sub>±02.09</sub> |  85.47<sub>±00.28</sub> |
|                     D/gdc |  2.382<sub>±0.001</sub> |     7.44<sub>±00.04</sub> + 3.36<sub>±00.00</sub> | 103.50<sub>±00.94</sub> |
|                  Go/gccgo |  2.949<sub>±0.007</sub> |    24.81<sub>±00.17</sub> + 0.00<sub>±00.00</sub> | 139.67<sub>±00.50</sub> |
|                     Julia |  3.079<sub>±0.006</sub> |  271.96<sub>±00.08</sub> + 97.24<sub>±00.20</sub> | 124.20<sub>±01.58</sub> |
|               Python/pypy |  3.236<sub>±0.002</sub> |   58.92<sub>±00.06</sub> + 31.49<sub>±00.17</sub> | 141.88<sub>±00.91</sub> |
|                     D/dmd |  3.387<sub>±0.004</sub> |     3.17<sub>±00.02</sub> + 3.84<sub>±00.02</sub> | 141.65<sub>±00.88</sub> |
|                       Tcl |  3.438<sub>±0.001</sub> |     5.20<sub>±00.02</sub> + 0.00<sub>±00.00</sub> | 137.53<sub>±00.49</sub> |
|                    Python |  3.512<sub>±0.001</sub> |    10.78<sub>±00.16</sub> + 0.07<sub>±00.07</sub> | 133.94<sub>±00.44</sub> |
|    Ruby/truffleruby (JVM) |  3.758<sub>±0.016</sub> | 374.30<sub>±05.93</sub> + 247.06<sub>±24.79</sub> | 189.38<sub>±03.07</sub> |
|                    Racket |  3.885<sub>±0.006</sub> |   95.36<sub>±00.20</sub> + 21.80<sub>±00.40</sub> | 154.27<sub>±00.56</sub> |
|                   C#/Mono |  4.632<sub>±0.005</sub> |   26.39<sub>±00.04</sub> + 18.66<sub>±00.02</sub> | 190.60<sub>±01.36</sub> |
|                Ruby/jruby |  6.005<sub>±0.008</sub> | 195.03<sub>±02.11</sub> + 165.97<sub>±10.93</sub> | 263.57<sub>±01.49</sub> |
|          Ruby/truffleruby |  8.324<sub>±0.018</sub> | 201.01<sub>±02.82</sub> + 554.96<sub>±13.20</sub> | 400.06<sub>±01.94</sub> |
| Perl (MIME::Base64::Perl) | 10.148<sub>±0.055</sub> |    15.93<sub>±00.11</sub> + 0.26<sub>±00.04</sub> | 448.06<sub>±03.65</sub> |

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
|    C++/clang++ (simdjson On-Demand) |  0.060<sub>±0.000</sub> |    112.46<sub>±00.06</sub> + 60.05<sub>±00.06</sub> |   2.51<sub>±00.01</sub> |
|        C++/g++ (simdjson On-Demand) |  0.061<sub>±0.000</sub> |    113.66<sub>±00.03</sub> + 59.81<sub>±00.00</sub> |   2.57<sub>±00.02</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.083<sub>±0.000</sub> |     112.54<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.37<sub>±00.03</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.087<sub>±0.000</sub> |     113.29<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.53<sub>±00.04</sub> |
|         C++/clang++ (DAW JSON Link) |  0.093<sub>±0.000</sub> |     112.47<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.90<sub>±00.07</sub> |
|             C++/g++ (DAW JSON Link) |  0.093<sub>±0.000</sub> |     113.30<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.88<sub>±00.03</sub> |
|                 Rust (Serde Custom) |  0.098<sub>±0.001</sub> |     111.40<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   4.14<sub>±00.04</sub> |
|          C++/clang++ (simdjson DOM) |  0.102<sub>±0.000</sub> |   112.55<sub>±00.09</sub> + 177.09<sub>±00.06</sub> |   4.62<sub>±00.02</sub> |
|              C++/g++ (simdjson DOM) |  0.108<sub>±0.000</sub> |   113.54<sub>±00.06</sub> + 172.73<sub>±00.13</sub> |   4.90<sub>±00.02</sub> |
|                  Rust (Serde Typed) |  0.111<sub>±0.000</sub> |    111.71<sub>±00.05</sub> + 11.05<sub>±00.07</sub> |   4.62<sub>±00.04</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.131<sub>±0.000</sub> |    112.93<sub>±00.04</sub> + 61.22<sub>±00.00</sub> |   5.43<sub>±00.05</sub> |
|                 C++/clang++ (gason) |  0.139<sub>±0.000</sub> |    112.57<sub>±00.02</sub> + 96.97<sub>±00.06</sub> |   5.67<sub>±00.04</sub> |
|                     C++/g++ (gason) |  0.140<sub>±0.000</sub> |    113.17<sub>±00.06</sub> + 96.93<sub>±00.04</sub> |   5.55<sub>±00.03</sub> |
|                 C++/g++ (RapidJSON) |  0.152<sub>±0.000</sub> |   113.21<sub>±00.04</sub> + 128.90<sub>±00.03</sub> |   6.47<sub>±00.04</sub> |
|              Scala (jsoniter-scala) |  0.157<sub>±0.002</sub> |    290.75<sub>±00.24</sub> + 20.43<sub>±00.24</sub> |   8.36<sub>±00.07</sub> |
|                   Go (rjson custom) |  0.196<sub>±0.000</sub> |     113.58<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   7.57<sub>±00.02</sub> |
|             C++/clang++ (RapidJSON) |  0.200<sub>±0.000</sub> |   112.47<sub>±00.04</sub> + 128.91<sub>±00.09</sub> |   8.55<sub>±00.08</sub> |
|                          Go (Sonic) |  0.208<sub>±0.002</sub> |     122.51<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |   8.94<sub>±00.09</sub> |
|         C++/g++ (RapidJSON Precise) |  0.217<sub>±0.000</sub> |   113.21<sub>±00.06</sub> + 122.47<sub>±01.29</sub> |   9.28<sub>±00.08</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.219<sub>±0.000</sub> |    113.03<sub>±00.08</sub> + 80.70<sub>±00.00</sub> |   9.26<sub>±00.03</sub> |
|                          Go (rjson) |  0.224<sub>±0.000</sub> |     113.61<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |   8.70<sub>±00.02</sub> |
|                                 Zig |  0.241<sub>±0.000</sub> |    111.03<sub>±00.02</sub> + 39.25<sub>±00.00</sub> |  10.37<sub>±00.07</sub> |
|                  Go (goccy/go-json) |  0.266<sub>±0.000</sub> |     114.29<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  10.56<sub>±00.02</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.285<sub>±0.000</sub> |   112.48<sub>±00.03</sub> + 128.91<sub>±00.03</sub> |  12.50<sub>±00.10</sub> |
|             C++/g++ (RapidJSON SAX) |  0.346<sub>±0.001</sub> |     113.11<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  15.32<sub>±00.06</sub> |
|                      C/clang (yajl) |  0.350<sub>±0.001</sub> |     110.97<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  15.14<sub>±00.06</sub> |
|                        C/gcc (yajl) |  0.353<sub>±0.001</sub> |     110.95<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  15.22<sub>±00.06</sub> |
|                C++/g++ (Boost.JSON) |  0.363<sub>±0.000</sub> |   113.32<sub>±00.04</sub> + 308.09<sub>±00.02</sub> |  15.44<sub>±00.10</sub> |
|            C++/clang++ (Boost.JSON) |  0.371<sub>±0.001</sub> |   112.68<sub>±00.03</sub> + 308.09<sub>±00.03</sub> |  15.74<sub>±00.10</sub> |
|                   Nim/clang (jsony) |  0.392<sub>±0.000</sub> |   111.56<sub>±00.05</sub> + 146.12<sub>±00.09</sub> |  16.46<sub>±00.12</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.401<sub>±0.001</sub> |     113.12<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  17.92<sub>±00.23</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.408<sub>±0.001</sub> |     194.77<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  17.40<sub>±00.07</sub> |
|                     Nim/gcc (jsony) |  0.412<sub>±0.000</sub> |   111.23<sub>±00.03</sub> + 154.69<sub>±00.03</sub> |  17.46<sub>±00.15</sub> |
|                             Node.js |  0.470<sub>±0.004</sub> |   152.69<sub>±00.04</sub> + 195.96<sub>±00.68</sub> |  22.05<sub>±00.12</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.489<sub>±0.001</sub> |     194.79<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  21.84<sub>±00.23</sub> |
|                       Go (jsoniter) |  0.513<sub>±0.000</sub> |     114.27<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  20.75<sub>±00.17</sub> |
|                Rust (Serde Untyped) |  0.532<sub>±0.001</sub> |   111.73<sub>±00.03</sub> + 839.98<sub>±00.00</sub> |  22.14<sub>±00.20</sub> |
|     C#/.NET Core (System.Text.Json) |  0.543<sub>±0.002</sub> |   489.53<sub>±00.11</sub> + 140.62<sub>±00.22</sub> |  24.07<sub>±00.23</sub> |
|                     Java (DSL-JSON) |  0.568<sub>±0.007</sub> |   262.39<sub>±00.11</sub> + 225.35<sub>±19.01</sub> |  29.30<sub>±00.32</sub> |
|                         Python/pypy |  0.608<sub>±0.002</sub> |   279.98<sub>±00.08</sub> + 125.71<sub>±00.04</sub> |  26.46<sub>±00.26</sub> |
|                               V/gcc |  0.615<sub>±0.001</sub> |   111.47<sub>±00.04</sub> + 496.21<sub>±00.00</sub> |  25.70<sub>±00.23</sub> |
|                             V/clang |  0.637<sub>±0.001</sub> |   111.87<sub>±00.01</sub> + 495.83<sub>±00.00</sub> |  26.74<sub>±00.19</sub> |
|                Nim/gcc (Packedjson) |  0.638<sub>±0.003</sub> |   111.93<sub>±00.02</sub> + 294.16<sub>±00.00</sub> |  27.28<sub>±00.40</sub> |
|                      Crystal (Pull) |  0.638<sub>±0.002</sub> |    113.26<sub>±00.02</sub> + 18.44<sub>±00.00</sub> |  28.10<sub>±00.40</sub> |
|                    Crystal (Schema) |  0.656<sub>±0.001</sub> |    113.23<sub>±00.03</sub> + 48.86<sub>±00.13</sub> |  28.63<sub>±00.07</sub> |
|              Nim/clang (Packedjson) |  0.662<sub>±0.001</sub> |   112.23<sub>±00.01</sub> + 294.16<sub>±00.00</sub> |  28.31<sub>±00.17</sub> |
|             Perl (Cpanel::JSON::XS) |  0.765<sub>±0.003</sub> |   125.17<sub>±00.05</sub> + 402.77<sub>±00.00</sub> |  31.84<sub>±00.14</sub> |
|                                  Go |  0.813<sub>±0.001</sub> |     113.95<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  33.60<sub>±00.06</sub> |
|                                 PHP |  0.813<sub>±0.001</sub> |   128.26<sub>±00.08</sub> + 517.83<sub>±00.03</sub> |  34.57<sub>±00.20</sub> |
|                             Crystal |  0.924<sub>±0.004</sub> |   113.21<sub>±00.02</sub> + 392.50<sub>±00.03</sub> |  39.68<sub>±00.33</sub> |
|                             Nim/gcc |  1.037<sub>±0.002</sub> |  111.91<sub>±00.03</sub> + 1001.34<sub>±00.00</sub> |  43.13<sub>±00.12</sub> |
|                        C#/.NET Core |  1.056<sub>±0.004</sub> |   495.03<sub>±00.30</sub> + 272.99<sub>±00.16</sub> |  50.49<sub>±00.23</sub> |
|                           Nim/clang |  1.076<sub>±0.002</sub> |   112.23<sub>±00.00</sub> + 999.02<sub>±00.00</sub> |  44.69<sub>±00.27</sub> |
|                    C++/g++ (json-c) |  1.159<sub>±0.003</sub> |  113.42<sub>±00.05</sub> + 1216.05<sub>±00.01</sub> |  47.99<sub>±00.30</sub> |
|                C++/clang++ (json-c) |  1.160<sub>±0.007</sub> |  112.70<sub>±00.07</sub> + 1216.04<sub>±00.01</sub> |  48.35<sub>±00.66</sub> |
|                             Clojure |  1.161<sub>±0.019</sub> |   452.13<sub>±02.50</sub> + 587.46<sub>±09.25</sub> |  61.05<sub>±00.99</sub> |
|              C++/clang++ (Nlohmann) |  1.176<sub>±0.001</sub> |   112.64<sub>±00.03</sub> + 360.11<sub>±00.02</sub> |  50.39<sub>±00.68</sub> |
|                        Ruby (--jit) |  1.280<sub>±0.005</sub> |   127.59<sub>±00.08</sub> + 212.39<sub>±00.01</sub> |  53.84<sub>±00.51</sub> |
|                            Go/gccgo |  1.290<sub>±0.004</sub> |     138.88<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |  53.03<sub>±00.36</sub> |
|                  C++/g++ (Nlohmann) |  1.327<sub>±0.003</sub> |   113.27<sub>±00.06</sub> + 448.05<sub>±00.01</sub> |  56.39<sub>±00.35</sub> |
|                                Ruby |  1.328<sub>±0.006</sub> |   121.40<sub>±00.05</sub> + 212.77<sub>±00.02</sub> |  55.73<sub>±00.73</sub> |
|                 CPython (UltraJSON) |  1.331<sub>±0.004</sub> |   123.14<sub>±00.05</sub> + 497.28<sub>±01.55</sub> |  51.19<sub>±00.22</sub> |
|                              Python |  1.374<sub>±0.002</sub> |   120.99<sub>±00.03</sub> + 325.95<sub>±00.01</sub> |  55.23<sub>±00.37</sub> |
|     F#/.NET Core (System.Text.Json) |  1.496<sub>±0.003</sub> |   498.18<sub>±00.08</sub> + 232.75<sub>±04.63</sub> |  68.50<sub>±00.69</sub> |
|                         Ruby (YAJL) |  1.719<sub>±0.009</sub> |   121.49<sub>±00.11</sub> + 218.39<sub>±00.03</sub> |  72.78<sub>±00.52</sub> |
|                              D/ldc2 |  1.723<sub>±0.003</sub> |   112.86<sub>±00.06</sub> + 708.76<sub>±00.04</sub> |  72.08<sub>±00.65</sub> |
|                             C#/Mono |  1.809<sub>±0.013</sub> |    252.32<sub>±00.17</sub> + 31.51<sub>±00.01</sub> |  78.45<sub>±00.96</sub> |
|                             Haskell |  1.982<sub>±0.007</sub> |   115.63<sub>±00.14</sub> + 723.73<sub>±00.48</sub> |  84.53<sub>±00.77</sub> |
|                           Rust (jq) |  2.529<sub>±0.003</sub> |   113.39<sub>±00.06</sub> + 902.98<sub>±01.54</sub> | 106.08<sub>±00.37</sub> |
|        C++/g++ (Boost.PropertyTree) |  2.611<sub>±0.008</sub> |  113.14<sub>±00.04</sub> + 1440.09<sub>±00.04</sub> | 111.72<sub>±00.36</sub> |
|    C++/clang++ (Boost.PropertyTree) |  2.671<sub>±0.005</sub> |  195.01<sub>±00.04</sub> + 1232.80<sub>±00.03</sub> | 112.86<sub>±00.70</sub> |
|                          Ruby/jruby |  2.884<sub>±0.022</sub> |   464.98<sub>±06.74</sub> + 972.45<sub>±49.49</sub> | 148.64<sub>±02.99</sub> |
|                               D/dmd |  3.073<sub>±0.003</sub> |   113.16<sub>±00.04</sub> + 708.78<sub>±00.07</sub> | 131.07<sub>±00.23</sub> |
|                          Vala/clang |  3.165<sub>±0.007</sub> |   115.04<sub>±00.04</sub> + 980.08<sub>±00.03</sub> | 137.65<sub>±00.31</sub> |
|                            Vala/gcc |  3.166<sub>±0.011</sub> |   115.06<sub>±00.03</sub> + 980.07<sub>±00.03</sub> | 136.93<sub>±00.87</sub> |
|                               D/gdc |  3.569<sub>±0.004</sub> |   116.68<sub>±00.06</sub> + 680.99<sub>±00.12</sub> | 151.81<sub>±01.75</sub> |
|                              Racket |  3.818<sub>±0.022</sub> |   222.34<sub>±00.45</sub> + 261.38<sub>±27.93</sub> | 159.23<sub>±01.29</sub> |
|                   Perl (JSON::Tiny) |  9.050<sub>±0.121</sub> |   125.59<sub>±00.05</sub> + 528.92<sub>±00.02</sub> | 394.96<sub>±03.88</sub> |
|                    Ruby/truffleruby | 10.718<sub>±0.048</sub> | 522.13<sub>±27.57</sub> + 1942.99<sub>±149.89</sub> | 617.74<sub>±02.74</sub> |
|              Ruby/truffleruby (JVM) | 11.011<sub>±0.153</sub> |  474.89<sub>±08.90</sub> + 2201.50<sub>±87.97</sub> | 693.97<sub>±13.20</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                Language |                   Time, s |                                        Memory, MiB |                  Energy, J |
| :---------------------- | ------------------------: | -------------------------------------------------: | -------------------------: |
|         D/ldc2 (lubeck) |    0.032<sub>±0.001</sub> |    40.38<sub>±00.48</sub> + 23.61<sub>±00.41</sub> |      3.61<sub>±00.09</sub> |
|   Nim/gcc (Arraymancer) |    0.064<sub>±0.004</sub> |     5.41<sub>±00.06</sub> + 57.41<sub>±00.10</sub> |      5.27<sub>±00.20</sub> |
|          Python (NumPy) |    0.065<sub>±0.000</sub> |    33.37<sub>±00.06</sub> + 57.87<sub>±00.04</sub> |      5.54<sub>±00.01</sub> |
|             Java (ND4J) |    0.076<sub>±0.001</sub> |   114.91<sub>±01.04</sub> + 92.22<sub>±00.01</sub> |      5.97<sub>±00.05</sub> |
|          Rust (ndarray) |    0.089<sub>±0.001</sub> |     2.50<sub>±00.04</sub> + 68.53<sub>±00.00</sub> |      6.03<sub>±00.08</sub> |
|      Julia (threads: 2) |    0.092<sub>±0.000</sub> |   300.38<sub>±00.45</sub> + 47.53<sub>±00.53</sub> |      5.54<sub>±00.03</sub> |
| Nim/clang (Arraymancer) |    0.128<sub>±0.020</sub> |     6.05<sub>±00.18</sub> + 57.54<sub>±00.17</sub> |      8.78<sub>±01.10</sub> |
|      Julia (threads: 1) |    0.142<sub>±0.000</sub> |   299.90<sub>±00.18</sub> + 47.76<sub>±00.23</sub> |      6.91<sub>±00.04</sub> |
|         C++/g++ (Eigen) |    0.145<sub>±0.000</sub> |     4.49<sub>±00.06</sub> + 85.26<sub>±00.00</sub> |      7.14<sub>±00.03</sub> |
|     C++/clang++ (Eigen) |    0.145<sub>±0.000</sub> |     4.72<sub>±00.03</sub> + 85.37<sub>±00.00</sub> |      7.07<sub>±00.04</sub> |
|   V/clang (VSL + CBLAS) |    0.254<sub>±0.004</sub> |     7.13<sub>±00.03</sub> + 51.89<sub>±00.00</sub> |     17.98<sub>±00.26</sub> |
|           V/clang (VSL) |    0.258<sub>±0.002</sub> |     7.21<sub>±00.17</sub> + 51.57<sub>±00.13</sub> |     17.89<sub>±00.11</sub> |
|     V/gcc (VSL + CBLAS) |    0.459<sub>±0.002</sub> |     7.25<sub>±00.06</sub> + 51.57<sub>±00.00</sub> |     34.63<sub>±00.06</sub> |
|             V/gcc (VSL) |    0.466<sub>±0.001</sub> |     6.90<sub>±00.04</sub> + 51.89<sub>±00.00</sub> |     31.75<sub>±00.07</sub> |
|         Julia (no BLAS) |    1.146<sub>±0.023</sub> |   271.40<sub>±00.23</sub> + 52.30<sub>±00.01</sub> |     49.02<sub>±00.55</sub> |
|                  D/ldc2 |    1.714<sub>±0.001</sub> |     3.43<sub>±00.04</sub> + 70.44<sub>±00.00</sub> |     62.98<sub>±00.24</sub> |
|                   D/gdc |    1.866<sub>±0.001</sub> |     7.40<sub>±00.14</sub> + 70.16<sub>±00.01</sub> |     72.94<sub>±00.11</sub> |
|                   D/dmd |    1.874<sub>±0.001</sub> |     3.32<sub>±00.03</sub> + 70.46<sub>±00.03</sub> |     70.33<sub>±00.49</sub> |
|                   C/gcc |    3.025<sub>±0.000</sub> |     1.53<sub>±00.02</sub> + 68.65<sub>±00.02</sub> |    109.57<sub>±01.18</sub> |
|                   V/gcc |    3.027<sub>±0.001</sub> |     2.60<sub>±00.03</sub> + 68.58<sub>±00.00</sub> |    112.33<sub>±00.33</sub> |
|              Vala/clang |    3.057<sub>±0.000</sub> |     5.52<sub>±00.03</sub> + 68.32<sub>±00.00</sub> |    104.60<sub>±00.31</sub> |
|                 V/clang |    3.058<sub>±0.000</sub> |     2.90<sub>±00.06</sub> + 68.58<sub>±00.00</sub> |    104.38<sub>±00.16</sub> |
|                 C/clang |    3.060<sub>±0.000</sub> |     1.55<sub>±00.02</sub> + 68.63<sub>±00.04</sub> |    104.48<sub>±00.15</sub> |
|                    Rust |    3.061<sub>±0.000</sub> |     2.14<sub>±00.07</sub> + 68.69<sub>±00.00</sub> |    105.19<sub>±00.52</sub> |
|                     Zig |    3.063<sub>±0.001</sub> |     1.92<sub>±00.01</sub> + 68.58<sub>±00.00</sub> |    108.31<sub>±00.18</sub> |
|                 Nim/gcc |    3.086<sub>±0.001</sub> |     2.60<sub>±00.01</sub> + 58.91<sub>±01.16</sub> |    114.12<sub>±00.58</sub> |
|                   Swift |    3.090<sub>±0.000</sub> |     7.99<sub>±00.06</sub> + 68.69<sub>±00.00</sub> |    109.94<sub>±00.59</sub> |
|               Nim/clang |    3.116<sub>±0.001</sub> |     2.88<sub>±00.01</sub> + 57.75<sub>±00.00</sub> |    107.30<sub>±00.87</sub> |
|                Vala/gcc |    3.123<sub>±0.000</sub> |     5.49<sub>±00.10</sub> + 68.32<sub>±00.00</sub> |    114.09<sub>±00.15</sub> |
|                      Go |    3.145<sub>±0.000</sub> |      4.09<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    112.66<sub>±01.59</sub> |
|                 Crystal |    3.147<sub>±0.001</sub> |     3.63<sub>±00.06</sub> + 60.02<sub>±00.02</sub> |    115.55<sub>±00.61</sub> |
|                Go/gccgo |    3.150<sub>±0.000</sub> |     24.19<sub>±00.16</sub> + 0.00<sub>±00.00</sub> |    110.47<sub>±00.13</sub> |
|                    Java |    3.165<sub>±0.002</sub> |    40.98<sub>±00.16</sub> + 74.30<sub>±00.20</sub> |    117.37<sub>±01.36</sub> |
|              Kotlin/JVM |    3.209<sub>±0.003</sub> |    42.32<sub>±00.21</sub> + 73.03<sub>±00.22</sub> |    129.95<sub>±00.33</sub> |
|                 Node.js |    3.252<sub>±0.002</sub> |    51.38<sub>±00.26</sub> + 70.87<sub>±00.47</sub> |    130.78<sub>±00.37</sub> |
|             Python/pypy |    3.254<sub>±0.001</sub> |    59.96<sub>±00.07</sub> + 68.90<sub>±00.03</sub> |    135.03<sub>±00.17</sub> |
|                   Scala |    3.332<sub>±0.003</sub> |   71.86<sub>±00.08</sub> + 153.79<sub>±00.24</sub> |    120.95<sub>±00.68</sub> |
|            C#/.NET Core |    4.893<sub>±0.001</sub> |    34.69<sub>±00.07</sub> + 68.91<sub>±00.03</sub> |    196.73<sub>±00.66</sub> |
|                 C#/Mono |    7.391<sub>±0.000</sub> |    26.02<sub>±00.07</sub> + 69.48<sub>±00.01</sub> |    297.35<sub>±00.51</sub> |
|        Ruby/truffleruby |   18.156<sub>±3.646</sub> |  360.35<sub>±12.57</sub> + 574.17<sub>±41.24</sub> |   647.87<sub>±107.81</sub> |
|  Ruby/truffleruby (JVM) |   25.014<sub>±0.847</sub> |  407.30<sub>±16.57</sub> + 334.68<sub>±58.11</sub> |    875.73<sub>±22.67</sub> |
|                    Perl |  149.225<sub>±1.755</sub> |    8.52<sub>±00.04</sub> + 379.63<sub>±00.02</sub> |   6447.37<sub>±59.99</sub> |
|                  Python |  155.211<sub>±1.106</sub> |    10.91<sub>±00.03</sub> + 68.84<sub>±00.00</sub> |   7060.84<sub>±46.09</sub> |
|            Ruby (--jit) |  156.368<sub>±0.039</sub> |    21.96<sub>±00.04</sub> + 68.35<sub>±00.02</sub> |   6925.51<sub>±35.94</sub> |
|                    Ruby |  195.516<sub>±0.462</sub> |    11.75<sub>±00.07</sub> + 69.19<sub>±00.01</sub> |   8795.03<sub>±25.92</sub> |
|                     Tcl |  202.975<sub>±0.793</sub> |    7.47<sub>±00.02</sub> + 400.44<sub>±00.00</sub> |   9311.02<sub>±86.81</sub> |
|              Ruby/jruby | 361.315<sub>±19.249</sub> | 273.70<sub>±12.97</sub> + 1142.23<sub>±73.73</sub> | 14778.53<sub>±653.23</sub> |

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
|                    Zig | 0.056<sub>±0.000</sub> |    1.03<sub>±00.02</sub> + 52.80<sub>±00.19</sub> |   2.34<sub>±00.03</sub> |
|            C++/clang++ | 0.062<sub>±0.000</sub> |    3.17<sub>±00.03</sub> + 54.80<sub>±00.00</sub> |   2.38<sub>±00.02</sub> |
|                C++/g++ | 0.063<sub>±0.000</sub> |    3.67<sub>±00.07</sub> + 70.55<sub>±00.25</sub> |   2.44<sub>±00.02</sub> |
|                     Go | 0.072<sub>±0.000</sub> |     3.08<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.06<sub>±00.04</sub> |
|                V/clang | 0.095<sub>±0.000</sub> |   2.14<sub>±00.23</sub> + 211.15<sub>±00.64</sub> |   3.91<sub>±00.04</sub> |
|                  V/gcc | 0.099<sub>±0.000</sub> |   1.95<sub>±00.03</sub> + 201.60<sub>±00.17</sub> |   4.10<sub>±00.03</sub> |
|                   Rust | 0.101<sub>±0.000</sub> |    2.09<sub>±00.11</sub> + 74.71<sub>±00.77</sub> |   3.98<sub>±00.05</sub> |
|                   Java | 0.130<sub>±0.004</sub> |  39.84<sub>±00.17</sub> + 123.02<sub>±05.25</sub> |   7.33<sub>±00.26</sub> |
|                Crystal | 0.138<sub>±0.000</sub> |    3.71<sub>±00.02</sub> + 88.43<sub>±00.00</sub> |   5.58<sub>±00.05</sub> |
|                Node.js | 0.218<sub>±0.002</sub> |  42.17<sub>±00.00</sub> + 147.09<sub>±02.74</sub> |  10.79<sub>±00.07</sub> |
|                  Scala | 0.228<sub>±0.003</sub> |  72.13<sub>±00.08</sub> + 230.29<sub>±00.88</sub> |  13.20<sub>±00.22</sub> |
|              Nim/clang | 0.278<sub>±0.000</sub> |   2.10<sub>±00.10</sub> + 587.68<sub>±01.03</sub> |  10.75<sub>±00.04</sub> |
|                Nim/gcc | 0.287<sub>±0.000</sub> |   1.90<sub>±00.02</sub> + 615.91<sub>±00.00</sub> |  10.83<sub>±00.07</sub> |
|             Lua/luajit | 0.298<sub>±0.001</sub> |   1.33<sub>±00.02</sub> + 156.66<sub>±00.63</sub> |  12.00<sub>±00.08</sub> |
|            Python/pypy | 0.616<sub>±0.003</sub> |  58.72<sub>±00.13</sub> + 249.30<sub>±00.15</sub> |  25.12<sub>±00.16</sub> |
|                  Julia | 0.651<sub>±0.001</sub> | 271.49<sub>±00.14</sub> + 370.21<sub>±01.14</sub> |  25.06<sub>±00.21</sub> |
|                 Racket | 0.752<sub>±0.002</sub> | 109.49<sub>±00.82</sub> + 247.11<sub>±00.45</sub> |  29.59<sub>±00.17</sub> |
|       Ruby/truffleruby | 0.890<sub>±0.012</sub> | 203.57<sub>±01.55</sub> + 800.19<sub>±11.11</sub> |  60.49<sub>±00.76</sub> |
|                    Lua | 1.142<sub>±0.006</sub> |   2.69<sub>±00.04</sub> + 283.50<sub>±01.03</sub> |  46.78<sub>±00.35</sub> |
|           Ruby (--jit) | 1.187<sub>±0.002</sub> |  22.98<sub>±00.02</sub> + 163.71<sub>±00.14</sub> |  47.95<sub>±00.43</sub> |
| Ruby/truffleruby (JVM) | 1.378<sub>±0.088</sub> | 376.75<sub>±09.21</sub> + 495.42<sub>±42.25</sub> |  89.49<sub>±05.72</sub> |
|                   Ruby | 1.860<sub>±0.003</sub> |  11.47<sub>±00.07</sub> + 172.19<sub>±00.35</sub> |  76.75<sub>±00.60</sub> |
|             Ruby/jruby | 2.314<sub>±0.075</sub> | 197.13<sub>±08.38</sub> + 521.12<sub>±52.24</sub> | 124.24<sub>±05.36</sub> |
|                 Python | 2.530<sub>±0.014</sub> |  10.84<sub>±00.04</sub> + 181.69<sub>±01.57</sub> | 107.30<sub>±01.03</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 8.0.300                         |
| C#/.NET Core     | 4.10.0-3.24216.12 (3af0081a)    |
| C#/Mono          | 6.12.0.200                      |
| Chez Scheme      | 9.5.8                           |
| Clojure          | "1.11.3"                        |
| Crystal          | 1.12.1                          |
| D/dmd            | v2.108.1                        |
| D/gdc            | 13.2.0                          |
| D/ldc2           | 1.38.0                          |
| Elixir           | 1.14.0                          |
| F#/.NET Core     | 12.8.300.0 for F# 8.0           |
| Go               | go1.22.3                        |
| Go/gccgo         | 13.2.0                          |
| Haskell          | 9.8.2                           |
| Idris 2          | latest                          |
| Java             | 22.0.1                          |
| Julia            | v"1.10.3"                       |
| Kotlin           | 2.0.0                           |
| Lua              | 5.4.6                           |
| Lua/luajit       | 2.1.1710398010                  |
| MLton            | 20210117                        |
| Nim              | 2.0.4                           |
| Node.js          | v22.2.0                         |
| OCaml            | 5.2.0                           |
| PHP              | 8.2.18                          |
| Perl             | v5.38.2                         |
| Python           | 3.11.9                          |
| Python/pypy      | 7.3.16-final0 for Python 3.10.14 |
| Racket           | "8.13"                          |
| Ruby             | 3.3.1p55                        |
| Ruby/jruby       | 9.4.7.0                         |
| Ruby/truffleruby | 24.0.1                          |
| Rust             | 1.78.0                          |
| Scala            | 3.4.2                           |
| Swift            | 5.10                            |
| Tcl              | 8.6                             |
| V                | 0.4.6 736067d                   |
| Vala             | 0.56.17                         |
| Zig              | 0.12.0                          |
| clang/clang++    | 16.0.6 (27)                     |
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
