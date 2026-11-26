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

My other benchmarks: [jit-benchmarks](https://github.com/kostya/jit-benchmarks), [crystal-benchmarks-game](https://github.com/kostya/crystal-benchmarks-game), [LangArena](https://kostya.github.io/LangArena/)

## Measurements

The measured values are:

 - time spent for the benchmark execution (loading required data and code self-testing are not measured);
 - memory consumption of the benchmark process, reported as `base` + `increase`, where `base` is the RSS before the benchmark and `increase` is the peak increase of the RSS during the benchmark;
 - energy consumption of the CPU package during the benchmark: PP0 (cores) + PP1 (uncores like GPU) + DRAM. Currently, only Intel CPU are supported via the powercap interface.

All values are presented as: `median`<sub>±`median absolute deviation`</sub>.

UPDATE: 2026-11-26

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|               Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :--------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|         Scala (Staged) |   0.309<sub>±0.015</sub> |   232.12<sub>±03.48</sub> + 5.22<sub>±01.44</sub> |    19.55<sub>±01.00</sub> |
|  C# (Staged)/.NET Core |   0.330<sub>±0.002</sub> |    37.96<sub>±00.18</sub> + 0.10<sub>±00.00</sub> |    17.46<sub>±00.28</sub> |
|        Racket (Staged) |   0.886<sub>±0.000</sub> |   113.02<sub>±00.22</sub> + 0.05<sub>±00.01</sub> |    34.24<sub>±00.03</sub> |
|                  V/gcc |   0.936<sub>±0.000</sub> |     2.02<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    38.60<sub>±00.07</sub> |
|                   Rust |   1.015<sub>±0.001</sub> |     2.07<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    43.48<sub>±00.45</sub> |
|                C++/g++ |   1.020<sub>±0.001</sub> |     3.89<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    43.16<sub>±00.42</sub> |
|                 D/ldc2 |   1.060<sub>±0.001</sub> |     3.45<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    44.91<sub>±00.08</sub> |
|                C/clang |   1.137<sub>±0.000</sub> |     1.73<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    48.59<sub>±00.46</sub> |
|                  D/gdc |   1.137<sub>±0.000</sub> |     9.07<sub>±00.22</sub> + 0.00<sub>±00.00</sub> |    50.13<sub>±00.57</sub> |
|            C++/clang++ |   1.144<sub>±0.001</sub> |     3.40<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    47.89<sub>±00.61</sub> |
|                  C/gcc |   1.147<sub>±0.001</sub> |     1.75<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    48.63<sub>±00.29</sub> |
|                    Zig |   1.165<sub>±0.000</sub> |     2.11<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    68.20<sub>±00.58</sub> |
|             Kotlin/JVM |   1.178<sub>±0.002</sub> |    49.57<sub>±00.15</sub> + 0.68<sub>±00.04</sub> |    48.85<sub>±00.13</sub> |
|                   Java |   1.186<sub>±0.003</sub> |    47.05<sub>±00.11</sub> + 0.40<sub>±00.03</sub> |    49.06<sub>±00.42</sub> |
|                Nim/gcc |   1.202<sub>±0.000</sub> |     2.09<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    49.43<sub>±00.05</sub> |
|               Vala/gcc |   1.207<sub>±0.000</sub> |     6.26<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    50.17<sub>±00.35</sub> |
|             Vala/clang |   1.234<sub>±0.001</sub> |     6.24<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    52.24<sub>±00.17</sub> |
|           C#/.NET Core |   1.262<sub>±0.001</sub> |    38.21<sub>±00.07</sub> + 0.01<sub>±00.00</sub> |    70.61<sub>±00.91</sub> |
|                     Go |   1.269<sub>±0.000</sub> |     4.28<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    52.32<sub>±00.31</sub> |
|               Go/gccgo |   1.358<sub>±0.003</sub> |    24.61<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |    56.58<sub>±00.33</sub> |
|                Crystal |   1.419<sub>±0.001</sub> |     3.70<sub>±00.05</sub> + 0.02<sub>±00.00</sub> |    60.04<sub>±00.16</sub> |
|                  Julia |   1.567<sub>±0.002</sub> |   223.80<sub>±00.11</sub> + 0.06<sub>±00.00</sub> |    67.94<sub>±00.56</sub> |
|           F#/.NET Core |   1.574<sub>±0.004</sub> |    43.98<sub>±00.08</sub> + 0.02<sub>±00.00</sub> |    66.57<sub>±01.30</sub> |
|              Nim/clang |   1.581<sub>±0.000</sub> |     2.51<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    65.57<sub>±00.51</sub> |
|                  OCaml |   1.592<sub>±0.001</sub> |     3.80<sub>±00.09</sub> + 2.43<sub>±00.00</sub> |    74.43<sub>±00.63</sub> |
|                 Racket |   1.737<sub>±0.003</sub> |   119.40<sub>±00.12</sub> + 1.07<sub>±00.93</sub> |    71.44<sub>±00.69</sub> |
|            Chez Scheme |   1.745<sub>±0.004</sub> |    27.14<sub>±00.12</sub> + 2.99<sub>±00.02</sub> |    73.79<sub>±00.26</sub> |
|                  MLton |   1.756<sub>±0.003</sub> |     1.90<sub>±00.03</sub> + 0.39<sub>±00.00</sub> |    74.69<sub>±00.57</sub> |
|                V/clang |   1.773<sub>±0.032</sub> |     2.02<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    77.73<sub>±01.12</sub> |
|                  Swift |   2.231<sub>±0.000</sub> |    18.05<sub>±00.14</sub> + 0.00<sub>±00.00</sub> |    89.75<sub>±00.56</sub> |
|                  Scala |   2.282<sub>±0.003</sub> |  62.95<sub>±00.15</sub> + 139.81<sub>±00.11</sub> |   103.67<sub>±00.88</sub> |
|                Node.js |   2.929<sub>±0.076</sub> |    49.90<sub>±00.01</sub> + 6.18<sub>±00.00</sub> |   123.86<sub>±03.24</sub> |
|                  D/dmd |   3.164<sub>±0.000</sub> |     3.67<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   119.16<sub>±00.52</sub> |
|       Haskell (MArray) |   3.495<sub>±0.005</sub> |     4.39<sub>±00.07</sub> + 4.16<sub>±00.03</sub> |   144.69<sub>±00.56</sub> |
|           Haskell (FP) |   3.875<sub>±0.007</sub> |     4.54<sub>±00.09</sub> + 4.15<sub>±00.04</sub> |   164.65<sub>±00.35</sub> |
| Ruby/truffleruby (JVM) |   4.532<sub>±0.136</sub> | 357.69<sub>±05.33</sub> + 388.38<sub>±79.49</sub> |   211.77<sub>±08.52</sub> |
|       Ruby/truffleruby |   4.975<sub>±0.445</sub> | 182.71<sub>±05.00</sub> + 437.14<sub>±62.45</sub> |   236.62<sub>±25.73</sub> |
|             Lua/luajit |   5.471<sub>±0.016</sub> |     2.84<sub>±00.04</sub> + 0.01<sub>±00.00</sub> |   226.63<sub>±01.40</sub> |
|            Python/pypy |   9.250<sub>±0.052</sub> |   63.30<sub>±00.44</sub> + 30.22<sub>±00.05</sub> |   409.27<sub>±04.77</sub> |
|                  Idris |  12.846<sub>±0.037</sub> |    22.88<sub>±00.22</sub> + 9.53<sub>±00.04</sub> |   593.41<sub>±02.80</sub> |
|           Ruby (--jit) |  15.724<sub>±0.003</sub> |    15.61<sub>±00.08</sub> + 0.01<sub>±00.00</sub> |   649.77<sub>±01.13</sub> |
|                 Elixir |  20.463<sub>±0.149</sub> |    77.32<sub>±00.58</sub> + 0.00<sub>±00.00</sub> |   812.82<sub>±05.77</sub> |
|                    PHP |  33.374<sub>±0.039</sub> |    18.80<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  1382.86<sub>±05.05</sub> |
|             Ruby/jruby |  33.816<sub>±1.147</sub> | 244.06<sub>±00.38</sub> + 164.23<sub>±00.17</sub> |  1539.01<sub>±69.27</sub> |
|                    Lua |  36.681<sub>±0.280</sub> |     2.51<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  1493.55<sub>±12.80</sub> |
|                 Python |  52.764<sub>±0.410</sub> |    13.72<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  2307.85<sub>±27.82</sub> |
|                   Ruby |  67.587<sub>±0.204</sub> |    15.09<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  2972.48<sub>±31.42</sub> |
|               Tcl (FP) | 192.578<sub>±0.362</sub> |     4.97<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  8372.27<sub>±37.08</sub> |
|                   Perl | 210.611<sub>±0.562</sub> |     7.78<sub>±00.02</sub> + 0.02<sub>±00.00</sub> |  8958.93<sub>±20.13</sub> |
|              Tcl (OOP) | 353.622<sub>±1.177</sub> |     5.06<sub>±00.09</sub> + 0.00<sub>±00.00</sub> | 15746.50<sub>±58.95</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|               Language |                 Time, s |                                        Memory, MiB |                Energy, J |
| :--------------------- | ----------------------: | -------------------------------------------------: | -----------------------: |
|  C# (Staged)/.NET Core |  2.944<sub>±0.002</sub> |     38.05<sub>±00.08</sub> + 3.26<sub>±00.00</sub> |  151.91<sub>±00.52</sub> |
|         Scala (Staged) |  6.069<sub>±0.079</sub> |   233.46<sub>±02.88</sub> + 84.04<sub>±08.05</sub> |  324.36<sub>±08.26</sub> |
|                  C/gcc | 12.255<sub>±0.036</sub> |      1.80<sub>±00.02</sub> + 0.21<sub>±00.00</sub> |  506.58<sub>±02.16</sub> |
|                   Java | 12.482<sub>±0.044</sub> |     47.02<sub>±00.24</sub> + 1.28<sub>±00.15</sub> |  501.72<sub>±02.16</sub> |
|                Crystal | 12.821<sub>±0.021</sub> |      3.77<sub>±00.06</sub> + 0.36<sub>±00.00</sub> |  531.03<sub>±00.89</sub> |
|                  V/gcc | 12.889<sub>±0.030</sub> |      2.17<sub>±00.03</sub> + 1.04<sub>±00.01</sub> |  512.52<sub>±01.62</sub> |
|            C++/clang++ | 13.126<sub>±0.012</sub> |      3.50<sub>±00.10</sub> + 0.61<sub>±00.00</sub> |  543.33<sub>±01.69</sub> |
|                C/clang | 13.240<sub>±0.010</sub> |      1.79<sub>±00.03</sub> + 0.21<sub>±00.00</sub> |  582.38<sub>±04.39</sub> |
|             Kotlin/JVM | 13.316<sub>±0.132</sub> |     49.56<sub>±00.08</sub> + 1.34<sub>±00.08</sub> |  559.05<sub>±03.05</sub> |
|                C++/g++ | 13.349<sub>±0.024</sub> |      3.97<sub>±00.04</sub> + 0.61<sub>±00.00</sub> |  548.60<sub>±01.68</sub> |
|               Vala/gcc | 13.763<sub>±0.026</sub> |      6.27<sub>±00.04</sub> + 0.50<sub>±00.00</sub> |  548.36<sub>±01.69</sub> |
|                 D/ldc2 | 13.842<sub>±0.029</sub> |      3.47<sub>±00.05</sub> + 0.60<sub>±00.00</sub> |  564.67<sub>±03.07</sub> |
|        Racket (Staged) | 14.293<sub>±0.023</sub> |   112.88<sub>±00.17</sub> + 73.29<sub>±01.82</sub> |  568.08<sub>±01.87</sub> |
|                   Rust | 14.340<sub>±0.009</sub> |      2.19<sub>±00.03</sub> + 0.21<sub>±00.00</sub> |  573.48<sub>±01.22</sub> |
|             Vala/clang | 14.776<sub>±0.009</sub> |      6.27<sub>±00.08</sub> + 0.50<sub>±00.00</sub> |  596.85<sub>±02.81</sub> |
|                Nim/gcc | 14.867<sub>±0.026</sub> |      2.30<sub>±00.05</sub> + 1.49<sub>±00.01</sub> |  604.31<sub>±01.97</sub> |
|                    Zig | 14.917<sub>±0.009</sub> |      2.25<sub>±00.02</sub> + 0.55<sub>±00.00</sub> |  591.09<sub>±03.39</sub> |
|                     Go | 14.991<sub>±0.012</sub> |      4.38<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  602.09<sub>±00.76</sub> |
|                  D/gdc | 14.998<sub>±0.014</sub> |      9.09<sub>±00.35</sub> + 0.61<sub>±00.00</sub> |  629.01<sub>±01.91</sub> |
|                  Swift | 15.051<sub>±0.008</sub> |     17.98<sub>±00.14</sub> + 0.12<sub>±00.00</sub> |  613.65<sub>±03.35</sub> |
|           C#/.NET Core | 15.835<sub>±0.009</sub> |     38.42<sub>±00.06</sub> + 0.69<sub>±00.00</sub> |  681.43<sub>±01.25</sub> |
|                  Scala | 16.113<sub>±0.072</sub> |   62.92<sub>±00.17</sub> + 121.78<sub>±05.49</sub> |  694.27<sub>±04.11</sub> |
|              Nim/clang | 17.677<sub>±0.043</sub> |      2.62<sub>±00.09</sub> + 1.49<sub>±00.00</sub> |  728.11<sub>±01.27</sub> |
|               Go/gccgo | 18.001<sub>±0.273</sub> |     24.75<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |  743.41<sub>±14.47</sub> |
|                V/clang | 18.343<sub>±0.085</sub> |      2.16<sub>±00.04</sub> + 1.01<sub>±00.01</sub> |  794.00<sub>±03.43</sub> |
|                  OCaml | 23.512<sub>±0.010</sub> |      4.65<sub>±00.04</sub> + 2.60<sub>±00.01</sub> | 1160.06<sub>±03.98</sub> |
|           F#/.NET Core | 27.074<sub>±0.010</sub> |     44.07<sub>±00.06</sub> + 1.37<sub>±00.00</sub> | 1150.41<sub>±02.52</sub> |
|                Node.js | 30.820<sub>±0.257</sub> |     49.91<sub>±00.10</sub> + 8.42<sub>±00.04</sub> | 1266.66<sub>±07.70</sub> |
|            Chez Scheme | 30.852<sub>±0.235</sub> |     27.41<sub>±00.10</sub> + 4.67<sub>±00.03</sub> | 1367.00<sub>±11.29</sub> |
|             Lua/luajit | 31.269<sub>±0.066</sub> |      2.88<sub>±00.03</sub> + 0.29<sub>±00.00</sub> | 1289.28<sub>±02.20</sub> |
|                  Julia | 32.737<sub>±0.042</sub> |    223.76<sub>±00.04</sub> + 0.00<sub>±00.00</sub> | 1375.53<sub>±05.44</sub> |
|       Haskell (MArray) | 35.267<sub>±0.007</sub> |      4.46<sub>±00.06</sub> + 5.05<sub>±00.01</sub> | 1459.48<sub>±01.27</sub> |
|                  MLton | 35.675<sub>±0.075</sub> |      1.97<sub>±00.04</sub> + 4.29<sub>±00.01</sub> | 1613.66<sub>±07.18</sub> |
|                 Racket | 35.779<sub>±0.203</sub> |    119.51<sub>±00.09</sub> + 1.87<sub>±00.45</sub> | 1569.21<sub>±10.52</sub> |
|            Python/pypy | 39.631<sub>±0.216</sub> |    63.25<sub>±00.34</sub> + 30.38<sub>±00.04</sub> | 1777.93<sub>±17.95</sub> |
|                  D/dmd | 39.807<sub>±0.004</sub> |      3.68<sub>±00.03</sub> + 0.61<sub>±00.00</sub> | 1453.73<sub>±01.66</sub> |
| Ruby/truffleruby (JVM) | 48.325<sub>±0.138</sub> | 358.83<sub>±04.20</sub> + 394.52<sub>±103.69</sub> | 2234.69<sub>±23.15</sub> |
|       Ruby/truffleruby | 51.497<sub>±0.292</sub> |  182.37<sub>±03.32</sub> + 302.18<sub>±49.85</sub> | 2528.95<sub>±17.09</sub> |
|                  Idris | 63.759<sub>±0.192</sub> |    23.03<sub>±00.23</sub> + 17.52<sub>±00.02</sub> | 2829.68<sub>±12.73</sub> |
|           Haskell (FP) | 77.997<sub>±0.164</sub> |     4.44<sub>±00.05</sub> + 74.85<sub>±00.01</sub> | 3311.70<sub>±09.94</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ---------------------: | ------------------------------------------------: | ----------------------: |
|                       PHP | 0.065<sub>±0.001</sub> |    19.08<sub>±00.14</sub> + 0.00<sub>±00.00</sub> |   3.26<sub>±00.06</sub> |
|            C/gcc (aklomp) | 0.097<sub>±0.000</sub> |     2.37<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.69<sub>±00.04</sub> |
|          C/clang (aklomp) | 0.097<sub>±0.002</sub> |     2.38<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   4.71<sub>±00.16</sub> |
|              Go (base64x) | 0.283<sub>±0.017</sub> |     5.68<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  16.32<sub>±00.94</sub> |
|                   Node.js | 0.327<sub>±0.006</sub> |  47.02<sub>±00.03</sub> + 114.48<sub>±02.33</sub> |  15.86<sub>±00.37</sub> |
|                   C/clang | 0.560<sub>±0.000</sub> |     2.25<sub>±00.05</sub> + 0.05<sub>±00.00</sub> |  21.84<sub>±00.27</sub> |
|                      Rust | 0.853<sub>±0.002</sub> |     2.60<sub>±00.03</sub> + 0.08<sub>±00.00</sub> |  36.16<sub>±00.34</sub> |
|                       Zig | 0.858<sub>±0.001</sub> |     2.54<sub>±00.01</sub> + 0.09<sub>±00.00</sub> |  33.86<sub>±00.24</sub> |
|                 Nim/clang | 1.009<sub>±0.004</sub> |     3.02<sub>±00.06</sub> + 5.00<sub>±00.01</sub> |  41.16<sub>±00.69</sub> |
|                   Crystal | 1.060<sub>±0.025</sub> |     4.08<sub>±00.12</sub> + 0.95<sub>±00.10</sub> |  48.86<sub>±02.23</sub> |
|                     C/gcc | 1.118<sub>±0.001</sub> |     2.19<sub>±00.04</sub> + 0.08<sub>±00.00</sub> |  42.47<sub>±00.36</sub> |
|                      Java | 1.329<sub>±0.003</sub> |  47.44<sub>±00.16</sub> + 226.50<sub>±00.13</sub> |  54.10<sub>±00.42</sub> |
|              C#/.NET Core | 1.330<sub>±0.020</sub> |    39.63<sub>±00.13</sub> + 3.44<sub>±00.06</sub> |  61.95<sub>±02.71</sub> |
|                   Nim/gcc | 1.361<sub>±0.004</sub> |     2.71<sub>±00.08</sub> + 5.30<sub>±00.00</sub> |  56.99<sub>±00.59</sub> |
|                    D/ldc2 | 1.363<sub>±0.003</sub> |     4.02<sub>±00.05</sub> + 3.41<sub>±00.00</sub> |  57.89<sub>±00.54</sub> |
|                   V/clang | 1.472<sub>±0.011</sub> |  2.63<sub>±00.04</sub> + 2378.76<sub>±02.76</sub> |  58.27<sub>±00.65</sub> |
|                        Go | 1.499<sub>±0.003</sub> |     5.10<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  69.44<sub>±00.80</sub> |
|                     Scala | 1.500<sub>±0.040</sub> |  60.89<sub>±00.74</sub> + 214.18<sub>±79.69</sub> |  65.45<sub>±01.63</sub> |
|                Kotlin/JVM | 1.504<sub>±0.010</sub> |  50.42<sub>±00.09</sub> + 123.85<sub>±51.97</sub> |  64.78<sub>±00.51</sub> |
|                  Vala/gcc | 1.591<sub>±0.004</sub> |     6.84<sub>±00.04</sub> + 0.10<sub>±00.00</sub> |  64.34<sub>±00.72</sub> |
|                     V/gcc | 1.592<sub>±0.009</sub> |  2.63<sub>±00.04</sub> + 2380.66<sub>±03.46</sub> |  62.10<sub>±00.59</sub> |
|                Vala/clang | 1.596<sub>±0.007</sub> |     6.80<sub>±00.11</sub> + 0.10<sub>±00.00</sub> |  65.06<sub>±00.74</sub> |
|              Ruby (--jit) | 1.642<sub>±0.006</sub> |   16.48<sub>±00.07</sub> + 41.38<sub>±02.50</sub> |  67.14<sub>±00.63</sub> |
|              F#/.NET Core | 1.735<sub>±0.082</sub> |   44.89<sub>±00.03</sub> + 23.98<sub>±04.15</sub> |  65.35<sub>±02.84</sub> |
|                      Ruby | 1.830<sub>±0.009</sub> |   15.72<sub>±00.07</sub> + 43.98<sub>±00.11</sub> |  75.23<sub>±01.03</sub> |
|       Perl (MIME::Base64) | 1.930<sub>±0.033</sub> |    13.94<sub>±00.02</sub> + 0.05<sub>±00.00</sub> |  81.87<sub>±01.25</sub> |
|                     D/dmd | 2.516<sub>±0.008</sub> |     3.78<sub>±00.06</sub> + 3.49<sub>±00.01</sub> | 104.59<sub>±01.33</sub> |
|                     Julia | 2.604<sub>±0.050</sub> |  246.44<sub>±00.18</sub> + 43.25<sub>±00.13</sub> | 109.89<sub>±01.53</sub> |
|                       Tcl | 2.929<sub>±0.005</sub> |     5.61<sub>±00.04</sub> + 0.12<sub>±00.00</sub> | 125.68<sub>±00.44</sub> |
|                  Go/gccgo | 3.131<sub>±0.007</sub> |    25.82<sub>±00.10</sub> + 0.00<sub>±00.00</sub> | 149.08<sub>±01.24</sub> |
|                    Python | 3.254<sub>±0.015</sub> |    12.54<sub>±00.05</sub> + 0.08<sub>±00.00</sub> | 128.18<sub>±02.42</sub> |
|               Python/pypy | 3.391<sub>±0.020</sub> |   62.03<sub>±00.55</sub> + 30.17<sub>±00.01</sub> | 152.78<sub>±02.24</sub> |
|       C++/g++ (libcrypto) | 3.468<sub>±0.004</sub> |     7.49<sub>±00.12</sub> + 0.14<sub>±00.02</sub> | 150.54<sub>±01.93</sub> |
|   C++/clang++ (libcrypto) | 3.470<sub>±0.008</sub> |     7.08<sub>±00.10</sub> + 0.15<sub>±00.02</sub> | 148.87<sub>±01.18</sub> |
|                     D/gdc | 3.818<sub>±0.015</sub> |     9.09<sub>±00.44</sub> + 3.63<sub>±00.03</sub> | 162.53<sub>±01.96</sub> |
|                    Racket | 3.977<sub>±0.020</sub> |  101.80<sub>±00.15</sub> + 22.35<sub>±00.33</sub> | 165.11<sub>±02.25</sub> |
|                Ruby/jruby | 4.037<sub>±0.036</sub> |  244.23<sub>±01.27</sub> + 51.41<sub>±26.75</sub> | 184.69<sub>±01.10</sub> |
|    Ruby/truffleruby (JVM) | 4.062<sub>±0.029</sub> | 392.38<sub>±02.39</sub> + 221.53<sub>±15.06</sub> | 202.82<sub>±02.58</sub> |
|          Ruby/truffleruby | 8.547<sub>±0.024</sub> | 402.08<sub>±15.27</sub> + 234.14<sub>±21.39</sub> | 399.48<sub>±04.99</sub> |
| Perl (MIME::Base64::Perl) | 9.667<sub>±0.029</sub> |    15.29<sub>±00.01</sub> + 0.28<sub>±00.02</sub> | 439.27<sub>±07.79</sub> |

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

|                            Language |                Time, s |                                        Memory, MiB |               Energy, J |
| :---------------------------------- | ---------------------: | -------------------------------------------------: | ----------------------: |
|    C++/clang++ (simdjson On-Demand) | 0.053<sub>±0.000</sub> |   113.46<sub>±00.04</sub> + 59.83<sub>±00.01</sub> |   2.28<sub>±00.04</sub> |
|        C++/g++ (simdjson On-Demand) | 0.055<sub>±0.000</sub> |   114.04<sub>±00.05</sub> + 59.84<sub>±00.01</sub> |   2.36<sub>±00.03</sub> |
|     C++/g++ (DAW JSON Link NoCheck) | 0.058<sub>±0.000</sub> |    113.80<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   2.43<sub>±00.03</sub> |
| C++/clang++ (DAW JSON Link NoCheck) | 0.061<sub>±0.000</sub> |    113.29<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   2.63<sub>±00.03</sub> |
|         C++/clang++ (DAW JSON Link) | 0.075<sub>±0.000</sub> |    113.26<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.25<sub>±00.03</sub> |
|             C++/g++ (DAW JSON Link) | 0.096<sub>±0.000</sub> |    113.83<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.16<sub>±00.05</sub> |
|          C++/clang++ (simdjson DOM) | 0.104<sub>±0.003</sub> |  113.46<sub>±00.04</sub> + 176.49<sub>±00.29</sub> |   4.69<sub>±00.08</sub> |
|                 Rust (Serde Custom) | 0.105<sub>±0.000</sub> |    111.85<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.52<sub>±00.04</sub> |
|              C++/g++ (simdjson DOM) | 0.106<sub>±0.002</sub> |  114.00<sub>±00.02</sub> + 176.13<sub>±00.67</sub> |   4.89<sub>±00.09</sub> |
|                  Rust (Serde Typed) | 0.113<sub>±0.000</sub> |   111.92<sub>±00.02</sub> + 12.03<sub>±00.01</sub> |   4.77<sub>±00.05</sub> |
|                 C++/clang++ (gason) | 0.132<sub>±0.001</sub> |   113.25<sub>±00.04</sub> + 96.75<sub>±00.00</sub> |   5.28<sub>±00.05</sub> |
|               D/ldc2 (Mir Asdf DOM) | 0.133<sub>±0.000</sub> |   113.26<sub>±00.04</sub> + 61.25<sub>±00.01</sub> |   5.53<sub>±00.03</sub> |
|                     C++/g++ (gason) | 0.133<sub>±0.000</sub> |   113.75<sub>±00.06</sub> + 96.75<sub>±00.00</sub> |   5.34<sub>±00.07</sub> |
|                    C/clang (yyjson) | 0.137<sub>±0.001</sub> |  111.93<sub>±00.04</sub> + 229.45<sub>±00.01</sub> |   5.47<sub>±00.06</sub> |
|                      C/gcc (yyjson) | 0.139<sub>±0.001</sub> |  111.94<sub>±00.06</sub> + 229.45<sub>±00.02</sub> |   5.57<sub>±00.13</sub> |
|                 C++/g++ (RapidJSON) | 0.144<sub>±0.001</sub> |  113.82<sub>±00.04</sub> + 128.32<sub>±00.28</sub> |   6.22<sub>±00.17</sub> |
|                  Go (goccy/go-json) | 0.157<sub>±0.003</sub> |    116.35<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   6.37<sub>±00.09</sub> |
|              Scala (jsoniter-scala) | 0.157<sub>±0.001</sub> |   166.14<sub>±00.09</sub> + 30.21<sub>±00.10</sub> |   8.90<sub>±00.08</sub> |
|             C++/clang++ (RapidJSON) | 0.193<sub>±0.001</sub> |  113.30<sub>±00.04</sub> + 128.59<sub>±00.00</sub> |   8.06<sub>±00.06</sub> |
|         C++/g++ (RapidJSON Precise) | 0.200<sub>±0.001</sub> |  113.80<sub>±00.03</sub> + 128.61<sub>±00.00</sub> |   8.64<sub>±00.14</sub> |
|                          Go (Sonic) | 0.209<sub>±0.003</sub> |    123.21<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   9.13<sub>±00.10</sub> |
|                   Go (rjson custom) | 0.210<sub>±0.000</sub> |    114.89<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   8.08<sub>±00.05</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) | 0.211<sub>±0.001</sub> |   113.44<sub>±00.05</sub> + 80.66<sub>±00.00</sub> |   9.05<sub>±00.05</sub> |
|                                 Zig | 0.236<sub>±0.000</sub> |   111.76<sub>±00.01</sub> + 33.22<sub>±00.02</sub> |  10.23<sub>±00.15</sub> |
|                          Go (rjson) | 0.242<sub>±0.001</sub> |    114.86<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   9.13<sub>±00.07</sub> |
|     C++/clang++ (RapidJSON Precise) | 0.262<sub>±0.002</sub> |  113.38<sub>±00.09</sub> + 128.60<sub>±00.00</sub> |  11.26<sub>±00.10</sub> |
|                C++/g++ (Boost.JSON) | 0.319<sub>±0.001</sub> |  114.00<sub>±00.07</sub> + 307.84<sub>±00.00</sub> |  14.00<sub>±00.22</sub> |
|            C++/clang++ (Boost.JSON) | 0.327<sub>±0.001</sub> |  113.37<sub>±00.06</sub> + 307.85<sub>±00.00</sub> |  14.25<sub>±00.05</sub> |
|                        C/gcc (yajl) | 0.372<sub>±0.001</sub> |    111.56<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  16.32<sub>±00.23</sub> |
|             C++/g++ (RapidJSON SAX) | 0.373<sub>±0.001</sub> |    113.84<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  16.49<sub>±00.15</sub> |
|                      C/clang (yajl) | 0.375<sub>±0.001</sub> |    111.57<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  16.26<sub>±00.12</sub> |
|                   Nim/clang (jsony) | 0.389<sub>±0.000</sub> |  112.24<sub>±00.07</sub> + 154.15<sub>±00.01</sub> |  16.34<sub>±00.20</sub> |
|                             Node.js | 0.390<sub>±0.002</sub> |  156.95<sub>±00.07</sub> + 230.78<sub>±01.45</sub> |  19.10<sub>±00.05</sub> |
|                     Java (DSL-JSON) | 0.401<sub>±0.002</sub> |  169.39<sub>±00.24</sub> + 150.96<sub>±00.10</sub> |  19.85<sub>±00.28</sub> |
|         C++/clang++ (RapidJSON SAX) | 0.408<sub>±0.001</sub> |    195.53<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  17.59<sub>±00.29</sub> |
|                     Nim/gcc (jsony) | 0.419<sub>±0.001</sub> |  111.94<sub>±00.07</sub> + 154.16<sub>±00.00</sub> |  17.87<sub>±00.22</sub> |
|              C++/clang++ (Nlohmann) | 0.429<sub>±0.001</sub> |  113.40<sub>±00.06</sub> + 335.85<sub>±00.00</sub> |  18.28<sub>±00.19</sub> |
|     C++/g++ (RapidJSON SAX Precise) | 0.440<sub>±0.000</sub> |    113.86<sub>±00.04</sub> + 0.02<sub>±00.00</sub> |  19.80<sub>±00.13</sub> |
|                        Ruby (--jit) | 0.453<sub>±0.002</sub> |  125.58<sub>±00.06</sub> + 210.67<sub>±00.01</sub> |  19.60<sub>±00.23</sub> |
|                                Ruby | 0.471<sub>±0.005</sub> |  125.08<sub>±00.09</sub> + 210.80<sub>±00.01</sub> |  20.38<sub>±00.50</sub> |
| C++/clang++ (RapidJSON SAX Precise) | 0.486<sub>±0.001</sub> |    195.60<sub>±00.05</sub> + 0.02<sub>±00.00</sub> |  21.60<sub>±00.36</sub> |
|                       Go (jsoniter) | 0.498<sub>±0.001</sub> |    115.33<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  20.76<sub>±00.18</sub> |
|                  C++/g++ (Nlohmann) | 0.523<sub>±0.002</sub> |  113.93<sub>±00.02</sub> + 423.86<sub>±00.00</sub> |  22.32<sub>±00.13</sub> |
|                         Python/pypy | 0.565<sub>±0.002</sub> |  283.16<sub>±00.31</sub> + 125.45<sub>±00.00</sub> |  24.63<sub>±00.17</sub> |
|                    Crystal (Schema) | 0.566<sub>±0.001</sub> |   113.78<sub>±00.05</sub> + 50.64<sub>±01.67</sub> |  25.30<sub>±00.26</sub> |
|                      Crystal (Pull) | 0.568<sub>±0.001</sub> |   113.79<sub>±00.05</sub> + 17.95<sub>±00.00</sub> |  25.51<sub>±00.37</sub> |
|                                  Go | 0.569<sub>±0.001</sub> |    115.48<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  23.07<sub>±00.39</sub> |
|                                Odin | 0.587<sub>±0.001</sub> |   111.90<sub>±00.06</sub> + 11.91<sub>±00.04</sub> |  24.60<sub>±00.16</sub> |
|                Rust (Serde Untyped) | 0.603<sub>±0.006</sub> |  112.00<sub>±00.05</sub> + 839.87<sub>±00.01</sub> |  25.31<sub>±00.16</sub> |
|                 CPython (UltraJSON) | 0.612<sub>±0.002</sub> |  124.61<sub>±00.06</sub> + 478.96<sub>±01.47</sub> |  24.88<sub>±00.22</sub> |
|                       Julia (JSON3) | 0.617<sub>±0.003</sub> |  503.99<sub>±00.12</sub> + 368.30<sub>±02.66</sub> |  26.25<sub>±00.26</sub> |
|                             V/clang | 0.650<sub>±0.001</sub> |  112.23<sub>±00.04</sub> + 496.00<sub>±00.00</sub> |  28.10<sub>±00.27</sub> |
|                               V/gcc | 0.664<sub>±0.003</sub> |  111.83<sub>±00.01</sub> + 496.00<sub>±00.00</sub> |  28.38<sub>±00.49</sub> |
|                Nim/gcc (Packedjson) | 0.682<sub>±0.005</sub> |  112.16<sub>±00.05</sub> + 294.26<sub>±00.02</sub> |  28.95<sub>±00.37</sub> |
|              Nim/clang (Packedjson) | 0.694<sub>±0.002</sub> |  112.62<sub>±00.09</sub> + 294.27<sub>±00.01</sub> |  29.92<sub>±00.46</sub> |
|                          Ruby/jruby | 0.715<sub>±0.012</sub> |  472.63<sub>±00.24</sub> + 370.22<sub>±01.39</sub> |  42.55<sub>±00.83</sub> |
|     C#/.NET Core (System.Text.Json) | 0.717<sub>±0.005</sub> |  486.22<sub>±00.08</sub> + 117.30<sub>±00.01</sub> |  32.68<sub>±00.60</sub> |
|             Perl (Cpanel::JSON::XS) | 0.727<sub>±0.010</sub> |  124.76<sub>±00.06</sub> + 402.84<sub>±00.00</sub> |  30.70<sub>±00.37</sub> |
|                             Crystal | 0.806<sub>±0.004</sub> |  113.84<sub>±00.07</sub> + 392.15<sub>±00.00</sub> |  36.74<sub>±00.21</sub> |
|                                 PHP | 0.824<sub>±0.003</sub> |  128.27<sub>±00.08</sub> + 517.84<sub>±00.00</sub> |  35.29<sub>±00.08</sub> |
|                             Clojure | 0.866<sub>±0.004</sub> |  517.12<sub>±02.87</sub> + 549.26<sub>±17.13</sub> |  43.88<sub>±01.00</sub> |
|                              Python | 0.882<sub>±0.002</sub> |  122.55<sub>±00.07</sub> + 325.87<sub>±00.00</sub> |  37.38<sub>±00.28</sub> |
|                        C#/.NET Core | 0.990<sub>±0.018</sub> |   491.13<sub>±00.06</sub> + 20.59<sub>±01.49</sub> |  47.20<sub>±00.97</sub> |
|                             Nim/gcc | 1.018<sub>±0.004</sub> | 112.23<sub>±00.05</sub> + 1001.34<sub>±00.02</sub> |  43.21<sub>±00.85</sub> |
|                           Nim/clang | 1.081<sub>±0.006</sub> |  112.64<sub>±00.06</sub> + 997.96<sub>±00.08</sub> |  45.16<sub>±00.76</sub> |
|                    C++/g++ (json-c) | 1.149<sub>±0.005</sub> | 113.84<sub>±00.03</sub> + 1215.86<sub>±00.00</sub> |  48.37<sub>±00.67</sub> |
|                C++/clang++ (json-c) | 1.151<sub>±0.002</sub> | 113.43<sub>±00.04</sub> + 1215.86<sub>±00.00</sub> |  48.81<sub>±00.43</sub> |
|                            Go/gccgo | 1.263<sub>±0.001</sub> |    140.19<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  52.48<sub>±00.40</sub> |
|                                  C3 | 1.446<sub>±0.002</sub> |  112.26<sub>±00.03</sub> + 859.95<sub>±00.00</sub> |  58.78<sub>±00.31</sub> |
|                              D/ldc2 | 1.557<sub>±0.007</sub> |  113.42<sub>±00.04</sub> + 671.14<sub>±00.00</sub> |  69.32<sub>±00.55</sub> |
|                         Ruby (YAJL) | 1.629<sub>±0.019</sub> |  124.66<sub>±00.05</sub> + 236.85<sub>±00.01</sub> |  69.95<sub>±01.23</sub> |
|     F#/.NET Core (System.Text.Json) | 1.957<sub>±0.005</sub> |  496.72<sub>±00.04</sub> + 124.55<sub>±00.35</sub> |  88.32<sub>±00.82</sub> |
|                             Haskell | 2.088<sub>±0.014</sub> |  116.26<sub>±00.08</sub> + 723.73<sub>±00.13</sub> |  89.49<sub>±00.73</sub> |
|                    Ruby/truffleruby | 2.266<sub>±0.022</sub> | 390.53<sub>±25.06</sub> + 1750.79<sub>±88.47</sub> | 139.29<sub>±02.66</sub> |
|    C++/clang++ (Boost.PropertyTree) | 2.448<sub>±0.005</sub> | 195.71<sub>±00.05</sub> + 1232.52<sub>±00.00</sub> | 106.00<sub>±00.94</sub> |
|              Ruby/truffleruby (JVM) | 2.475<sub>±0.020</sub> | 487.72<sub>±09.27</sub> + 1499.37<sub>±97.54</sub> | 160.57<sub>±02.95</sub> |
|                               D/dmd | 2.609<sub>±0.004</sub> |  113.74<sub>±00.07</sub> + 671.11<sub>±00.00</sub> | 115.23<sub>±01.29</sub> |
|        C++/g++ (Boost.PropertyTree) | 2.781<sub>±0.007</sub> | 113.96<sub>±00.01</sub> + 1439.85<sub>±00.00</sub> | 119.42<sub>±00.63</sub> |
|                           Rust (jq) | 2.868<sub>±0.007</sub> |  113.85<sub>±00.04</sub> + 905.40<sub>±01.72</sub> | 118.80<sub>±01.13</sub> |
|                            Vala/gcc | 2.870<sub>±0.007</sub> |  116.26<sub>±00.01</sub> + 980.00<sub>±00.00</sub> | 124.21<sub>±01.26</sub> |
|                          Vala/clang | 2.881<sub>±0.009</sub> |  116.23<sub>±00.03</sub> + 980.00<sub>±00.00</sub> | 124.70<sub>±01.30</sub> |
|                               D/gdc | 2.972<sub>±0.009</sub> |  118.42<sub>±00.28</sub> + 671.20<sub>±00.07</sub> | 129.61<sub>±02.31</sub> |
|                              Racket | 4.753<sub>±0.029</sub> |  231.71<sub>±00.10</sub> + 285.07<sub>±01.65</sub> | 191.96<sub>±01.56</sub> |
|                   Perl (JSON::Tiny) | 8.483<sub>±0.033</sub> |  125.40<sub>±00.10</sub> + 528.51<sub>±00.01</sub> | 376.51<sub>±03.35</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                Language |                  Time, s |                                        Memory, MiB |                  Energy, J |
| :---------------------- | -----------------------: | -------------------------------------------------: | -------------------------: |
|         D/ldc2 (lubeck) |   0.044<sub>±0.000</sub> |     8.66<sub>±01.14</sub> + 56.39<sub>±01.13</sub> |      4.35<sub>±00.05</sub> |
|   Nim/gcc (Arraymancer) |   0.073<sub>±0.004</sub> |     6.13<sub>±00.07</sub> + 58.31<sub>±00.00</sub> |      5.86<sub>±00.18</sub> |
| Nim/clang (Arraymancer) |   0.075<sub>±0.011</sub> |    11.52<sub>±00.34</sub> + 53.73<sub>±00.42</sub> |      6.06<sub>±00.64</sub> |
|          Python (NumPy) |   0.078<sub>±0.001</sub> |    33.06<sub>±03.23</sub> + 54.90<sub>±03.28</sub> |      6.38<sub>±00.06</sub> |
|      Julia (threads: 2) |   0.080<sub>±0.000</sub> |   243.14<sub>±00.04</sub> + 69.47<sub>±00.03</sub> |      5.14<sub>±00.01</sub> |
|     C++/clang++ (Eigen) |   0.081<sub>±0.001</sub> |    12.47<sub>±00.65</sub> + 78.66<sub>±00.61</sub> |      6.41<sub>±00.12</sub> |
|         C++/g++ (Eigen) |   0.082<sub>±0.001</sub> |    11.65<sub>±00.98</sub> + 79.08<sub>±01.03</sub> |      5.67<sub>±00.05</sub> |
|             Java (ND4J) |   0.083<sub>±0.001</sub> |   126.62<sub>±01.04</sub> + 92.51<sub>±00.19</sub> |      6.55<sub>±00.13</sub> |
|          Rust (ndarray) |   0.091<sub>±0.002</sub> |     2.81<sub>±00.02</sub> + 68.39<sub>±00.03</sub> |      5.91<sub>±00.10</sub> |
|      Julia (threads: 1) |   0.131<sub>±0.000</sub> |   243.14<sub>±00.01</sub> + 68.83<sub>±00.04</sub> |      6.54<sub>±00.06</sub> |
|     V/gcc (VSL + CBLAS) |   0.207<sub>±0.005</sub> |     2.59<sub>±00.05</sub> + 53.79<sub>±00.38</sub> |     18.96<sub>±00.24</sub> |
|             V/gcc (VSL) |   0.221<sub>±0.004</sub> |     2.60<sub>±00.07</sub> + 53.48<sub>±00.52</sub> |     15.90<sub>±00.31</sub> |
|           V/clang (VSL) |   0.225<sub>±0.004</sub> |     2.98<sub>±00.06</sub> + 53.91<sub>±00.60</sub> |     15.99<sub>±00.42</sub> |
|   V/clang (VSL + CBLAS) |   0.229<sub>±0.005</sub> |     2.94<sub>±00.06</sub> + 53.32<sub>±00.36</sub> |     16.89<sub>±00.29</sub> |
|         Julia (no BLAS) |   1.083<sub>±0.026</sub> |   254.50<sub>±00.08</sub> + 60.87<sub>±00.04</sub> |     47.96<sub>±01.40</sub> |
|                  D/ldc2 |   1.722<sub>±0.003</sub> |     4.01<sub>±00.05</sub> + 70.35<sub>±00.01</sub> |     63.57<sub>±00.35</sub> |
|                   D/gdc |   1.765<sub>±0.003</sub> |     8.70<sub>±00.28</sub> + 70.06<sub>±00.07</sub> |     66.00<sub>±00.43</sub> |
|                   D/dmd |   1.888<sub>±0.005</sub> |     3.65<sub>±00.03</sub> + 70.42<sub>±00.01</sub> |     72.77<sub>±00.94</sub> |
|                   V/gcc |   3.037<sub>±0.004</sub> |     2.52<sub>±00.07</sub> + 68.80<sub>±00.01</sub> |    113.30<sub>±00.52</sub> |
|                   C/gcc |   3.038<sub>±0.004</sub> |     2.29<sub>±00.04</sub> + 68.33<sub>±00.02</sub> |    113.26<sub>±01.01</sub> |
|                    Rust |   3.040<sub>±0.002</sub> |     2.47<sub>±00.04</sub> + 68.45<sub>±00.00</sub> |    121.00<sub>±00.71</sub> |
|              Vala/clang |   3.064<sub>±0.001</sub> |     6.62<sub>±00.02</sub> + 68.43<sub>±00.01</sub> |    106.94<sub>±01.32</sub> |
|                 C/clang |   3.068<sub>±0.001</sub> |     2.29<sub>±00.04</sub> + 68.33<sub>±00.02</sub> |    105.58<sub>±00.29</sub> |
|                 V/clang |   3.069<sub>±0.002</sub> |     2.92<sub>±00.06</sub> + 68.80<sub>±00.01</sub> |    106.77<sub>±01.09</sub> |
|                     Zig |   3.081<sub>±0.003</sub> |     2.49<sub>±00.04</sub> + 68.56<sub>±00.01</sub> |    110.04<sub>±00.46</sub> |
|                Vala/gcc |   3.093<sub>±0.001</sub> |     6.58<sub>±00.05</sub> + 68.47<sub>±00.03</sub> |    113.95<sub>±01.30</sub> |
|                 Nim/gcc |   3.100<sub>±0.001</sub> |     2.85<sub>±00.04</sub> + 62.48<sub>±02.14</sub> |    116.09<sub>±00.99</sub> |
|                    Java |   3.101<sub>±0.036</sub> |    47.00<sub>±00.18</sub> + 87.86<sub>±07.87</sub> |    124.22<sub>±01.30</sub> |
|                   Swift |   3.107<sub>±0.001</sub> |     8.98<sub>±00.05</sub> + 68.68<sub>±00.00</sub> |    112.54<sub>±01.06</sub> |
|               Nim/clang |   3.142<sub>±0.002</sub> |     3.21<sub>±00.09</sub> + 57.76<sub>±00.01</sub> |    111.15<sub>±01.08</sub> |
|                Go/gccgo |   3.151<sub>±0.005</sub> |     25.18<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    119.21<sub>±01.69</sub> |
|                 Crystal |   3.153<sub>±0.001</sub> |     4.14<sub>±00.03</sub> + 59.55<sub>±00.01</sub> |    112.46<sub>±00.64</sub> |
|                      Go |   3.156<sub>±0.002</sub> |      4.69<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    114.58<sub>±02.81</sub> |
|              Kotlin/JVM |   3.167<sub>±0.002</sub> |    48.27<sub>±00.07</sub> + 82.10<sub>±00.33</sub> |    124.51<sub>±00.96</sub> |
|                 Node.js |   3.209<sub>±0.003</sub> |    57.48<sub>±00.18</sub> + 71.12<sub>±00.15</sub> |    126.46<sub>±00.68</sub> |
|             Python/pypy |   3.259<sub>±0.003</sub> |    63.04<sub>±00.54</sub> + 68.65<sub>±00.02</sub> |    136.50<sub>±00.73</sub> |
|                   Scala |   3.314<sub>±0.012</sub> |   61.67<sub>±00.21</sub> + 140.45<sub>±01.08</sub> |    124.15<sub>±02.39</sub> |
|            C#/.NET Core |   4.482<sub>±0.004</sub> |    38.02<sub>±00.14</sub> + 69.12<sub>±00.03</sub> |    182.68<sub>±02.03</sub> |
|        Ruby/truffleruby |  15.999<sub>±1.028</sub> |  373.84<sub>±20.47</sub> + 350.24<sub>±77.36</sub> |    574.51<sub>±26.50</sub> |
|  Ruby/truffleruby (JVM) |  17.282<sub>±0.408</sub> |  373.36<sub>±06.26</sub> + 289.60<sub>±21.65</sub> |    630.81<sub>±15.24</sub> |
|            Ruby (--jit) | 129.487<sub>±0.191</sub> |    16.11<sub>±00.06</sub> + 70.36<sub>±00.00</sub> |   5865.02<sub>±24.42</sub> |
|                  Python | 134.006<sub>±0.471</sub> |    12.98<sub>±00.11</sub> + 68.71<sub>±00.00</sub> |   6126.81<sub>±35.31</sub> |
|                    Perl | 138.728<sub>±1.000</sub> |    9.16<sub>±00.05</sub> + 379.69<sub>±00.01</sub> |   6113.99<sub>±63.18</sub> |
|                     Tcl | 200.715<sub>±0.558</sub> |    7.92<sub>±00.03</sub> + 400.34<sub>±00.00</sub> |   9193.79<sub>±66.63</sub> |
|                    Ruby | 201.992<sub>±0.399</sub> |    15.48<sub>±00.07</sub> + 70.39<sub>±00.00</sub> |   8994.30<sub>±29.17</sub> |
|              Ruby/jruby | 308.008<sub>±3.256</sub> | 241.28<sub>±02.69</sub> + 1848.06<sub>±06.07</sub> | 12510.09<sub>±188.03</sub> |

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
|                    Zig | 0.062<sub>±0.000</sub> |    2.17<sub>±00.04</sub> + 70.49<sub>±00.68</sub> |   2.52<sub>±00.01</sub> |
|                C++/g++ | 0.065<sub>±0.001</sub> |    4.20<sub>±00.03</sub> + 83.60<sub>±01.79</sub> |   2.57<sub>±00.04</sub> |
|            C++/clang++ | 0.066<sub>±0.000</sub> |    3.64<sub>±00.05</sub> + 62.81<sub>±00.42</sub> |   2.66<sub>±00.03</sub> |
|                     Go | 0.073<sub>±0.001</sub> |     4.21<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   3.32<sub>±00.05</sub> |
|                   Rust | 0.106<sub>±0.000</sub> |    2.27<sub>±00.01</sub> + 72.92<sub>±00.01</sub> |   4.22<sub>±00.03</sub> |
|                  V/gcc | 0.111<sub>±0.001</sub> |   2.15<sub>±00.04</sub> + 212.79<sub>±02.12</sub> |   4.68<sub>±00.08</sub> |
|                Crystal | 0.115<sub>±0.001</sub> |    3.83<sub>±00.06</sub> + 89.67<sub>±00.00</sub> |   5.31<sub>±00.06</sub> |
|                V/clang | 0.118<sub>±0.001</sub> |   2.14<sub>±00.06</sub> + 219.59<sub>±01.61</sub> |   4.92<sub>±00.05</sub> |
|                   Java | 0.171<sub>±0.007</sub> |   46.90<sub>±00.12</sub> + 98.49<sub>±01.38</sub> |   9.79<sub>±00.34</sub> |
|                  Scala | 0.203<sub>±0.002</sub> |  62.48<sub>±00.14</sub> + 129.09<sub>±04.05</sub> |  12.19<sub>±00.18</sub> |
|                Node.js | 0.210<sub>±0.001</sub> |  45.38<sub>±00.05</sub> + 186.64<sub>±06.76</sub> |  10.79<sub>±00.07</sub> |
|                Nim/gcc | 0.291<sub>±0.001</sub> |   2.04<sub>±00.02</sub> + 589.46<sub>±03.84</sub> |  11.21<sub>±00.18</sub> |
|             Lua/luajit | 0.292<sub>±0.001</sub> |   2.84<sub>±00.05</sub> + 153.80<sub>±00.92</sub> |  11.97<sub>±00.05</sub> |
|              Nim/clang | 0.300<sub>±0.003</sub> |   2.44<sub>±00.04</sub> + 592.54<sub>±05.96</sub> |  12.10<sub>±00.22</sub> |
|                  Julia | 0.358<sub>±0.001</sub> | 251.72<sub>±00.14</sub> + 240.13<sub>±02.61</sub> |  14.02<sub>±00.41</sub> |
|            Python/pypy | 0.607<sub>±0.004</sub> |  61.80<sub>±00.34</sub> + 247.18<sub>±00.05</sub> |  24.75<sub>±00.32</sub> |
|       Ruby/truffleruby | 0.715<sub>±0.005</sub> | 177.26<sub>±01.75</sub> + 475.54<sub>±36.53</sub> |  47.75<sub>±00.48</sub> |
|                 Racket | 0.792<sub>±0.002</sub> | 112.74<sub>±00.26</sub> + 254.15<sub>±01.17</sub> |  31.38<sub>±00.17</sub> |
|                    Lua | 1.037<sub>±0.005</sub> |   2.62<sub>±00.03</sub> + 200.01<sub>±00.10</sub> |  43.45<sub>±00.45</sub> |
|           Ruby (--jit) | 1.102<sub>±0.004</sub> |  15.55<sub>±00.08</sub> + 180.16<sub>±02.56</sub> |  45.18<sub>±01.04</sub> |
| Ruby/truffleruby (JVM) | 1.183<sub>±0.019</sub> | 348.81<sub>±11.91</sub> + 312.47<sub>±40.93</sub> |  77.19<sub>±01.47</sub> |
|                   Ruby | 1.734<sub>±0.006</sub> |  15.18<sub>±00.05</sub> + 179.51<sub>±00.49</sub> |  74.36<sub>±00.86</sub> |
|                 Python | 1.842<sub>±0.010</sub> |  12.45<sub>±00.06</sub> + 181.76<sub>±00.78</sub> |  77.93<sub>±01.01</sub> |
|             Ruby/jruby | 2.990<sub>±0.052</sub> | 233.70<sub>±00.67</sub> + 853.95<sub>±00.46</sub> | 177.07<sub>±04.23</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux forky/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 11.0.100                        |
| C#/.NET Core     | 5.12.0-1.26529.118 (297acf930fb0f5c07fd4e51d78e521b4ad2e5d1f) |
| C3               | 0.8.6                           |
| Chez Scheme      | 10.4.1                          |
| Clojure          | "1.12.7"                        |
| Crystal          | 1.22.3                          |
| D/dmd            | v2.114.0                        |
| D/gdc            | 16.2.0                          |
| D/ldc2           | 1.43.0                          |
| Elixir           | 1.20.5                          |
| F#/.NET Core     | 15.2.101.0 for F# 11.0          |
| Go               | go1.27.4                        |
| Go/gccgo         | 16.2.0                          |
| Haskell          | 9.12.4                          |
| Idris 2          | 0.8.0                           |
| Java             | 27.0.1.1                        |
| Julia            | v"1.13.4"                       |
| Kotlin           | 2.4.21                          |
| Lua              | 5.5.1                           |
| Lua/luajit       | 2.1.1794898267                  |
| MLton            | 20241230                        |
| Nim              | 2.4.2                           |
| Node.js          | v26.13.1                        |
| OCaml            | 5.5.2                           |
| Odin             | dev-2026-11-nightly:9a69916     |
| PHP              | 8.4.28                          |
| Perl             | v5.44.0                         |
| Python           | 3.14.10                         |
| Python/pypy      | 8.0.1-final0 for Python 3.12.15 |
| Racket           | "9.3"                           |
| Ruby             | 4.0.10p0                        |
| Ruby/jruby       | 10.1.3.0                        |
| Ruby/truffleruby | 40.0.0                          |
| Rust             | 1.100.0                         |
| Scala            | 3.10.1                          |
| Swift            | 6.4.3                           |
| Tcl              | 8.6                             |
| V                | 0.5.2 7647ce1                   |
| Vala             | 0.56.19                         |
| Zig              | 0.17.1                          |
| clang/clang++    | 22.1.8 (4)                      |
| gcc/g++          | 16.2.0                          |

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
