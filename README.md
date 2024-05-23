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

UPDATE: 2024-05-22

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
|         Scala (Staged) |   0.368<sub>±0.002</sub> |  216.63<sub>±01.13</sub> + 20.92<sub>±01.02</sub> |     23.48<sub>±00.35</sub> |
|        Racket (Staged) |   0.886<sub>±0.000</sub> |   100.59<sub>±00.24</sub> + 0.00<sub>±00.00</sub> |     34.30<sub>±00.04</sub> |
|                   Rust |   1.010<sub>±0.000</sub> |     0.93<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     42.79<sub>±00.06</sub> |
|                  V/gcc |   1.052<sub>±0.000</sub> |     1.81<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     43.23<sub>±00.07</sub> |
|                C++/g++ |   1.108<sub>±0.003</sub> |     1.82<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     45.61<sub>±00.06</sub> |
|                  C/gcc |   1.114<sub>±0.004</sub> |     0.87<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     47.04<sub>±00.72</sub> |
|                  D/gdc |   1.119<sub>±0.000</sub> |     6.30<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     48.52<sub>±00.20</sub> |
|            C++/clang++ |   1.126<sub>±0.001</sub> |     1.62<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     47.12<sub>±00.56</sub> |
|                C/clang |   1.140<sub>±0.000</sub> |     0.88<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     48.18<sub>±00.11</sub> |
|                Nim/gcc |   1.172<sub>±0.001</sub> |     0.88<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     49.58<sub>±00.03</sub> |
|                   Java |   1.186<sub>±0.001</sub> |    39.87<sub>±00.15</sub> + 1.12<sub>±00.07</sub> |     49.02<sub>±00.14</sub> |
|               Vala/gcc |   1.208<sub>±0.002</sub> |     4.52<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     49.92<sub>±00.20</sub> |
|                 D/ldc2 |   1.212<sub>±0.000</sub> |     1.39<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     49.90<sub>±00.08</sub> |
|             Vala/clang |   1.232<sub>±0.001</sub> |     4.54<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     51.95<sub>±00.55</sub> |
|             Kotlin/JVM |   1.241<sub>±0.006</sub> |    43.17<sub>±00.10</sub> + 0.73<sub>±00.13</sub> |     51.37<sub>±00.39</sub> |
|                    Zig |   1.250<sub>±0.000</sub> |     0.90<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     52.00<sub>±00.50</sub> |
|                     Go |   1.266<sub>±0.001</sub> |     2.92<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     51.67<sub>±00.06</sub> |
|           C#/.NET Core |   1.367<sub>±0.002</sub> |    32.48<sub>±00.13</sub> + 0.35<sub>±00.00</sub> |     57.82<sub>±00.38</sub> |
|               Go/gccgo |   1.478<sub>±0.002</sub> |    23.52<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     62.23<sub>±00.14</sub> |
|              Nim/clang |   1.564<sub>±0.000</sub> |     1.14<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     64.25<sub>±00.05</sub> |
|                Crystal |   1.587<sub>±0.001</sub> |     2.94<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     67.04<sub>±00.08</sub> |
|           F#/.NET Core |   1.596<sub>±0.005</sub> |    37.18<sub>±00.05</sub> + 0.38<sub>±00.00</sub> |     67.54<sub>±00.26</sub> |
|                  OCaml |   1.666<sub>±0.003</sub> |     3.22<sub>±00.03</sub> + 2.40<sub>±00.06</sub> |     77.38<sub>±01.12</sub> |
|            Chez Scheme |   1.728<sub>±0.009</sub> |    24.76<sub>±00.03</sub> + 4.32<sub>±00.04</sub> |     73.06<sub>±00.25</sub> |
|                 Racket |   1.743<sub>±0.028</sub> |   93.00<sub>±00.16</sub> + 21.70<sub>±00.24</sub> |     71.50<sub>±01.19</sub> |
|                  Julia |   2.005<sub>±0.002</sub> |   249.55<sub>±00.06</sub> + 0.39<sub>±00.02</sub> |     80.47<sub>±00.28</sub> |
|                C#/Mono |   2.053<sub>±0.011</sub> |    25.56<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     86.98<sub>±00.39</sub> |
|                V/clang |   2.072<sub>±0.043</sub> |     1.87<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     91.45<sub>±02.59</sub> |
|                  MLton |   2.104<sub>±0.016</sub> |     1.65<sub>±00.03</sub> + 0.25<sub>±00.00</sub> |     86.58<sub>±00.78</sub> |
|                  Scala |   2.719<sub>±0.005</sub> |  72.47<sub>±00.19</sub> + 186.38<sub>±00.23</sub> |    117.38<sub>±00.49</sub> |
|                Node.js |   3.109<sub>±0.040</sub> |    39.22<sub>±00.03</sub> + 3.05<sub>±00.00</sub> |    131.56<sub>±01.70</sub> |
|                  D/dmd |   3.325<sub>±0.002</sub> |     3.35<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    124.44<sub>±00.12</sub> |
|       Haskell (MArray) |   3.351<sub>±0.003</sub> |     3.33<sub>±00.04</sub> + 5.01<sub>±00.05</sub> |    137.97<sub>±00.12</sub> |
|           Haskell (FP) |   3.729<sub>±0.002</sub> |     3.29<sub>±00.02</sub> + 5.06<sub>±00.01</sub> |    157.92<sub>±00.16</sub> |
| Ruby/truffleruby (JVM) |   4.835<sub>±0.315</sub> | 392.60<sub>±11.44</sub> + 613.72<sub>±75.70</sub> |    231.20<sub>±15.70</sub> |
|       Ruby/truffleruby |   5.359<sub>±0.111</sub> | 231.64<sub>±04.96</sub> + 614.89<sub>±40.60</sub> |    261.55<sub>±05.19</sub> |
|                  Swift |   5.687<sub>±0.035</sub> |    16.49<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    211.51<sub>±01.74</sub> |
|             Lua/luajit |   5.885<sub>±0.013</sub> |     2.46<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    241.48<sub>±01.05</sub> |
|            Python/pypy |   9.528<sub>±0.067</sub> |   59.68<sub>±00.22</sub> + 29.54<sub>±00.16</sub> |    421.32<sub>±02.48</sub> |
|                  Idris |  15.038<sub>±0.009</sub> |    20.65<sub>±00.04</sub> + 8.81<sub>±00.04</sub> |    655.65<sub>±03.19</sub> |
|                 Elixir |  20.364<sub>±0.033</sub> |    69.97<sub>±00.69</sub> + 0.00<sub>±00.00</sub> |    803.28<sub>±03.13</sub> |
|                    PHP |  34.238<sub>±0.051</sub> |    17.89<sub>±00.24</sub> + 0.00<sub>±00.00</sub> |   1444.37<sub>±03.95</sub> |
|                    Lua |  37.552<sub>±0.101</sub> |     2.23<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1541.38<sub>±02.60</sub> |
|           Ruby (--jit) |  48.990<sub>±0.097</sub> |    16.17<sub>±00.03</sub> + 1.79<sub>±00.03</sub> |   2076.12<sub>±03.94</sub> |
|                 Python |  59.034<sub>±0.179</sub> |    10.14<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   2610.69<sub>±16.45</sub> |
|                   Ruby |  67.526<sub>±0.444</sub> |    14.92<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   2925.73<sub>±37.41</sub> |
|             Ruby/jruby |  82.926<sub>±1.428</sub> | 196.23<sub>±03.00</sub> + 196.83<sub>±09.04</sub> |   3684.55<sub>±67.38</sub> |
|               Tcl (FP) | 190.823<sub>±0.879</sub> |     3.91<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |   8438.34<sub>±32.09</sub> |
|                   Perl | 243.859<sub>±0.390</sub> |     7.08<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  10803.89<sub>±32.06</sub> |
|              Tcl (OOP) | 375.323<sub>±1.694</sub> |     3.92<sub>±00.06</sub> + 0.00<sub>±00.00</sub> | 16589.53<sub>±109.18</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|               Language |                 Time, s |                                       Memory, MiB |                Energy, J |
| :--------------------- | ----------------------: | ------------------------------------------------: | -----------------------: |
|         Scala (Staged) |  7.775<sub>±0.137</sub> | 216.68<sub>±03.72</sub> + 104.91<sub>±04.86</sub> |  477.84<sub>±10.73</sub> |
|                C++/g++ |  9.742<sub>±0.022</sub> |     1.86<sub>±00.03</sub> + 2.28<sub>±00.04</sub> |  392.83<sub>±01.08</sub> |
|           C#/.NET Core | 12.028<sub>±0.044</sub> |    32.54<sub>±00.13</sub> + 1.40<sub>±00.00</sub> |  479.38<sub>±01.33</sub> |
|                  C/gcc | 12.436<sub>±0.010</sub> |     0.87<sub>±00.00</sub> + 0.90<sub>±00.06</sub> |  499.51<sub>±01.50</sub> |
|                   Java | 12.789<sub>±0.104</sub> |    39.88<sub>±00.07</sub> + 2.20<sub>±00.06</sub> |  506.58<sub>±07.58</sub> |
|             Kotlin/JVM | 12.881<sub>±0.169</sub> |    43.14<sub>±00.10</sub> + 1.86<sub>±00.42</sub> |  539.50<sub>±07.57</sub> |
|           F#/.NET Core | 13.105<sub>±0.036</sub> |    37.20<sub>±00.05</sub> + 2.06<sub>±00.00</sub> |  523.37<sub>±01.78</sub> |
|                C/clang | 13.128<sub>±0.023</sub> |     0.88<sub>±00.01</sub> + 0.90<sub>±00.00</sub> |  565.41<sub>±03.18</sub> |
|                    Zig | 13.367<sub>±0.016</sub> |     0.89<sub>±00.02</sub> + 1.42<sub>±00.06</sub> |  556.41<sub>±00.94</sub> |
|                  V/gcc | 13.757<sub>±0.038</sub> |     1.82<sub>±00.02</sub> + 1.16<sub>±00.03</sub> |  552.45<sub>±02.88</sub> |
|            C++/clang++ | 13.831<sub>±0.010</sub> |     1.59<sub>±00.01</sub> + 1.94<sub>±00.04</sub> |  561.99<sub>±03.09</sub> |
|        Racket (Staged) | 13.962<sub>±0.085</sub> |  100.54<sub>±00.37</sub> + 74.55<sub>±01.45</sub> |  554.37<sub>±02.00</sub> |
|                 D/ldc2 | 14.037<sub>±0.042</sub> |     3.02<sub>±00.03</sub> + 0.79<sub>±00.02</sub> |  563.77<sub>±01.66</sub> |
|                   Rust | 14.202<sub>±0.012</sub> |     0.91<sub>±00.01</sub> + 1.11<sub>±00.04</sub> |  565.92<sub>±00.87</sub> |
|                     Go | 14.223<sub>±0.027</sub> |     2.90<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  564.53<sub>±01.56</sub> |
|                  D/gdc | 14.392<sub>±0.025</sub> |     6.29<sub>±00.02</sub> + 1.43<sub>±00.04</sub> |  606.89<sub>±01.67</sub> |
|               Vala/gcc | 14.402<sub>±0.034</sub> |     4.45<sub>±00.04</sub> + 1.21<sub>±00.05</sub> |  570.53<sub>±01.91</sub> |
|             Vala/clang | 14.821<sub>±0.013</sub> |     4.45<sub>±00.05</sub> + 1.23<sub>±00.03</sub> |  604.34<sub>±04.34</sub> |
|                Crystal | 15.370<sub>±0.015</sub> |     2.90<sub>±00.04</sub> + 0.75<sub>±00.04</sub> |  641.58<sub>±02.85</sub> |
|                Nim/gcc | 15.526<sub>±0.017</sub> |     2.07<sub>±00.03</sub> + 1.29<sub>±00.00</sub> |  655.88<sub>±00.74</sub> |
|                  Scala | 15.947<sub>±0.026</sub> |  72.63<sub>±00.25</sub> + 136.99<sub>±00.15</sub> |  718.79<sub>±03.07</sub> |
|                  Swift | 18.190<sub>±0.029</sub> |    16.31<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  742.06<sub>±03.24</sub> |
|               Go/gccgo | 18.785<sub>±0.181</sub> |    23.45<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  782.84<sub>±07.00</sub> |
|              Nim/clang | 19.782<sub>±0.317</sub> |     2.34<sub>±00.05</sub> + 1.29<sub>±00.00</sub> |  811.47<sub>±18.83</sub> |
|                V/clang | 20.726<sub>±0.114</sub> |     1.84<sub>±00.02</sub> + 1.18<sub>±00.01</sub> |  908.93<sub>±07.01</sub> |
|                  OCaml | 25.253<sub>±0.009</sub> |     4.03<sub>±00.04</sub> + 3.51<sub>±00.03</sub> | 1202.82<sub>±02.37</sub> |
|                Node.js | 27.677<sub>±0.770</sub> |    39.36<sub>±00.02</sub> + 6.38<sub>±00.45</sub> | 1158.13<sub>±33.62</sub> |
|            Chez Scheme | 27.820<sub>±0.019</sub> |    25.51<sub>±00.04</sub> + 3.69<sub>±00.02</sub> | 1208.12<sub>±05.81</sub> |
|                  Julia | 29.420<sub>±0.209</sub> |   249.62<sub>±00.04</sub> + 0.40<sub>±00.02</sub> | 1125.11<sub>±15.07</sub> |
|                C#/Mono | 31.007<sub>±0.025</sub> |    25.64<sub>±00.05</sub> + 0.83<sub>±00.00</sub> | 1292.31<sub>±07.06</sub> |
|                  MLton | 33.812<sub>±0.028</sub> |     1.68<sub>±00.02</sub> + 4.11<sub>±00.00</sub> | 1543.08<sub>±11.31</sub> |
|             Lua/luajit | 34.664<sub>±0.047</sub> |     2.55<sub>±00.05</sub> + 0.44<sub>±00.00</sub> | 1388.00<sub>±04.02</sub> |
|                 Racket | 35.182<sub>±0.883</sub> |   92.97<sub>±00.11</sub> + 22.08<sub>±00.90</sub> | 1563.87<sub>±32.83</sub> |
|       Haskell (MArray) | 35.583<sub>±0.080</sub> |     4.41<sub>±00.03</sub> + 4.74<sub>±00.00</sub> | 1430.91<sub>±02.75</sub> |
|                  D/dmd | 37.966<sub>±0.007</sub> |     3.24<sub>±00.04</sub> + 0.87<sub>±00.01</sub> | 1375.78<sub>±00.62</sub> |
|            Python/pypy | 40.842<sub>±0.111</sub> |   59.56<sub>±00.06</sub> + 30.34<sub>±00.12</sub> | 1798.94<sub>±06.36</sub> |
|       Ruby/truffleruby | 47.770<sub>±1.153</sub> | 231.23<sub>±02.27</sub> + 596.94<sub>±70.17</sub> | 2293.29<sub>±24.16</sub> |
| Ruby/truffleruby (JVM) | 49.197<sub>±0.669</sub> | 404.03<sub>±07.49</sub> + 486.99<sub>±42.94</sub> | 2250.75<sub>±38.16</sub> |
|                  Idris | 66.213<sub>±0.198</sub> |    21.99<sub>±00.01</sub> + 9.54<sub>±00.01</sub> | 2841.60<sub>±11.91</sub> |
|           Haskell (FP) | 78.752<sub>±0.164</sub> |    3.81<sub>±00.50</sub> + 75.23<sub>±00.47</sub> | 3253.62<sub>±11.87</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|          C/clang (aklomp) |  0.096<sub>±0.000</sub> |     2.05<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   4.57<sub>±00.03</sub> |
|            C/gcc (aklomp) |  0.098<sub>±0.000</sub> |     2.12<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   4.65<sub>±00.03</sub> |
|                       PHP |  0.105<sub>±0.000</sub> |    18.61<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |   4.91<sub>±00.01</sub> |
|              Go (base64x) |  0.265<sub>±0.003</sub> |     6.24<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  12.61<sub>±00.12</sub> |
|                      Rust |  0.849<sub>±0.000</sub> |     2.32<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  34.90<sub>±00.07</sub> |
|                   C/clang |  0.997<sub>±0.000</sub> |     1.97<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  36.59<sub>±00.03</sub> |
|                    D/ldc2 |  1.070<sub>±0.003</sub> |     3.60<sub>±00.03</sub> + 3.40<sub>±00.00</sub> |  44.54<sub>±00.20</sub> |
|                     C/gcc |  1.091<sub>±0.005</sub> |     1.99<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  39.84<sub>±00.12</sub> |
|                   Crystal |  1.099<sub>±0.000</sub> |     3.56<sub>±00.02</sub> + 1.31<sub>±00.03</sub> |  44.77<sub>±00.26</sub> |
|                 Nim/clang |  1.102<sub>±0.001</sub> |     1.97<sub>±00.02</sub> + 5.82<sub>±00.04</sub> |  44.69<sub>±00.16</sub> |
|                   Nim/gcc |  1.327<sub>±0.003</sub> |     1.66<sub>±00.03</sub> + 5.27<sub>±00.06</sub> |  54.63<sub>±00.31</sub> |
|                      Java |  1.517<sub>±0.003</sub> |  40.73<sub>±00.03</sub> + 223.62<sub>±25.54</sub> |  59.90<sub>±00.23</sub> |
|                   V/clang |  1.520<sub>±0.001</sub> |  2.34<sub>±00.01</sub> + 2376.76<sub>±01.68</sub> |  57.27<sub>±00.17</sub> |
|                     V/gcc |  1.555<sub>±0.001</sub> |  2.32<sub>±00.01</sub> + 2384.53<sub>±02.19</sub> |  56.53<sub>±00.08</sub> |
|                     Scala |  1.584<sub>±0.001</sub> |  68.49<sub>±00.42</sub> + 310.69<sub>±04.21</sub> |  64.45<sub>±00.28</sub> |
|                Vala/clang |  1.644<sub>±0.001</sub> |     5.63<sub>±00.05</sub> + 0.01<sub>±00.00</sub> |  62.66<sub>±00.31</sub> |
|                  Vala/gcc |  1.644<sub>±0.001</sub> |     5.69<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  62.74<sub>±00.27</sub> |
|                Kotlin/JVM |  1.645<sub>±0.001</sub> |  44.08<sub>±00.26</sub> + 246.42<sub>±02.94</sub> |  66.00<sub>±00.48</sub> |
|              Ruby (--jit) |  1.653<sub>±0.001</sub> |   16.86<sub>±00.04</sub> + 39.29<sub>±00.26</sub> |  64.04<sub>±00.12</sub> |
|                      Ruby |  1.654<sub>±0.002</sub> |   15.61<sub>±00.01</sub> + 42.85<sub>±00.44</sub> |  64.36<sub>±00.09</sub> |
|       C++/g++ (libcrypto) |  1.707<sub>±0.002</sub> |     5.66<sub>±00.04</sub> + 0.64<sub>±00.06</sub> |  68.31<sub>±00.81</sub> |
|                        Go |  1.708<sub>±0.003</sub> |     3.69<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  71.71<sub>±00.42</sub> |
|   C++/clang++ (libcrypto) |  1.711<sub>±0.002</sub> |     4.99<sub>±00.05</sub> + 0.67<sub>±00.00</sub> |  67.93<sub>±00.47</sub> |
|                   Node.js |  1.718<sub>±0.008</sub> |   39.64<sub>±00.08</sub> + 36.76<sub>±00.25</sub> |  70.98<sub>±00.40</sub> |
|       Perl (MIME::Base64) |  1.899<sub>±0.007</sub> |    14.79<sub>±00.04</sub> + 0.13<sub>±00.04</sub> |  75.33<sub>±00.56</sub> |
|              F#/.NET Core |  2.056<sub>±0.022</sub> |   38.13<sub>±00.12</sub> + 12.78<sub>±00.98</sub> |  81.15<sub>±00.66</sub> |
|              C#/.NET Core |  2.168<sub>±0.011</sub> |   33.64<sub>±00.09</sub> + 12.27<sub>±01.86</sub> |  85.60<sub>±00.41</sub> |
|                     D/gdc |  2.376<sub>±0.002</sub> |     7.32<sub>±00.02</sub> + 3.36<sub>±00.00</sub> | 102.54<sub>±00.71</sub> |
|                  Go/gccgo |  2.946<sub>±0.004</sub> |    24.36<sub>±00.07</sub> + 0.00<sub>±00.00</sub> | 139.41<sub>±00.45</sub> |
|                     Julia |  2.976<sub>±0.003</sub> |  265.73<sub>±00.10</sub> + 43.68<sub>±00.12</sub> | 119.09<sub>±01.00</sub> |
|                    Python |  3.029<sub>±0.028</sub> |    10.27<sub>±00.02</sub> + 0.09<sub>±00.00</sub> | 117.09<sub>±00.92</sub> |
|                       Zig |  3.199<sub>±0.006</sub> |     1.51<sub>±00.02</sub> + 0.00<sub>±00.00</sub> | 123.98<sub>±00.21</sub> |
|               Python/pypy |  3.252<sub>±0.006</sub> |   59.46<sub>±00.07</sub> + 31.64<sub>±00.06</sub> | 141.73<sub>±00.94</sub> |
|                     D/dmd |  3.307<sub>±0.003</sub> |     3.06<sub>±00.03</sub> + 3.89<sub>±00.02</sub> | 138.40<sub>±00.38</sub> |
|                       Tcl |  3.563<sub>±0.002</sub> |     5.12<sub>±00.01</sub> + 0.00<sub>±00.00</sub> | 143.20<sub>±01.71</sub> |
|    Ruby/truffleruby (JVM) |  3.598<sub>±0.167</sub> | 389.55<sub>±04.55</sub> + 282.15<sub>±14.81</sub> | 181.14<sub>±06.48</sub> |
|                    Racket |  3.899<sub>±0.017</sub> |   91.14<sub>±00.07</sub> + 19.11<sub>±00.54</sub> | 154.12<sub>±01.14</sub> |
|                   C#/Mono |  4.639<sub>±0.004</sub> |   26.31<sub>±00.12</sub> + 18.68<sub>±00.04</sub> | 187.27<sub>±01.21</sub> |
|                Ruby/jruby |  6.161<sub>±0.019</sub> | 194.23<sub>±07.33</sub> + 147.01<sub>±29.67</sub> | 268.70<sub>±02.36</sub> |
|          Ruby/truffleruby |  7.748<sub>±0.008</sub> | 226.42<sub>±02.02</sub> + 532.61<sub>±40.35</sub> | 372.98<sub>±00.98</sub> |
| Perl (MIME::Base64::Perl) | 10.104<sub>±0.066</sub> |    16.18<sub>±00.06</sub> + 0.39<sub>±00.10</sub> | 445.74<sub>±02.69</sub> |

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
|    C++/clang++ (simdjson On-Demand) |  0.060<sub>±0.000</sub> |    112.36<sub>±00.09</sub> + 60.11<sub>±00.03</sub> |   2.50<sub>±00.01</sub> |
|        C++/g++ (simdjson On-Demand) |  0.061<sub>±0.000</sub> |    113.47<sub>±00.03</sub> + 59.81<sub>±00.00</sub> |   2.55<sub>±00.01</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.082<sub>±0.000</sub> |     112.40<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.36<sub>±00.02</sub> |
|         C++/clang++ (DAW JSON Link) |  0.083<sub>±0.000</sub> |     112.32<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.49<sub>±00.02</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.083<sub>±0.000</sub> |     113.07<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.33<sub>±00.01</sub> |
|             C++/g++ (DAW JSON Link) |  0.087<sub>±0.000</sub> |     113.14<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.64<sub>±00.02</sub> |
|                  Rust (Serde Typed) |  0.098<sub>±0.001</sub> |    111.63<sub>±00.02</sub> + 11.25<sub>±00.07</sub> |   4.16<sub>±00.02</sub> |
|          C++/clang++ (simdjson DOM) |  0.100<sub>±0.001</sub> |   112.38<sub>±00.04</sub> + 177.15<sub>±00.03</sub> |   4.55<sub>±00.06</sub> |
|                 Rust (Serde Custom) |  0.102<sub>±0.000</sub> |     111.65<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   4.28<sub>±00.03</sub> |
|              C++/g++ (simdjson DOM) |  0.106<sub>±0.001</sub> |   113.49<sub>±00.02</sub> + 173.38<sub>±00.57</sub> |   4.84<sub>±00.05</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.133<sub>±0.000</sub> |    112.76<sub>±00.03</sub> + 61.22<sub>±00.00</sub> |   5.51<sub>±00.06</sub> |
|                 C++/clang++ (gason) |  0.139<sub>±0.000</sub> |    112.40<sub>±00.01</sub> + 96.97<sub>±00.06</sub> |   5.63<sub>±00.02</sub> |
|                     C++/g++ (gason) |  0.140<sub>±0.000</sub> |    113.09<sub>±00.06</sub> + 96.97<sub>±00.06</sub> |   5.55<sub>±00.01</sub> |
|                 C++/g++ (RapidJSON) |  0.152<sub>±0.000</sub> |   113.12<sub>±00.04</sub> + 128.94<sub>±00.06</sub> |   6.47<sub>±00.07</sub> |
|              Scala (jsoniter-scala) |  0.156<sub>±0.002</sub> |    291.63<sub>±00.25</sub> + 19.39<sub>±00.22</sub> |   8.76<sub>±00.12</sub> |
|                   Go (rjson custom) |  0.198<sub>±0.000</sub> |     114.75<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   7.64<sub>±00.01</sub> |
|             C++/clang++ (RapidJSON) |  0.202<sub>±0.000</sub> |   112.41<sub>±00.04</sub> + 129.00<sub>±00.00</sub> |   8.61<sub>±00.07</sub> |
|         C++/g++ (RapidJSON Precise) |  0.216<sub>±0.001</sub> |   113.09<sub>±00.02</sub> + 126.54<sub>±01.43</sub> |   9.20<sub>±00.03</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.217<sub>±0.000</sub> |    112.86<sub>±00.02</sub> + 80.70<sub>±00.00</sub> |   9.14<sub>±00.05</sub> |
|                          Go (Sonic) |  0.219<sub>±0.003</sub> |     122.98<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   9.50<sub>±00.14</sub> |
|                                 Zig |  0.222<sub>±0.000</sub> |    110.92<sub>±00.01</sub> + 39.28<sub>±00.29</sub> |   9.65<sub>±00.04</sub> |
|                          Go (rjson) |  0.233<sub>±0.000</sub> |     114.82<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   9.01<sub>±00.02</sub> |
|                  Go (goccy/go-json) |  0.269<sub>±0.000</sub> |     115.49<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  10.55<sub>±00.04</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.284<sub>±0.001</sub> |   112.39<sub>±00.04</sub> + 129.00<sub>±00.00</sub> |  12.32<sub>±00.14</sub> |
|             C++/g++ (RapidJSON SAX) |  0.332<sub>±0.000</sub> |     112.96<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  14.76<sub>±00.05</sub> |
|                        C/gcc (yajl) |  0.359<sub>±0.001</sub> |     110.89<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  15.48<sub>±00.08</sub> |
|                      C/clang (yajl) |  0.360<sub>±0.000</sub> |     110.84<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  15.53<sub>±00.03</sub> |
|                C++/g++ (Boost.JSON) |  0.360<sub>±0.001</sub> |   113.23<sub>±00.01</sub> + 308.16<sub>±00.02</sub> |  15.36<sub>±00.10</sub> |
|            C++/clang++ (Boost.JSON) |  0.369<sub>±0.001</sub> |   112.51<sub>±00.04</sub> + 308.15<sub>±00.03</sub> |  15.73<sub>±00.08</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.385<sub>±0.000</sub> |     112.97<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  17.20<sub>±00.18</sub> |
|                   Nim/clang (jsony) |  0.392<sub>±0.000</sub> |   111.41<sub>±00.05</sub> + 146.15<sub>±00.10</sub> |  16.44<sub>±00.06</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.404<sub>±0.000</sub> |     194.70<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  17.14<sub>±00.03</sub> |
|                     Nim/gcc (jsony) |  0.408<sub>±0.001</sub> |   111.10<sub>±00.03</sub> + 154.80<sub>±00.25</sub> |  17.45<sub>±00.15</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.492<sub>±0.001</sub> |     194.62<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  21.71<sub>±00.10</sub> |
|                       Go (jsoniter) |  0.502<sub>±0.001</sub> |     115.53<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  20.34<sub>±00.12</sub> |
|                Rust (Serde Untyped) |  0.532<sub>±0.001</sub> |   111.58<sub>±00.01</sub> + 840.04<sub>±00.01</sub> |  22.22<sub>±00.07</sub> |
|     C#/.NET Core (System.Text.Json) |  0.543<sub>±0.003</sub> |   489.95<sub>±00.13</sub> + 140.83<sub>±00.09</sub> |  24.34<sub>±00.10</sub> |
|                       Julia (JSON3) |  0.581<sub>±0.001</sub> |   468.48<sub>±00.06</sub> + 221.19<sub>±00.97</sub> |  24.74<sub>±00.22</sub> |
|                             Node.js |  0.587<sub>±0.008</sub> |   150.63<sub>±00.03</sub> + 195.31<sub>±00.62</sub> |  27.66<sub>±00.37</sub> |
|                     Java (DSL-JSON) |  0.606<sub>±0.015</sub> |   262.58<sub>±00.11</sub> + 198.42<sub>±47.26</sub> |  31.13<sub>±00.90</sub> |
|                         Python/pypy |  0.609<sub>±0.002</sub> |   279.78<sub>±00.07</sub> + 125.78<sub>±00.10</sub> |  26.14<sub>±00.20</sub> |
|                               V/gcc |  0.612<sub>±0.002</sub> |   111.38<sub>±00.03</sub> + 496.18<sub>±00.03</sub> |  25.55<sub>±00.05</sub> |
|                             V/clang |  0.614<sub>±0.001</sub> |   111.46<sub>±00.03</sub> + 496.21<sub>±00.00</sub> |  25.87<sub>±00.30</sub> |
|                Nim/gcc (Packedjson) |  0.627<sub>±0.002</sub> |   111.85<sub>±00.02</sub> + 294.16<sub>±00.00</sub> |  26.56<sub>±00.17</sub> |
|                      Crystal (Pull) |  0.630<sub>±0.001</sub> |    113.23<sub>±00.02</sub> + 18.39<sub>±00.03</sub> |  27.56<sub>±00.13</sub> |
|              Nim/clang (Packedjson) |  0.645<sub>±0.002</sub> |   112.19<sub>±00.04</sub> + 294.16<sub>±00.00</sub> |  27.66<sub>±00.14</sub> |
|                    Crystal (Schema) |  0.657<sub>±0.001</sub> |    113.25<sub>±00.01</sub> + 48.84<sub>±00.02</sub> |  28.76<sub>±00.16</sub> |
|             Perl (Cpanel::JSON::XS) |  0.760<sub>±0.005</sub> |   125.54<sub>±00.05</sub> + 402.80<sub>±00.03</sub> |  31.86<sub>±00.10</sub> |
|                                 PHP |  0.806<sub>±0.002</sub> |   127.71<sub>±00.09</sub> + 517.86<sub>±00.06</sub> |  34.34<sub>±00.11</sub> |
|                                  Go |  0.865<sub>±0.002</sub> |     114.90<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |  35.63<sub>±00.07</sub> |
|                             Crystal |  0.930<sub>±0.004</sub> |   113.27<sub>±00.03</sub> + 392.50<sub>±00.00</sub> |  40.26<sub>±00.42</sub> |
|                             Nim/gcc |  1.011<sub>±0.002</sub> |  111.87<sub>±00.03</sub> + 1001.34<sub>±00.00</sub> |  42.14<sub>±00.24</sub> |
|                           Nim/clang |  1.050<sub>±0.003</sub> |   112.14<sub>±00.01</sub> + 999.02<sub>±00.00</sub> |  43.68<sub>±00.16</sub> |
|                        C#/.NET Core |  1.054<sub>±0.005</sub> |   495.82<sub>±00.18</sub> + 273.33<sub>±00.02</sub> |  50.59<sub>±00.39</sub> |
|                C++/clang++ (json-c) |  1.124<sub>±0.002</sub> |  112.61<sub>±00.04</sub> + 1216.08<sub>±00.00</sub> |  46.57<sub>±00.40</sub> |
|                    C++/g++ (json-c) |  1.125<sub>±0.004</sub> |  113.22<sub>±00.05</sub> + 1216.08<sub>±00.05</sub> |  47.30<sub>±00.59</sub> |
|              C++/clang++ (Nlohmann) |  1.169<sub>±0.003</sub> |   112.58<sub>±00.02</sub> + 360.17<sub>±00.03</sub> |  50.24<sub>±00.37</sub> |
|                             Clojure |  1.189<sub>±0.018</sub> |   453.04<sub>±02.63</sub> + 627.61<sub>±03.05</sub> |  64.02<sub>±00.87</sub> |
|                 CPython (UltraJSON) |  1.275<sub>±0.007</sub> |   122.68<sub>±00.01</sub> + 495.97<sub>±02.20</sub> |  47.32<sub>±00.25</sub> |
|                              Python |  1.310<sub>±0.003</sub> |   120.04<sub>±00.01</sub> + 326.36<sub>±00.04</sub> |  51.84<sub>±00.08</sub> |
|                            Go/gccgo |  1.312<sub>±0.001</sub> |     138.89<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  53.70<sub>±00.06</sub> |
|                  C++/g++ (Nlohmann) |  1.318<sub>±0.004</sub> |   113.27<sub>±00.03</sub> + 448.05<sub>±00.00</sub> |  55.69<sub>±00.47</sub> |
|                                Ruby |  1.431<sub>±0.007</sub> |   125.11<sub>±00.02</sub> + 261.54<sub>±00.04</sub> |  59.35<sub>±00.32</sub> |
|     F#/.NET Core (System.Text.Json) |  1.490<sub>±0.003</sub> |   498.50<sub>±00.19</sub> + 228.02<sub>±01.81</sub> |  68.39<sub>±00.53</sub> |
|                        Ruby (--jit) |  1.511<sub>±0.005</sub> |   126.20<sub>±00.03</sub> + 263.91<sub>±00.04</sub> |  63.62<sub>±00.52</sub> |
|                              D/ldc2 |  1.709<sub>±0.005</sub> |   112.68<sub>±00.04</sub> + 680.34<sub>±00.05</sub> |  71.02<sub>±00.52</sub> |
|                         Ruby (YAJL) |  1.710<sub>±0.007</sub> |   125.05<sub>±00.04</sub> + 276.06<sub>±00.03</sub> |  71.72<sub>±00.46</sub> |
|                             C#/Mono |  1.794<sub>±0.018</sub> |    252.95<sub>±00.09</sub> + 31.51<sub>±00.01</sub> |  78.14<sub>±00.87</sub> |
|                             Haskell |  1.954<sub>±0.010</sub> |   115.54<sub>±00.19</sub> + 724.11<sub>±00.16</sub> |  83.60<sub>±00.51</sub> |
|                           Rust (jq) |  2.514<sub>±0.006</sub> |   113.34<sub>±00.06</sub> + 904.16<sub>±01.28</sub> | 104.73<sub>±00.81</sub> |
|        C++/g++ (Boost.PropertyTree) |  2.610<sub>±0.004</sub> |  113.10<sub>±00.02</sub> + 1440.12<sub>±00.00</sub> | 111.42<sub>±00.31</sub> |
|    C++/clang++ (Boost.PropertyTree) |  2.675<sub>±0.004</sub> |  194.86<sub>±00.08</sub> + 1232.84<sub>±00.00</sub> | 112.42<sub>±00.25</sub> |
|                          Ruby/jruby |  2.852<sub>±0.034</sub> |  459.95<sub>±04.84</sub> + 924.41<sub>±111.79</sub> | 147.11<sub>±01.36</sub> |
|                               D/dmd |  3.062<sub>±0.004</sub> |   113.09<sub>±00.02</sub> + 708.78<sub>±00.06</sub> | 130.38<sub>±00.62</sub> |
|                          Vala/clang |  3.219<sub>±0.007</sub> |   114.97<sub>±00.06</sub> + 980.04<sub>±00.01</sub> | 137.44<sub>±01.21</sub> |
|                            Vala/gcc |  3.237<sub>±0.020</sub> |   114.98<sub>±00.05</sub> + 980.04<sub>±00.03</sub> | 138.29<sub>±00.82</sub> |
|                               D/gdc |  3.507<sub>±0.007</sub> |   116.49<sub>±00.02</sub> + 681.00<sub>±00.13</sub> | 148.49<sub>±01.16</sub> |
|                              Racket |  3.822<sub>±0.020</sub> |   315.73<sub>±01.52</sub> + 228.04<sub>±01.93</sub> | 158.45<sub>±00.60</sub> |
|                   Perl (JSON::Tiny) |  9.261<sub>±0.027</sub> |   125.80<sub>±00.05</sub> + 528.96<sub>±00.07</sub> | 403.49<sub>±05.37</sub> |
|                    Ruby/truffleruby | 10.559<sub>±0.056</sub> | 453.64<sub>±07.39</sub> + 1996.42<sub>±188.53</sub> | 606.35<sub>±02.82</sub> |
|              Ruby/truffleruby (JVM) | 10.889<sub>±0.111</sub> | 507.95<sub>±09.50</sub> + 2524.50<sub>±190.33</sub> | 684.04<sub>±07.51</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :---------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|         D/ldc2 (lubeck) |    0.042<sub>±0.000</sub> |    6.05<sub>±00.02</sub> + 57.76<sub>±00.04</sub> |      4.39<sub>±00.03</sub> |
|     V/gcc (VSL + CBLAS) |    0.046<sub>±0.000</sub> |    6.65<sub>±00.01</sub> + 58.28<sub>±00.00</sub> |      4.67<sub>±00.02</sub> |
|   V/clang (VSL + CBLAS) |    0.046<sub>±0.000</sub> |    6.66<sub>±00.00</sub> + 58.28<sub>±00.00</sub> |      4.65<sub>±00.03</sub> |
|   Nim/gcc (Arraymancer) |    0.057<sub>±0.001</sub> |    5.46<sub>±00.01</sub> + 57.54<sub>±00.08</sub> |      5.12<sub>±00.12</sub> |
|          Python (NumPy) |    0.063<sub>±0.000</sub> |   31.89<sub>±00.03</sub> + 58.47<sub>±00.05</sub> |      6.01<sub>±00.03</sub> |
| Nim/clang (Arraymancer) |    0.069<sub>±0.002</sub> |    6.08<sub>±00.12</sub> + 57.64<sub>±00.11</sub> |      6.21<sub>±00.46</sub> |
|             Java (ND4J) |    0.076<sub>±0.001</sub> |  111.19<sub>±00.44</sub> + 92.15<sub>±00.01</sub> |      6.10<sub>±00.09</sub> |
|          Rust (ndarray) |    0.084<sub>±0.001</sub> |    2.28<sub>±00.02</sub> + 68.47<sub>±00.00</sub> |      5.88<sub>±00.03</sub> |
|      Julia (threads: 2) |    0.084<sub>±0.000</sub> |  285.04<sub>±00.19</sub> + 57.15<sub>±00.10</sub> |      5.30<sub>±00.02</sub> |
|      Julia (threads: 1) |    0.134<sub>±0.000</sub> |  285.02<sub>±00.10</sub> + 56.87<sub>±00.07</sub> |      6.67<sub>±00.04</sub> |
|         C++/g++ (Eigen) |    0.141<sub>±0.000</sub> |    4.33<sub>±00.03</sub> + 85.25<sub>±00.00</sub> |      7.02<sub>±00.08</sub> |
|     C++/clang++ (Eigen) |    0.143<sub>±0.000</sub> |    4.72<sub>±00.03</sub> + 85.37<sub>±00.00</sub> |      6.98<sub>±00.04</sub> |
|           V/clang (VSL) |    0.265<sub>±0.002</sub> |    7.33<sub>±00.05</sub> + 51.57<sub>±00.00</sub> |     18.57<sub>±00.09</sub> |
|             V/gcc (VSL) |    0.508<sub>±0.005</sub> |    7.05<sub>±00.07</sub> + 51.83<sub>±00.00</sub> |     36.92<sub>±00.31</sub> |
|         Julia (no BLAS) |    1.019<sub>±0.001</sub> |  267.03<sub>±00.09</sub> + 51.55<sub>±00.02</sub> |     45.33<sub>±00.41</sub> |
|                  D/ldc2 |    1.716<sub>±0.002</sub> |    3.25<sub>±00.03</sub> + 70.41<sub>±00.03</sub> |     63.27<sub>±00.12</sub> |
|                   D/gdc |    1.869<sub>±0.001</sub> |    7.30<sub>±00.05</sub> + 70.16<sub>±00.01</sub> |     73.09<sub>±00.06</sub> |
|                   D/dmd |    1.879<sub>±0.001</sub> |    3.17<sub>±00.01</sub> + 70.45<sub>±00.06</sub> |     71.11<sub>±00.08</sub> |
|                   C/gcc |    3.024<sub>±0.000</sub> |    1.47<sub>±00.04</sub> + 68.69<sub>±00.02</sub> |    111.60<sub>±00.32</sub> |
|                   V/gcc |    3.029<sub>±0.001</sub> |    2.50<sub>±00.07</sub> + 68.58<sub>±00.00</sub> |    112.30<sub>±00.15</sub> |
|              Vala/clang |    3.055<sub>±0.000</sub> |    5.45<sub>±00.03</sub> + 68.32<sub>±00.00</sub> |    104.58<sub>±00.38</sub> |
|                 V/clang |    3.058<sub>±0.000</sub> |    2.82<sub>±00.02</sub> + 68.58<sub>±00.00</sub> |    104.68<sub>±00.27</sub> |
|                 C/clang |    3.059<sub>±0.000</sub> |    1.48<sub>±00.02</sub> + 68.69<sub>±00.02</sub> |    104.43<sub>±00.07</sub> |
|                    Rust |    3.060<sub>±0.000</sub> |    2.08<sub>±00.01</sub> + 68.57<sub>±00.00</sub> |    104.85<sub>±00.38</sub> |
|                     Zig |    3.063<sub>±0.001</sub> |    1.77<sub>±00.04</sub> + 68.58<sub>±00.00</sub> |    108.28<sub>±00.08</sub> |
|                 Nim/gcc |    3.086<sub>±0.001</sub> |    2.50<sub>±00.01</sub> + 58.65<sub>±00.90</sub> |    114.10<sub>±00.18</sub> |
|                   Swift |    3.087<sub>±0.000</sub> |    7.92<sub>±00.01</sub> + 68.75<sub>±00.00</sub> |    110.56<sub>±00.70</sub> |
|               Nim/clang |    3.115<sub>±0.001</sub> |    2.81<sub>±00.03</sub> + 59.55<sub>±01.80</sub> |    107.17<sub>±00.80</sub> |
|                Vala/gcc |    3.120<sub>±0.000</sub> |    4.07<sub>±00.12</sub> + 69.68<sub>±00.07</sub> |    114.14<sub>±00.13</sub> |
|                    Java |    3.122<sub>±0.054</sub> |   40.63<sub>±00.08</sub> + 68.78<sub>±00.42</sub> |    121.93<sub>±01.25</sub> |
|                      Go |    3.144<sub>±0.000</sub> |     3.16<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |    113.86<sub>±00.30</sub> |
|                 Crystal |    3.148<sub>±0.000</sub> |    3.58<sub>±00.04</sub> + 60.04<sub>±00.05</sub> |    115.34<sub>±00.21</sub> |
|                Go/gccgo |    3.149<sub>±0.001</sub> |    24.08<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |    110.56<sub>±00.11</sub> |
|              Kotlin/JVM |    3.198<sub>±0.004</sub> |   41.84<sub>±00.09</sub> + 69.12<sub>±00.11</sub> |    129.91<sub>±00.57</sub> |
|                 Node.js |    3.215<sub>±0.003</sub> |   44.18<sub>±00.03</sub> + 73.52<sub>±00.25</sub> |    129.44<sub>±00.13</sub> |
|             Python/pypy |    3.254<sub>±0.002</sub> |   60.25<sub>±00.06</sub> + 68.93<sub>±00.04</sub> |    135.10<sub>±00.11</sub> |
|                   Scala |    3.291<sub>±0.004</sub> |  68.87<sub>±00.16</sub> + 160.97<sub>±00.31</sub> |    119.99<sub>±00.09</sub> |
|            C#/.NET Core |    4.892<sub>±0.001</sub> |   34.50<sub>±00.06</sub> + 68.91<sub>±00.00</sub> |    196.60<sub>±00.37</sub> |
|                 C#/Mono |    7.391<sub>±0.000</sub> |   26.07<sub>±00.06</sub> + 69.47<sub>±00.01</sub> |    303.45<sub>±02.00</sub> |
|        Ruby/truffleruby |   17.994<sub>±0.531</sub> | 393.10<sub>±15.17</sub> + 540.22<sub>±45.98</sub> |    642.97<sub>±17.80</sub> |
|  Ruby/truffleruby (JVM) |   24.992<sub>±0.621</sub> | 426.21<sub>±15.07</sub> + 375.57<sub>±32.72</sub> |    878.03<sub>±25.98</sub> |
|            Ruby (--jit) |  130.222<sub>±0.647</sub> |   17.96<sub>±00.11</sub> + 69.69<sub>±00.08</sub> |   5759.30<sub>±56.19</sub> |
|                  Python |  137.008<sub>±1.286</sub> |   10.50<sub>±00.01</sub> + 68.84<sub>±00.00</sub> |   5966.24<sub>±69.75</sub> |
|                    Ruby |  147.268<sub>±0.419</sub> |   15.82<sub>±00.06</sub> + 69.13<sub>±00.02</sub> |   6486.49<sub>±54.62</sub> |
|                     Tcl |  203.860<sub>±0.610</sub> |   7.29<sub>±00.04</sub> + 400.44<sub>±00.00</sub> |   9399.36<sub>±48.95</sub> |
|                    Perl |  211.497<sub>±2.918</sub> |   9.52<sub>±00.03</sub> + 599.66<sub>±00.06</sub> |   8506.55<sub>±88.52</sub> |
|              Ruby/jruby | 372.794<sub>±11.975</sub> | 286.44<sub>±15.01</sub> + 731.29<sub>±47.64</sub> | 15595.51<sub>±522.26</sub> |

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
|                    Zig | 0.055<sub>±0.000</sub> |    0.89<sub>±00.02</sub> + 52.96<sub>±00.13</sub> |   2.30<sub>±00.03</sub> |
|            C++/clang++ | 0.061<sub>±0.000</sub> |    3.08<sub>±00.03</sub> + 55.45<sub>±00.13</sub> |   2.33<sub>±00.02</sub> |
|                C++/g++ | 0.061<sub>±0.000</sub> |    3.62<sub>±00.03</sub> + 71.33<sub>±00.13</sub> |   2.41<sub>±00.02</sub> |
|                     Go | 0.075<sub>±0.001</sub> |     2.98<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.12<sub>±00.05</sub> |
|                V/clang | 0.094<sub>±0.000</sub> |   1.85<sub>±00.01</sub> + 213.78<sub>±00.77</sub> |   3.88<sub>±00.04</sub> |
|                  V/gcc | 0.100<sub>±0.000</sub> |   1.85<sub>±00.04</sub> + 200.32<sub>±00.52</sub> |   4.10<sub>±00.05</sub> |
|                   Rust | 0.116<sub>±0.000</sub> |    2.04<sub>±00.06</sub> + 72.94<sub>±00.00</sub> |   4.51<sub>±00.04</sub> |
|                Crystal | 0.137<sub>±0.000</sub> |    3.67<sub>±00.05</sub> + 88.43<sub>±00.00</sub> |   5.60<sub>±00.05</sub> |
|                   Java | 0.142<sub>±0.004</sub> |  39.67<sub>±00.23</sub> + 152.19<sub>±02.26</sub> |   8.17<sub>±00.25</sub> |
|                  Scala | 0.205<sub>±0.006</sub> |  73.00<sub>±00.19</sub> + 211.33<sub>±01.15</sub> |  11.51<sub>±00.26</sub> |
|                Node.js | 0.259<sub>±0.001</sub> |  38.48<sub>±00.01</sub> + 151.74<sub>±00.18</sub> |  12.62<sub>±00.09</sub> |
|              Nim/clang | 0.274<sub>±0.000</sub> |   2.03<sub>±00.02</sub> + 605.34<sub>±01.29</sub> |  10.54<sub>±00.08</sub> |
|                Nim/gcc | 0.281<sub>±0.001</sub> |   1.71<sub>±00.06</sub> + 615.91<sub>±00.00</sub> |  10.62<sub>±00.09</sub> |
|             Lua/luajit | 0.296<sub>±0.001</sub> |   2.60<sub>±00.05</sub> + 156.12<sub>±01.21</sub> |  11.79<sub>±00.16</sub> |
|            Python/pypy | 0.616<sub>±0.003</sub> |  59.06<sub>±00.05</sub> + 249.15<sub>±00.07</sub> |  24.84<sub>±00.17</sub> |
|                  Julia | 0.647<sub>±0.001</sub> | 267.38<sub>±00.10</sub> + 343.34<sub>±00.06</sub> |  24.95<sub>±00.23</sub> |
|                 Racket | 0.733<sub>±0.002</sub> | 102.52<sub>±00.21</sub> + 242.97<sub>±01.65</sub> |  29.02<sub>±00.33</sub> |
|       Ruby/truffleruby | 0.859<sub>±0.019</sub> | 227.35<sub>±02.72</sub> + 720.35<sub>±89.33</sub> |  58.20<sub>±01.77</sub> |
|                    Lua | 1.240<sub>±0.003</sub> |   2.62<sub>±00.06</sub> + 282.88<sub>±00.68</sub> |  49.93<sub>±00.49</sub> |
| Ruby/truffleruby (JVM) | 1.342<sub>±0.060</sub> | 393.08<sub>±08.09</sub> + 511.72<sub>±50.34</sub> |  87.16<sub>±03.88</sub> |
|           Ruby (--jit) | 1.562<sub>±0.009</sub> |  15.93<sub>±00.03</sub> + 170.80<sub>±00.75</sub> |  72.25<sub>±00.40</sub> |
|                   Ruby | 1.611<sub>±0.012</sub> |  14.91<sub>±00.05</sub> + 142.89<sub>±00.01</sub> |  67.00<sub>±00.55</sub> |
|                 Python | 2.115<sub>±0.012</sub> |  10.14<sub>±00.01</sub> + 180.79<sub>±00.90</sub> |  90.66<sub>±01.17</sub> |
|             Ruby/jruby | 2.262<sub>±0.046</sub> | 190.88<sub>±03.18</sub> + 527.51<sub>±36.65</sub> | 119.07<sub>±04.10</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 8.0.100                         |
| C#/.NET Core     | 4.8.0-3.23524.11 (f43cd10b)     |
| C#/Mono          | 6.12.0.200                      |
| Chez Scheme      | 9.5.8                           |
| Clojure          | "1.11.1"                        |
| Crystal          | 1.10.1                          |
| D/dmd            | v2.106.0                        |
| D/gdc            | 13.2.0                          |
| D/ldc2           | 1.35.0                          |
| Elixir           | 1.14.0                          |
| F#/.NET Core     | 12.8.0.0 for F# 8.0             |
| Go               | go1.21.4                        |
| Go/gccgo         | 13.2.0                          |
| Haskell          | 9.4.8                           |
| Idris 2          | 0.6.0                           |
| Java             | 21.0.1                          |
| Julia            | v"1.9.4"                        |
| Kotlin           | 1.9.21                          |
| Lua              | 5.4.4                           |
| Lua/luajit       | 2.1.1700206165                  |
| MLton            | 20210117                        |
| Nim              | 2.0.0                           |
| Node.js          | v21.3.0                         |
| OCaml            | 5.1.0                           |
| PHP              | 8.2.10                          |
| Perl             | v5.36.0                         |
| Python           | 3.11.6                          |
| Python/pypy      | 7.3.13-final0 for Python 3.10.13 |
| Racket           | "8.11.1"                        |
| Ruby             | 3.2.2p53                        |
| Ruby/jruby       | 9.4.5.0                         |
| Ruby/truffleruby | 23.1.1                          |
| Rust             | 1.74.0                          |
| Scala            | 3.3.1                           |
| Swift            | 5.9.1                           |
| Tcl              | 8.6                             |
| V                | 0.4.3 b5ba122                   |
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
