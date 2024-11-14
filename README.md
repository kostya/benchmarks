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

UPDATE: 2024-11-13

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
|         Scala (Staged) |   0.425<sub>±0.017</sub> |  208.26<sub>±02.66</sub> + 36.81<sub>±04.81</sub> |     27.57<sub>±01.62</sub> |
|        Racket (Staged) |   0.885<sub>±0.000</sub> |   105.98<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |     34.27<sub>±00.03</sub> |
|                   Rust |   1.012<sub>±0.000</sub> |     2.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     42.99<sub>±00.30</sub> |
|                C++/g++ |   1.095<sub>±0.002</sub> |     3.38<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     45.08<sub>±00.23</sub> |
|                  C/gcc |   1.111<sub>±0.001</sub> |     1.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     46.73<sub>±00.25</sub> |
|                  D/gdc |   1.118<sub>±0.002</sub> |     6.75<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     48.63<sub>±00.23</sub> |
|                C/clang |   1.138<sub>±0.001</sub> |     1.38<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     48.14<sub>±00.11</sub> |
|            C++/clang++ |   1.166<sub>±0.000</sub> |     2.88<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     49.24<sub>±01.17</sub> |
|                 D/ldc2 |   1.167<sub>±0.001</sub> |     3.12<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     49.12<sub>±00.20</sub> |
|                Nim/gcc |   1.168<sub>±0.001</sub> |     1.94<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     48.20<sub>±00.22</sub> |
|                   Java |   1.188<sub>±0.000</sub> |    42.25<sub>±00.11</sub> + 0.81<sub>±00.06</sub> |     48.12<sub>±00.33</sub> |
|                  V/gcc |   1.198<sub>±0.001</sub> |     2.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     47.85<sub>±00.12</sub> |
|               Vala/gcc |   1.210<sub>±0.001</sub> |     5.38<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     50.36<sub>±00.52</sub> |
|             Kotlin/JVM |   1.227<sub>±0.002</sub> |    44.92<sub>±00.09</sub> + 0.88<sub>±00.12</sub> |     51.69<sub>±00.13</sub> |
|             Vala/clang |   1.234<sub>±0.000</sub> |     5.31<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     52.04<sub>±00.89</sub> |
|                     Go |   1.247<sub>±0.001</sub> |     3.62<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     51.44<sub>±00.38</sub> |
|                    Zig |   1.266<sub>±0.003</sub> |     1.25<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     53.30<sub>±01.05</sub> |
|               Go/gccgo |   1.288<sub>±0.000</sub> |    23.88<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |     54.12<sub>±00.51</sub> |
|           C#/.NET Core |   1.367<sub>±0.001</sub> |    32.38<sub>±00.06</sub> + 0.12<sub>±00.00</sub> |     58.25<sub>±00.89</sub> |
|                  Julia |   1.551<sub>±0.014</sub> |   248.75<sub>±00.06</sub> + 0.25<sub>±00.00</sub> |     66.53<sub>±00.84</sub> |
|              Nim/clang |   1.582<sub>±0.000</sub> |     2.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     64.79<sub>±00.92</sub> |
|           F#/.NET Core |   1.593<sub>±0.002</sub> |    37.49<sub>±00.06</sub> + 0.38<sub>±00.12</sub> |     68.98<sub>±01.23</sub> |
|                Crystal |   1.635<sub>±0.004</sub> |     3.44<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     68.71<sub>±00.54</sub> |
|                  OCaml |   1.657<sub>±0.003</sub> |     3.25<sub>±00.00</sub> + 2.62<sub>±00.00</sub> |     76.47<sub>±00.71</sub> |
|            Chez Scheme |   1.748<sub>±0.002</sub> |    25.12<sub>±00.00</sub> + 3.38<sub>±00.00</sub> |     73.58<sub>±00.24</sub> |
|                 Racket |   1.794<sub>±0.027</sub> |   93.84<sub>±00.05</sub> + 22.75<sub>±00.19</sub> |     73.46<sub>±01.48</sub> |
|                V/clang |   1.891<sub>±0.019</sub> |     2.06<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     82.57<sub>±01.03</sub> |
|                C#/Mono |   2.034<sub>±0.001</sub> |    25.84<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |     86.11<sub>±00.25</sub> |
|                  MLton |   2.077<sub>±0.011</sub> |     1.75<sub>±00.00</sub> + 0.38<sub>±00.00</sub> |     86.06<sub>±01.09</sub> |
|                  Scala |   2.777<sub>±0.005</sub> |  62.17<sub>±00.16</sub> + 145.05<sub>±00.32</sub> |    122.07<sub>±00.64</sub> |
|                Node.js |   3.150<sub>±0.005</sub> |    46.78<sub>±00.00</sub> + 6.12<sub>±00.00</sub> |    131.99<sub>±01.28</sub> |
|       Haskell (MArray) |   3.236<sub>±0.003</sub> |     4.12<sub>±00.00</sub> + 4.12<sub>±00.00</sub> |    134.34<sub>±00.29</sub> |
|                  D/dmd |   3.328<sub>±0.001</sub> |     3.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    125.22<sub>±00.34</sub> |
|           Haskell (FP) |   3.786<sub>±0.003</sub> |     4.25<sub>±00.06</sub> + 4.00<sub>±00.00</sub> |    160.86<sub>±00.26</sub> |
|       Ruby/truffleruby |   4.669<sub>±0.152</sub> | 201.88<sub>±00.69</sub> + 814.31<sub>±49.81</sub> |    225.04<sub>±10.28</sub> |
|                  Swift |   5.593<sub>±0.008</sub> |    17.38<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    210.94<sub>±02.31</sub> |
|             Lua/luajit |   5.687<sub>±0.024</sub> |     2.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    234.94<sub>±01.49</sub> |
| Ruby/truffleruby (JVM) |   5.884<sub>±0.028</sub> | 390.44<sub>±02.98</sub> + 468.57<sub>±35.67</sub> |    289.91<sub>±02.92</sub> |
|            Python/pypy |   9.575<sub>±0.073</sub> |   60.50<sub>±00.12</sub> + 29.45<sub>±00.01</sub> |    426.59<sub>±05.95</sub> |
|                  Idris |  15.559<sub>±0.040</sub> |    20.63<sub>±00.06</sub> + 8.38<sub>±00.00</sub> |    700.59<sub>±04.61</sub> |
|                 Elixir |  20.530<sub>±0.047</sub> |    71.27<sub>±00.45</sub> + 0.00<sub>±00.00</sub> |    816.30<sub>±01.74</sub> |
|           Ruby (--jit) |  31.953<sub>±0.050</sub> |    21.47<sub>±00.07</sub> + 4.88<sub>±00.00</sub> |   1320.18<sub>±03.40</sub> |
|                    PHP |  34.067<sub>±0.088</sub> |    18.88<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   1442.70<sub>±16.86</sub> |
|                    Lua |  37.027<sub>±0.060</sub> |     2.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   1523.51<sub>±15.61</sub> |
|                 Python |  54.076<sub>±0.669</sub> |    11.88<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   2418.75<sub>±26.40</sub> |
|                   Ruby |  71.836<sub>±0.205</sub> |    11.31<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3123.03<sub>±27.59</sub> |
|             Ruby/jruby |  79.903<sub>±1.524</sub> | 203.90<sub>±02.51</sub> + 222.73<sub>±32.77</sub> |   3573.69<sub>±72.79</sub> |
|               Tcl (FP) | 193.372<sub>±0.720</sub> |     4.62<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |  8541.35<sub>±121.54</sub> |
|                   Perl | 223.395<sub>±1.420</sub> |     7.06<sub>±00.06</sub> + 0.12<sub>±00.00</sub> |   9755.30<sub>±45.94</sub> |
|              Tcl (OOP) | 382.403<sub>±3.408</sub> |     4.62<sub>±00.12</sub> + 0.12<sub>±00.00</sub> | 16926.54<sub>±148.14</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|               Language |                 Time, s |                                       Memory, MiB |                Energy, J |
| :--------------------- | ----------------------: | ------------------------------------------------: | -----------------------: |
|         Scala (Staged) |  7.751<sub>±0.157</sub> | 211.17<sub>±04.45</sub> + 132.95<sub>±12.75</sub> |  465.58<sub>±11.30</sub> |
|                C++/g++ | 10.076<sub>±0.018</sub> |     3.50<sub>±00.00</sub> + 0.62<sub>±00.00</sub> |  416.16<sub>±01.64</sub> |
|           C#/.NET Core | 11.991<sub>±0.035</sub> |    32.56<sub>±00.06</sub> + 1.38<sub>±00.00</sub> |  481.39<sub>±02.69</sub> |
|                   Java | 12.441<sub>±0.041</sub> |    42.18<sub>±00.16</sub> + 2.18<sub>±00.07</sub> |  500.84<sub>±03.37</sub> |
|                  C/gcc | 12.597<sub>±0.005</sub> |     1.62<sub>±00.00</sub> + 0.25<sub>±00.00</sub> |  509.44<sub>±01.23</sub> |
|            C++/clang++ | 12.910<sub>±0.015</sub> |     3.00<sub>±00.00</sub> + 0.62<sub>±00.00</sub> |  534.77<sub>±00.89</sub> |
|             Kotlin/JVM | 13.168<sub>±0.070</sub> |    44.94<sub>±00.06</sub> + 1.94<sub>±00.27</sub> |  551.88<sub>±02.60</sub> |
|                C/clang | 13.205<sub>±0.013</sub> |     1.62<sub>±00.00</sub> + 0.25<sub>±00.00</sub> |  581.89<sub>±04.42</sub> |
|                    Zig | 13.314<sub>±0.028</sub> |     1.38<sub>±00.06</sub> + 1.00<sub>±00.00</sub> |  558.65<sub>±04.76</sub> |
|           F#/.NET Core | 13.335<sub>±0.024</sub> |    37.61<sub>±00.06</sub> + 2.12<sub>±00.12</sub> |  534.94<sub>±02.52</sub> |
|                     Go | 14.113<sub>±0.014</sub> |     3.62<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  568.61<sub>±02.43</sub> |
|                   Rust | 14.242<sub>±0.020</sub> |     1.88<sub>±00.00</sub> + 0.25<sub>±00.00</sub> |  569.99<sub>±01.08</sub> |
|                 D/ldc2 | 14.272<sub>±0.019</sub> |     3.25<sub>±00.06</sub> + 0.75<sub>±00.00</sub> |  584.52<sub>±03.64</sub> |
|                  V/gcc | 14.333<sub>±0.038</sub> |     2.12<sub>±00.06</sub> + 1.00<sub>±00.00</sub> |  560.92<sub>±03.16</sub> |
|               Vala/gcc | 14.436<sub>±0.031</sub> |     5.38<sub>±00.00</sub> + 0.62<sub>±00.00</sub> |  572.24<sub>±04.78</sub> |
|        Racket (Staged) | 14.758<sub>±0.159</sub> |  105.70<sub>±00.14</sub> + 71.38<sub>±01.87</sub> |  584.52<sub>±06.50</sub> |
|             Vala/clang | 14.781<sub>±0.010</sub> |     5.38<sub>±00.12</sub> + 0.62<sub>±00.00</sub> |  593.05<sub>±01.04</sub> |
|                Crystal | 14.942<sub>±0.019</sub> |     3.50<sub>±00.00</sub> + 0.25<sub>±00.00</sub> |  631.98<sub>±02.50</sub> |
|                Nim/gcc | 15.017<sub>±0.014</sub> |     2.06<sub>±00.06</sub> + 1.50<sub>±00.00</sub> |  608.83<sub>±01.49</sub> |
|                  D/gdc | 15.433<sub>±0.016</sub> |     6.75<sub>±00.00</sub> + 0.88<sub>±00.00</sub> |  642.43<sub>±03.76</sub> |
|                  Scala | 16.477<sub>±0.112</sub> |  62.29<sub>±00.09</sub> + 144.88<sub>±00.19</sub> |  717.49<sub>±03.80</sub> |
|              Nim/clang | 17.695<sub>±0.052</sub> |     2.38<sub>±00.12</sub> + 1.50<sub>±00.00</sub> |  719.34<sub>±09.74</sub> |
|                  Swift | 18.038<sub>±0.044</sub> |    17.62<sub>±00.12</sub> + 0.12<sub>±00.00</sub> |  740.60<sub>±03.26</sub> |
|                V/clang | 18.356<sub>±0.084</sub> |     2.12<sub>±00.06</sub> + 1.00<sub>±00.00</sub> |  806.70<sub>±06.57</sub> |
|               Go/gccgo | 18.686<sub>±0.314</sub> |    24.06<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  773.76<sub>±12.67</sub> |
|                  OCaml | 24.514<sub>±0.018</sub> |     4.38<sub>±00.00</sub> + 3.12<sub>±00.00</sub> | 1178.94<sub>±06.82</sub> |
|                  Julia | 30.494<sub>±0.068</sub> |   248.88<sub>±00.00</sub> + 0.25<sub>±00.00</sub> | 1291.74<sub>±10.50</sub> |
|                C#/Mono | 31.075<sub>±0.053</sub> |    25.81<sub>±00.15</sub> + 0.75<sub>±00.00</sub> | 1320.05<sub>±03.33</sub> |
|            Chez Scheme | 31.201<sub>±0.191</sub> |    25.45<sub>±00.00</sub> + 3.12<sub>±00.00</sub> | 1372.33<sub>±16.13</sub> |
|             Lua/luajit | 31.900<sub>±0.061</sub> |     2.50<sub>±00.06</sub> + 0.25<sub>±00.00</sub> | 1318.65<sub>±02.87</sub> |
|                  MLton | 33.684<sub>±0.058</sub> |     1.75<sub>±00.00</sub> + 4.27<sub>±00.00</sub> | 1527.54<sub>±10.09</sub> |
|                Node.js | 34.372<sub>±0.081</sub> |    46.80<sub>±00.00</sub> + 8.62<sub>±00.12</sub> | 1380.19<sub>±03.68</sub> |
|       Haskell (MArray) | 35.178<sub>±0.039</sub> |     4.00<sub>±00.06</sub> + 5.12<sub>±00.00</sub> | 1431.66<sub>±08.02</sub> |
|                 Racket | 35.789<sub>±0.831</sub> |   93.83<sub>±00.07</sub> + 23.62<sub>±00.12</sub> | 1591.23<sub>±45.98</sub> |
|                  D/dmd | 37.954<sub>±0.004</sub> |     3.38<sub>±00.06</sub> + 0.88<sub>±00.00</sub> | 1390.20<sub>±05.03</sub> |
|            Python/pypy | 41.239<sub>±0.111</sub> |   60.38<sub>±00.12</sub> + 30.13<sub>±00.05</sub> | 1834.39<sub>±16.72</sub> |
|       Ruby/truffleruby | 48.778<sub>±0.439</sub> | 201.31<sub>±00.94</sub> + 734.44<sub>±18.06</sub> | 2375.16<sub>±22.12</sub> |
| Ruby/truffleruby (JVM) | 50.823<sub>±2.734</sub> | 393.21<sub>±05.77</sub> + 405.25<sub>±32.51</sub> | 2255.45<sub>±41.59</sub> |
|                  Idris | 66.743<sub>±0.120</sub> |    21.82<sub>±00.00</sub> + 9.25<sub>±00.00</sub> | 2977.76<sub>±12.58</sub> |
|           Haskell (FP) | 78.765<sub>±0.269</sub> |    4.00<sub>±00.00</sub> + 76.00<sub>±00.00</sub> | 3323.00<sub>±06.05</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|          C/clang (aklomp) |  0.095<sub>±0.001</sub> |     2.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   4.50<sub>±00.05</sub> |
|            C/gcc (aklomp) |  0.097<sub>±0.000</sub> |     2.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   4.56<sub>±00.02</sub> |
|                       PHP |  0.105<sub>±0.000</sub> |    19.25<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   4.93<sub>±00.03</sub> |
|              Go (base64x) |  0.275<sub>±0.002</sub> |     6.62<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  13.00<sub>±00.06</sub> |
|                   Node.js |  0.663<sub>±0.004</sub> |   45.19<sub>±00.02</sub> + 40.96<sub>±00.16</sub> |  29.74<sub>±00.50</sub> |
|                       Zig |  0.703<sub>±0.000</sub> |     1.75<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  26.41<sub>±00.10</sub> |
|                      Rust |  0.875<sub>±0.000</sub> |     2.38<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  36.15<sub>±00.19</sub> |
|                   C/clang |  0.918<sub>±0.000</sub> |     2.00<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  33.80<sub>±00.13</sub> |
|                 Nim/clang |  0.944<sub>±0.002</sub> |     2.81<sub>±00.06</sub> + 4.94<sub>±00.06</sub> |  37.54<sub>±00.31</sub> |
|                   Crystal |  1.039<sub>±0.001</sub> |     3.88<sub>±00.00</sub> + 1.00<sub>±00.00</sub> |  42.82<sub>±00.44</sub> |
|                     C/gcc |  1.106<sub>±0.001</sub> |     1.88<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  40.27<sub>±00.33</sub> |
|                    D/ldc2 |  1.179<sub>±0.001</sub> |     3.75<sub>±00.00</sub> + 3.38<sub>±00.00</sub> |  48.73<sub>±00.37</sub> |
|                   Nim/gcc |  1.347<sub>±0.001</sub> |     2.62<sub>±00.00</sub> + 4.62<sub>±00.00</sub> |  54.54<sub>±00.35</sub> |
|                      Java |  1.433<sub>±0.002</sub> |  43.05<sub>±00.10</sub> + 215.12<sub>±03.94</sub> |  59.06<sub>±00.69</sub> |
|              Ruby (--jit) |  1.503<sub>±0.002</sub> |   15.09<sub>±00.06</sub> + 72.88<sub>±00.38</sub> |  58.96<sub>±00.26</sub> |
|                     Scala |  1.531<sub>±0.002</sub> |  59.09<sub>±00.11</sub> + 265.94<sub>±05.44</sub> |  65.30<sub>±00.39</sub> |
|                Kotlin/JVM |  1.567<sub>±0.003</sub> |  45.92<sub>±00.12</sub> + 263.50<sub>±05.75</sub> |  67.15<sub>±00.22</sub> |
|                     V/gcc |  1.630<sub>±0.001</sub> |  2.62<sub>±00.00</sub> + 2379.12<sub>±01.50</sub> |  59.57<sub>±00.39</sub> |
|                  Vala/gcc |  1.645<sub>±0.001</sub> |     5.75<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  63.19<sub>±00.43</sub> |
|                Vala/clang |  1.645<sub>±0.001</sub> |     5.81<sub>±00.06</sub> + 0.12<sub>±00.00</sub> |  63.50<sub>±01.04</sub> |
|                        Go |  1.659<sub>±0.005</sub> |     4.25<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  68.14<sub>±00.65</sub> |
|   C++/clang++ (libcrypto) |  1.705<sub>±0.003</sub> |     6.06<sub>±00.06</sub> + 0.11<sub>±00.00</sub> |  68.13<sub>±00.60</sub> |
|       C++/g++ (libcrypto) |  1.706<sub>±0.004</sub> |     6.50<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  68.03<sub>±00.51</sub> |
|                      Ruby |  1.738<sub>±0.002</sub> |   11.75<sub>±00.06</sub> + 39.74<sub>±00.39</sub> |  68.37<sub>±00.46</sub> |
|       Perl (MIME::Base64) |  2.000<sub>±0.003</sub> |    14.94<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  80.98<sub>±00.50</sub> |
|              C#/.NET Core |  2.494<sub>±0.039</sub> |   33.85<sub>±00.03</sub> + 17.63<sub>±01.91</sub> |  97.82<sub>±01.66</sub> |
|              F#/.NET Core |  2.580<sub>±0.027</sub> |   38.48<sub>±00.01</sub> + 19.97<sub>±01.83</sub> |  98.40<sub>±01.39</sub> |
|                       Tcl |  2.753<sub>±0.005</sub> |     5.12<sub>±00.00</sub> + 0.12<sub>±00.00</sub> | 114.23<sub>±00.59</sub> |
|                    Python |  2.872<sub>±0.001</sub> |    11.62<sub>±00.00</sub> + 0.12<sub>±00.00</sub> | 112.57<sub>±00.90</sub> |
|                     Julia |  2.983<sub>±0.002</sub> |  263.48<sub>±00.11</sub> + 28.98<sub>±00.08</sub> | 120.37<sub>±00.91</sub> |
|                  Go/gccgo |  3.020<sub>±0.005</sub> |    25.00<sub>±00.12</sub> + 0.00<sub>±00.00</sub> | 142.55<sub>±00.34</sub> |
|                   V/clang |  3.248<sub>±0.001</sub> |  2.75<sub>±00.00</sub> + 2388.56<sub>±00.50</sub> | 119.99<sub>±00.66</sub> |
|               Python/pypy |  3.249<sub>±0.009</sub> |   60.25<sub>±00.12</sub> + 31.31<sub>±00.05</sub> | 143.38<sub>±01.06</sub> |
|    Ruby/truffleruby (JVM) |  3.443<sub>±0.007</sub> | 389.16<sub>±05.72</sub> + 294.50<sub>±17.44</sub> | 173.84<sub>±01.07</sub> |
|                    Racket |  3.903<sub>±0.006</sub> |   93.95<sub>±00.11</sub> + 25.88<sub>±00.12</sub> | 155.48<sub>±00.43</sub> |
|                     D/gdc |  3.920<sub>±0.001</sub> |     7.06<sub>±00.06</sub> + 3.75<sub>±00.06</sub> | 162.11<sub>±01.78</sub> |
|                     D/dmd |  4.020<sub>±0.002</sub> |     3.62<sub>±00.00</sub> + 3.38<sub>±00.00</sub> | 164.62<sub>±00.95</sub> |
|                   C#/Mono |  4.800<sub>±0.010</sub> |   26.58<sub>±00.20</sub> + 18.86<sub>±00.03</sub> | 198.99<sub>±01.14</sub> |
|                Ruby/jruby |  6.210<sub>±0.070</sub> | 196.29<sub>±01.99</sub> + 144.32<sub>±10.97</sub> | 273.42<sub>±04.78</sub> |
|          Ruby/truffleruby |  8.259<sub>±0.038</sub> | 200.88<sub>±00.50</sub> + 542.00<sub>±02.56</sub> | 399.78<sub>±03.55</sub> |
| Perl (MIME::Base64::Perl) | 10.394<sub>±0.088</sub> |    16.25<sub>±00.06</sub> + 0.21<sub>±00.03</sub> | 457.56<sub>±03.07</sub> |

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
|    C++/clang++ (simdjson On-Demand) |  0.061<sub>±0.000</sub> |    112.88<sub>±00.00</sub> + 59.75<sub>±00.00</sub> |   2.55<sub>±00.01</sub> |
|        C++/g++ (simdjson On-Demand) |  0.061<sub>±0.000</sub> |    113.50<sub>±00.00</sub> + 59.75<sub>±00.00</sub> |   2.56<sub>±00.01</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.071<sub>±0.000</sub> |     112.71<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   2.95<sub>±00.03</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.083<sub>±0.000</sub> |     113.21<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   3.39<sub>±00.02</sub> |
|         C++/clang++ (DAW JSON Link) |  0.092<sub>±0.000</sub> |     112.77<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.84<sub>±00.03</sub> |
|             C++/g++ (DAW JSON Link) |  0.092<sub>±0.000</sub> |     113.33<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   3.78<sub>±00.04</sub> |
|          C++/clang++ (simdjson DOM) |  0.103<sub>±0.001</sub> |   112.81<sub>±00.06</sub> + 174.75<sub>±00.81</sub> |   4.67<sub>±00.06</sub> |
|              C++/g++ (simdjson DOM) |  0.105<sub>±0.001</sub> |   113.38<sub>±00.00</sub> + 172.62<sub>±00.31</sub> |   4.82<sub>±00.05</sub> |
|                 Rust (Serde Custom) |  0.107<sub>±0.000</sub> |     111.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   4.54<sub>±00.03</sub> |
|                  Rust (Serde Typed) |  0.112<sub>±0.000</sub> |    111.62<sub>±00.00</sub> + 12.12<sub>±00.00</sub> |   4.73<sub>±00.07</sub> |
|                     C++/g++ (gason) |  0.127<sub>±0.000</sub> |    113.21<sub>±00.00</sub> + 96.75<sub>±00.00</sub> |   5.11<sub>±00.06</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.132<sub>±0.000</sub> |    112.88<sub>±00.00</sub> + 61.25<sub>±00.00</sub> |   5.41<sub>±00.05</sub> |
|                 C++/clang++ (gason) |  0.134<sub>±0.000</sub> |    112.70<sub>±00.06</sub> + 96.75<sub>±00.00</sub> |   5.32<sub>±00.03</sub> |
|                 C++/g++ (RapidJSON) |  0.144<sub>±0.002</sub> |   113.34<sub>±00.06</sub> + 125.54<sub>±01.73</sub> |   6.15<sub>±00.12</sub> |
|              Scala (jsoniter-scala) |  0.153<sub>±0.001</sub> |    279.53<sub>±00.11</sub> + 26.48<sub>±00.73</sub> |   8.43<sub>±00.17</sub> |
|                   Go (rjson custom) |  0.194<sub>±0.000</sub> |     113.94<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   7.52<sub>±00.02</sub> |
|             C++/clang++ (RapidJSON) |  0.195<sub>±0.001</sub> |   112.83<sub>±00.00</sub> + 128.62<sub>±00.00</sub> |   8.20<sub>±00.03</sub> |
|         C++/g++ (RapidJSON Precise) |  0.202<sub>±0.001</sub> |   113.34<sub>±00.00</sub> + 128.75<sub>±00.00</sub> |   8.61<sub>±00.08</sub> |
|                          Go (Sonic) |  0.204<sub>±0.002</sub> |     123.62<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   8.87<sub>±00.07</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.220<sub>±0.001</sub> |    112.88<sub>±00.00</sub> + 80.75<sub>±00.00</sub> |   9.32<sub>±00.03</sub> |
|                          Go (rjson) |  0.221<sub>±0.000</sub> |     114.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   8.58<sub>±00.04</sub> |
|                                 Zig |  0.225<sub>±0.000</sub> |    111.12<sub>±00.00</sub> + 39.25<sub>±00.00</sub> |   9.76<sub>±00.13</sub> |
|                  Go (goccy/go-json) |  0.260<sub>±0.001</sub> |     114.44<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  10.41<sub>±00.16</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.262<sub>±0.001</sub> |   112.71<sub>±00.00</sub> + 128.75<sub>±00.00</sub> |  11.06<sub>±00.14</sub> |
|             C++/g++ (RapidJSON SAX) |  0.351<sub>±0.001</sub> |     113.21<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  15.53<sub>±00.10</sub> |
|                      C/clang (yajl) |  0.362<sub>±0.001</sub> |     111.31<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  15.71<sub>±00.27</sub> |
|                        C/gcc (yajl) |  0.365<sub>±0.000</sub> |     111.31<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  15.77<sub>±00.10</sub> |
|                C++/g++ (Boost.JSON) |  0.365<sub>±0.001</sub> |   113.60<sub>±00.00</sub> + 307.75<sub>±00.00</sub> |  15.48<sub>±00.06</sub> |
|            C++/clang++ (Boost.JSON) |  0.375<sub>±0.002</sub> |   112.96<sub>±00.12</sub> + 307.75<sub>±00.00</sub> |  16.06<sub>±00.10</sub> |
|                   Nim/clang (jsony) |  0.382<sub>±0.001</sub> |   112.06<sub>±00.06</sub> + 154.12<sub>±00.00</sub> |  16.10<sub>±00.29</sub> |
|                     Nim/gcc (jsony) |  0.408<sub>±0.001</sub> |   111.62<sub>±00.00</sub> + 156.19<sub>±01.75</sub> |  17.33<sub>±00.14</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.417<sub>±0.001</sub> |     195.08<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  17.70<sub>±00.04</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.427<sub>±0.000</sub> |     113.34<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  19.06<sub>±00.18</sub> |
|                             Node.js |  0.437<sub>±0.001</sub> |   155.54<sub>±00.06</sub> + 201.31<sub>±03.75</sub> |  21.57<sub>±00.11</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.514<sub>±0.000</sub> |     195.08<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  22.66<sub>±00.11</sub> |
|                       Go (jsoniter) |  0.542<sub>±0.001</sub> |     114.25<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |  22.00<sub>±00.31</sub> |
|     C#/.NET Core (System.Text.Json) |  0.549<sub>±0.002</sub> |   490.00<sub>±00.08</sub> + 140.62<sub>±00.19</sub> |  24.45<sub>±00.29</sub> |
|                Rust (Serde Untyped) |  0.558<sub>±0.001</sub> |   111.75<sub>±00.00</sub> + 839.88<sub>±00.00</sub> |  23.69<sub>±00.30</sub> |
|                     Java (DSL-JSON) |  0.571<sub>±0.004</sub> |   267.72<sub>±00.12</sub> + 288.95<sub>±01.92</sub> |  30.06<sub>±00.56</sub> |
|                         Python/pypy |  0.614<sub>±0.000</sub> |   280.79<sub>±00.01</sub> + 125.21<sub>±00.00</sub> |  26.83<sub>±00.15</sub> |
|                             V/clang |  0.634<sub>±0.001</sub> |   111.75<sub>±00.12</sub> + 496.00<sub>±00.00</sub> |  26.49<sub>±00.14</sub> |
|                               V/gcc |  0.634<sub>±0.000</sub> |   111.81<sub>±00.06</sub> + 496.00<sub>±00.00</sub> |  26.77<sub>±00.27</sub> |
|                      Crystal (Pull) |  0.634<sub>±0.002</sub> |    113.44<sub>±00.06</sub> + 18.12<sub>±00.00</sub> |  27.95<sub>±00.31</sub> |
|                    Crystal (Schema) |  0.647<sub>±0.001</sub> |    113.38<sub>±00.00</sub> + 50.75<sub>±00.00</sub> |  28.21<sub>±00.09</sub> |
|                Nim/gcc (Packedjson) |  0.673<sub>±0.001</sub> |   112.00<sub>±00.00</sub> + 294.19<sub>±00.06</sub> |  28.78<sub>±00.44</sub> |
|              Nim/clang (Packedjson) |  0.675<sub>±0.001</sub> |   112.38<sub>±00.12</sub> + 294.25<sub>±00.00</sub> |  29.45<sub>±00.42</sub> |
|                 CPython (UltraJSON) |  0.720<sub>±0.002</sub> |   123.90<sub>±00.00</sub> + 476.31<sub>±00.88</sub> |  28.65<sub>±00.08</sub> |
|             Perl (Cpanel::JSON::XS) |  0.772<sub>±0.006</sub> |   125.44<sub>±00.06</sub> + 402.88<sub>±00.00</sub> |  32.07<sub>±00.29</sub> |
|                                 PHP |  0.805<sub>±0.002</sub> |   128.44<sub>±00.06</sub> + 517.88<sub>±00.00</sub> |  34.83<sub>±00.19</sub> |
|                                  Go |  0.825<sub>±0.001</sub> |     114.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  34.28<sub>±00.40</sub> |
|                              Python |  0.895<sub>±0.004</sub> |   121.73<sub>±00.06</sub> + 325.88<sub>±00.00</sub> |  37.83<sub>±00.56</sub> |
|                             Crystal |  0.947<sub>±0.008</sub> |   113.50<sub>±00.00</sub> + 392.12<sub>±00.00</sub> |  41.02<sub>±00.80</sub> |
|                             Nim/gcc |  1.023<sub>±0.002</sub> |  112.06<sub>±00.06</sub> + 1001.25<sub>±00.00</sub> |  42.77<sub>±00.18</sub> |
|                        C#/.NET Core |  1.064<sub>±0.004</sub> |   496.26<sub>±00.18</sub> + 273.00<sub>±00.12</sub> |  50.88<sub>±00.49</sub> |
|                           Nim/clang |  1.091<sub>±0.002</sub> |   112.38<sub>±00.00</sub> + 999.00<sub>±00.00</sub> |  45.69<sub>±00.46</sub> |
|                             Clojure |  1.141<sub>±0.022</sub> |   411.01<sub>±04.96</sub> + 545.75<sub>±15.06</sub> |  60.70<sub>±00.83</sub> |
|                    C++/g++ (json-c) |  1.203<sub>±0.002</sub> |  113.35<sub>±00.00</sub> + 1215.88<sub>±00.00</sub> |  50.53<sub>±00.81</sub> |
|              C++/clang++ (Nlohmann) |  1.205<sub>±0.001</sub> |   112.89<sub>±00.06</sub> + 359.88<sub>±00.00</sub> |  50.78<sub>±00.19</sub> |
|                C++/clang++ (json-c) |  1.205<sub>±0.005</sub> |  112.91<sub>±00.06</sub> + 1215.88<sub>±00.00</sub> |  50.10<sub>±00.29</sub> |
|                            Go/gccgo |  1.227<sub>±0.002</sub> |     139.38<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  50.43<sub>±00.12</sub> |
|                  C++/g++ (Nlohmann) |  1.274<sub>±0.002</sub> |   113.40<sub>±00.06</sub> + 447.88<sub>±00.00</sub> |  53.66<sub>±00.57</sub> |
|                        Ruby (--jit) |  1.330<sub>±0.017</sub> |   127.03<sub>±00.43</sub> + 213.50<sub>±00.62</sub> |  56.45<sub>±00.76</sub> |
|                                Ruby |  1.360<sub>±0.004</sub> |   121.25<sub>±00.00</sub> + 212.75<sub>±00.00</sub> |  57.49<sub>±00.53</sub> |
|     F#/.NET Core (System.Text.Json) |  1.498<sub>±0.006</sub> |   498.96<sub>±00.06</sub> + 231.38<sub>±04.12</sub> |  68.65<sub>±00.71</sub> |
|                              D/ldc2 |  1.742<sub>±0.002</sub> |   113.25<sub>±00.00</sub> + 708.38<sub>±00.06</sub> |  73.21<sub>±00.37</sub> |
|                         Ruby (YAJL) |  1.745<sub>±0.005</sub> |   121.38<sub>±00.12</sub> + 219.06<sub>±00.06</sub> |  73.96<sub>±00.69</sub> |
|                             C#/Mono |  1.789<sub>±0.010</sub> |    253.37<sub>±00.07</sub> + 31.54<sub>±00.03</sub> |  77.28<sub>±00.84</sub> |
|                             Haskell |  2.008<sub>±0.005</sub> |   115.88<sub>±00.00</sub> + 723.56<sub>±00.19</sub> |  86.20<sub>±00.70</sub> |
|        C++/g++ (Boost.PropertyTree) |  2.515<sub>±0.009</sub> |  113.40<sub>±00.06</sub> + 1439.88<sub>±00.00</sub> | 107.37<sub>±01.05</sub> |
|                           Rust (jq) |  2.597<sub>±0.006</sub> |   113.50<sub>±00.06</sub> + 903.47<sub>±01.19</sub> | 108.44<sub>±00.97</sub> |
|    C++/clang++ (Boost.PropertyTree) |  2.625<sub>±0.007</sub> |  195.14<sub>±00.06</sub> + 1232.62<sub>±00.00</sub> | 112.22<sub>±00.99</sub> |
|                                Odin |  2.824<sub>±0.003</sub> |    111.44<sub>±00.06</sub> + 20.00<sub>±00.00</sub> | 117.93<sub>±00.75</sub> |
|                          Ruby/jruby |  2.869<sub>±0.026</sub> |   469.56<sub>±03.09</sub> + 890.16<sub>±23.07</sub> | 146.96<sub>±02.03</sub> |
|                            Vala/gcc |  3.089<sub>±0.009</sub> |   115.31<sub>±00.06</sub> + 980.00<sub>±00.00</sub> | 131.83<sub>±00.56</sub> |
|                          Vala/clang |  3.094<sub>±0.006</sub> |   115.38<sub>±00.00</sub> + 980.00<sub>±00.00</sub> | 132.11<sub>±00.59</sub> |
|                               D/dmd |  3.101<sub>±0.003</sub> |   113.38<sub>±00.00</sub> + 708.56<sub>±00.06</sub> | 133.40<sub>±01.11</sub> |
|                               D/gdc |  3.493<sub>±0.022</sub> |   116.62<sub>±00.06</sub> + 708.75<sub>±00.00</sub> | 148.35<sub>±00.78</sub> |
|                              Racket |  3.819<sub>±0.025</sub> |   320.69<sub>±00.42</sub> + 225.56<sub>±00.12</sub> | 159.99<sub>±01.22</sub> |
|                   Perl (JSON::Tiny) |  9.297<sub>±0.067</sub> |   126.00<sub>±00.00</sub> + 528.69<sub>±00.01</sub> | 409.51<sub>±01.80</sub> |
|              Ruby/truffleruby (JVM) | 10.154<sub>±0.186</sub> | 483.81<sub>±04.05</sub> + 2243.44<sub>±141.72</sub> | 637.52<sub>±13.44</sub> |
|                    Ruby/truffleruby | 10.192<sub>±0.089</sub> |  398.88<sub>±02.12</sub> + 1932.31<sub>±17.38</sub> | 586.73<sub>±04.37</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                Language |                  Time, s |                                        Memory, MiB |                  Energy, J |
| :---------------------- | -----------------------: | -------------------------------------------------: | -------------------------: |
|         D/ldc2 (lubeck) |   0.042<sub>±0.001</sub> |    16.19<sub>±02.25</sub> + 48.19<sub>±02.31</sub> |      4.17<sub>±00.04</sub> |
|          Python (NumPy) |   0.066<sub>±0.001</sub> |    39.59<sub>±02.13</sub> + 52.95<sub>±02.12</sub> |      5.61<sub>±00.04</sub> |
|   Nim/gcc (Arraymancer) |   0.073<sub>±0.002</sub> |    10.69<sub>±03.00</sub> + 53.19<sub>±03.06</sub> |      5.81<sub>±00.10</sub> |
|             Java (ND4J) |   0.082<sub>±0.001</sub> |   117.87<sub>±01.02</sub> + 92.62<sub>±00.00</sub> |      6.38<sub>±00.11</sub> |
|          Rust (ndarray) |   0.091<sub>±0.001</sub> |     2.50<sub>±00.00</sub> + 68.48<sub>±00.00</sub> |      6.18<sub>±00.06</sub> |
| Nim/clang (Arraymancer) |   0.112<sub>±0.027</sub> |    20.44<sub>±02.25</sub> + 44.06<sub>±02.25</sub> |      7.99<sub>±01.36</sub> |
|      Julia (threads: 2) |   0.119<sub>±0.000</sub> |   263.91<sub>±00.08</sub> + 57.19<sub>±00.06</sub> |      6.62<sub>±00.02</sub> |
|         C++/g++ (Eigen) |   0.147<sub>±0.000</sub> |     4.21<sub>±00.00</sub> + 85.54<sub>±00.00</sub> |      7.23<sub>±00.09</sub> |
|     C++/clang++ (Eigen) |   0.147<sub>±0.000</sub> |     4.75<sub>±00.00</sub> + 85.42<sub>±00.00</sub> |      7.31<sub>±00.03</sub> |
|      Julia (threads: 1) |   0.170<sub>±0.000</sub> |   263.98<sub>±00.07</sub> + 56.62<sub>±00.00</sub> |      7.95<sub>±00.04</sub> |
|   V/clang (VSL + CBLAS) |   0.274<sub>±0.003</sub> |    14.31<sub>±02.00</sub> + 44.88<sub>±01.88</sub> |     19.26<sub>±00.20</sub> |
|           V/clang (VSL) |   0.274<sub>±0.004</sub> |    14.38<sub>±02.75</sub> + 44.69<sub>±02.69</sub> |     18.94<sub>±00.23</sub> |
|     V/gcc (VSL + CBLAS) |   0.469<sub>±0.006</sub> |    12.94<sub>±02.44</sub> + 46.31<sub>±02.38</sub> |     35.29<sub>±00.27</sub> |
|             V/gcc (VSL) |   0.484<sub>±0.002</sub> |    13.25<sub>±02.44</sub> + 45.94<sub>±02.25</sub> |     32.84<sub>±00.15</sub> |
|         Julia (no BLAS) |   1.110<sub>±0.018</sub> |   263.50<sub>±00.00</sub> + 51.88<sub>±00.00</sub> |     48.27<sub>±00.89</sub> |
|                   D/gdc |   1.504<sub>±0.000</sub> |      7.12<sub>±00.06</sub> + 4.00<sub>±00.00</sub> |     56.83<sub>±00.29</sub> |
|                  D/ldc2 |   1.723<sub>±0.002</sub> |     3.62<sub>±00.06</sub> + 70.38<sub>±00.00</sub> |     64.10<sub>±00.41</sub> |
|                   D/dmd |   1.881<sub>±0.002</sub> |     3.44<sub>±00.06</sub> + 70.38<sub>±00.00</sub> |     71.95<sub>±00.51</sub> |
|                   C/gcc |   3.034<sub>±0.000</sub> |     2.00<sub>±00.00</sub> + 68.38<sub>±00.00</sub> |    112.68<sub>±00.46</sub> |
|                   V/gcc |   3.036<sub>±0.000</sub> |     2.50<sub>±00.00</sub> + 68.75<sub>±00.00</sub> |    113.43<sub>±00.28</sub> |
|              Vala/clang |   3.061<sub>±0.001</sub> |     5.66<sub>±00.03</sub> + 68.38<sub>±00.00</sub> |    107.97<sub>±01.66</sub> |
|                    Rust |   3.064<sub>±0.001</sub> |     2.25<sub>±00.00</sub> + 68.50<sub>±00.00</sub> |    107.36<sub>±01.14</sub> |
|                 V/clang |   3.064<sub>±0.001</sub> |     2.88<sub>±00.00</sub> + 68.75<sub>±00.00</sub> |    106.71<sub>±00.68</sub> |
|                 C/clang |   3.066<sub>±0.000</sub> |     2.00<sub>±00.00</sub> + 68.38<sub>±00.00</sub> |    105.48<sub>±00.31</sub> |
|                     Zig |   3.068<sub>±0.002</sub> |     1.75<sub>±00.00</sub> + 68.62<sub>±00.00</sub> |    109.29<sub>±00.23</sub> |
|                 Nim/gcc |   3.094<sub>±0.002</sub> |     2.50<sub>±00.00</sub> + 57.88<sub>±00.00</sub> |    115.51<sub>±00.14</sub> |
|                    Java |   3.107<sub>±0.006</sub> |    43.27<sub>±00.10</sub> + 78.61<sub>±00.08</sub> |    123.58<sub>±00.40</sub> |
|                   Swift |   3.117<sub>±0.001</sub> |     8.31<sub>±00.06</sub> + 68.62<sub>±00.00</sub> |    112.98<sub>±00.66</sub> |
|               Nim/clang |   3.121<sub>±0.001</sub> |     2.88<sub>±00.12</sub> + 57.88<sub>±00.00</sub> |    108.74<sub>±00.94</sub> |
|                Vala/gcc |   3.130<sub>±0.001</sub> |     5.53<sub>±00.03</sub> + 68.50<sub>±00.00</sub> |    115.26<sub>±00.63</sub> |
|                      Go |   3.153<sub>±0.000</sub> |      4.00<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |    114.91<sub>±00.69</sub> |
|                 Crystal |   3.154<sub>±0.001</sub> |     4.00<sub>±00.06</sub> + 59.50<sub>±00.00</sub> |    113.86<sub>±01.39</sub> |
|                Go/gccgo |   3.160<sub>±0.000</sub> |     24.38<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    112.72<sub>±00.89</sub> |
|              Kotlin/JVM |   3.197<sub>±0.002</sub> |    43.74<sub>±00.09</sub> + 78.32<sub>±00.17</sub> |    124.82<sub>±00.65</sub> |
|                 Node.js |   3.201<sub>±0.001</sub> |    54.88<sub>±00.13</sub> + 70.75<sub>±00.12</sub> |    125.28<sub>±00.24</sub> |
|             Python/pypy |   3.263<sub>±0.002</sub> |    61.00<sub>±00.00</sub> + 68.75<sub>±00.12</sub> |    136.35<sub>±00.22</sub> |
|                   Scala |   3.295<sub>±0.004</sub> |   61.33<sub>±00.10</sub> + 150.06<sub>±00.06</sub> |    121.09<sub>±00.77</sub> |
|            C#/.NET Core |   4.890<sub>±0.001</sub> |    34.75<sub>±00.07</sub> + 68.88<sub>±00.00</sub> |    199.16<sub>±01.76</sub> |
|                 C#/Mono |   7.394<sub>±0.002</sub> |    26.13<sub>±00.15</sub> + 69.62<sub>±00.00</sub> |    307.51<sub>±01.02</sub> |
|        Ruby/truffleruby |  23.778<sub>±0.432</sub> |  323.12<sub>±01.06</sub> + 495.12<sub>±08.81</sub> |    822.87<sub>±11.67</sub> |
|  Ruby/truffleruby (JVM) |  24.266<sub>±0.702</sub> |  460.01<sub>±13.73</sub> + 344.48<sub>±85.17</sub> |    849.21<sub>±19.35</sub> |
|                  Python | 131.064<sub>±0.887</sub> |    11.94<sub>±00.06</sub> + 68.62<sub>±00.00</sub> |   6080.98<sub>±50.15</sub> |
|                    Perl | 149.885<sub>±1.491</sub> |    8.75<sub>±00.12</sub> + 379.62<sub>±00.00</sub> |   6523.98<sub>±63.31</sub> |
|            Ruby (--jit) | 153.769<sub>±0.077</sub> |    21.87<sub>±00.03</sub> + 68.44<sub>±00.06</sub> |   6836.84<sub>±17.55</sub> |
|                     Tcl | 205.038<sub>±0.931</sub> |    7.50<sub>±00.00</sub> + 400.25<sub>±00.00</sub> |   9334.51<sub>±36.78</sub> |
|                    Ruby | 213.180<sub>±0.222</sub> |    11.62<sub>±00.00</sub> + 69.25<sub>±00.00</sub> |   9606.45<sub>±45.45</sub> |
|              Ruby/jruby | 378.246<sub>±8.095</sub> | 269.65<sub>±12.79</sub> + 1169.97<sub>±99.95</sub> | 15729.25<sub>±399.84</sub> |

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
|                    Zig | 0.060<sub>±0.000</sub> |    1.50<sub>±00.00</sub> + 47.88<sub>±00.25</sub> |   2.52<sub>±00.02</sub> |
|                C++/g++ | 0.064<sub>±0.001</sub> |    3.62<sub>±00.12</sub> + 85.86<sub>±00.56</sub> |   2.50<sub>±00.02</sub> |
|                     Go | 0.072<sub>±0.001</sub> |     3.56<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.13<sub>±00.06</sub> |
|            C++/clang++ | 0.075<sub>±0.000</sub> |    3.12<sub>±00.12</sub> + 64.66<sub>±00.12</sub> |   2.85<sub>±00.03</sub> |
|                V/clang | 0.100<sub>±0.000</sub> |   2.25<sub>±00.00</sub> + 200.38<sub>±00.94</sub> |   4.15<sub>±00.03</sub> |
|                   Rust | 0.103<sub>±0.000</sub> |    2.00<sub>±00.00</sub> + 72.98<sub>±00.00</sub> |   4.05<sub>±00.06</sub> |
|                  V/gcc | 0.105<sub>±0.001</sub> |   2.12<sub>±00.00</sub> + 215.81<sub>±02.50</sub> |   4.36<sub>±00.05</sub> |
|                   Java | 0.136<sub>±0.003</sub> |  41.95<sub>±00.12</sub> + 126.09<sub>±05.43</sub> |   7.73<sub>±00.17</sub> |
|                Crystal | 0.142<sub>±0.000</sub> |    3.44<sub>±00.06</sub> + 89.06<sub>±00.62</sub> |   5.77<sub>±00.06</sub> |
|                  Scala | 0.193<sub>±0.002</sub> |  63.27<sub>±00.10</sub> + 147.50<sub>±04.91</sub> |  11.42<sub>±00.09</sub> |
|                Node.js | 0.195<sub>±0.000</sub> |  44.05<sub>±00.00</sub> + 147.14<sub>±00.26</sub> |  10.22<sub>±00.04</sub> |
|                Nim/gcc | 0.290<sub>±0.002</sub> |   1.75<sub>±00.00</sub> + 596.00<sub>±04.81</sub> |  11.18<sub>±00.16</sub> |
|              Nim/clang | 0.291<sub>±0.001</sub> |   2.12<sub>±00.00</sub> + 585.56<sub>±03.38</sub> |  11.34<sub>±00.10</sub> |
|             Lua/luajit | 0.305<sub>±0.001</sub> |   2.56<sub>±00.06</sub> + 157.53<sub>±00.65</sub> |  12.31<sub>±00.03</sub> |
|                  Julia | 0.419<sub>±0.002</sub> | 263.62<sub>±00.00</sub> + 216.46<sub>±02.14</sub> |  15.98<sub>±00.08</sub> |
|            Python/pypy | 0.636<sub>±0.003</sub> |  60.00<sub>±00.12</sub> + 248.82<sub>±00.07</sub> |  26.09<sub>±00.26</sub> |
|                 Racket | 0.751<sub>±0.001</sub> | 111.73<sub>±00.41</sub> + 246.77<sub>±00.46</sub> |  29.66<sub>±00.18</sub> |
|       Ruby/truffleruby | 0.895<sub>±0.009</sub> | 201.19<sub>±02.19</sub> + 649.06<sub>±40.88</sub> |  60.60<sub>±00.54</sub> |
|           Ruby (--jit) | 1.214<sub>±0.003</sub> |  23.14<sub>±00.06</sub> + 163.74<sub>±00.49</sub> |  49.45<sub>±00.50</sub> |
|                    Lua | 1.218<sub>±0.004</sub> |   2.50<sub>±00.00</sub> + 283.91<sub>±00.62</sub> |  49.71<sub>±00.55</sub> |
| Ruby/truffleruby (JVM) | 1.305<sub>±0.052</sub> | 393.06<sub>±03.76</sub> + 462.28<sub>±33.59</sub> |  84.79<sub>±03.35</sub> |
|                   Ruby | 1.964<sub>±0.007</sub> |  11.38<sub>±00.06</sub> + 172.75<sub>±00.31</sub> |  82.09<sub>±00.37</sub> |
|             Ruby/jruby | 2.245<sub>±0.083</sub> | 200.22<sub>±01.74</sub> + 552.07<sub>±44.14</sub> | 120.12<sub>±05.85</sub> |
|                 Python | 2.275<sub>±0.007</sub> |  11.62<sub>±00.00</sub> + 172.88<sub>±00.69</sub> |  98.93<sub>±01.08</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux trixie/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 8.0.403                         |
| C#/.NET Core     | 4.11.0-3.24468.6 (b4e5d1dd)     |
| C#/Mono          | 6.12.0.200                      |
| Chez Scheme      | 10.0.0                          |
| Clojure          | "1.12.0"                        |
| Crystal          | 1.14.0                          |
| D/dmd            | v2.109.1                        |
| D/gdc            | 14.2.0                          |
| D/ldc2           | 1.39.0                          |
| Elixir           | 1.14.0                          |
| F#/.NET Core     | 12.8.401.0 for F# 8.0           |
| Go               | go1.23.2                        |
| Go/gccgo         | 14.2.0                          |
| Haskell          | 9.8.2                           |
| Idris 2          | 0.6.0                           |
| Java             | 23.0.1                          |
| Julia            | v"1.11.1"                       |
| Kotlin           | 2.0.21                          |
| Lua              | 5.4.6                           |
| Lua/luajit       | 2.1.1723681758                  |
| MLton            | 20210117                        |
| Nim              | 2.2.0                           |
| Node.js          | v23.1.0                         |
| OCaml            | 5.2.0                           |
| Odin             | dev-2024-11-nightly             |
| PHP              | 8.2.24                          |
| Perl             | v5.40.0                         |
| Python           | 3.12.6                          |
| Python/pypy      | 7.3.17-final0 for Python 3.10.14 |
| Racket           | "8.14"                          |
| Ruby             | 3.3.5p100                       |
| Ruby/jruby       | 9.4.8.0                         |
| Ruby/truffleruby | 24.1.1                          |
| Rust             | 1.82.0                          |
| Scala            | 3.5.2                           |
| Swift            | 6.0.2                           |
| Tcl              | 8.6                             |
| V                | 0.4.8 2ab1523                   |
| Vala             | 0.56.17                         |
| Zig              | 0.13.0                          |
| clang/clang++    | 19.1.2 (1)                      |
| gcc/g++          | 14.2.0                          |

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
