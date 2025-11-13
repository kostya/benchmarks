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

UPDATE: 2025-08-17

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|               Language |                  Time, s |                                        Memory, MiB |                  Energy, J |
| :--------------------- | -----------------------: | -------------------------------------------------: | -------------------------: |
|         Scala (Staged) |   0.409<sub>±0.011</sub> |    229.62<sub>±03.27</sub> + 7.00<sub>±01.44</sub> |     26.31<sub>±00.71</sub> |
|        Racket (Staged) |   0.885<sub>±0.000</sub> |    101.37<sub>±01.65</sub> + 0.06<sub>±00.06</sub> |     34.39<sub>±00.17</sub> |
|                  V/gcc |   0.985<sub>±0.001</sub> |      2.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     41.65<sub>±00.28</sub> |
|                   Rust |   1.008<sub>±0.000</sub> |      1.88<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     42.93<sub>±00.33</sub> |
|                 D/ldc2 |   1.078<sub>±0.001</sub> |      3.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     45.78<sub>±00.24</sub> |
|                C++/g++ |   1.097<sub>±0.002</sub> |      3.38<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     46.21<sub>±00.59</sub> |
|                  C/gcc |   1.112<sub>±0.001</sub> |      1.38<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |     47.10<sub>±00.09</sub> |
|                  D/gdc |   1.116<sub>±0.001</sub> |      6.75<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     48.89<sub>±00.28</sub> |
|                C/clang |   1.136<sub>±0.000</sub> |      1.44<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     48.52<sub>±00.33</sub> |
|            C++/clang++ |   1.167<sub>±0.002</sub> |      2.88<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     48.87<sub>±00.76</sub> |
|                Nim/gcc |   1.171<sub>±0.001</sub> |      2.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     48.80<sub>±00.70</sub> |
|                   Java |   1.191<sub>±0.000</sub> |     41.74<sub>±00.09</sub> + 1.00<sub>±00.00</sub> |     49.32<sub>±00.60</sub> |
|               Vala/gcc |   1.202<sub>±0.001</sub> |      5.25<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |     50.70<sub>±00.44</sub> |
|             Kotlin/JVM |   1.224<sub>±0.002</sub> |     44.48<sub>±00.07</sub> + 1.31<sub>±00.25</sub> |     51.60<sub>±00.64</sub> |
|             Vala/clang |   1.235<sub>±0.000</sub> |      5.19<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     52.40<sub>±00.39</sub> |
|                     Go |   1.256<sub>±0.000</sub> |      3.75<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     52.69<sub>±00.80</sub> |
|           C#/.NET Core |   1.283<sub>±0.001</sub> |     34.56<sub>±00.07</sub> + 0.12<sub>±00.12</sub> |     55.31<sub>±00.24</sub> |
|               Go/gccgo |   1.287<sub>±0.000</sub> |     23.50<sub>±00.38</sub> + 0.00<sub>±00.00</sub> |     54.23<sub>±00.16</sub> |
|                    Zig |   1.327<sub>±0.000</sub> |      1.38<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |     55.13<sub>±00.40</sub> |
|           F#/.NET Core |   1.507<sub>±0.003</sub> |     39.33<sub>±00.10</sub> + 0.19<sub>±00.06</sub> |     65.29<sub>±00.40</sub> |
|                Crystal |   1.545<sub>±0.001</sub> |      3.25<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     65.69<sub>±00.40</sub> |
|                  Julia |   1.564<sub>±0.003</sub> |    244.00<sub>±00.12</sub> + 0.38<sub>±00.00</sub> |     67.80<sub>±00.43</sub> |
|              Nim/clang |   1.579<sub>±0.000</sub> |      2.25<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     65.59<sub>±00.57</sub> |
|                  OCaml |   1.607<sub>±0.002</sub> |      3.25<sub>±00.00</sub> + 2.62<sub>±00.00</sub> |     75.45<sub>±00.78</sub> |
|                  MLton |   1.737<sub>±0.006</sub> |      1.62<sub>±00.00</sub> + 0.38<sub>±00.00</sub> |     74.87<sub>±00.28</sub> |
|            Chez Scheme |   1.758<sub>±0.005</sub> |     25.88<sub>±00.29</sub> + 3.25<sub>±00.00</sub> |     74.43<sub>±00.21</sub> |
|                 Racket |   1.761<sub>±0.013</sub> |    112.86<sub>±00.03</sub> + 0.50<sub>±00.38</sub> |     72.49<sub>±02.28</sub> |
|                V/clang |   1.846<sub>±0.009</sub> |      2.06<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     80.00<sub>±00.53</sub> |
|                  Scala |   2.379<sub>±0.003</sub> |   66.03<sub>±00.14</sub> + 196.44<sub>±00.12</sub> |    108.88<sub>±00.92</sub> |
|                Node.js |   2.954<sub>±0.060</sub> |     49.57<sub>±00.06</sub> + 5.78<sub>±00.12</sub> |    126.35<sub>±03.17</sub> |
|                  D/dmd |   3.010<sub>±0.001</sub> |      3.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    113.43<sub>±01.11</sub> |
|       Haskell (MArray) |   3.497<sub>±0.001</sub> |      4.00<sub>±00.00</sub> + 4.00<sub>±00.00</sub> |    144.74<sub>±00.32</sub> |
|           Haskell (FP) |   3.819<sub>±0.008</sub> |      4.12<sub>±00.06</sub> + 4.06<sub>±00.06</sub> |    163.47<sub>±01.56</sub> |
| Ruby/truffleruby (JVM) |   4.645<sub>±0.070</sub> |  354.81<sub>±05.59</sub> + 478.37<sub>±54.20</sub> |    221.41<sub>±05.66</sub> |
|       Ruby/truffleruby |   5.169<sub>±0.461</sub> | 197.19<sub>±00.69</sub> + 693.00<sub>±117.12</sub> |    252.03<sub>±23.51</sub> |
|                  Swift |   5.497<sub>±0.001</sub> |     17.00<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |    205.09<sub>±04.14</sub> |
|             Lua/luajit |   5.686<sub>±0.024</sub> |      2.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    235.32<sub>±01.30</sub> |
|            Python/pypy |   9.398<sub>±0.038</sub> |    60.19<sub>±00.19</sub> + 30.10<sub>±00.05</sub> |    419.65<sub>±03.72</sub> |
|                  Idris |  15.088<sub>±0.032</sub> |     21.55<sub>±00.26</sub> + 8.00<sub>±00.00</sub> |    677.49<sub>±01.79</sub> |
|                 Elixir |  20.456<sub>±0.025</sub> |     90.88<sub>±00.30</sub> + 0.00<sub>±00.00</sub> |    814.83<sub>±07.69</sub> |
|           Ruby (--jit) |  30.807<sub>±0.033</sub> |     21.88<sub>±00.13</sub> + 4.62<sub>±00.00</sub> |   1275.90<sub>±04.28</sub> |
|                    PHP |  34.055<sub>±0.041</sub> |     17.61<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   1447.00<sub>±15.80</sub> |
|                    Lua |  37.051<sub>±0.168</sub> |      2.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   1528.23<sub>±05.85</sub> |
|             Ruby/jruby |  38.543<sub>±0.745</sub> |   255.48<sub>±00.77</sub> + 70.31<sub>±00.25</sub> |   1732.28<sub>±34.61</sub> |
|                 Python |  61.009<sub>±0.870</sub> |     11.38<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   2601.10<sub>±34.54</sub> |
|                   Ruby |  74.107<sub>±0.332</sub> |     12.12<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3209.85<sub>±28.74</sub> |
|               Tcl (FP) | 189.615<sub>±0.465</sub> |      4.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   8409.77<sub>±69.18</sub> |
|                   Perl | 225.517<sub>±1.873</sub> |      7.06<sub>±00.06</sub> + 0.12<sub>±00.00</sub> |  9777.03<sub>±135.29</sub> |
|              Tcl (OOP) | 373.095<sub>±1.341</sub> |      4.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 16604.13<sub>±190.89</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|               Language |                 Time, s |                                       Memory, MiB |                Energy, J |
| :--------------------- | ----------------------: | ------------------------------------------------: | -----------------------: |
|         Scala (Staged) |  7.118<sub>±0.079</sub> |  228.10<sub>±04.74</sub> + 71.23<sub>±06.06</sub> |  356.09<sub>±07.48</sub> |
|                C++/g++ | 10.143<sub>±0.051</sub> |     3.50<sub>±00.00</sub> + 0.62<sub>±00.00</sub> |  434.41<sub>±04.03</sub> |
|                  C/gcc | 12.544<sub>±0.019</sub> |     1.62<sub>±00.00</sub> + 0.25<sub>±00.00</sub> |  509.37<sub>±03.30</sub> |
|               Vala/gcc | 12.718<sub>±0.010</sub> |     5.25<sub>±00.06</sub> + 0.50<sub>±00.00</sub> |  511.94<sub>±00.90</sub> |
|           C#/.NET Core | 12.742<sub>±0.028</sub> |    34.69<sub>±00.06</sub> + 1.25<sub>±00.06</sub> |  494.75<sub>±01.32</sub> |
|            C++/clang++ | 12.803<sub>±0.022</sub> |     3.00<sub>±00.00</sub> + 0.62<sub>±00.00</sub> |  545.25<sub>±01.09</sub> |
|                    Zig | 13.030<sub>±0.017</sub> |     1.50<sub>±00.00</sub> + 1.00<sub>±00.00</sub> |  556.10<sub>±03.71</sub> |
|                   Java | 13.041<sub>±0.020</sub> |    41.77<sub>±00.15</sub> + 1.94<sub>±00.06</sub> |  537.41<sub>±01.88</sub> |
|           F#/.NET Core | 13.104<sub>±0.049</sub> |    39.32<sub>±00.12</sub> + 2.00<sub>±00.00</sub> |  527.94<sub>±02.27</sub> |
|                C/clang | 13.120<sub>±0.021</sub> |     1.50<sub>±00.06</sub> + 0.25<sub>±00.00</sub> |  579.20<sub>±04.21</sub> |
|             Kotlin/JVM | 13.463<sub>±0.052</sub> |    44.46<sub>±00.06</sub> + 2.56<sub>±00.06</sub> |  571.73<sub>±03.08</sub> |
|                 D/ldc2 | 14.340<sub>±0.012</sub> |     3.06<sub>±00.06</sub> + 0.75<sub>±00.00</sub> |  591.01<sub>±03.53</sub> |
|                   Rust | 14.460<sub>±0.021</sub> |     1.94<sub>±00.06</sub> + 0.25<sub>±00.00</sub> |  577.37<sub>±02.52</sub> |
|                     Go | 14.540<sub>±0.007</sub> |     3.75<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  584.48<sub>±01.83</sub> |
|             Vala/clang | 14.802<sub>±0.012</sub> |     5.12<sub>±00.00</sub> + 0.50<sub>±00.00</sub> |  595.99<sub>±01.53</sub> |
|                Crystal | 14.891<sub>±0.054</sub> |     3.38<sub>±00.06</sub> + 0.38<sub>±00.00</sub> |  631.17<sub>±02.60</sub> |
|                  D/gdc | 15.174<sub>±0.010</sub> |     6.62<sub>±00.00</sub> + 0.88<sub>±00.00</sub> |  640.85<sub>±03.66</sub> |
|        Racket (Staged) | 15.722<sub>±0.085</sub> |   99.73<sub>±00.12</sub> + 71.69<sub>±01.06</sub> |  613.58<sub>±05.37</sub> |
|                Nim/gcc | 15.766<sub>±0.016</sub> |     2.00<sub>±00.06</sub> + 1.50<sub>±00.00</sub> |  635.49<sub>±04.84</sub> |
|                  Scala | 15.785<sub>±0.067</sub> |  65.94<sub>±00.09</sub> + 145.00<sub>±00.19</sub> |  692.62<sub>±02.98</sub> |
|              Nim/clang | 17.648<sub>±0.035</sub> |     2.25<sub>±00.06</sub> + 1.50<sub>±00.00</sub> |  726.43<sub>±04.78</sub> |
|                  V/gcc | 17.865<sub>±0.016</sub> |     2.19<sub>±00.06</sub> + 1.00<sub>±00.00</sub> |  675.21<sub>±02.51</sub> |
|                  Swift | 18.083<sub>±0.085</sub> |    16.94<sub>±00.25</sub> + 0.12<sub>±00.00</sub> |  742.88<sub>±01.58</sub> |
|                V/clang | 18.691<sub>±0.126</sub> |     2.25<sub>±00.00</sub> + 1.00<sub>±00.00</sub> |  813.69<sub>±05.16</sub> |
|               Go/gccgo | 19.206<sub>±0.179</sub> |    23.38<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |  796.93<sub>±09.15</sub> |
|                  OCaml | 24.557<sub>±0.006</sub> |     4.38<sub>±00.00</sub> + 3.14<sub>±00.00</sub> | 1195.26<sub>±09.14</sub> |
|                  Julia | 30.315<sub>±0.060</sub> |   244.06<sub>±00.07</sub> + 0.25<sub>±00.00</sub> | 1274.74<sub>±02.15</sub> |
|            Chez Scheme | 30.884<sub>±0.151</sub> |    26.14<sub>±00.17</sub> + 3.00<sub>±00.00</sub> | 1367.28<sub>±07.09</sub> |
|             Lua/luajit | 31.804<sub>±0.026</sub> |     2.50<sub>±00.00</sub> + 0.25<sub>±00.00</sub> | 1312.84<sub>±01.18</sub> |
|                Node.js | 34.692<sub>±0.097</sub> |    49.51<sub>±00.00</sub> + 8.19<sub>±00.06</sub> | 1413.09<sub>±11.38</sub> |
|                 Racket | 35.373<sub>±0.400</sub> |   112.90<sub>±00.09</sub> + 2.75<sub>±00.44</sub> | 1573.97<sub>±10.34</sub> |
|                  MLton | 35.680<sub>±0.051</sub> |     1.62<sub>±00.00</sub> + 4.28<sub>±00.00</sub> | 1623.52<sub>±10.03</sub> |
|       Haskell (MArray) | 36.789<sub>±0.009</sub> |     4.12<sub>±00.06</sub> + 5.00<sub>±00.00</sub> | 1522.12<sub>±01.36</sub> |
|                  D/dmd | 38.105<sub>±0.005</sub> |     3.50<sub>±00.00</sub> + 0.75<sub>±00.00</sub> | 1391.62<sub>±02.04</sub> |
|            Python/pypy | 41.153<sub>±0.229</sub> |   60.12<sub>±00.19</sub> + 31.00<sub>±00.09</sub> | 1825.02<sub>±12.90</sub> |
|       Ruby/truffleruby | 48.381<sub>±0.372</sub> | 196.62<sub>±01.12</sub> + 582.69<sub>±01.75</sub> | 2346.69<sub>±17.27</sub> |
| Ruby/truffleruby (JVM) | 50.429<sub>±0.473</sub> | 355.72<sub>±06.70</sub> + 431.93<sub>±35.08</sub> | 2253.46<sub>±91.61</sub> |
|                  Idris | 64.504<sub>±0.237</sub> |    21.73<sub>±00.14</sub> + 9.88<sub>±00.00</sub> | 2857.66<sub>±12.12</sub> |
|           Haskell (FP) | 79.505<sub>±0.192</sub> |    4.00<sub>±00.00</sub> + 74.81<sub>±00.06</sub> | 3368.24<sub>±08.74</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|                       PHP |  0.065<sub>±0.001</sub> |    18.04<sub>±00.18</sub> + 0.00<sub>±00.00</sub> |   3.27<sub>±00.03</sub> |
|          C/clang (aklomp) |  0.095<sub>±0.000</sub> |     2.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   4.57<sub>±00.10</sub> |
|            C/gcc (aklomp) |  0.097<sub>±0.000</sub> |     2.12<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   4.65<sub>±00.04</sub> |
|              Go (base64x) |  0.283<sub>±0.009</sub> |     5.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  14.12<sub>±00.33</sub> |
|                   Node.js |  0.635<sub>±0.005</sub> |   47.63<sub>±00.06</sub> + 81.13<sub>±00.95</sub> |  29.16<sub>±00.53</sub> |
|                       Zig |  0.703<sub>±0.000</sub> |     1.81<sub>±00.06</sub> + 0.12<sub>±00.00</sub> |  26.42<sub>±00.09</sub> |
|                      Rust |  0.842<sub>±0.000</sub> |     2.31<sub>±00.06</sub> + 0.12<sub>±00.00</sub> |  34.91<sub>±00.90</sub> |
|                   C/clang |  0.918<sub>±0.000</sub> |     2.00<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  34.47<sub>±00.55</sub> |
|                 Nim/clang |  0.945<sub>±0.000</sub> |     2.75<sub>±00.00</sub> + 5.00<sub>±00.00</sub> |  38.18<sub>±01.09</sub> |
|                   Crystal |  1.061<sub>±0.017</sub> |     3.75<sub>±00.00</sub> + 1.00<sub>±00.00</sub> |  43.27<sub>±00.84</sub> |
|                     C/gcc |  1.117<sub>±0.002</sub> |     1.88<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  41.76<sub>±00.87</sub> |
|                   Nim/gcc |  1.348<sub>±0.002</sub> |     2.62<sub>±00.00</sub> + 4.62<sub>±00.00</sub> |  54.45<sub>±00.73</sub> |
|                    D/ldc2 |  1.352<sub>±0.002</sub> |     3.75<sub>±00.00</sub> + 3.38<sub>±00.00</sub> |  55.63<sub>±00.63</sub> |
|                      Java |  1.435<sub>±0.004</sub> |  43.04<sub>±00.06</sub> + 248.75<sub>±00.12</sub> |  60.04<sub>±01.08</sub> |
|                Kotlin/JVM |  1.561<sub>±0.021</sub> |  45.70<sub>±00.10</sub> + 252.06<sub>±00.31</sub> |  68.06<sub>±02.36</sub> |
|                        Go |  1.592<sub>±0.007</sub> |     4.31<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  66.55<sub>±01.37</sub> |
|                     Scala |  1.594<sub>±0.050</sub> |  63.26<sub>±00.10</sub> + 315.25<sub>±04.19</sub> |  68.76<sub>±01.29</sub> |
|                     V/gcc |  1.641<sub>±0.003</sub> |  2.75<sub>±00.00</sub> + 2383.00<sub>±01.44</sub> |  62.77<sub>±03.08</sub> |
|                Vala/clang |  1.646<sub>±0.003</sub> |     5.62<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  64.21<sub>±01.67</sub> |
|                  Vala/gcc |  1.647<sub>±0.002</sub> |     5.75<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  65.43<sub>±02.11</sub> |
|                      Ruby |  1.861<sub>±0.005</sub> |   12.38<sub>±00.00</sub> + 43.13<sub>±00.49</sub> |  73.89<sub>±01.16</sub> |
|       Perl (MIME::Base64) |  1.997<sub>±0.008</sub> |    14.88<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  81.05<sub>±01.87</sub> |
|              Ruby (--jit) |  2.033<sub>±0.005</sub> |   46.26<sub>±00.04</sub> + 64.14<sub>±00.04</sub> |  79.75<sub>±00.49</sub> |
|              C#/.NET Core |  2.372<sub>±0.021</sub> |    35.25<sub>±00.06</sub> + 5.34<sub>±00.06</sub> |  97.09<sub>±01.95</sub> |
|              F#/.NET Core |  2.445<sub>±0.076</sub> |   39.86<sub>±00.06</sub> + 20.22<sub>±00.94</sub> |  98.47<sub>±03.45</sub> |
|                       Tcl |  2.749<sub>±0.003</sub> |     5.25<sub>±00.00</sub> + 0.12<sub>±00.00</sub> | 115.56<sub>±01.50</sub> |
|                  Go/gccgo |  3.037<sub>±0.007</sub> |    24.56<sub>±00.19</sub> + 0.00<sub>±00.00</sub> | 144.41<sub>±01.00</sub> |
|                     Julia |  3.085<sub>±0.019</sub> |  258.12<sub>±00.02</sub> + 28.20<sub>±00.07</sub> | 126.59<sub>±02.11</sub> |
|                    Python |  3.099<sub>±0.002</sub> |    11.25<sub>±00.00</sub> + 0.10<sub>±00.00</sub> | 126.46<sub>±01.27</sub> |
|                   V/clang |  3.258<sub>±0.003</sub> |  2.62<sub>±00.00</sub> + 2388.00<sub>±01.12</sub> | 121.78<sub>±02.22</sub> |
|    Ruby/truffleruby (JVM) |  3.505<sub>±0.036</sub> | 350.81<sub>±05.12</sub> + 256.81<sub>±12.70</sub> | 182.88<sub>±06.01</sub> |
|               Python/pypy |  3.534<sub>±0.003</sub> |   59.50<sub>±00.12</sub> + 30.36<sub>±00.08</sub> | 158.06<sub>±01.43</sub> |
|   C++/clang++ (libcrypto) |  3.664<sub>±0.007</sub> |     6.38<sub>±00.00</sub> + 0.25<sub>±00.00</sub> | 152.34<sub>±02.81</sub> |
|       C++/g++ (libcrypto) |  3.664<sub>±0.003</sub> |     7.00<sub>±00.00</sub> + 0.25<sub>±00.00</sub> | 153.36<sub>±02.88</sub> |
|                     D/dmd |  3.760<sub>±0.003</sub> |     3.62<sub>±00.00</sub> + 3.38<sub>±00.00</sub> | 158.04<sub>±01.48</sub> |
|                    Racket |  3.886<sub>±0.007</sub> |   95.64<sub>±00.10</sub> + 21.81<sub>±00.19</sub> | 156.59<sub>±01.21</sub> |
|                     D/gdc |  3.976<sub>±0.003</sub> |     7.00<sub>±00.06</sub> + 3.62<sub>±00.00</sub> | 166.06<sub>±02.47</sub> |
|                Ruby/jruby |  4.129<sub>±0.009</sub> |  240.66<sub>±00.49</sub> + 78.50<sub>±00.12</sub> | 184.97<sub>±01.57</sub> |
|          Ruby/truffleruby |  8.789<sub>±0.034</sub> | 196.12<sub>±01.06</sub> + 544.31<sub>±06.06</sub> | 422.24<sub>±01.81</sub> |
| Perl (MIME::Base64::Perl) | 10.391<sub>±0.080</sub> |    16.25<sub>±00.12</sub> + 0.19<sub>±00.01</sub> | 462.87<sub>±10.60</sub> |

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
|        C++/g++ (simdjson On-Demand) |  0.060<sub>±0.000</sub> |    113.38<sub>±00.00</sub> + 59.75<sub>±00.00</sub> |   2.55<sub>±00.02</sub> |
|    C++/clang++ (simdjson On-Demand) |  0.062<sub>±0.000</sub> |    113.00<sub>±00.00</sub> + 59.75<sub>±00.00</sub> |   2.59<sub>±00.03</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.070<sub>±0.000</sub> |     112.82<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   2.94<sub>±00.04</sub> |
|         C++/clang++ (DAW JSON Link) |  0.085<sub>±0.000</sub> |     112.76<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.61<sub>±00.05</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.087<sub>±0.000</sub> |     113.21<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   3.54<sub>±00.05</sub> |
|             C++/g++ (DAW JSON Link) |  0.089<sub>±0.000</sub> |     113.21<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   3.66<sub>±00.02</sub> |
|                 Rust (Serde Custom) |  0.103<sub>±0.000</sub> |     111.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   4.43<sub>±00.03</sub> |
|          C++/clang++ (simdjson DOM) |  0.103<sub>±0.001</sub> |   112.88<sub>±00.00</sub> + 175.06<sub>±00.69</sub> |   4.72<sub>±00.08</sub> |
|              C++/g++ (simdjson DOM) |  0.107<sub>±0.002</sub> |   113.44<sub>±00.06</sub> + 175.86<sub>±00.89</sub> |   4.89<sub>±00.06</sub> |
|                  Rust (Serde Typed) |  0.110<sub>±0.000</sub> |    111.75<sub>±00.00</sub> + 11.12<sub>±00.00</sub> |   4.70<sub>±00.02</sub> |
|                     C++/g++ (gason) |  0.127<sub>±0.000</sub> |    113.21<sub>±00.00</sub> + 96.75<sub>±00.00</sub> |   5.16<sub>±00.06</sub> |
|                      C/gcc (yyjson) |  0.131<sub>±0.000</sub> |   111.69<sub>±00.06</sub> + 229.38<sub>±00.00</sub> |   5.39<sub>±00.05</sub> |
|                    C/clang (yyjson) |  0.132<sub>±0.001</sub> |   111.75<sub>±00.00</sub> + 229.38<sub>±00.00</sub> |   5.44<sub>±00.05</sub> |
|                 C++/clang++ (gason) |  0.133<sub>±0.001</sub> |    112.70<sub>±00.00</sub> + 96.75<sub>±00.00</sub> |   5.35<sub>±00.04</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.133<sub>±0.001</sub> |    112.81<sub>±00.06</sub> + 61.25<sub>±00.00</sub> |   5.56<sub>±00.07</sub> |
|              Scala (jsoniter-scala) |  0.147<sub>±0.001</sub> |    283.94<sub>±00.09</sub> + 25.62<sub>±00.19</sub> |   7.89<sub>±00.03</sub> |
|                 C++/g++ (RapidJSON) |  0.154<sub>±0.001</sub> |   113.21<sub>±00.00</sub> + 123.44<sub>±00.88</sub> |   6.61<sub>±00.07</sub> |
|             C++/clang++ (RapidJSON) |  0.194<sub>±0.001</sub> |   112.95<sub>±00.00</sub> + 128.62<sub>±00.00</sub> |   8.25<sub>±00.16</sub> |
|                   Go (rjson custom) |  0.199<sub>±0.000</sub> |     114.00<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   7.68<sub>±00.01</sub> |
|                          Go (Sonic) |  0.209<sub>±0.001</sub> |     121.88<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |   9.14<sub>±00.05</sub> |
|         C++/g++ (RapidJSON Precise) |  0.212<sub>±0.001</sub> |   113.21<sub>±00.00</sub> + 128.75<sub>±00.00</sub> |   9.15<sub>±00.11</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.220<sub>±0.001</sub> |    113.00<sub>±00.00</sub> + 80.75<sub>±00.00</sub> |   9.29<sub>±00.05</sub> |
|                          Go (rjson) |  0.224<sub>±0.000</sub> |     114.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   8.65<sub>±00.03</sub> |
|                                 Zig |  0.239<sub>±0.001</sub> |    111.12<sub>±00.00</sub> + 36.44<sub>±00.06</sub> |  10.29<sub>±00.04</sub> |
|                  Go (goccy/go-json) |  0.252<sub>±0.000</sub> |     114.50<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   9.99<sub>±00.08</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.262<sub>±0.001</sub> |   112.83<sub>±00.00</sub> + 128.75<sub>±00.00</sub> |  11.08<sub>±00.14</sub> |
|                        C/gcc (yajl) |  0.362<sub>±0.000</sub> |     111.25<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  15.71<sub>±00.12</sub> |
|                      C/clang (yajl) |  0.364<sub>±0.000</sub> |     111.25<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  15.65<sub>±00.08</sub> |
|             C++/g++ (RapidJSON SAX) |  0.366<sub>±0.001</sub> |     113.33<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  15.95<sub>±00.12</sub> |
|                C++/g++ (Boost.JSON) |  0.374<sub>±0.002</sub> |   113.47<sub>±00.00</sub> + 307.75<sub>±00.00</sub> |  15.97<sub>±00.15</sub> |
|                             Node.js |  0.374<sub>±0.001</sub> |   158.06<sub>±00.06</sub> + 283.88<sub>±03.00</sub> |  18.22<sub>±00.13</sub> |
|            C++/clang++ (Boost.JSON) |  0.377<sub>±0.002</sub> |   113.00<sub>±00.07</sub> + 307.75<sub>±00.00</sub> |  16.12<sub>±00.12</sub> |
|                   Nim/clang (jsony) |  0.386<sub>±0.001</sub> |   112.12<sub>±00.00</sub> + 159.75<sub>±01.94</sub> |  16.19<sub>±00.20</sub> |
|                     Nim/gcc (jsony) |  0.418<sub>±0.001</sub> |   111.75<sub>±00.00</sub> + 155.62<sub>±01.50</sub> |  17.56<sub>±00.10</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.423<sub>±0.001</sub> |     195.20<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  17.99<sub>±00.12</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.433<sub>±0.000</sub> |     113.34<sub>±00.00</sub> + 0.12<sub>±00.00</sub> |  19.54<sub>±00.20</sub> |
|                       Go (jsoniter) |  0.482<sub>±0.001</sub> |     114.50<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |  19.93<sub>±00.04</sub> |
|                      Crystal (Pull) |  0.484<sub>±0.002</sub> |    113.44<sub>±00.06</sub> + 18.00<sub>±00.00</sub> |  21.76<sub>±00.28</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.500<sub>±0.001</sub> |     195.20<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  22.35<sub>±00.40</sub> |
|     C#/.NET Core (System.Text.Json) |  0.500<sub>±0.001</sub> |   481.12<sub>±00.00</sub> + 119.63<sub>±00.08</sub> |  24.55<sub>±00.12</sub> |
|                    Crystal (Schema) |  0.510<sub>±0.001</sub> |    113.38<sub>±00.00</sub> + 48.31<sub>±00.06</sub> |  22.61<sub>±00.39</sub> |
|                Rust (Serde Untyped) |  0.557<sub>±0.002</sub> |   111.88<sub>±00.12</sub> + 839.75<sub>±00.00</sub> |  23.75<sub>±00.42</sub> |
|                     Java (DSL-JSON) |  0.571<sub>±0.020</sub> |   277.81<sub>±00.14</sub> + 163.69<sub>±00.38</sub> |  29.34<sub>±01.43</sub> |
|                         Python/pypy |  0.606<sub>±0.001</sub> |   280.66<sub>±00.07</sub> + 125.44<sub>±00.01</sub> |  26.55<sub>±00.11</sub> |
|                             V/clang |  0.635<sub>±0.001</sub> |   112.12<sub>±00.00</sub> + 496.00<sub>±00.00</sub> |  27.08<sub>±00.57</sub> |
|                               V/gcc |  0.639<sub>±0.005</sub> |   111.88<sub>±00.00</sub> + 496.00<sub>±00.00</sub> |  27.16<sub>±00.35</sub> |
|              Nim/clang (Packedjson) |  0.664<sub>±0.002</sub> |   112.38<sub>±00.00</sub> + 294.25<sub>±00.00</sub> |  28.59<sub>±00.36</sub> |
|                Nim/gcc (Packedjson) |  0.674<sub>±0.004</sub> |   112.12<sub>±00.00</sub> + 294.12<sub>±00.00</sub> |  28.60<sub>±00.31</sub> |
|                       Julia (JSON3) |  0.700<sub>±0.001</sub> | 631.76<sub>±00.04</sub> + 332.81<sub>±01.00</sub>   |  29.33<sub>±00.22</sub> |
|                 CPython (UltraJSON) |  0.700<sub>±0.002</sub> |   123.35<sub>±00.12</sub> + 478.56<sub>±01.81</sub> |  28.00<sub>±00.40</sub> |
|             Perl (Cpanel::JSON::XS) |  0.767<sub>±0.011</sub> |   125.38<sub>±00.00</sub> + 402.88<sub>±00.00</sub> |  32.13<sub>±00.63</sub> |
|                             Crystal |  0.795<sub>±0.007</sub> |   113.44<sub>±00.06</sub> + 392.12<sub>±00.00</sub> |  34.30<sub>±00.39</sub> |
|                                  Go |  0.834<sub>±0.001</sub> |     114.00<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  34.46<sub>±00.07</sub> |
|                                 PHP |  0.836<sub>±0.001</sub> |   127.36<sub>±00.12</sub> + 517.82<sub>±00.00</sub> |  36.28<sub>±00.23</sub> |
|                              Python |  0.936<sub>±0.003</sub> |   121.30<sub>±00.06</sub> + 325.88<sub>±00.00</sub> |  39.40<sub>±00.44</sub> |
|                        Ruby (--jit) |  1.014<sub>±0.001</sub> |   126.75<sub>±00.02</sub> + 210.64<sub>±00.03</sub> |  42.62<sub>±00.35</sub> |
|                                Ruby |  1.031<sub>±0.002</sub> |   122.00<sub>±00.12</sub> + 212.08<sub>±00.20</sub> |  43.59<sub>±00.44</sub> |
|                             Nim/gcc |  1.036<sub>±0.005</sub> |  112.12<sub>±00.00</sub> + 1000.88<sub>±00.38</sub> |  43.26<sub>±00.27</sub> |
|                           Nim/clang |  1.092<sub>±0.004</sub> |   112.38<sub>±00.00</sub> + 999.00<sub>±00.00</sub> |  45.82<sub>±00.27</sub> |
|                             Clojure |  1.128<sub>±0.017</sub> |   476.94<sub>±02.22</sub> + 405.56<sub>±14.12</sub> |  59.15<sub>±01.15</sub> |
|                        C#/.NET Core |  1.169<sub>±0.005</sub> |    488.06<sub>±00.06</sub> + 41.46<sub>±00.17</sub> |  57.40<sub>±00.51</sub> |
|              C++/clang++ (Nlohmann) |  1.182<sub>±0.002</sub> |   112.95<sub>±00.00</sub> + 359.88<sub>±00.00</sub> |  50.72<sub>±00.62</sub> |
|                C++/clang++ (json-c) |  1.185<sub>±0.002</sub> |  112.96<sub>±00.00</sub> + 1215.88<sub>±00.00</sub> |  49.83<sub>±00.62</sub> |
|                    C++/g++ (json-c) |  1.187<sub>±0.004</sub> |  113.35<sub>±00.00</sub> + 1215.88<sub>±00.00</sub> |  49.45<sub>±00.40</sub> |
|                            Go/gccgo |  1.256<sub>±0.003</sub> |     139.25<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |  51.58<sub>±00.27</sub> |
|                  C++/g++ (Nlohmann) |  1.266<sub>±0.003</sub> |   113.33<sub>±00.00</sub> + 447.88<sub>±00.00</sub> |  53.77<sub>±00.41</sub> |
|     F#/.NET Core (System.Text.Json) |  1.494<sub>±0.003</sub> |   490.33<sub>±00.11</sub> + 133.64<sub>±00.43</sub> |  70.35<sub>±00.46</sub> |
|                         Ruby (YAJL) |  1.565<sub>±0.019</sub> |   121.69<sub>±00.06</sub> + 233.88<sub>±00.00</sub> |  68.39<sub>±01.94</sub> |
|                              D/ldc2 |  1.633<sub>±0.002</sub> |   113.12<sub>±00.06</sub> + 638.62<sub>±00.00</sub> |  70.66<sub>±00.51</sub> |
|                                  C3 |  1.959<sub>±0.003</sub> |   111.88<sub>±00.00</sub> + 795.88<sub>±00.00</sub> |  82.11<sub>±00.61</sub> |
|                             Haskell |  2.019<sub>±0.013</sub> |   115.75<sub>±00.12</sub> + 723.44<sub>±00.12</sub> |  86.17<sub>±00.72</sub> |
|    C++/clang++ (Boost.PropertyTree) |  2.589<sub>±0.004</sub> |  195.33<sub>±00.00</sub> + 1232.50<sub>±00.00</sub> | 111.50<sub>±00.48</sub> |
|                           Rust (jq) |  2.637<sub>±0.006</sub> |   113.50<sub>±00.00</sub> + 905.97<sub>±02.12</sub> | 110.08<sub>±00.97</sub> |
|                               D/dmd |  2.778<sub>±0.003</sub> |   113.50<sub>±00.00</sub> + 626.00<sub>±00.00</sub> | 121.50<sub>±00.76</sub> |
|                          Ruby/jruby |  2.838<sub>±0.016</sub> |   555.29<sub>±00.73</sub> + 572.50<sub>±77.88</sub> | 152.07<sub>±00.83</sub> |
|        C++/g++ (Boost.PropertyTree) |  2.907<sub>±0.010</sub> |  113.46<sub>±00.00</sub> + 1439.88<sub>±00.00</sub> | 124.59<sub>±00.96</sub> |
|                            Vala/gcc |  2.980<sub>±0.017</sub> |   115.25<sub>±00.00</sub> + 980.00<sub>±00.00</sub> | 127.79<sub>±01.75</sub> |
|                          Vala/clang |  2.983<sub>±0.012</sub> |   115.25<sub>±00.00</sub> + 980.00<sub>±00.00</sub> | 127.36<sub>±00.59</sub> |
|                                Odin |  3.026<sub>±0.007</sub> |    111.38<sub>±00.00</sub> + 20.00<sub>±00.00</sub> | 126.12<sub>±01.22</sub> |
|                               D/gdc |  3.459<sub>±0.007</sub> |   116.75<sub>±00.00</sub> + 708.88<sub>±00.06</sub> | 148.15<sub>±01.48</sub> |
|                              Racket |  3.861<sub>±0.015</sub> |   223.42<sub>±00.33</sub> + 288.29<sub>±04.07</sub> | 163.00<sub>±01.74</sub> |
|                   Perl (JSON::Tiny) |  9.034<sub>±0.089</sub> |   126.00<sub>±00.00</sub> + 528.56<sub>±00.00</sub> | 401.98<sub>±06.07</sub> |
|              Ruby/truffleruby (JVM) | 10.618<sub>±0.148</sub> | 482.80<sub>±07.29</sub> + 2212.51<sub>±207.00</sub> | 678.79<sub>±08.84</sub> |
|                    Ruby/truffleruby | 11.341<sub>±0.066</sub> |  402.19<sub>±11.19</sub> + 1941.56<sub>±30.56</sub> | 665.04<sub>±03.89</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                Language |                   Time, s |                                        Memory, MiB |                  Energy, J |
| :---------------------- | ------------------------: | -------------------------------------------------: | -------------------------: |
|         D/ldc2 (lubeck) |    0.042<sub>±0.001</sub> |    16.38<sub>±01.31</sub> + 47.88<sub>±01.38</sub> |      4.27<sub>±00.05</sub> |
|          Python (NumPy) |    0.066<sub>±0.001</sub> |    32.78<sub>±02.50</sub> + 52.17<sub>±02.29</sub> |      5.62<sub>±00.07</sub> |
|   Nim/gcc (Arraymancer) |    0.073<sub>±0.005</sub> |     5.62<sub>±00.12</sub> + 58.06<sub>±00.19</sub> |      5.88<sub>±00.23</sub> |
|         C++/g++ (Eigen) |    0.081<sub>±0.000</sub> |    10.06<sub>±01.62</sub> + 79.75<sub>±01.75</sub> |      5.68<sub>±00.02</sub> |
|     C++/clang++ (Eigen) |    0.081<sub>±0.000</sub> |    12.88<sub>±00.50</sub> + 77.46<sub>±00.56</sub> |      6.41<sub>±00.04</sub> |
|             Java (ND4J) |    0.082<sub>±0.001</sub> |   122.16<sub>±01.44</sub> + 92.56<sub>±00.19</sub> |      6.51<sub>±00.11</sub> |
|          Rust (ndarray) |    0.091<sub>±0.001</sub> |     2.62<sub>±00.00</sub> + 68.36<sub>±00.00</sub> |      6.20<sub>±00.10</sub> |
| Nim/clang (Arraymancer) |    0.114<sub>±0.045</sub> |    18.81<sub>±00.44</sub> + 45.62<sub>±00.50</sub> |      8.31<sub>±02.57</sub> |
|      Julia (threads: 2) |    0.120<sub>±0.000</sub> |   259.86<sub>±00.08</sub> + 56.95<sub>±00.02</sub> |      6.69<sub>±00.05</sub> |
|      Julia (threads: 1) |    0.170<sub>±0.000</sub> |   259.82<sub>±00.17</sub> + 56.36<sub>±00.04</sub> |      8.09<sub>±00.03</sub> |
|           V/clang (VSL) |    0.224<sub>±0.002</sub> |    10.62<sub>±00.12</sub> + 51.60<sub>±00.03</sub> |     18.12<sub>±00.11</sub> |
|   V/clang (VSL + CBLAS) |    0.227<sub>±0.001</sub> |    10.62<sub>±00.12</sub> + 51.47<sub>±00.09</sub> |     18.15<sub>±00.23</sub> |
|     V/gcc (VSL + CBLAS) |    0.423<sub>±0.003</sub> |     2.50<sub>±00.00</sub> + 51.38<sub>±00.00</sub> |     32.87<sub>±00.19</sub> |
|             V/gcc (VSL) |    0.441<sub>±0.002</sub> |     2.50<sub>±00.00</sub> + 51.38<sub>±00.00</sub> |     30.29<sub>±00.18</sub> |
|         Julia (no BLAS) |    1.113<sub>±0.017</sub> |   256.38<sub>±00.00</sub> + 51.50<sub>±00.00</sub> |     49.41<sub>±00.63</sub> |
|                   D/gdc |    1.505<sub>±0.001</sub> |      7.12<sub>±00.00</sub> + 4.00<sub>±00.00</sub> |     58.37<sub>±00.11</sub> |
|                  D/ldc2 |    1.722<sub>±0.001</sub> |     3.62<sub>±00.00</sub> + 70.38<sub>±00.00</sub> |     64.31<sub>±00.15</sub> |
|                   D/dmd |    1.880<sub>±0.002</sub> |     3.56<sub>±00.06</sub> + 70.38<sub>±00.00</sub> |     72.70<sub>±00.56</sub> |
|                   C/gcc |    3.034<sub>±0.001</sub> |     2.00<sub>±00.00</sub> + 68.38<sub>±00.00</sub> |    113.24<sub>±00.33</sub> |
|                   V/gcc |    3.036<sub>±0.000</sub> |     2.50<sub>±00.06</sub> + 68.75<sub>±00.00</sub> |    113.56<sub>±00.46</sub> |
|                    Java |    3.057<sub>±0.001</sub> |    43.14<sub>±00.13</sub> + 68.75<sub>±00.12</sub> |    123.21<sub>±00.95</sub> |
|              Vala/clang |    3.062<sub>±0.001</sub> |     5.54<sub>±00.04</sub> + 68.38<sub>±00.00</sub> |    109.60<sub>±00.81</sub> |
|                 V/clang |    3.065<sub>±0.000</sub> |     2.88<sub>±00.00</sub> + 68.75<sub>±00.00</sub> |    109.23<sub>±00.06</sub> |
|                 C/clang |    3.067<sub>±0.001</sub> |     2.00<sub>±00.00</sub> + 68.38<sub>±00.00</sub> |    109.34<sub>±01.23</sub> |
|                    Rust |    3.067<sub>±0.001</sub> |     2.25<sub>±00.12</sub> + 68.50<sub>±00.00</sub> |    109.32<sub>±00.64</sub> |
|                     Zig |    3.071<sub>±0.003</sub> |     1.88<sub>±00.00</sub> + 68.56<sub>±00.06</sub> |    109.78<sub>±00.28</sub> |
|                 Nim/gcc |    3.091<sub>±0.002</sub> |     2.62<sub>±00.06</sub> + 57.88<sub>±00.00</sub> |    115.31<sub>±00.37</sub> |
|                   Swift |    3.102<sub>±0.001</sub> |     8.38<sub>±00.12</sub> + 68.62<sub>±00.00</sub> |    113.76<sub>±00.36</sub> |
|               Nim/clang |    3.121<sub>±0.001</sub> |     2.94<sub>±00.06</sub> + 57.75<sub>±00.00</sub> |    111.65<sub>±00.36</sub> |
|                Vala/gcc |    3.131<sub>±0.001</sub> |     5.45<sub>±00.06</sub> + 68.50<sub>±00.00</sub> |    118.43<sub>±00.81</sub> |
|                      Go |    3.153<sub>±0.001</sub> |      4.12<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |    115.20<sub>±00.44</sub> |
|                 Crystal |    3.154<sub>±0.001</sub> |     3.75<sub>±00.00</sub> + 59.50<sub>±00.00</sub> |    116.10<sub>±00.24</sub> |
|                Go/gccgo |    3.159<sub>±0.001</sub> |     24.44<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |    115.54<sub>±00.39</sub> |
|              Kotlin/JVM |    3.160<sub>±0.002</sub> |    43.80<sub>±00.16</sub> + 70.00<sub>±00.12</sub> |    124.32<sub>±00.30</sub> |
|                 Node.js |    3.212<sub>±0.005</sub> |    57.91<sub>±00.28</sub> + 70.83<sub>±00.19</sub> |    126.71<sub>±00.50</sub> |
|                   Scala |    3.243<sub>±0.003</sub> |   64.43<sub>±00.14</sub> + 158.69<sub>±00.06</sub> |    119.81<sub>±00.30</sub> |
|             Python/pypy |    3.263<sub>±0.002</sub> |    60.62<sub>±00.12</sub> + 68.94<sub>±00.06</sub> |    136.66<sub>±00.49</sub> |
|            C#/.NET Core |    4.725<sub>±0.001</sub> |    36.62<sub>±00.06</sub> + 69.44<sub>±00.06</sub> |    193.26<sub>±01.10</sub> |
|  Ruby/truffleruby (JVM) |   17.667<sub>±0.235</sub> |  417.21<sub>±09.41</sub> + 339.96<sub>±72.03</sub> |    722.45<sub>±10.75</sub> |
|        Ruby/truffleruby |   17.776<sub>±0.189</sub> |  324.88<sub>±03.31</sub> + 504.50<sub>±13.19</sub> |    638.49<sub>±08.58</sub> |
|                  Python |  136.267<sub>±0.437</sub> |    11.62<sub>±00.06</sub> + 68.75<sub>±00.00</sub> |   6131.10<sub>±41.35</sub> |
|                    Perl |  149.049<sub>±1.633</sub> |    8.62<sub>±00.06</sub> + 379.75<sub>±00.00</sub> |   6511.38<sub>±65.17</sub> |
|            Ruby (--jit) |  152.991<sub>±0.294</sub> |    21.83<sub>±01.04</sub> + 68.62<sub>±00.06</sub> |   6802.85<sub>±30.51</sub> |
|                     Tcl |  205.116<sub>±0.537</sub> |    7.62<sub>±00.00</sub> + 400.38<sub>±00.00</sub> |   9417.77<sub>±28.45</sub> |
|                    Ruby |  211.868<sub>±0.274</sub> |    12.75<sub>±00.00</sub> + 69.62<sub>±00.00</sub> |   9430.93<sub>±36.77</sub> |
|              Ruby/jruby | 330.203<sub>±16.724</sub> | 299.99<sub>±00.72</sub> + 880.65<sub>±189.19</sub> | 13544.82<sub>±520.08</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

Notes:

 - All languages but V and Python use unordered hashmaps (V and Python don't provide those out of box, and
 their hashmaps use keys in the insertion order);
 - The results are always sorted (could be unstable or stable though).

[Primes](primes)

|               Language |                Time, s |                                       Memory, MiB |              Energy, J |
| :--------------------- | ---------------------: | ------------------------------------------------: | ---------------------: |
|                C++/g++ | 0.064<sub>±0.000</sub> |    3.62<sub>±00.00</sub> + 85.55<sub>±00.44</sub> |  2.57<sub>±00.06</sub> |
|                     Go | 0.070<sub>±0.000</sub> |     3.62<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  3.09<sub>±00.09</sub> |
|                    Zig | 0.071<sub>±0.000</sub> |    1.50<sub>±00.12</sub> + 64.69<sub>±00.06</sub> |  2.94<sub>±00.01</sub> |
|            C++/clang++ | 0.075<sub>±0.000</sub> |    3.25<sub>±00.00</sub> + 64.48<sub>±00.19</sub> |  2.90<sub>±00.06</sub> |
|                V/clang | 0.100<sub>±0.000</sub> |   2.12<sub>±00.00</sub> + 200.44<sub>±00.69</sub> |  4.18<sub>±00.08</sub> |
|                   Rust | 0.102<sub>±0.000</sub> |    2.00<sub>±00.00</sub> + 74.25<sub>±00.12</sub> |  4.05<sub>±00.06</sub> |
|                  V/gcc | 0.106<sub>±0.001</sub> |   2.25<sub>±00.00</sub> + 211.06<sub>±02.06</sub> |  4.52<sub>±00.05</sub> |
|                   Java | 0.113<sub>±0.000</sub> |  41.79<sub>±00.14</sub> + 107.88<sub>±03.81</sub> |  6.06<sub>±00.22</sub> |
|                Crystal | 0.144<sub>±0.000</sub> |    3.50<sub>±00.00</sub> + 89.88<sub>±00.00</sub> |  5.91<sub>±00.10</sub> |
|                  Scala | 0.184<sub>±0.004</sub> |  66.51<sub>±00.15</sub> + 158.44<sub>±02.00</sub> | 10.67<sub>±00.30</sub> |
|                Node.js | 0.216<sub>±0.001</sub> |  46.88<sub>±00.06</sub> + 227.55<sub>±04.77</sub> | 10.68<sub>±00.15</sub> |
|                Nim/gcc | 0.293<sub>±0.000</sub> |   1.75<sub>±00.12</sub> + 582.06<sub>±01.00</sub> | 11.25<sub>±00.17</sub> |
|              Nim/clang | 0.295<sub>±0.001</sub> |   2.00<sub>±00.00</sub> + 603.44<sub>±05.06</sub> | 11.62<sub>±00.12</sub> |
|             Lua/luajit | 0.305<sub>±0.001</sub> |   2.50<sub>±00.00</sub> + 157.22<sub>±01.05</sub> | 12.36<sub>±00.17</sub> |
|                  Julia | 0.419<sub>±0.002</sub> | 258.38<sub>±00.12</sub> + 215.71<sub>±03.03</sub> | 16.24<sub>±00.31</sub> |
|            Python/pypy | 0.617<sub>±0.003</sub> |  59.62<sub>±00.12</sub> + 247.16<sub>±00.04</sub> | 25.09<sub>±00.34</sub> |
|                 Racket | 0.757<sub>±0.004</sub> | 109.51<sub>±00.71</sub> + 248.35<sub>±01.07</sub> | 30.05<sub>±00.58</sub> |
|       Ruby/truffleruby | 0.910<sub>±0.010</sub> | 197.81<sub>±01.44</sub> + 672.44<sub>±54.69</sub> | 61.70<sub>±00.61</sub> |
|                    Lua | 1.230<sub>±0.005</sub> |   2.56<sub>±00.06</sub> + 283.84<sub>±00.75</sub> | 50.66<sub>±00.69</sub> |
|           Ruby (--jit) | 1.289<sub>±0.014</sub> |  23.96<sub>±00.03</sub> + 174.84<sub>±00.31</sub> | 54.03<sub>±01.54</sub> |
| Ruby/truffleruby (JVM) | 1.363<sub>±0.033</sub> | 350.78<sub>±04.33</sub> + 506.71<sub>±28.12</sub> | 88.50<sub>±02.14</sub> |
|             Ruby/jruby | 1.491<sub>±0.021</sub> | 250.45<sub>±00.62</sub> + 577.06<sub>±02.31</sub> | 80.03<sub>±01.19</sub> |
|                   Ruby | 1.992<sub>±0.027</sub> |  12.12<sub>±00.00</sub> + 183.69<sub>±00.50</sub> | 85.16<sub>±02.19</sub> |
|                 Python | 2.096<sub>±0.012</sub> |  11.44<sub>±00.06</sub> + 181.46<sub>±01.00</sub> | 90.37<sub>±01.30</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux trixie/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 9.0.304                         |
| C#/.NET Core     | 4.14.0-3.25359.3 (6dbcfd2f)     |
| C3               | 0.7.4                           |
| Chez Scheme      | 10.0.0                          |
| Clojure          | "1.12.1"                        |
| Crystal          | 1.17.1                          |
| D/dmd            | v2.111.0                        |
| D/gdc            | 14.2.0                          |
| D/ldc2           | 1.41.0                          |
| Elixir           | 1.18.3                          |
| F#/.NET Core     | 13.9.303.0 for F# 9.0           |
| Go               | go1.25.0                        |
| Go/gccgo         | 14.2.0                          |
| Haskell          | 9.10.2                          |
| Idris 2          | 0.7.0                           |
| Java             | 24.0.2                          |
| Julia            | v"1.11.6"                       |
| Kotlin           | 2.2.0                           |
| Lua              | 5.4.7                           |
| Lua/luajit       | 2.1.1748495995                  |
| MLton            | 20241230                        |
| Nim              | 2.2.4                           |
| Node.js          | v24.5.0                         |
| OCaml            | 5.3.0                           |
| Odin             | dev-2025-08-nightly             |
| PHP              | 8.4.11                          |
| Perl             | v5.40.1                         |
| Python           | 3.13.5                          |
| Python/pypy      | 7.3.20-final0 for Python 3.11.13 |
| Racket           | "8.17"                          |
| Ruby             | 3.4.5p51                        |
| Ruby/jruby       | 10.0.2.0                        |
| Ruby/truffleruby | 24.2.2                          |
| Rust             | 1.89.0                          |
| Scala            | 3.7.2                           |
| Swift            | 6.1.2                           |
| Tcl              | 8.6                             |
| V                | 0.4.11 b9e5757                  |
| Vala             | 0.56.18                         |
| Zig              | 0.14.1                          |
| clang/clang++    | 19.1.7 (3+b1)                   |
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
