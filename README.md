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

UPDATE: 2026-07-19

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|               Language |                  Time, s |                                        Memory, MiB |                 Energy, J |
| :--------------------- | -----------------------: | -------------------------------------------------: | ------------------------: |
|  C# (Staged)/.NET Core |   0.426<sub>±0.008</sub> |     36.54<sub>±00.21</sub> + 0.19<sub>±00.00</sub> |    15.95<sub>±00.17</sub> |
|         Scala (Staged) |   0.452<sub>±0.011</sub> |   236.82<sub>±03.88</sub> + 35.69<sub>±06.05</sub> |    28.71<sub>±00.85</sub> |
|        Racket (Staged) |   0.886<sub>±0.001</sub> |    106.36<sub>±00.19</sub> + 0.11<sub>±00.04</sub> |    34.42<sub>±00.06</sub> |
|                  V/gcc |   0.936<sub>±0.000</sub> |      2.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    38.97<sub>±00.34</sub> |
|                   Rust |   1.007<sub>±0.000</sub> |      2.11<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    43.32<sub>±00.34</sub> |
|                C++/g++ |   1.019<sub>±0.000</sub> |      3.79<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    43.21<sub>±00.10</sub> |
|                 D/ldc2 |   1.059<sub>±0.001</sub> |      3.43<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    45.17<sub>±00.41</sub> |
|                  D/gdc |   1.138<sub>±0.001</sub> |      7.31<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    50.23<sub>±00.57</sub> |
|                C/clang |   1.139<sub>±0.002</sub> |      1.72<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    48.73<sub>±00.51</sub> |
|            C++/clang++ |   1.142<sub>±0.000</sub> |      3.36<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    47.25<sub>±00.18</sub> |
|                  C/gcc |   1.147<sub>±0.001</sub> |      1.63<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    48.70<sub>±00.37</sub> |
|                   Java |   1.193<sub>±0.000</sub> |     43.33<sub>±00.05</sub> + 0.96<sub>±00.02</sub> |    49.38<sub>±00.06</sub> |
|                Nim/gcc |   1.210<sub>±0.000</sub> |      2.15<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |    50.32<sub>±00.19</sub> |
|               Vala/gcc |   1.217<sub>±0.002</sub> |      5.81<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    50.27<sub>±00.19</sub> |
|             Kotlin/JVM |   1.221<sub>±0.002</sub> |     46.05<sub>±00.06</sub> + 1.42<sub>±00.13</sub> |    51.07<sub>±00.29</sub> |
|                    Zig |   1.223<sub>±0.001</sub> |      2.03<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    53.08<sub>±00.78</sub> |
|             Vala/clang |   1.234<sub>±0.000</sub> |      5.87<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    52.45<sub>±00.17</sub> |
|                     Go |   1.258<sub>±0.001</sub> |      4.28<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    51.64<sub>±00.06</sub> |
|           C#/.NET Core |   1.284<sub>±0.001</sub> |     36.76<sub>±00.10</sub> + 0.10<sub>±00.00</sub> |    54.93<sub>±00.24</sub> |
|               Go/gccgo |   1.359<sub>±0.003</sub> |     24.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    57.36<sub>±00.64</sub> |
|           F#/.NET Core |   1.548<sub>±0.003</sub> |     41.53<sub>±00.09</sub> + 0.04<sub>±00.00</sub> |    65.18<sub>±01.11</sub> |
|                Crystal |   1.568<sub>±0.001</sub> |      3.57<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    66.49<sub>±00.36</sub> |
|                  Julia |   1.594<sub>±0.009</sub> |    284.23<sub>±00.14</sub> + 0.00<sub>±00.00</sub> |    68.55<sub>±00.49</sub> |
|                  OCaml |   1.617<sub>±0.003</sub> |      3.74<sub>±00.06</sub> + 2.43<sub>±00.00</sub> |    76.60<sub>±00.51</sub> |
|              Nim/clang |   1.643<sub>±0.002</sub> |      2.44<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    66.40<sub>±00.59</sub> |
|                 Racket |   1.722<sub>±0.040</sub> |    119.17<sub>±00.12</sub> + 0.17<sub>±00.12</sub> |    71.34<sub>±00.91</sub> |
|                  MLton |   1.730<sub>±0.003</sub> |      1.89<sub>±00.01</sub> + 0.39<sub>±00.00</sub> |    73.71<sub>±00.28</sub> |
|            Chez Scheme |   1.759<sub>±0.003</sub> |     26.17<sub>±00.25</sub> + 3.35<sub>±00.02</sub> |    75.11<sub>±00.66</sub> |
|                V/clang |   1.794<sub>±0.046</sub> |      1.99<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    77.45<sub>±01.57</sub> |
|                  Scala |   2.389<sub>±0.003</sub> |   61.29<sub>±00.05</sub> + 139.08<sub>±00.06</sub> |   107.22<sub>±00.30</sub> |
|                Node.js |   2.808<sub>±0.009</sub> |     48.44<sub>±00.04</sub> + 5.84<sub>±00.00</sub> |   120.16<sub>±00.82</sub> |
|                  D/dmd |   3.245<sub>±0.000</sub> |      3.70<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |   122.18<sub>±00.21</sub> |
|       Haskell (MArray) |   3.496<sub>±0.001</sub> |      4.29<sub>±00.03</sub> + 4.12<sub>±00.00</sub> |   144.88<sub>±00.63</sub> |
|           Haskell (FP) |   3.822<sub>±0.004</sub> |      4.42<sub>±00.08</sub> + 4.13<sub>±00.01</sub> |   162.71<sub>±00.11</sub> |
| Ruby/truffleruby (JVM) |   4.523<sub>±0.058</sub> | 378.04<sub>±02.21</sub> + 444.58<sub>±163.71</sub> |   212.93<sub>±04.90</sub> |
|       Ruby/truffleruby |   5.505<sub>±0.030</sub> |  233.79<sub>±01.73</sub> + 633.02<sub>±45.03</sub> |   268.97<sub>±02.62</sub> |
|             Lua/luajit |   5.626<sub>±0.004</sub> |      2.76<sub>±00.05</sub> + 0.01<sub>±00.00</sub> |   232.72<sub>±00.22</sub> |
|                  Swift |   6.569<sub>±0.001</sub> |     17.73<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   272.03<sub>±00.21</sub> |
|            Python/pypy |   9.191<sub>±0.045</sub> |    62.46<sub>±00.03</sub> + 30.63<sub>±00.01</sub> |   408.98<sub>±04.57</sub> |
|                  Idris |  13.458<sub>±0.042</sub> |     22.06<sub>±00.19</sub> + 7.92<sub>±00.04</sub> |   611.71<sub>±02.25</sub> |
|           Ruby (--jit) |  15.901<sub>±0.011</sub> |     15.27<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   658.52<sub>±00.61</sub> |
|                 Elixir |  20.810<sub>±0.061</sub> |     76.75<sub>±00.63</sub> + 0.00<sub>±00.00</sub> |   824.04<sub>±07.15</sub> |
|             Ruby/jruby |  32.740<sub>±0.616</sub> |   238.66<sub>±00.41</sub> + 80.14<sub>±18.23</sub> |  1509.72<sub>±21.91</sub> |
|                    PHP |  34.019<sub>±0.076</sub> |     18.00<sub>±00.08</sub> + 0.01<sub>±00.01</sub> |  1423.17<sub>±11.77</sub> |
|                    Lua |  37.573<sub>±0.191</sub> |      2.45<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  1541.22<sub>±14.35</sub> |
|                 Python |  49.330<sub>±0.680</sub> |     12.08<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  2108.25<sub>±28.35</sub> |
|                   Ruby |  67.367<sub>±0.409</sub> |     14.77<sub>±00.07</sub> + 0.01<sub>±00.00</sub> |  2908.01<sub>±32.95</sub> |
|               Tcl (FP) | 189.103<sub>±0.450</sub> |      4.84<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  8320.06<sub>±98.94</sub> |
|                   Perl | 208.931<sub>±2.144</sub> |      7.39<sub>±00.06</sub> + 0.03<sub>±00.00</sub> | 9355.64<sub>±118.23</sub> |
|              Tcl (OOP) | 365.029<sub>±1.156</sub> |      4.88<sub>±00.06</sub> + 0.00<sub>±00.00</sub> | 16288.39<sub>±48.20</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|               Language |                 Time, s |                                       Memory, MiB |                 Energy, J |
| :--------------------- | ----------------------: | ------------------------------------------------: | ------------------------: |
|  C# (Staged)/.NET Core |  2.894<sub>±0.002</sub> |    36.50<sub>±00.09</sub> + 3.38<sub>±00.00</sub> |   110.83<sub>±00.90</sub> |
|         Scala (Staged) |  6.446<sub>±0.207</sub> | 240.51<sub>±02.76</sub> + 204.86<sub>±11.98</sub> |   353.03<sub>±24.42</sub> |
|                  C/gcc | 12.463<sub>±0.049</sub> |     1.79<sub>±00.04</sub> + 0.22<sub>±00.00</sub> |   515.12<sub>±06.52</sub> |
|                Nim/gcc | 12.483<sub>±0.079</sub> |     2.23<sub>±00.06</sub> + 1.50<sub>±00.01</sub> |   520.46<sub>±08.64</sub> |
|                   Java | 12.811<sub>±0.074</sub> |    43.28<sub>±00.06</sub> + 2.12<sub>±00.25</sub> |   518.57<sub>±14.07</sub> |
|                  V/gcc | 12.834<sub>±0.067</sub> |     2.06<sub>±00.05</sub> + 1.03<sub>±00.01</sub> |   511.07<sub>±07.90</sub> |
|                C/clang | 13.261<sub>±0.078</sub> |     1.79<sub>±00.07</sub> + 0.22<sub>±00.00</sub> |   581.82<sub>±11.36</sub> |
|             Kotlin/JVM | 13.299<sub>±0.049</sub> |    46.10<sub>±00.07</sub> + 2.69<sub>±00.14</sub> |   565.03<sub>±10.68</sub> |
|            C++/clang++ | 13.337<sub>±0.028</sub> |     3.35<sub>±00.04</sub> + 0.61<sub>±00.00</sub> |   558.87<sub>±06.31</sub> |
|                C++/g++ | 13.436<sub>±0.073</sub> |     3.84<sub>±00.07</sub> + 0.61<sub>±00.00</sub> |   556.54<sub>±05.82</sub> |
|               Vala/gcc | 13.498<sub>±0.044</sub> |     5.86<sub>±00.05</sub> + 0.49<sub>±00.00</sub> |   544.14<sub>±04.40</sub> |
|                 D/ldc2 | 13.859<sub>±0.032</sub> |     3.46<sub>±00.07</sub> + 0.60<sub>±00.00</sub> |   574.71<sub>±08.45</sub> |
|        Racket (Staged) | 14.341<sub>±0.099</sub> |  106.54<sub>±00.41</sub> + 73.20<sub>±01.38</sub> |   589.50<sub>±34.08</sub> |
|                   Rust | 14.401<sub>±0.043</sub> |     2.15<sub>±00.03</sub> + 0.23<sub>±00.00</sub> |   579.07<sub>±10.97</sub> |
|                     Go | 14.512<sub>±0.040</sub> |     4.31<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   589.83<sub>±08.42</sub> |
|                Crystal | 14.736<sub>±0.162</sub> |     3.67<sub>±00.04</sub> + 0.28<sub>±00.00</sub> |   630.58<sub>±12.50</sub> |
|             Vala/clang | 15.033<sub>±0.027</sub> |     5.91<sub>±00.08</sub> + 0.49<sub>±00.00</sub> |   620.38<sub>±08.54</sub> |
|                  D/gdc | 15.084<sub>±0.071</sub> |     7.46<sub>±00.13</sub> + 0.61<sub>±00.00</sub> |   644.56<sub>±14.34</sub> |
|                    Zig | 15.328<sub>±0.067</sub> |     2.16<sub>±00.03</sub> + 0.57<sub>±00.01</sub> |   626.90<sub>±14.87</sub> |
|           C#/.NET Core | 15.540<sub>±0.041</sub> |    36.85<sub>±00.11</sub> + 0.79<sub>±00.00</sub> |   655.32<sub>±13.53</sub> |
|                  Scala | 15.889<sub>±0.085</sub> |  61.30<sub>±00.06</sub> + 148.18<sub>±09.21</sub> |   747.41<sub>±44.46</sub> |
|              Nim/clang | 17.799<sub>±0.054</sub> |     2.54<sub>±00.02</sub> + 1.50<sub>±00.01</sub> |   735.52<sub>±21.62</sub> |
|               Go/gccgo | 18.055<sub>±0.212</sub> |    24.04<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |   805.36<sub>±42.22</sub> |
|                V/clang | 18.387<sub>±0.244</sub> |     2.11<sub>±00.07</sub> + 1.02<sub>±00.01</sub> |   817.76<sub>±36.08</sub> |
|                  OCaml | 23.556<sub>±0.103</sub> |     4.58<sub>±00.05</sub> + 2.62<sub>±00.00</sub> |  1163.54<sub>±26.42</sub> |
|                  Swift | 26.540<sub>±0.073</sub> |    17.86<sub>±00.11</sub> + 0.12<sub>±00.00</sub> |  1118.34<sub>±19.11</sub> |
|           F#/.NET Core | 27.249<sub>±0.073</sub> |    41.69<sub>±00.06</sub> + 1.39<sub>±00.00</sub> |  1161.14<sub>±17.44</sub> |
|                Node.js | 31.199<sub>±0.447</sub> |    48.44<sub>±00.02</sub> + 7.87<sub>±00.07</sub> |  1363.03<sub>±82.79</sub> |
|            Chez Scheme | 31.665<sub>±0.867</sub> |    26.57<sub>±00.24</sub> + 2.99<sub>±00.02</sub> |  1421.03<sub>±75.28</sub> |
|             Lua/luajit | 31.898<sub>±0.122</sub> |     2.82<sub>±00.08</sub> + 0.29<sub>±00.00</sub> |  1328.97<sub>±21.13</sub> |
|                  Julia | 34.067<sub>±0.274</sub> |   284.14<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  1486.81<sub>±67.97</sub> |
|                  MLton | 35.606<sub>±0.212</sub> |     1.92<sub>±00.02</sub> + 4.29<sub>±00.01</sub> |  1613.48<sub>±19.03</sub> |
|                 Racket | 35.813<sub>±0.436</sub> |   119.38<sub>±00.13</sub> + 1.53<sub>±00.32</sub> |  1643.25<sub>±73.30</sub> |
|       Haskell (MArray) | 36.933<sub>±0.132</sub> |     4.32<sub>±00.09</sub> + 5.04<sub>±00.00</sub> |  1529.33<sub>±06.29</sub> |
|                  D/dmd | 39.001<sub>±0.050</sub> |     3.73<sub>±00.10</sub> + 0.62<sub>±00.00</sub> |  1476.89<sub>±48.15</sub> |
|            Python/pypy | 41.018<sub>±0.561</sub> |   62.46<sub>±00.04</sub> + 30.88<sub>±00.04</sub> |  1823.60<sub>±19.81</sub> |
| Ruby/truffleruby (JVM) | 48.343<sub>±0.465</sub> | 379.10<sub>±02.22</sub> + 348.47<sub>±54.10</sub> |  2166.38<sub>±77.68</sub> |
|       Ruby/truffleruby | 48.397<sub>±0.604</sub> | 236.26<sub>±01.36</sub> + 490.09<sub>±10.76</sub> | 2462.23<sub>±104.24</sub> |
|                  Idris | 64.810<sub>±0.732</sub> |   22.27<sub>±00.19</sub> + 15.85<sub>±00.02</sub> |  2899.86<sub>±52.71</sub> |
|           Haskell (FP) | 80.044<sub>±0.460</sub> |    4.47<sub>±00.03</sub> + 74.85<sub>±00.01</sub> |  3474.18<sub>±85.77</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|                       PHP |  0.065<sub>±0.000</sub> |    18.48<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |   3.25<sub>±00.02</sub> |
|            C/gcc (aklomp) |  0.097<sub>±0.000</sub> |     2.31<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   4.63<sub>±00.09</sub> |
|          C/clang (aklomp) |  0.097<sub>±0.001</sub> |     2.31<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   4.53<sub>±00.04</sub> |
|              Go (base64x) |  0.262<sub>±0.005</sub> |     5.61<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  14.78<sub>±00.30</sub> |
|                   Node.js |  0.327<sub>±0.004</sub> |  46.27<sub>±00.06</sub> + 114.22<sub>±01.76</sub> |  15.41<sub>±00.19</sub> |
|                   C/clang |  0.559<sub>±0.000</sub> |     2.19<sub>±00.05</sub> + 0.05<sub>±00.01</sub> |  20.98<sub>±00.07</sub> |
|                       Zig |  0.703<sub>±0.000</sub> |     2.42<sub>±00.02</sub> + 0.07<sub>±00.00</sub> |  26.45<sub>±00.02</sub> |
|                      Rust |  0.851<sub>±0.000</sub> |     2.49<sub>±00.05</sub> + 0.09<sub>±00.01</sub> |  35.20<sub>±00.09</sub> |
|                 Nim/clang |  1.007<sub>±0.001</sub> |     3.00<sub>±00.06</sub> + 5.00<sub>±00.01</sub> |  40.05<sub>±00.59</sub> |
|                   Crystal |  1.087<sub>±0.029</sub> |     4.12<sub>±00.06</sub> + 1.35<sub>±00.42</sub> |  48.40<sub>±02.27</sub> |
|                     C/gcc |  1.117<sub>±0.000</sub> |     2.18<sub>±00.02</sub> + 0.08<sub>±00.01</sub> |  41.25<sub>±00.35</sub> |
|                   Nim/gcc |  1.351<sub>±0.001</sub> |     2.66<sub>±00.07</sub> + 4.39<sub>±00.01</sub> |  54.62<sub>±00.18</sub> |
|                    D/ldc2 |  1.352<sub>±0.001</sub> |     4.02<sub>±00.05</sub> + 3.39<sub>±00.00</sub> |  55.08<sub>±00.38</sub> |
|                      Java |  1.515<sub>±0.003</sub> |  44.83<sub>±00.10</sub> + 226.26<sub>±00.03</sub> |  61.44<sub>±00.27</sub> |
|                        Go |  1.539<sub>±0.003</sub> |     5.00<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  69.30<sub>±00.54</sub> |
|              F#/.NET Core |  1.540<sub>±0.003</sub> |    42.32<sub>±00.15</sub> + 4.04<sub>±00.06</sub> |  61.98<sub>±00.46</sub> |
|                     V/gcc |  1.586<sub>±0.001</sub> |  2.56<sub>±00.02</sub> + 2381.80<sub>±05.05</sub> |  58.46<sub>±00.34</sub> |
|                     Scala |  1.612<sub>±0.010</sub> |  59.93<sub>±00.10</sub> + 312.37<sub>±24.69</sub> |  69.02<sub>±00.58</sub> |
|                Vala/clang |  1.645<sub>±0.001</sub> |     6.32<sub>±00.04</sub> + 0.11<sub>±00.00</sub> |  63.84<sub>±00.41</sub> |
|                  Vala/gcc |  1.646<sub>±0.001</sub> |     6.34<sub>±00.03</sub> + 0.11<sub>±00.00</sub> |  63.67<sub>±00.34</sub> |
|                Kotlin/JVM |  1.660<sub>±0.004</sub> |  47.47<sub>±00.17</sub> + 347.92<sub>±04.11</sub> |  70.15<sub>±00.39</sub> |
|       Perl (MIME::Base64) |  1.745<sub>±0.002</sub> |    15.10<sub>±00.04</sub> + 0.05<sub>±00.00</sub> |  67.99<sub>±00.24</sub> |
|                      Ruby |  1.824<sub>±0.003</sub> |   15.49<sub>±00.04</sub> + 43.54<sub>±00.38</sub> |  72.53<sub>±00.23</sub> |
|              Ruby (--jit) |  1.850<sub>±0.002</sub> |   16.27<sub>±00.12</sub> + 44.09<sub>±00.02</sub> |  73.90<sub>±00.36</sub> |
|              C#/.NET Core |  2.411<sub>±0.014</sub> |    38.11<sub>±00.08</sub> + 3.53<sub>±00.03</sub> |  94.63<sub>±00.69</sub> |
|                       Tcl |  2.939<sub>±0.006</sub> |     5.39<sub>±00.05</sub> + 0.13<sub>±00.00</sub> | 122.24<sub>±00.23</sub> |
|                  Go/gccgo |  3.101<sub>±0.007</sub> |    24.72<sub>±00.06</sub> + 0.00<sub>±00.00</sub> | 145.04<sub>±00.28</sub> |
|                   V/clang |  3.162<sub>±0.001</sub> |  2.55<sub>±00.03</sub> + 2387.39<sub>±00.54</sub> | 118.09<sub>±00.51</sub> |
|                    Python |  3.317<sub>±0.004</sub> |    11.93<sub>±00.06</sub> + 0.08<sub>±00.00</sub> | 131.23<sub>±00.48</sub> |
|       C++/g++ (libcrypto) |  3.426<sub>±0.002</sub> |     7.42<sub>±00.13</sub> + 0.16<sub>±00.00</sub> | 143.30<sub>±00.59</sub> |
|   C++/clang++ (libcrypto) |  3.428<sub>±0.003</sub> |     6.72<sub>±00.12</sub> + 0.17<sub>±00.01</sub> | 142.53<sub>±00.11</sub> |
|                     D/dmd |  3.474<sub>±0.002</sub> |     3.74<sub>±00.06</sub> + 3.49<sub>±00.00</sub> | 145.71<sub>±00.33</sub> |
|               Python/pypy |  3.535<sub>±0.004</sub> |   62.26<sub>±00.05</sub> + 30.43<sub>±00.05</sub> | 155.98<sub>±01.43</sub> |
|    Ruby/truffleruby (JVM) |  3.779<sub>±0.015</sub> | 398.25<sub>±04.00</sub> + 278.28<sub>±17.40</sub> | 192.92<sub>±01.04</sub> |
|                     D/gdc |  3.794<sub>±0.007</sub> |     7.64<sub>±00.01</sub> + 3.61<sub>±00.03</sub> | 155.91<sub>±00.50</sub> |
|                    Racket |  3.958<sub>±0.030</sub> |   99.29<sub>±00.20</sub> + 23.99<sub>±00.31</sub> | 159.00<sub>±01.16</sub> |
|                Ruby/jruby |  4.188<sub>±0.008</sub> |  238.62<sub>±01.74</sub> + 45.00<sub>±01.20</sub> | 185.99<sub>±00.65</sub> |
|                     Julia |  7.800<sub>±0.072</sub> |  304.79<sub>±00.10</sub> + 29.96<sub>±00.13</sub> | 292.52<sub>±02.83</sub> |
|          Ruby/truffleruby |  8.102<sub>±0.007</sub> | 370.42<sub>±01.27</sub> + 509.24<sub>±15.60</sub> | 405.84<sub>±01.63</sub> |
| Perl (MIME::Base64::Perl) | 10.102<sub>±0.024</sub> |    16.42<sub>±00.10</sub> + 0.15<sub>±00.01</sub> | 443.86<sub>±02.81</sub> |

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

|                            Language |                Time, s |                                         Memory, MiB |               Energy, J |
| :---------------------------------- | ---------------------: | --------------------------------------------------: | ----------------------: |
|    C++/clang++ (simdjson On-Demand) | 0.061<sub>±0.000</sub> |    113.19<sub>±00.06</sub> + 59.81<sub>±00.01</sub> |   2.55<sub>±00.02</sub> |
|        C++/g++ (simdjson On-Demand) | 0.063<sub>±0.000</sub> |    113.83<sub>±00.04</sub> + 59.83<sub>±00.01</sub> |   2.66<sub>±00.03</sub> |
| C++/clang++ (DAW JSON Link NoCheck) | 0.069<sub>±0.000</sub> |     113.21<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   2.93<sub>±00.03</sub> |
|         C++/clang++ (DAW JSON Link) | 0.083<sub>±0.000</sub> |     113.17<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   3.49<sub>±00.01</sub> |
|     C++/g++ (DAW JSON Link NoCheck) | 0.092<sub>±0.000</sub> |     113.61<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   3.75<sub>±00.03</sub> |
|                 Rust (Serde Custom) | 0.105<sub>±0.000</sub> |     111.85<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   4.51<sub>±00.03</sub> |
|          C++/clang++ (simdjson DOM) | 0.106<sub>±0.001</sub> |   113.13<sub>±00.03</sub> + 171.99<sub>±01.21</sub> |   4.78<sub>±00.05</sub> |
|              C++/g++ (simdjson DOM) | 0.108<sub>±0.001</sub> |   113.72<sub>±00.01</sub> + 176.78<sub>±00.01</sub> |   4.94<sub>±00.07</sub> |
|                  Rust (Serde Typed) | 0.114<sub>±0.000</sub> |    111.86<sub>±00.05</sub> + 11.88<sub>±00.01</sub> |   4.85<sub>±00.02</sub> |
|             C++/g++ (DAW JSON Link) | 0.120<sub>±0.000</sub> |     113.66<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.98<sub>±00.06</sub> |
|                 C++/clang++ (gason) | 0.131<sub>±0.000</sub> |    113.25<sub>±00.04</sub> + 96.75<sub>±00.00</sub> |   5.23<sub>±00.01</sub> |
|                     C++/g++ (gason) | 0.132<sub>±0.000</sub> |    113.59<sub>±00.04</sub> + 96.75<sub>±00.00</sub> |   5.36<sub>±00.06</sub> |
|               D/ldc2 (Mir Asdf DOM) | 0.133<sub>±0.000</sub> |    113.20<sub>±00.03</sub> + 61.25<sub>±00.00</sub> |   5.48<sub>±00.04</sub> |
|                    C/clang (yyjson) | 0.136<sub>±0.000</sub> |   111.92<sub>±00.04</sub> + 229.43<sub>±00.01</sub> |   5.40<sub>±00.04</sub> |
|                      C/gcc (yyjson) | 0.137<sub>±0.000</sub> |   111.89<sub>±00.05</sub> + 229.45<sub>±00.01</sub> |   5.47<sub>±00.03</sub> |
|                 C++/g++ (RapidJSON) | 0.143<sub>±0.000</sub> |   113.67<sub>±00.06</sub> + 125.63<sub>±00.84</sub> |   6.14<sub>±00.01</sub> |
|              Scala (jsoniter-scala) | 0.145<sub>±0.001</sub> |    279.31<sub>±00.15</sub> + 31.12<sub>±00.09</sub> |   7.92<sub>±00.13</sub> |
|             C++/clang++ (RapidJSON) | 0.192<sub>±0.001</sub> |   113.17<sub>±00.05</sub> + 128.67<sub>±00.00</sub> |   8.07<sub>±00.09</sub> |
|         C++/g++ (RapidJSON Precise) | 0.204<sub>±0.000</sub> |   113.70<sub>±00.04</sub> + 123.39<sub>±00.98</sub> |   8.82<sub>±00.04</sub> |
|                          Go (Sonic) | 0.208<sub>±0.007</sub> |     122.76<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   8.98<sub>±00.20</sub> |
|                   Go (rjson custom) | 0.210<sub>±0.000</sub> |     114.56<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   7.94<sub>±00.08</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) | 0.220<sub>±0.001</sub> |    113.35<sub>±00.09</sub> + 80.66<sub>±00.01</sub> |   9.07<sub>±00.02</sub> |
|                                 Zig | 0.230<sub>±0.000</sub> |    111.75<sub>±00.02</sub> + 33.03<sub>±00.02</sub> |   9.90<sub>±00.02</sub> |
|                          Go (rjson) | 0.232<sub>±0.000</sub> |     114.63<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   8.86<sub>±00.05</sub> |
|     C++/clang++ (RapidJSON Precise) | 0.261<sub>±0.000</sub> |   113.15<sub>±00.07</sub> + 128.70<sub>±00.00</sub> |  11.26<sub>±00.05</sub> |
|                  Go (goccy/go-json) | 0.265<sub>±0.000</sub> |     115.16<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  10.38<sub>±00.03</sub> |
|                C++/g++ (Boost.JSON) | 0.361<sub>±0.001</sub> |   113.82<sub>±00.03</sub> + 307.85<sub>±00.00</sub> |  15.41<sub>±00.10</sub> |
|            C++/clang++ (Boost.JSON) | 0.370<sub>±0.001</sub> |   113.29<sub>±00.06</sub> + 307.85<sub>±00.00</sub> |  15.74<sub>±00.11</sub> |
|                        C/gcc (yajl) | 0.371<sub>±0.001</sub> |     111.51<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  16.10<sub>±00.09</sub> |
|                      C/clang (yajl) | 0.373<sub>±0.001</sub> |     111.53<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  16.18<sub>±00.11</sub> |
|             C++/g++ (RapidJSON SAX) | 0.376<sub>±0.000</sub> |     113.79<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  16.59<sub>±00.05</sub> |
|                   Nim/clang (jsony) | 0.380<sub>±0.000</sub> |   112.18<sub>±00.09</sub> + 154.15<sub>±00.00</sub> |  16.06<sub>±00.10</sub> |
|                             Node.js | 0.387<sub>±0.001</sub> |   157.45<sub>±00.03</sub> + 233.89<sub>±05.71</sub> |  19.24<sub>±00.08</sub> |
|         C++/clang++ (RapidJSON SAX) | 0.392<sub>±0.002</sub> |     195.42<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  17.00<sub>±00.15</sub> |
|                     Java (DSL-JSON) | 0.415<sub>±0.002</sub> |   278.98<sub>±00.05</sub> + 176.75<sub>±00.10</sub> |  20.79<sub>±00.27</sub> |
|                     Nim/gcc (jsony) | 0.422<sub>±0.000</sub> |   111.90<sub>±00.07</sub> + 154.15<sub>±00.00</sub> |  17.81<sub>±00.08</sub> |
|     C++/g++ (RapidJSON SAX Precise) | 0.452<sub>±0.001</sub> |     113.75<sub>±00.04</sub> + 0.02<sub>±00.00</sub> |  20.32<sub>±00.14</sub> |
| C++/clang++ (RapidJSON SAX Precise) | 0.481<sub>±0.000</sub> |     195.43<sub>±00.05</sub> + 0.02<sub>±00.00</sub> |  21.34<sub>±00.21</sub> |
|                      Crystal (Pull) | 0.485<sub>±0.001</sub> |    113.76<sub>±00.04</sub> + 17.95<sub>±00.00</sub> |  21.45<sub>±00.26</sub> |
|                    Crystal (Schema) | 0.500<sub>±0.003</sub> |    113.76<sub>±00.05</sub> + 49.44<sub>±01.19</sub> |  22.16<sub>±00.12</sub> |
|                        Ruby (--jit) | 0.514<sub>±0.001</sub> |   125.44<sub>±00.09</sub> + 212.64<sub>±00.01</sub> |  22.18<sub>±00.07</sub> |
|                       Go (jsoniter) | 0.516<sub>±0.001</sub> |     115.04<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  21.11<sub>±00.31</sub> |
|                                Ruby | 0.519<sub>±0.001</sub> |   124.98<sub>±00.05</sub> + 212.63<sub>±00.01</sub> |  22.30<sub>±00.15</sub> |
|                Rust (Serde Untyped) | 0.552<sub>±0.001</sub> |   112.01<sub>±00.02</sub> + 839.88<sub>±00.00</sub> |  23.15<sub>±00.22</sub> |
|     C#/.NET Core (System.Text.Json) | 0.581<sub>±0.005</sub> |   483.48<sub>±00.07</sub> + 129.87<sub>±00.02</sub> |  25.54<sub>±00.19</sub> |
|                 CPython (UltraJSON) | 0.610<sub>±0.002</sub> |   124.17<sub>±00.07</sub> + 476.32<sub>±01.72</sub> |  24.90<sub>±00.14</sub> |
|                         Python/pypy | 0.612<sub>±0.001</sub> |   283.52<sub>±00.02</sub> + 125.49<sub>±00.04</sub> |  26.69<sub>±00.20</sub> |
|                             V/clang | 0.627<sub>±0.001</sub> |   112.17<sub>±00.02</sub> + 496.00<sub>±00.00</sub> |  26.76<sub>±00.20</sub> |
|                               V/gcc | 0.633<sub>±0.002</sub> |   111.67<sub>±00.04</sub> + 496.00<sub>±00.00</sub> |  26.47<sub>±00.12</sub> |
|                Nim/gcc (Packedjson) | 0.649<sub>±0.001</sub> |   112.20<sub>±00.11</sub> + 294.23<sub>±00.01</sub> |  27.40<sub>±00.25</sub> |
|              Nim/clang (Packedjson) | 0.684<sub>±0.002</sub> |   112.58<sub>±00.07</sub> + 294.24<sub>±00.01</sub> |  29.51<sub>±00.20</sub> |
|                       Julia (JSON3) | 0.728<sub>±0.002</sub> |   761.39<sub>±00.17</sub> + 301.41<sub>±01.48</sub> |  30.06<sub>±00.25</sub> |
|                             Crystal | 0.749<sub>±0.003</sub> |   113.80<sub>±00.05</sub> + 392.13<sub>±00.00</sub> |  33.29<sub>±00.36</sub> |
|             Perl (Cpanel::JSON::XS) | 0.760<sub>±0.004</sub> |   125.63<sub>±00.11</sub> + 402.85<sub>±00.00</sub> |  31.94<sub>±00.32</sub> |
|                                 PHP | 0.816<sub>±0.001</sub> |   127.58<sub>±00.07</sub> + 517.86<sub>±00.00</sub> |  35.01<sub>±00.30</sub> |
|                                  Go | 0.835<sub>±0.000</sub> |     114.82<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  34.53<sub>±00.04</sub> |
|                              Python | 0.885<sub>±0.002</sub> |   121.87<sub>±00.08</sub> + 325.86<sub>±00.00</sub> |  37.24<sub>±00.22</sub> |
|                             Clojure | 0.979<sub>±0.006</sub> |   496.34<sub>±04.14</sub> + 735.37<sub>±02.30</sub> |  49.26<sub>±00.26</sub> |
|              C++/clang++ (Nlohmann) | 1.011<sub>±0.002</sub> |   113.32<sub>±00.07</sub> + 359.87<sub>±00.00</sub> |  42.82<sub>±00.22</sub> |
|                        C#/.NET Core | 1.013<sub>±0.005</sub> |    491.33<sub>±00.14</sub> + 32.84<sub>±00.37</sub> |  47.88<sub>±00.45</sub> |
|                             Nim/gcc | 1.038<sub>±0.002</sub> |  112.22<sub>±00.02</sub> + 1000.58<sub>±00.02</sub> |  43.47<sub>±00.70</sub> |
|                           Nim/clang | 1.098<sub>±0.004</sub> |   112.52<sub>±00.06</sub> + 999.03<sub>±00.02</sub> |  45.85<sub>±00.49</sub> |
|                  C++/g++ (Nlohmann) | 1.133<sub>±0.002</sub> |   113.75<sub>±00.03</sub> + 447.87<sub>±00.00</sub> |  47.08<sub>±00.22</sub> |
|                C++/clang++ (json-c) | 1.169<sub>±0.005</sub> |  113.34<sub>±00.02</sub> + 1215.86<sub>±00.00</sub> |  50.01<sub>±00.33</sub> |
|                    C++/g++ (json-c) | 1.170<sub>±0.003</sub> |  113.74<sub>±00.01</sub> + 1215.86<sub>±00.00</sub> |  48.86<sub>±00.53</sub> |
|                            Go/gccgo | 1.273<sub>±0.003</sub> |     139.50<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  52.46<sub>±00.14</sub> |
|                         Ruby (YAJL) | 1.543<sub>±0.015</sub> |   124.94<sub>±00.07</sub> + 234.06<sub>±00.01</sub> |  65.82<sub>±01.01</sub> |
|                              D/ldc2 | 1.591<sub>±0.007</sub> |   113.43<sub>±00.08</sub> + 671.12<sub>±00.00</sub> |  70.84<sub>±00.64</sub> |
|     F#/.NET Core (System.Text.Json) | 1.639<sub>±0.005</sub> |   492.85<sub>±00.05</sub> + 173.18<sub>±00.23</sub> |  72.71<sub>±00.58</sub> |
|                                  C3 | 1.967<sub>±0.002</sub> |   112.15<sub>±00.02</sub> + 859.96<sub>±00.00</sub> |  81.57<sub>±00.82</sub> |
|                             Haskell | 2.030<sub>±0.004</sub> |   116.14<sub>±00.05</sub> + 723.88<sub>±00.21</sub> |  87.52<sub>±01.02</sub> |
|                          Ruby/jruby | 2.393<sub>±0.010</sub> |   478.68<sub>±02.19</sub> + 772.89<sub>±04.52</sub> | 119.28<sub>±01.19</sub> |
|    C++/clang++ (Boost.PropertyTree) | 2.406<sub>±0.004</sub> |  195.60<sub>±00.03</sub> + 1232.58<sub>±00.02</sub> | 103.80<sub>±00.54</sub> |
|                            Vala/gcc | 2.733<sub>±0.009</sub> |   115.78<sub>±00.04</sub> + 980.00<sub>±00.00</sub> | 117.92<sub>±01.12</sub> |
|                          Vala/clang | 2.739<sub>±0.008</sub> |   115.80<sub>±00.06</sub> + 980.00<sub>±00.00</sub> | 118.97<sub>±00.67</sub> |
|        C++/g++ (Boost.PropertyTree) | 2.756<sub>±0.004</sub> |  113.87<sub>±00.04</sub> + 1439.85<sub>±00.00</sub> | 118.33<sub>±00.43</sub> |
|                               D/dmd | 2.881<sub>±0.006</sub> |   113.70<sub>±00.08</sub> + 671.08<sub>±00.00</sub> | 126.74<sub>±00.64</sub> |
|                               D/gdc | 2.955<sub>±0.004</sub> |   117.39<sub>±00.09</sub> + 671.42<sub>±00.06</sub> | 127.80<sub>±01.19</sub> |
|                           Rust (jq) | 2.996<sub>±0.007</sub> |   113.66<sub>±00.04</sub> + 903.60<sub>±01.32</sub> | 122.26<sub>±01.26</sub> |
|                                Odin | 3.072<sub>±0.017</sub> |    111.81<sub>±00.01</sub> + 19.93<sub>±00.03</sub> | 128.21<sub>±01.64</sub> |
|                              Racket | 4.743<sub>±0.029</sub> |   228.62<sub>±00.13</sub> + 288.27<sub>±03.82</sub> | 192.78<sub>±00.97</sub> |
|              Ruby/truffleruby (JVM) | 5.199<sub>±0.031</sub> | 552.04<sub>±18.06</sub> + 1446.21<sub>±141.07</sub> | 323.38<sub>±03.12</sub> |
|                    Ruby/truffleruby | 6.444<sub>±0.030</sub> |  461.58<sub>±02.69</sub> + 1406.22<sub>±30.17</sub> | 349.41<sub>±02.32</sub> |
|                   Perl (JSON::Tiny) | 8.820<sub>±0.045</sub> |   126.16<sub>±00.08</sub> + 528.52<sub>±00.01</sub> | 389.30<sub>±01.27</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                Language |                  Time, s |                                        Memory, MiB |                  Energy, J |
| :---------------------- | -----------------------: | -------------------------------------------------: | -------------------------: |
|         D/ldc2 (lubeck) |   0.042<sub>±0.001</sub> |    15.16<sub>±01.57</sub> + 49.83<sub>±01.58</sub> |      4.22<sub>±00.03</sub> |
|          Python (NumPy) |   0.065<sub>±0.000</sub> |    37.33<sub>±00.80</sub> + 49.68<sub>±00.80</sub> |      5.53<sub>±00.04</sub> |
|   Nim/gcc (Arraymancer) |   0.075<sub>±0.002</sub> |     6.15<sub>±00.27</sub> + 58.25<sub>±00.03</sub> |      6.01<sub>±00.09</sub> |
| Nim/clang (Arraymancer) |   0.075<sub>±0.007</sub> |    17.09<sub>±02.72</sub> + 48.14<sub>±02.77</sub> |      6.09<sub>±00.41</sub> |
|     C++/clang++ (Eigen) |   0.080<sub>±0.000</sub> |    12.36<sub>±00.50</sub> + 78.64<sub>±00.40</sub> |      6.33<sub>±00.06</sub> |
|         C++/g++ (Eigen) |   0.082<sub>±0.001</sub> |     8.73<sub>±00.67</sub> + 81.70<sub>±00.75</sub> |      5.71<sub>±00.03</sub> |
|             Java (ND4J) |   0.082<sub>±0.001</sub> |   129.40<sub>±02.31</sub> + 88.99<sub>±03.19</sub> |      6.51<sub>±00.10</sub> |
|          Rust (ndarray) |   0.090<sub>±0.001</sub> |     2.82<sub>±00.02</sub> + 68.38<sub>±00.02</sub> |      5.92<sub>±00.09</sub> |
|      Julia (threads: 2) |   0.138<sub>±0.001</sub> |   304.67<sub>±00.15</sub> + 54.55<sub>±00.03</sub> |      7.40<sub>±00.06</sub> |
|      Julia (threads: 1) |   0.189<sub>±0.001</sub> |   304.57<sub>±00.09</sub> + 54.00<sub>±00.06</sub> |      8.81<sub>±00.08</sub> |
|     V/gcc (VSL + CBLAS) |   0.216<sub>±0.006</sub> |     2.42<sub>±00.03</sub> + 52.76<sub>±00.36</sub> |     19.39<sub>±00.20</sub> |
|           V/clang (VSL) |   0.227<sub>±0.003</sub> |     2.88<sub>±00.05</sub> + 52.75<sub>±00.17</sub> |     16.00<sub>±00.46</sub> |
|             V/gcc (VSL) |   0.234<sub>±0.003</sub> |     2.50<sub>±00.02</sub> + 52.74<sub>±00.36</sub> |     16.63<sub>±00.18</sub> |
|   V/clang (VSL + CBLAS) |   0.237<sub>±0.006</sub> |     2.88<sub>±00.04</sub> + 52.83<sub>±00.29</sub> |     17.33<sub>±00.54</sub> |
|         Julia (no BLAS) |   1.092<sub>±0.016</sub> |   317.98<sub>±00.22</sub> + 47.69<sub>±00.11</sub> |     47.68<sub>±00.83</sub> |
|                  D/ldc2 |   1.720<sub>±0.001</sub> |     3.96<sub>±00.05</sub> + 70.35<sub>±00.02</sub> |     63.98<sub>±00.45</sub> |
|                   D/gdc |   1.762<sub>±0.001</sub> |     7.84<sub>±00.09</sub> + 70.12<sub>±00.02</sub> |     66.20<sub>±00.69</sub> |
|                   D/dmd |   1.881<sub>±0.004</sub> |     3.73<sub>±00.07</sub> + 70.38<sub>±00.00</sub> |     72.09<sub>±00.81</sub> |
|                   C/gcc |   3.033<sub>±0.001</sub> |     2.24<sub>±00.02</sub> + 68.33<sub>±00.01</sub> |    113.17<sub>±00.43</sub> |
|                   V/gcc |   3.036<sub>±0.003</sub> |     2.44<sub>±00.05</sub> + 68.82<sub>±00.01</sub> |    113.85<sub>±00.47</sub> |
|              Vala/clang |   3.062<sub>±0.001</sub> |     6.14<sub>±00.03</sub> + 68.43<sub>±00.01</sub> |    108.88<sub>±00.63</sub> |
|                 C/clang |   3.065<sub>±0.001</sub> |     2.18<sub>±00.07</sub> + 68.33<sub>±00.00</sub> |    105.83<sub>±00.61</sub> |
|                 V/clang |   3.067<sub>±0.001</sub> |     2.79<sub>±00.04</sub> + 68.81<sub>±00.02</sub> |    108.57<sub>±00.78</sub> |
|                    Java |   3.067<sub>±0.002</sub> |    44.78<sub>±00.16</sub> + 79.63<sub>±00.51</sub> |    123.20<sub>±00.10</sub> |
|                    Rust |   3.074<sub>±0.001</sub> |     2.48<sub>±00.03</sub> + 68.45<sub>±00.01</sub> |    109.75<sub>±00.08</sub> |
|                     Zig |   3.083<sub>±0.003</sub> |     2.37<sub>±00.02</sub> + 68.64<sub>±00.01</sub> |    110.17<sub>±00.28</sub> |
|                Vala/gcc |   3.090<sub>±0.000</sub> |     6.08<sub>±00.07</sub> + 68.47<sub>±00.01</sub> |    113.98<sub>±00.53</sub> |
|                 Nim/gcc |   3.096<sub>±0.001</sub> |     2.82<sub>±00.03</sub> + 60.44<sub>±02.70</sub> |    115.73<sub>±00.32</sub> |
|                   Swift |   3.101<sub>±0.001</sub> |     8.84<sub>±00.07</sub> + 68.70<sub>±00.00</sub> |    113.48<sub>±00.54</sub> |
|               Nim/clang |   3.139<sub>±0.000</sub> |     3.10<sub>±00.03</sub> + 57.76<sub>±00.01</sub> |    111.89<sub>±00.36</sub> |
|                 Crystal |   3.150<sub>±0.001</sub> |     4.16<sub>±00.04</sub> + 59.54<sub>±00.02</sub> |    112.74<sub>±00.61</sub> |
|                Go/gccgo |   3.152<sub>±0.001</sub> |     24.46<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    119.33<sub>±01.93</sub> |
|                      Go |   3.154<sub>±0.001</sub> |      4.71<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    115.05<sub>±00.72</sub> |
|              Kotlin/JVM |   3.170<sub>±0.003</sub> |    45.60<sub>±00.06</sub> + 78.31<sub>±00.71</sub> |    125.13<sub>±00.21</sub> |
|                 Node.js |   3.212<sub>±0.005</sub> |    55.70<sub>±00.11</sub> + 70.74<sub>±00.24</sub> |    127.56<sub>±00.66</sub> |
|             Python/pypy |   3.254<sub>±0.003</sub> |    62.89<sub>±00.16</sub> + 68.91<sub>±00.03</sub> |    136.35<sub>±00.62</sub> |
|                   Scala |   3.296<sub>±0.056</sub> |   61.03<sub>±00.35</sub> + 143.64<sub>±10.19</sub> |    121.47<sub>±01.88</sub> |
|            C#/.NET Core |   4.481<sub>±0.002</sub> |    38.67<sub>±00.13</sub> + 69.12<sub>±00.01</sub> |    184.00<sub>±01.19</sub> |
|        Ruby/truffleruby |  15.737<sub>±0.911</sub> |  333.28<sub>±08.86</sub> + 678.76<sub>±13.99</sub> |    572.44<sub>±33.80</sub> |
|  Ruby/truffleruby (JVM) |  17.269<sub>±0.213</sub> |  409.96<sub>±05.56</sub> + 390.82<sub>±30.74</sub> |    631.63<sub>±07.87</sub> |
|            Ruby (--jit) | 123.646<sub>±0.130</sub> |    15.56<sub>±00.04</sub> + 70.55<sub>±00.25</sub> |   5526.35<sub>±19.62</sub> |
|                  Python | 126.209<sub>±0.402</sub> |    12.31<sub>±00.02</sub> + 68.78<sub>±00.01</sub> |   5827.45<sub>±27.70</sub> |
|                    Perl | 133.960<sub>±1.473</sub> |    8.79<sub>±00.08</sub> + 379.67<sub>±00.00</sub> |   5884.82<sub>±47.83</sub> |
|                     Tcl | 205.178<sub>±1.065</sub> |    7.73<sub>±00.07</sub> + 400.34<sub>±00.00</sub> |   9271.40<sub>±54.82</sub> |
|                    Ruby | 206.898<sub>±0.787</sub> |    14.97<sub>±00.04</sub> + 70.90<sub>±00.00</sub> |   9206.81<sub>±19.54</sub> |
|              Ruby/jruby | 299.956<sub>±6.650</sub> | 258.92<sub>±00.15</sub> + 2037.78<sub>±02.60</sub> | 12257.83<sub>±233.04</sub> |

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
|                    Zig | 0.064<sub>±0.000</sub> |    1.99<sub>±00.02</sub> + 67.01<sub>±00.19</sub> |   2.65<sub>±00.02</sub> |
|                C++/g++ | 0.064<sub>±0.001</sub> |    4.13<sub>±00.07</sub> + 86.26<sub>±00.21</sub> |   2.55<sub>±00.04</sub> |
|            C++/clang++ | 0.066<sub>±0.000</sub> |    3.50<sub>±00.06</sub> + 63.30<sub>±00.15</sub> |   2.65<sub>±00.03</sub> |
|                     Go | 0.070<sub>±0.001</sub> |     4.24<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.29<sub>±00.06</sub> |
|                   Rust | 0.098<sub>±0.000</sub> |    2.22<sub>±00.04</sub> + 72.94<sub>±00.02</sub> |   3.89<sub>±00.02</sub> |
|                  V/gcc | 0.111<sub>±0.001</sub> |   2.05<sub>±00.05</sub> + 232.05<sub>±01.95</sub> |   4.67<sub>±00.02</sub> |
|                Crystal | 0.116<sub>±0.001</sub> |    3.74<sub>±00.01</sub> + 89.94<sub>±00.01</sub> |   5.34<sub>±00.09</sub> |
|                V/clang | 0.118<sub>±0.000</sub> |   2.07<sub>±00.05</sub> + 219.86<sub>±00.78</sub> |   4.88<sub>±00.02</sub> |
|                   Java | 0.161<sub>±0.002</sub> |  43.54<sub>±00.09</sub> + 116.76<sub>±02.21</sub> |   8.84<sub>±00.17</sub> |
|                Node.js | 0.207<sub>±0.001</sub> |  45.03<sub>±00.01</sub> + 172.08<sub>±00.27</sub> |  10.60<sub>±00.11</sub> |
|                  Scala | 0.245<sub>±0.003</sub> |  63.09<sub>±00.08</sub> + 145.79<sub>±01.32</sub> |  14.72<sub>±00.42</sub> |
|                Nim/gcc | 0.293<sub>±0.001</sub> |   2.03<sub>±00.03</sub> + 584.95<sub>±04.18</sub> |  11.16<sub>±00.10</sub> |
|              Nim/clang | 0.301<sub>±0.001</sub> |   2.36<sub>±00.01</sub> + 579.63<sub>±03.36</sub> |  11.49<sub>±00.11</sub> |
|             Lua/luajit | 0.309<sub>±0.002</sub> |   2.71<sub>±00.06</sub> + 155.41<sub>±01.11</sub> |  12.79<sub>±00.36</sub> |
|            Python/pypy | 0.617<sub>±0.003</sub> |  62.12<sub>±00.05</sub> + 247.40<sub>±01.95</sub> |  25.07<sub>±00.34</sub> |
|                  Julia | 0.680<sub>±0.001</sub> | 312.61<sub>±00.26</sub> + 227.17<sub>±02.18</sub> |  25.34<sub>±00.31</sub> |
|                 Racket | 0.791<sub>±0.005</sub> | 114.28<sub>±00.14</sub> + 251.89<sub>±01.03</sub> |  31.12<sub>±00.36</sub> |
|       Ruby/truffleruby | 0.871<sub>±0.019</sub> | 231.12<sub>±02.61</sub> + 505.99<sub>±28.55</sub> |  59.37<sub>±01.37</sub> |
|                    Lua | 1.056<sub>±0.003</sub> |   2.49<sub>±00.01</sub> + 200.06<sub>±00.13</sub> |  44.34<sub>±00.33</sub> |
|           Ruby (--jit) | 1.118<sub>±0.005</sub> |  15.30<sub>±00.08</sub> + 184.61<sub>±00.73</sub> |  45.68<sub>±00.68</sub> |
| Ruby/truffleruby (JVM) | 1.219<sub>±0.055</sub> | 375.11<sub>±03.25</sub> + 365.16<sub>±31.23</sub> |  79.17<sub>±03.57</sub> |
|                 Python | 1.725<sub>±0.007</sub> |  12.03<sub>±00.08</sub> + 181.09<sub>±00.84</sub> |  72.90<sub>±00.73</sub> |
|                   Ruby | 1.794<sub>±0.022</sub> |  14.79<sub>±00.07</sub> + 180.84<sub>±00.18</sub> |  75.20<sub>±01.40</sub> |
|             Ruby/jruby | 3.094<sub>±0.053</sub> | 233.67<sub>±00.54</sub> + 877.14<sub>±01.42</sub> | 182.28<sub>±04.15</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux forky/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 10.0.302                        |
| C#/.NET Core     | 5.6.0-2.26329.109 (35b593bebfcba58f8e78298cef14c2761f5d86c6) |
| C3               | 0.8.2                           |
| Chez Scheme      | 10.0.0                          |
| Clojure          | "1.12.5"                        |
| Crystal          | 1.21.0                          |
| D/dmd            | v2.112.0                        |
| D/gdc            | 16.1.0                          |
| D/ldc2           | 1.42.0                          |
| Elixir           | 1.20.1                          |
| F#/.NET Core     | 15.2.302.0 for F# 10.0          |
| Go               | go1.26.5                        |
| Go/gccgo         | 16.1.0                          |
| Haskell          | 9.12.4                          |
| Idris 2          | 0.8.0                           |
| Java             | 26.0.1                          |
| Julia            | v"1.12.6"                       |
| Kotlin           | 2.4.10                          |
| Lua              | 5.5.0                           |
| Lua/luajit       | 2.1.1782832189                  |
| MLton            | 20241230                        |
| Nim              | 2.2.10                          |
| Node.js          | v26.5.0                         |
| OCaml            | 5.5.0                           |
| Odin             | dev-2026-07-nightly:819fdc7     |
| PHP              | 8.4.23                          |
| Perl             | v5.40.1                         |
| Python           | 3.14.6                          |
| Python/pypy      | 7.3.23-final0 for Python 3.11.15 |
| Racket           | "9.2"                           |
| Ruby             | 4.0.6p0                         |
| Ruby/jruby       | 10.1.0.0                        |
| Ruby/truffleruby | 34.0.1                          |
| Rust             | 1.97.1                          |
| Scala            | 3.8.4                           |
| Swift            | 6.3.3                           |
| Tcl              | 8.6                             |
| V                | 0.5.2 7647ce1                   |
| Vala             | 0.56.19                         |
| Zig              | 0.16.0                          |
| clang/clang++    | 22.1.8 (1+b1)                   |
| gcc/g++          | 16.1.0                          |

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
