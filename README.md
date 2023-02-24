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

UPDATE: 2022-12-10

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
|  Racket (Syntax Objects) |   1.297<sub>±0.000</sub> |   110.95<sub>±00.38</sub> + 0.00<sub>±00.00</sub> |     47.09<sub>±00.05</sub> |
|                  C++/g++ |   1.300<sub>±0.001</sub> |     1.84<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     51.96<sub>±00.05</sub> |
|                     Rust |   1.545<sub>±0.001</sub> |     0.94<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     61.73<sub>±00.09</sub> |
|                     Java |   1.651<sub>±0.000</sub> |    37.60<sub>±00.21</sub> + 1.55<sub>±00.33</sub> |     64.40<sub>±00.14</sub> |
|                    V/gcc |   1.654<sub>±0.000</sub> |     1.84<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     64.02<sub>±00.14</sub> |
|                   D/ldc2 |   1.657<sub>±0.000</sub> |     3.02<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     66.26<sub>±00.09</sub> |
|                    D/gdc |   1.661<sub>±0.000</sub> |     6.59<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     68.09<sub>±00.48</sub> |
|                    C/gcc |   1.667<sub>±0.000</sub> |     0.89<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     64.70<sub>±00.13</sub> |
|                  C/clang |   1.670<sub>±0.000</sub> |     0.89<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     65.33<sub>±00.38</sub> |
|               Kotlin/JVM |   1.683<sub>±0.009</sub> |    41.55<sub>±00.07</sub> + 0.43<sub>±00.09</sub> |     65.59<sub>±00.30</sub> |
|              C++/clang++ |   1.742<sub>±0.000</sub> |     1.61<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     67.71<sub>±00.22</sub> |
|                      Zig |   1.835<sub>±0.000</sub> |     0.91<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     71.17<sub>±00.13</sub> |
|                       Go |   1.905<sub>±0.000</sub> |     3.11<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     73.82<sub>±00.15</sub> |
|                    OCaml |   1.912<sub>±0.001</sub> |     2.87<sub>±00.02</sub> + 1.98<sub>±00.00</sub> |     85.40<sub>±00.82</sub> |
|              Chez Scheme |   1.933<sub>±0.000</sub> |    24.84<sub>±00.03</sub> + 4.34<sub>±00.06</sub> |     79.82<sub>±00.12</sub> |
|             F#/.NET Core |   1.952<sub>±0.000</sub> |   108.79<sub>±00.11</sub> + 0.60<sub>±00.00</sub> |     80.12<sub>±00.23</sub> |
|                  Nim/gcc |   1.962<sub>±0.001</sub> |     2.00<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     77.19<sub>±00.80</sub> |
|             C#/.NET Core |   1.979<sub>±0.000</sub> |   104.35<sub>±00.10</sub> + 0.21<sub>±00.00</sub> |     81.29<sub>±00.08</sub> |
|                   Racket |   2.016<sub>±0.013</sub> |    92.74<sub>±00.18</sub> + 0.00<sub>±00.00</sub> |     81.31<sub>±00.66</sub> |
|                 Vala/gcc |   2.090<sub>±0.001</sub> |     4.34<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     78.35<sub>±00.12</sub> |
|                Nim/clang |   2.111<sub>±0.000</sub> |     2.29<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     81.86<sub>±00.13</sub> |
|                 Go/gccgo |   2.176<sub>±0.000</sub> |    24.22<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     85.32<sub>±00.77</sub> |
|               Vala/clang |   2.266<sub>±0.000</sub> |     4.31<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     85.08<sub>±00.08</sub> |
|                  V/clang |   2.356<sub>±0.025</sub> |     1.88<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    101.86<sub>±01.18</sub> |
|                  Crystal |   2.363<sub>±0.000</sub> |     2.90<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     94.12<sub>±00.63</sub> |
|                    Julia |   2.581<sub>±0.006</sub> |   207.32<sub>±00.06</sub> + 0.65<sub>±00.02</sub> |     99.89<sub>±00.47</sub> |
|                    MLton |   2.608<sub>±0.000</sub> |     0.98<sub>±00.02</sub> + 0.92<sub>±00.08</sub> |    104.55<sub>±01.65</sub> |
|                  C#/Mono |   2.685<sub>±0.000</sub> |    24.98<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |    107.55<sub>±00.29</sub> |
|                    D/dmd |   3.218<sub>±0.001</sub> |     3.52<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    123.14<sub>±01.32</sub> |
|                    Scala |   3.228<sub>±0.010</sub> |  66.81<sub>±00.18</sub> + 166.50<sub>±25.97</sub> |    135.27<sub>±01.02</sub> |
|         Haskell (MArray) |   3.851<sub>±0.000</sub> |     4.90<sub>±01.05</sub> + 5.80<sub>±01.02</sub> |    154.33<sub>±00.11</sub> |
|             Haskell (FP) |   3.889<sub>±0.003</sub> |     5.92<sub>±00.05</sub> + 4.93<sub>±00.00</sub> |    165.24<sub>±00.42</sub> |
|                  Node.js |   4.150<sub>±0.002</sub> |    42.88<sub>±00.04</sub> + 0.31<sub>±00.00</sub> |    165.97<sub>±00.46</sub> |
|                    Swift |   5.450<sub>±0.000</sub> |    15.28<sub>±00.83</sub> + 0.00<sub>±00.00</sub> |    204.49<sub>±00.28</sub> |
| Ruby/truffleruby (--jvm) |   6.413<sub>±0.133</sub> | 409.74<sub>±02.52</sub> + 674.12<sub>±70.76</sub> |    309.38<sub>±09.75</sub> |
|               Lua/luajit |   6.491<sub>±0.016</sub> |     2.43<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    259.21<sub>±00.50</sub> |
|         Ruby/truffleruby |   7.043<sub>±0.033</sub> | 303.07<sub>±00.24</sub> + 359.89<sub>±06.85</sub> |    332.99<sub>±01.45</sub> |
|              Python/pypy |  12.191<sub>±0.015</sub> |   60.78<sub>±00.15</sub> + 30.13<sub>±00.02</sub> |    510.36<sub>±02.22</sub> |
|                    Idris |  15.762<sub>±0.045</sub> |    20.63<sub>±00.07</sub> + 8.82<sub>±00.06</sub> |    680.89<sub>±05.22</sub> |
|             Ruby (--jit) |  40.816<sub>±0.150</sub> |   270.58<sub>±00.01</sub> + 0.25<sub>±00.01</sub> |   1635.12<sub>±05.64</sub> |
|                   Elixir |  43.256<sub>±0.087</sub> |    71.45<sub>±01.13</sub> + 0.00<sub>±00.00</sub> |   1928.27<sub>±14.75</sub> |
|                      Lua |  50.611<sub>±0.046</sub> |     2.27<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   1955.56<sub>±02.08</sub> |
|                     Ruby |  87.260<sub>±1.288</sub> |    13.96<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   3609.87<sub>±41.40</sub> |
|               Ruby/jruby |  87.322<sub>±1.059</sub> | 189.47<sub>±01.28</sub> + 116.82<sub>±02.04</sub> |   3849.08<sub>±46.49</sub> |
|                   Python | 195.534<sub>±1.465</sub> |    10.41<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   7834.28<sub>±87.23</sub> |
|                 Tcl (FP) | 271.758<sub>±1.871</sub> |     3.81<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |  11395.20<sub>±79.30</sub> |
|                     Perl | 315.489<sub>±2.478</sub> |     7.14<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  12703.58<sub>±98.49</sub> |
|                Tcl (OOP) | 524.988<sub>±2.867</sub> |     3.96<sub>±00.02</sub> + 0.00<sub>±00.00</sub> | 22223.01<sub>±139.78</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                 Time, s |                                       Memory, MiB |                Energy, J |
| :----------------------- | ----------------------: | ------------------------------------------------: | -----------------------: |
|                  C++/g++ | 10.145<sub>±0.016</sub> |     1.83<sub>±00.02</sub> + 2.39<sub>±00.04</sub> |  410.27<sub>±02.84</sub> |
|                    C/gcc | 14.201<sub>±0.004</sub> |     0.89<sub>±00.02</sub> + 0.81<sub>±00.03</sub> |  557.98<sub>±02.04</sub> |
|  Racket (Syntax Objects) | 14.285<sub>±0.030</sub> |  112.31<sub>±00.49</sub> + 71.15<sub>±00.08</sub> |  566.11<sub>±01.26</sub> |
|               Kotlin/JVM | 14.315<sub>±0.027</sub> |    41.63<sub>±00.10</sub> + 1.19<sub>±00.17</sub> |  586.70<sub>±01.57</sub> |
|                   D/ldc2 | 14.920<sub>±0.020</sub> |     3.04<sub>±00.02</sub> + 0.84<sub>±00.04</sub> |  598.76<sub>±02.58</sub> |
|                      Zig | 15.007<sub>±0.008</sub> |     0.91<sub>±00.01</sub> + 1.41<sub>±00.00</sub> |  592.52<sub>±02.01</sub> |
|                  C/clang | 15.128<sub>±0.013</sub> |     0.89<sub>±00.02</sub> + 0.87<sub>±00.09</sub> |  647.86<sub>±05.03</sub> |
|                     Rust | 15.262<sub>±0.021</sub> |     0.91<sub>±00.01</sub> + 1.10<sub>±00.03</sub> |  598.93<sub>±02.53</sub> |
|                    D/gdc | 15.371<sub>±0.073</sub> |     6.57<sub>±00.02</sub> + 1.36<sub>±00.02</sub> |  644.91<sub>±02.18</sub> |
|              C++/clang++ | 15.573<sub>±0.006</sub> |     1.62<sub>±00.01</sub> + 1.98<sub>±00.05</sub> |  636.74<sub>±04.30</sub> |
|                    V/gcc | 15.903<sub>±0.340</sub> |     1.85<sub>±00.04</sub> + 1.19<sub>±00.01</sub> |  632.63<sub>±15.82</sub> |
|                  Crystal | 15.924<sub>±0.168</sub> |     2.91<sub>±00.03</sub> + 0.60<sub>±00.02</sub> |  663.38<sub>±11.25</sub> |
|             C#/.NET Core | 16.346<sub>±0.019</sub> |   104.49<sub>±00.05</sub> + 0.88<sub>±00.00</sub> |  682.67<sub>±04.50</sub> |
|                    Swift | 18.069<sub>±0.067</sub> |    16.12<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  727.04<sub>±03.39</sub> |
|                 Vala/gcc | 18.476<sub>±0.004</sub> |     4.22<sub>±00.04</sub> + 1.45<sub>±00.02</sub> |  691.94<sub>±00.47</sub> |
|                     Java | 18.659<sub>±0.023</sub> |    37.73<sub>±00.23</sub> + 2.05<sub>±00.31</sub> |  744.47<sub>±01.69</sub> |
|                       Go | 19.488<sub>±0.018</sub> |     3.07<sub>±00.04</sub> + 1.28<sub>±00.00</sub> |  730.68<sub>±00.89</sub> |
|                 Go/gccgo | 19.922<sub>±0.005</sub> |    24.25<sub>±00.12</sub> + 1.27<sub>±00.01</sub> |  819.93<sub>±00.81</sub> |
|               Vala/clang | 20.250<sub>±0.007</sub> |     4.26<sub>±00.03</sub> + 1.46<sub>±00.12</sub> |  783.48<sub>±00.44</sub> |
|                    Scala | 20.975<sub>±0.012</sub> |  67.25<sub>±00.16</sub> + 141.43<sub>±00.12</sub> |  881.11<sub>±01.30</sub> |
|                  Nim/gcc | 22.399<sub>±0.278</sub> |     2.00<sub>±00.02</sub> + 0.52<sub>±00.00</sub> |  901.24<sub>±14.68</sub> |
|                  V/clang | 23.011<sub>±0.086</sub> |     1.87<sub>±00.01</sub> + 1.18<sub>±00.00</sub> |  977.86<sub>±04.52</sub> |
|                Nim/clang | 24.833<sub>±0.482</sub> |     2.28<sub>±00.01</sub> + 0.51<sub>±00.00</sub> |  987.29<sub>±23.00</sub> |
|                    OCaml | 26.219<sub>±0.039</sub> |     3.45<sub>±00.02</sub> + 3.74<sub>±00.12</sub> | 1247.55<sub>±10.90</sub> |
|              Chez Scheme | 27.790<sub>±0.046</sub> |    25.31<sub>±00.03</sub> + 3.92<sub>±00.00</sub> | 1215.67<sub>±02.96</sub> |
|                  Node.js | 28.194<sub>±0.527</sub> |    42.91<sub>±00.05</sub> + 6.17<sub>±00.19</sub> | 1178.23<sub>±21.84</sub> |
|                    Julia | 29.584<sub>±0.036</sub> |   207.95<sub>±00.09</sub> + 0.49<sub>±00.01</sub> | 1126.40<sub>±06.02</sub> |
|                  C#/Mono | 31.521<sub>±0.047</sub> |    25.01<sub>±00.04</sub> + 0.82<sub>±00.00</sub> | 1337.07<sub>±04.01</sub> |
|             F#/.NET Core | 34.950<sub>±0.035</sub> |   108.86<sub>±00.10</sub> + 2.22<sub>±00.06</sub> | 1442.68<sub>±02.23</sub> |
|               Lua/luajit | 35.086<sub>±0.039</sub> |     2.44<sub>±00.03</sub> + 0.44<sub>±00.00</sub> | 1401.10<sub>±01.44</sub> |
|                   Racket | 36.847<sub>±0.435</sub> |    93.33<sub>±00.71</sub> + 0.25<sub>±00.08</sub> | 1621.75<sub>±16.77</sub> |
|         Haskell (MArray) | 37.327<sub>±0.011</sub> |     5.72<sub>±00.02</sub> + 6.11<sub>±00.00</sub> | 1520.44<sub>±07.50</sub> |
|                    D/dmd | 37.414<sub>±0.004</sub> |     3.50<sub>±00.05</sub> + 0.97<sub>±00.03</sub> | 1365.92<sub>±05.55</sub> |
|                    MLton | 44.858<sub>±0.031</sub> |     1.61<sub>±00.09</sub> + 4.11<sub>±00.00</sub> | 1924.75<sub>±13.80</sub> |
|              Python/pypy | 48.295<sub>±0.176</sub> |   60.86<sub>±00.03</sub> + 30.54<sub>±00.28</sub> | 2043.82<sub>±21.99</sub> |
| Ruby/truffleruby (--jvm) | 66.508<sub>±0.063</sub> | 408.76<sub>±03.32</sub> + 611.12<sub>±29.67</sub> | 2784.36<sub>±07.21</sub> |
|                    Idris | 67.460<sub>±0.094</sub> |    21.96<sub>±00.06</sub> + 9.56<sub>±00.02</sub> | 2915.79<sub>±10.83</sub> |
|             Haskell (FP) | 79.443<sub>±0.133</sub> |    5.82<sub>±00.04</sub> + 75.83<sub>±00.00</sub> | 3315.86<sub>±27.08</sub> |
|         Ruby/truffleruby | 81.328<sub>±0.292</sub> | 302.54<sub>±01.09</sub> + 263.13<sub>±20.03</sub> | 3816.09<sub>±42.42</sub> |


## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.099<sub>±0.000</sub> |     2.07<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   4.64<sub>±00.06</sub> |
|          C/clang (aklomp) |  0.099<sub>±0.001</sub> |     2.07<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4.59<sub>±00.07</sub> |
|                       PHP |  0.106<sub>±0.000</sub> |    17.31<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |   4.78<sub>±00.07</sub> |
|                      Rust |  0.970<sub>±0.000</sub> |     2.08<sub>±00.02</sub> + 0.21<sub>±00.03</sub> |  39.37<sub>±00.21</sub> |
|                   C/clang |  0.997<sub>±0.000</sub> |     2.00<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  36.74<sub>±00.21</sub> |
|                     C/gcc |  1.012<sub>±0.000</sub> |     2.07<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  37.18<sub>±00.48</sub> |
|                    D/ldc2 |  1.037<sub>±0.001</sub> |     3.40<sub>±00.27</sub> + 3.61<sub>±00.19</sub> |  43.16<sub>±00.52</sub> |
|                   Nim/gcc |  1.082<sub>±0.001</sub> |     1.68<sub>±00.05</sub> + 5.23<sub>±00.09</sub> |  42.43<sub>±00.42</sub> |
|                 Nim/clang |  1.095<sub>±0.001</sub> |     1.90<sub>±00.04</sub> + 5.27<sub>±00.07</sub> |  43.55<sub>±00.35</sub> |
|                   Crystal |  1.102<sub>±0.001</sub> |     3.49<sub>±00.04</sub> + 1.18<sub>±00.04</sub> |  44.99<sub>±00.15</sub> |
|                     V/gcc |  1.349<sub>±0.000</sub> |  2.39<sub>±00.03</sub> + 2376.18<sub>±00.26</sub> |  50.68<sub>±00.01</sub> |
|                   V/clang |  1.380<sub>±0.001</sub> |  2.38<sub>±00.01</sub> + 2376.79<sub>±00.86</sub> |  52.53<sub>±00.10</sub> |
|              Ruby (--jit) |  1.386<sub>±0.010</sub> |  271.00<sub>±00.04</sub> + 90.38<sub>±10.11</sub> |  54.11<sub>±00.59</sub> |
|                      Ruby |  1.388<sub>±0.001</sub> |   14.65<sub>±00.01</sub> + 58.72<sub>±00.08</sub> |  53.68<sub>±00.33</sub> |
|                      Java |  1.511<sub>±0.013</sub> |  39.32<sub>±00.04</sub> + 254.12<sub>±22.32</sub> |  62.27<sub>±00.93</sub> |
|                     Scala |  1.606<sub>±0.055</sub> |  64.90<sub>±00.28</sub> + 330.26<sub>±14.92</sub> |  68.84<sub>±01.17</sub> |
|                Kotlin/JVM |  1.636<sub>±0.028</sub> |  42.55<sub>±00.12</sub> + 249.05<sub>±04.14</sub> |  69.13<sub>±01.45</sub> |
|                  Vala/gcc |  1.643<sub>±0.001</sub> |     5.54<sub>±00.05</sub> + 0.13<sub>±00.02</sub> |  62.89<sub>±00.31</sub> |
|                Vala/clang |  1.644<sub>±0.000</sub> |     5.50<sub>±00.05</sub> + 0.15<sub>±00.02</sub> |  63.06<sub>±00.69</sub> |
|   C++/clang++ (libcrypto) |  1.711<sub>±0.002</sub> |     5.09<sub>±00.05</sub> + 0.71<sub>±00.03</sub> |  68.44<sub>±00.89</sub> |
|       C++/g++ (libcrypto) |  1.712<sub>±0.003</sub> |     5.70<sub>±00.10</sub> + 0.72<sub>±00.04</sub> |  68.35<sub>±00.55</sub> |
|                   Node.js |  1.724<sub>±0.003</sub> |   42.61<sub>±00.06</sub> + 43.70<sub>±00.10</sub> |  69.32<sub>±00.88</sub> |
|                        Go |  1.877<sub>±0.001</sub> |     4.25<sub>±00.03</sub> + 4.01<sub>±00.11</sub> |  80.54<sub>±00.28</sub> |
|       Perl (MIME::Base64) |  1.951<sub>±0.017</sub> |    14.90<sub>±00.05</sub> + 0.13<sub>±00.05</sub> |  78.31<sub>±00.82</sub> |
|              F#/.NET Core |  2.342<sub>±0.043</sub> |  109.31<sub>±00.06</sub> + 46.62<sub>±03.14</sub> |  87.36<sub>±00.83</sub> |
|                     D/gdc |  2.436<sub>±0.004</sub> |     7.55<sub>±00.04</sub> + 3.35<sub>±00.00</sub> | 106.92<sub>±00.60</sub> |
|              C#/.NET Core |  2.647<sub>±0.058</sub> |  104.54<sub>±00.13</sub> + 47.83<sub>±06.26</sub> |  96.55<sub>±02.21</sub> |
|                    Python |  2.711<sub>±0.002</sub> |    10.28<sub>±00.05</sub> + 0.60<sub>±00.01</sub> | 105.56<sub>±00.92</sub> |
|                       Zig |  2.951<sub>±0.000</sub> |     1.58<sub>±00.02</sub> + 0.00<sub>±00.00</sub> | 130.14<sub>±01.64</sub> |
|                     D/dmd |  3.026<sub>±0.001</sub> |     4.15<sub>±00.04</sub> + 3.35<sub>±00.00</sub> | 129.20<sub>±01.47</sub> |
|               Python/pypy |  3.582<sub>±0.002</sub> |   60.86<sub>±00.01</sub> + 31.50<sub>±00.15</sub> | 157.11<sub>±01.03</sub> |
|                       Tcl |  3.632<sub>±0.004</sub> |     5.10<sub>±00.02</sub> + 0.00<sub>±00.00</sub> | 146.59<sub>±01.31</sub> |
|                  Go/gccgo |  3.714<sub>±0.003</sub> |    25.25<sub>±00.16</sub> + 7.84<sub>±00.38</sub> | 166.93<sub>±00.68</sub> |
|                   C#/Mono |  4.712<sub>±0.019</sub> |   25.65<sub>±00.17</sub> + 18.70<sub>±00.02</sub> | 194.51<sub>±00.78</sub> |
|                     Julia |  5.051<sub>±0.005</sub> |  226.05<sub>±00.04</sub> + 42.61<sub>±00.43</sub> | 189.58<sub>±03.14</sub> |
|  Ruby/truffleruby (--jvm) |  5.136<sub>±0.010</sub> | 407.71<sub>±02.03</sub> + 211.28<sub>±15.72</sub> | 240.17<sub>±02.78</sub> |
|                Ruby/jruby | 10.708<sub>±0.121</sub> |  186.69<sub>±00.85</sub> + 79.64<sub>±00.52</sub> | 421.28<sub>±03.40</sub> |
| Perl (MIME::Base64::Perl) | 13.619<sub>±0.141</sub> |    16.21<sub>±00.08</sub> + 0.23<sub>±00.05</sub> | 572.56<sub>±03.68</sub> |
|          Ruby/truffleruby | 15.536<sub>±0.010</sub> | 302.86<sub>±00.68</sub> + 263.68<sub>±04.10</sub> | 649.08<sub>±08.68</sub> |

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
|    C++/clang++ (simdjson On-Demand) |  0.066<sub>±0.000</sub> |   112.52<sub>±00.02</sub> + 60.11<sub>±00.03</sub> |    2.71<sub>±00.01</sub> |
|        C++/g++ (simdjson On-Demand) |  0.066<sub>±0.000</sub> |   113.29<sub>±00.32</sub> + 60.11<sub>±00.29</sub> |    2.72<sub>±00.02</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.070<sub>±0.001</sub> |    112.38<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    2.95<sub>±00.03</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.085<sub>±0.000</sub> |    113.23<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    3.36<sub>±00.01</sub> |
|         C++/clang++ (DAW JSON Link) |  0.086<sub>±0.001</sub> |    112.41<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    3.61<sub>±00.05</sub> |
|             C++/g++ (DAW JSON Link) |  0.101<sub>±0.000</sub> |    113.23<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    4.14<sub>±00.02</sub> |
|                  Rust (Serde Typed) |  0.102<sub>±0.000</sub> |   111.62<sub>±00.07</sub> + 12.02<sub>±00.13</sub> |    4.29<sub>±00.03</sub> |
|                 Rust (Serde Custom) |  0.108<sub>±0.000</sub> |    111.35<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    4.45<sub>±00.02</sub> |
|                     C++/g++ (gason) |  0.133<sub>±0.000</sub> |   113.15<sub>±00.09</sub> + 96.96<sub>±00.04</sub> |    5.26<sub>±00.01</sub> |
|              C++/g++ (simdjson DOM) |  0.133<sub>±0.000</sub> |  113.27<sub>±00.32</sub> + 176.90<sub>±00.30</sub> |    5.70<sub>±00.02</sub> |
|                 C++/clang++ (gason) |  0.144<sub>±0.000</sub> |   112.35<sub>±00.06</sub> + 96.97<sub>±00.00</sub> |    5.82<sub>±00.01</sub> |
|          C++/clang++ (simdjson DOM) |  0.144<sub>±0.000</sub> |  112.47<sub>±00.04</sub> + 177.15<sub>±00.00</sub> |    6.15<sub>±00.02</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.145<sub>±0.000</sub> |   112.78<sub>±00.05</sub> + 61.27<sub>±00.03</sub> |    5.98<sub>±00.03</sub> |
|                 C++/g++ (RapidJSON) |  0.168<sub>±0.000</sub> |  113.23<sub>±00.05</sub> + 122.23<sub>±00.59</sub> |    7.01<sub>±00.07</sub> |
|              Scala (jsoniter-scala) |  0.175<sub>±0.001</sub> |   288.19<sub>±00.12</sub> + 25.08<sub>±00.53</sub> |    9.59<sub>±00.05</sub> |
|             C++/clang++ (RapidJSON) |  0.230<sub>±0.000</sub> |  112.41<sub>±00.07</sub> + 128.97<sub>±00.03</sub> |    9.57<sub>±00.04</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.236<sub>±0.000</sub> |   112.89<sub>±00.02</sub> + 80.70<sub>±00.00</sub> |    9.85<sub>±00.07</sub> |
|                          Go (Sonic) |  0.237<sub>±0.002</sub> |  119.77<sub>±00.09</sub> + 111.84<sub>±00.06</sub> |   10.21<sub>±00.10</sub> |
|         C++/g++ (RapidJSON Precise) |  0.243<sub>±0.001</sub> |  113.23<sub>±00.07</sub> + 128.88<sub>±00.06</sub> |   10.27<sub>±00.09</sub> |
|                  Go (goccy/go-json) |  0.271<sub>±0.000</sub> |  115.83<sub>±00.09</sub> + 112.15<sub>±00.19</sub> |   10.63<sub>±00.02</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.313<sub>±0.000</sub> |  112.41<sub>±00.01</sub> + 129.00<sub>±00.00</sub> |   13.43<sub>±00.08</sub> |
|                      C/clang (yajl) |  0.368<sub>±0.000</sub> |    110.86<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   16.11<sub>±00.17</sub> |
|                        C/gcc (yajl) |  0.371<sub>±0.001</sub> |    110.87<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   15.98<sub>±00.04</sub> |
|                                 Zig |  0.387<sub>±0.001</sub> |   110.92<sub>±00.02</sub> + 12.18<sub>±00.00</sub> |   19.13<sub>±00.06</sub> |
|                   Nim/clang (jsony) |  0.401<sub>±0.000</sub> |   111.40<sub>±00.01</sub> + 42.74<sub>±00.01</sub> |   17.16<sub>±00.05</sub> |
|                C++/g++ (Boost.JSON) |  0.401<sub>±0.000</sub> |  113.30<sub>±00.05</sub> + 435.95<sub>±00.07</sub> |   16.68<sub>±00.06</sub> |
|             C++/g++ (RapidJSON SAX) |  0.404<sub>±0.001</sub> |    113.02<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   16.62<sub>±00.05</sub> |
|                     Nim/gcc (jsony) |  0.408<sub>±0.001</sub> |   111.08<sub>±00.03</sub> + 42.77<sub>±00.04</sub> |   17.46<sub>±00.08</sub> |
|            C++/clang++ (Boost.JSON) |  0.421<sub>±0.001</sub> |  112.42<sub>±00.04</sub> + 436.31<sub>±00.00</sub> |   17.73<sub>±00.11</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.451<sub>±0.001</sub> |    113.02<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   19.37<sub>±00.14</sub> |
|                       Go (jsoniter) |  0.517<sub>±0.003</sub> |    230.90<sub>±00.03</sub> + 1.35<sub>±00.18</sub> |   21.68<sub>±00.15</sub> |
|                             Node.js |  0.552<sub>±0.002</sub> |  152.51<sub>±00.05</sub> + 187.99<sub>±00.47</sub> |   25.65<sub>±00.06</sub> |
|     C#/.NET Core (System.Text.Json) |  0.569<sub>±0.001</sub> |  561.19<sub>±00.19</sub> + 139.73<sub>±00.01</sub> |   25.20<sub>±00.08</sub> |
|                Rust (Serde Untyped) |  0.578<sub>±0.001</sub> |  111.60<sub>±00.03</sub> + 839.97<sub>±00.00</sub> |   23.67<sub>±00.11</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.600<sub>±0.000</sub> |    194.64<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   23.95<sub>±00.06</sub> |
|                     Java (DSL-JSON) |  0.603<sub>±0.024</sub> |  259.68<sub>±00.36</sub> + 190.97<sub>±02.88</sub> |   30.71<sub>±00.70</sub> |
|                             V/clang |  0.631<sub>±0.000</sub> |  111.45<sub>±00.03</sub> + 496.21<sub>±00.00</sub> |   26.92<sub>±00.13</sub> |
|                               V/gcc |  0.637<sub>±0.001</sub> |  111.44<sub>±00.04</sub> + 496.21<sub>±00.03</sub> |   26.60<sub>±00.16</sub> |
|                      Crystal (Pull) |  0.655<sub>±0.004</sub> |   113.07<sub>±00.01</sub> + 18.41<sub>±00.03</sub> |   28.93<sub>±00.44</sub> |
|                    Crystal (Schema) |  0.676<sub>±0.003</sub> |   113.04<sub>±00.04</sub> + 48.84<sub>±00.04</sub> |   29.69<sub>±00.38</sub> |
|                         Python/pypy |  0.688<sub>±0.004</sub> |  280.92<sub>±00.08</sub> + 125.74<sub>±00.04</sub> |   29.52<sub>±00.25</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.736<sub>±0.002</sub> |    194.60<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   30.11<sub>±00.22</sub> |
|                       Julia (JSON3) |  0.765<sub>±0.003</sub> |  443.49<sub>±00.12</sub> + 252.04<sub>±02.56</sub> |   31.12<sub>±00.16</sub> |
|             Perl (Cpanel::JSON::XS) |  0.778<sub>±0.005</sub> |  125.51<sub>±00.07</sub> + 402.87<sub>±00.01</sub> |   32.36<sub>±00.19</sub> |
|                                  Go |  0.856<sub>±0.000</sub> |   117.05<sub>±00.14</sub> + 79.91<sub>±00.13</sub> |   34.96<sub>±00.15</sub> |
|                                 PHP |  0.958<sub>±0.002</sub> |  126.54<sub>±00.05</sub> + 682.21<sub>±00.00</sub> |   39.86<sub>±00.14</sub> |
|                             Crystal |  0.966<sub>±0.002</sub> |  113.04<sub>±00.02</sub> + 392.50<sub>±00.02</sub> |   41.41<sub>±00.39</sub> |
|              Nim/clang (Packedjson) |  0.976<sub>±0.001</sub> |  112.15<sub>±00.01</sub> + 294.42<sub>±00.00</sub> |   40.29<sub>±00.34</sub> |
|                Nim/gcc (Packedjson) |  0.995<sub>±0.001</sub> |  111.83<sub>±00.02</sub> + 294.42<sub>±00.00</sub> |   41.30<sub>±00.28</sub> |
|                        C#/.NET Core |  1.063<sub>±0.007</sub> |  569.35<sub>±00.12</sub> + 295.46<sub>±00.14</sub> |   47.16<sub>±00.46</sub> |
|                    C++/g++ (json-c) |  1.176<sub>±0.005</sub> | 113.39<sub>±00.08</sub> + 1216.08<sub>±00.00</sub> |   48.91<sub>±00.17</sub> |
|                C++/clang++ (json-c) |  1.181<sub>±0.007</sub> | 112.68<sub>±00.06</sub> + 1216.08<sub>±00.00</sub> |   49.16<sub>±00.56</sub> |
|                             Clojure |  1.215<sub>±0.015</sub> |  460.72<sub>±06.56</sub> + 546.03<sub>±53.52</sub> |   64.32<sub>±00.42</sub> |
|                            Go/gccgo |  1.269<sub>±0.002</sub> |   138.83<sub>±00.11</sub> + 83.59<sub>±00.17</sub> |   53.56<sub>±00.42</sub> |
|              C++/clang++ (Nlohmann) |  1.272<sub>±0.007</sub> |  112.52<sub>±00.07</sub> + 360.17<sub>±00.03</sub> |   54.07<sub>±00.55</sub> |
|                             Nim/gcc |  1.336<sub>±0.002</sub> |  111.83<sub>±00.03</sub> + 919.88<sub>±00.00</sub> |   55.94<sub>±00.16</sub> |
|                           Nim/clang |  1.344<sub>±0.002</sub> |  112.15<sub>±00.03</sub> + 925.03<sub>±00.00</sub> |   55.50<sub>±00.20</sub> |
|                 CPython (UltraJSON) |  1.444<sub>±0.002</sub> |  122.17<sub>±00.08</sub> + 544.51<sub>±01.81</sub> |   53.21<sub>±00.20</sub> |
|                                Ruby |  1.449<sub>±0.008</sub> |  123.97<sub>±00.04</sub> + 263.13<sub>±00.02</sub> |   60.95<sub>±00.39</sub> |
|                        Ruby (--jit) |  1.449<sub>±0.006</sub> |  380.63<sub>±00.03</sub> + 263.24<sub>±00.01</sub> |   61.11<sub>±00.37</sub> |
|                  C++/g++ (Nlohmann) |  1.507<sub>±0.002</sub> |  113.38<sub>±00.05</sub> + 448.03<sub>±00.03</sub> |   61.19<sub>±00.43</sub> |
|                              Python |  1.525<sub>±0.003</sub> |  120.25<sub>±00.10</sub> + 375.17<sub>±00.02</sub> |   59.36<sub>±00.18</sub> |
|     F#/.NET Core (System.Text.Json) |  1.601<sub>±0.004</sub> |  570.59<sub>±00.09</sub> + 232.18<sub>±06.44</sub> |   69.79<sub>±00.32</sub> |
|                         Ruby (YAJL) |  1.965<sub>±0.005</sub> |  123.81<sub>±00.04</sub> + 282.68<sub>±00.01</sub> |   82.27<sub>±00.88</sub> |
|                              D/ldc2 |  2.082<sub>±0.001</sub> |  112.86<sub>±00.06</sub> + 680.20<sub>±00.05</sub> |   86.06<sub>±00.50</sub> |
|                             C#/Mono |  2.312<sub>±0.040</sub> |   252.46<sub>±00.07</sub> + 31.53<sub>±00.01</sub> |   96.95<sub>±01.22</sub> |
|                             Haskell |  2.539<sub>±0.005</sub> |  116.59<sub>±00.75</sub> + 697.45<sub>±00.73</sub> |  107.60<sub>±01.21</sub> |
|                           Rust (jq) |  2.640<sub>±0.002</sub> |  113.50<sub>±00.04</sub> + 778.25<sub>±01.42</sub> |  110.56<sub>±01.02</sub> |
|                          Ruby/jruby |  2.930<sub>±0.031</sub> |  461.99<sub>±01.62</sub> + 841.93<sub>±75.18</sub> |  148.36<sub>±04.17</sub> |
|    C++/clang++ (Boost.PropertyTree) |  3.166<sub>±0.007</sub> | 194.88<sub>±00.04</sub> + 1232.84<sub>±00.00</sub> |  130.92<sub>±01.68</sub> |
|        C++/g++ (Boost.PropertyTree) |  3.336<sub>±0.004</sub> | 113.22<sub>±00.05</sub> + 1440.12<sub>±00.03</sub> |  139.30<sub>±00.49</sub> |
|                            Vala/gcc |  3.613<sub>±0.014</sub> |  114.79<sub>±00.03</sub> + 940.47<sub>±00.05</sub> |  154.92<sub>±00.77</sub> |
|                          Vala/clang |  3.633<sub>±0.006</sub> |  114.81<sub>±00.05</sub> + 940.45<sub>±00.03</sub> |  156.14<sub>±00.64</sub> |
|                               D/gdc |  3.660<sub>±0.004</sub> |  116.67<sub>±00.04</sub> + 681.24<sub>±00.12</sub> |  156.47<sub>±00.70</sub> |
|                               D/dmd |  4.514<sub>±0.003</sub> |  113.37<sub>±00.04</sub> + 680.34<sub>±00.05</sub> |  177.98<sub>±00.69</sub> |
|                   Perl (JSON::Tiny) |  9.382<sub>±0.067</sub> |  125.86<sub>±00.04</sub> + 528.91<sub>±00.03</sub> |  404.50<sub>±03.82</sub> |
|            Ruby/truffleruby (--jvm) | 12.311<sub>±0.034</sub> | 505.03<sub>±05.57</sub> + 2467.82<sub>±54.26</sub> |  747.19<sub>±05.43</sub> |
|                    Ruby/truffleruby | 22.970<sub>±0.503</sub> | 451.72<sub>±03.63</sub> + 2314.87<sub>±07.88</sub> | 1119.70<sub>±23.10</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.043<sub>±0.000</sub> |    6.06<sub>±00.02</sub> + 57.94<sub>±00.12</sub> |      4.41<sub>±00.02</sub> |
|      V/gcc (VSL + CBLAS) |    0.047<sub>±0.000</sub> |    6.96<sub>±00.02</sub> + 58.03<sub>±00.00</sub> |      4.55<sub>±00.04</sub> |
|    V/clang (VSL + CBLAS) |    0.047<sub>±0.000</sub> |    7.03<sub>±00.05</sub> + 57.93<sub>±00.05</sub> |      4.57<sub>±00.02</sub> |
|    Nim/gcc (Arraymancer) |    0.066<sub>±0.004</sub> |    4.91<sub>±00.04</sub> + 57.94<sub>±00.17</sub> |      5.47<sub>±00.28</sub> |
|           Python (NumPy) |    0.067<sub>±0.000</sub> |   30.07<sub>±00.04</sub> + 58.18<sub>±00.05</sub> |      6.27<sub>±00.06</sub> |
|  Nim/clang (Arraymancer) |    0.069<sub>±0.002</sub> |    6.24<sub>±00.10</sub> + 57.58<sub>±00.12</sub> |      6.02<sub>±00.48</sub> |
|          C++/g++ (Eigen) |    0.072<sub>±0.002</sub> |   31.90<sub>±06.49</sub> + 58.05<sub>±06.57</sub> |      5.39<sub>±00.10</sub> |
|      C++/clang++ (Eigen) |    0.072<sub>±0.003</sub> |   36.08<sub>±05.02</sub> + 54.22<sub>±05.03</sub> |      5.90<sub>±00.13</sub> |
|              Java (ND4J) |    0.079<sub>±0.002</sub> |  106.62<sub>±01.24</sub> + 91.92<sub>±00.12</sub> |      6.18<sub>±00.16</sub> |
|           Rust (ndarray) |    0.089<sub>±0.001</sub> |    2.39<sub>±00.05</sub> + 68.47<sub>±00.00</sub> |      6.05<sub>±00.11</sub> |
|       Julia (threads: 2) |    0.137<sub>±0.000</sub> |  237.36<sub>±00.09</sub> + 52.34<sub>±00.39</sub> |      7.19<sub>±00.05</sub> |
|       Julia (threads: 1) |    0.187<sub>±0.000</sub> |  237.13<sub>±00.04</sub> + 52.82<sub>±00.46</sub> |      8.46<sub>±00.04</sub> |
|            V/clang (VSL) |    0.242<sub>±0.005</sub> |    7.04<sub>±00.09</sub> + 51.83<sub>±00.00</sub> |     17.31<sub>±00.37</sub> |
|              V/gcc (VSL) |    0.477<sub>±0.001</sub> |    7.06<sub>±00.04</sub> + 51.82<sub>±00.00</sub> |     32.25<sub>±00.08</sub> |
|          Julia (no BLAS) |    1.052<sub>±0.021</sub> |  224.75<sub>±00.14</sub> + 51.52<sub>±00.02</sub> |     46.61<sub>±00.88</sub> |
|                   D/ldc2 |    1.718<sub>±0.003</sub> |    3.27<sub>±00.02</sub> + 70.46<sub>±00.03</sub> |     63.13<sub>±00.15</sub> |
|                    D/gdc |    1.870<sub>±0.002</sub> |    6.83<sub>±00.03</sub> + 70.88<sub>±00.07</sub> |     73.06<sub>±00.14</sub> |
|                    D/dmd |    1.874<sub>±0.001</sub> |    3.58<sub>±00.03</sub> + 70.48<sub>±00.02</sub> |     71.02<sub>±00.09</sub> |
|                    C/gcc |    3.029<sub>±0.000</sub> |    1.47<sub>±00.03</sub> + 68.70<sub>±00.03</sub> |    109.60<sub>±00.60</sub> |
|                    V/gcc |    3.038<sub>±0.001</sub> |    2.13<sub>±00.03</sub> + 69.01<sub>±00.00</sub> |    112.35<sub>±00.44</sub> |
|                  V/clang |    3.060<sub>±0.000</sub> |    2.43<sub>±00.02</sub> + 68.95<sub>±00.03</sub> |    104.62<sub>±00.04</sub> |
|               Vala/clang |    3.061<sub>±0.000</sub> |    3.93<sub>±00.08</sub> + 69.80<sub>±00.11</sub> |    104.74<sub>±00.17</sub> |
|                  C/clang |    3.062<sub>±0.000</sub> |    1.46<sub>±00.02</sub> + 68.73<sub>±00.05</sub> |    104.49<sub>±00.11</sub> |
|                     Rust |    3.065<sub>±0.000</sub> |    2.11<sub>±00.04</sub> + 68.57<sub>±00.00</sub> |    105.21<sub>±00.14</sub> |
|                      Zig |    3.074<sub>±0.001</sub> |    1.50<sub>±00.07</sub> + 68.89<sub>±00.03</sub> |    108.77<sub>±00.55</sub> |
|                  Nim/gcc |    3.090<sub>±0.000</sub> |    2.51<sub>±00.02</sub> + 66.26<sub>±00.00</sub> |    111.06<sub>±00.60</sub> |
|                     Java |    3.093<sub>±0.004</sub> |   39.19<sub>±00.08</sub> + 68.43<sub>±00.16</sub> |    109.77<sub>±01.88</sub> |
|                    Swift |    3.095<sub>±0.000</sub> |    7.30<sub>±00.04</sub> + 68.92<sub>±00.02</sub> |    109.29<sub>±00.43</sub> |
|                Nim/clang |    3.122<sub>±0.001</sub> |    2.82<sub>±00.03</sub> + 66.00<sub>±00.00</sub> |    106.98<sub>±00.17</sub> |
|                 Vala/gcc |    3.126<sub>±0.001</sub> |    3.89<sub>±00.02</sub> + 69.86<sub>±00.02</sub> |    114.22<sub>±00.23</sub> |
|                       Go |    3.153<sub>±0.001</sub> |    3.85<sub>±00.04</sub> + 73.12<sub>±00.11</sub> |    113.47<sub>±00.19</sub> |
|                 Go/gccgo |    3.160<sub>±0.001</sub> |   24.62<sub>±00.01</sub> + 73.50<sub>±00.10</sub> |    112.00<sub>±00.14</sub> |
|                  Crystal |    3.161<sub>±0.000</sub> |    3.44<sub>±00.04</sub> + 59.97<sub>±00.04</sub> |    115.46<sub>±00.14</sub> |
|                  Node.js |    3.238<sub>±0.003</sub> |   47.55<sub>±00.19</sub> + 76.79<sub>±00.11</sub> |    130.43<sub>±00.45</sub> |
|              Python/pypy |    3.274<sub>±0.001</sub> |   61.70<sub>±00.04</sub> + 68.69<sub>±00.06</sub> |    136.27<sub>±00.85</sub> |
|                    Scala |    3.325<sub>±0.008</sub> |  65.60<sub>±00.29</sub> + 140.85<sub>±00.30</sub> |    121.60<sub>±00.97</sub> |
|               Kotlin/JVM |    3.664<sub>±0.003</sub> |   40.44<sub>±00.17</sub> + 68.06<sub>±00.29</sub> |    152.07<sub>±01.68</sub> |
|             C#/.NET Core |    4.384<sub>±0.001</sub> |  106.09<sub>±00.16</sub> + 69.06<sub>±00.00</sub> |    175.92<sub>±00.95</sub> |
|                  C#/Mono |    7.414<sub>±0.001</sub> |   25.41<sub>±00.05</sub> + 69.60<sub>±00.04</sub> |    299.70<sub>±01.56</sub> |
|         Ruby/truffleruby |   17.828<sub>±2.987</sub> | 380.74<sub>±06.95</sub> + 518.10<sub>±27.76</sub> |    660.11<sub>±94.70</sub> |
| Ruby/truffleruby (--jvm) |   28.869<sub>±0.417</sub> | 552.53<sub>±21.56</sub> + 367.72<sub>±55.70</sub> |    994.44<sub>±18.01</sub> |
|                     Ruby |  198.042<sub>±0.817</sub> |   14.98<sub>±00.04</sub> + 69.05<sub>±00.01</sub> |   8578.84<sub>±83.38</sub> |
|             Ruby (--jit) |  198.618<sub>±1.593</sub> |  271.64<sub>±00.04</sub> + 72.66<sub>±00.01</sub> |   8637.61<sub>±80.58</sub> |
|                     Perl |  225.366<sub>±0.521</sub> |   9.57<sub>±00.03</sub> + 599.64<sub>±00.07</sub> |   9143.92<sub>±52.78</sub> |
|                   Python |  278.785<sub>±0.562</sub> |   10.71<sub>±00.27</sub> + 68.87<sub>±00.27</sub> |  11177.48<sub>±38.78</sub> |
|                      Tcl |  335.868<sub>±4.545</sub> |   7.32<sub>±00.05</sub> + 400.18<sub>±00.00</sub> | 14014.34<sub>±226.31</sub> |
|               Ruby/jruby | 399.505<sub>±23.779</sub> | 268.81<sub>±01.64</sub> + 756.38<sub>±38.36</sub> | 16344.90<sub>±758.83</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

Notes:

 - All languages but V and Python use unordered hashmaps (V and Python don't provide those out of box, and
 their hashmaps use keys in the insertion order);
 - The results are always sorted (could be unstable or stable though).

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                  C++/g++ | 0.071<sub>±0.000</sub> |    3.68<sub>±00.04</sub> + 77.91<sub>±00.26</sub> |   2.71<sub>±00.02</sub> |
|                      Zig | 0.072<sub>±0.000</sub> |    0.91<sub>±00.02</sub> + 57.81<sub>±00.03</sub> |   2.97<sub>±00.03</sub> |
|              C++/clang++ | 0.072<sub>±0.000</sub> |    1.65<sub>±00.05</sub> + 61.74<sub>±00.35</sub> |   2.70<sub>±00.02</sub> |
|                  V/clang | 0.106<sub>±0.000</sub> |   1.91<sub>±00.04</sub> + 212.02<sub>±00.30</sub> |   4.25<sub>±00.01</sub> |
|                    V/gcc | 0.107<sub>±0.000</sub> |   1.87<sub>±00.05</sub> + 208.57<sub>±00.33</sub> |   4.35<sub>±00.03</sub> |
|                     Rust | 0.115<sub>±0.000</sub> |    0.94<sub>±00.01</sub> + 74.00<sub>±00.01</sub> |   4.47<sub>±00.06</sub> |
|                  Crystal | 0.151<sub>±0.000</sub> |    2.88<sub>±00.06</sub> + 89.06<sub>±00.05</sub> |   6.01<sub>±00.03</sub> |
|                     Java | 0.164<sub>±0.002</sub> |  38.52<sub>±00.14</sub> + 153.60<sub>±05.25</sub> |   9.25<sub>±00.20</sub> |
|                  Node.js | 0.225<sub>±0.002</sub> |  42.34<sub>±00.13</sub> + 150.08<sub>±00.35</sub> |  11.26<sub>±00.08</sub> |
|                Nim/clang | 0.298<sub>±0.000</sub> |   1.99<sub>±00.03</sub> + 598.51<sub>±01.16</sub> |  11.77<sub>±00.05</sub> |
|                    Scala | 0.299<sub>±0.006</sub> |  67.35<sub>±00.22</sub> + 148.34<sub>±04.80</sub> |  15.31<sub>±00.13</sub> |
|                  Nim/gcc | 0.324<sub>±0.031</sub> |   1.69<sub>±00.02</sub> + 599.54<sub>±15.34</sub> |  12.39<sub>±01.06</sub> |
|               Lua/luajit | 0.338<sub>±0.002</sub> |   1.21<sub>±00.03</sub> + 157.08<sub>±00.75</sub> |  13.26<sub>±00.21</sub> |
|                    Julia | 0.593<sub>±0.000</sub> | 226.44<sub>±00.06</sub> + 376.93<sub>±00.44</sub> |  23.15<sub>±00.20</sub> |
|              Python/pypy | 0.830<sub>±0.002</sub> |  59.75<sub>±00.10</sub> + 251.87<sub>±00.06</sub> |  32.55<sub>±00.20</sub> |
| Ruby/truffleruby (--jvm) | 1.221<sub>±0.033</sub> | 409.10<sub>±02.87</sub> + 542.80<sub>±45.51</sub> |  79.93<sub>±02.22</sub> |
|             Ruby (--jit) | 1.442<sub>±0.004</sub> | 270.68<sub>±00.04</sub> + 146.96<sub>±00.01</sub> |  58.58<sub>±00.76</sub> |
|                      Lua | 1.493<sub>±0.004</sub> |   2.28<sub>±00.05</sub> + 284.23<sub>±00.90</sub> |  58.17<sub>±00.36</sub> |
|         Ruby/truffleruby | 1.657<sub>±0.050</sub> | 299.73<sub>±00.64</sub> + 496.36<sub>±29.59</sub> |  92.44<sub>±02.52</sub> |
|                     Ruby | 2.063<sub>±0.011</sub> |  13.96<sub>±00.03</sub> + 146.97<sub>±00.02</sub> |  83.40<sub>±00.73</sub> |
|               Ruby/jruby | 2.143<sub>±0.020</sub> | 187.79<sub>±00.61</sub> + 545.57<sub>±11.80</sub> | 111.13<sub>±02.43</sub> |
|                   Python | 4.984<sub>±0.041</sub> |  10.42<sub>±00.03</sub> + 234.84<sub>±00.78</sub> | 194.67<sub>±02.44</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 7.0.100                         |
| C#/.NET Core     | 4.4.0-4.22520.11 (9e075f03)     |
| C#/Mono          | 6.8.0.105                       |
| Chez Scheme      | 9.5.8                           |
| Clojure          | "1.11.1"                        |
| Crystal          | 1.6.2                           |
| D/dmd            | v2.101.0                        |
| D/gdc            | 12.2.0                          |
| D/ldc2           | 1.30.0                          |
| Elixir           | 1.14.0                          |
| F#/.NET Core     | 12.4.0.0 for F# 7.0             |
| Go               | go1.19.4                        |
| Go/gccgo         | 12.2.0                          |
| Haskell          | 9.4.3                           |
| Idris 2          | 0.6.0                           |
| Java             | 19.0.1                          |
| Julia            | v"1.8.3"                        |
| Kotlin           | 1.7.21                          |
| Lua              | 5.4.4                           |
| Lua/luajit       | 2.1.0-beta3                     |
| MLton            | 20210117                        |
| Nim              | 1.6.10                          |
| Node.js          | v19.2.0                         |
| OCaml            | 4.14.0                          |
| PHP              | 8.1.12                          |
| Perl             | v5.36.0                         |
| Python           | 3.10.8                          |
| Python/pypy      | 7.3.10-final0 for Python 3.9.15 |
| Racket           | "8.7"                           |
| Ruby             | 3.1.3p185                       |
| Ruby/jruby       | 9.4.0.0                         |
| Ruby/truffleruby | 22.3.0                          |
| Rust             | 1.65.0                          |
| Scala            | 3.2.1                           |
| Swift            | 5.7.1                           |
| Tcl              | 8.6                             |
| V                | 0.3.2 d62fc77                   |
| Vala             | 0.56.3                          |
| Zig              | 0.10.0                          |
| clang/clang++    | 14.0.6                          |
| gcc/g++          | 12.2.0                          |

## Using Docker

Build the image:

    $ docker build docker/ -t benchmarks

The pinned packages could be missing, in that case please update them with:

    $ ./run.sh update_apt

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
