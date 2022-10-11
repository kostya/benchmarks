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

UPDATE: 2022-09-23

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
|  Racket (Syntax Objects) |   1.293<sub>±0.001</sub> |   111.65<sub>±00.71</sub> + 0.00<sub>±00.00</sub> |     46.98<sub>±00.02</sub> |
|                  C++/g++ |   1.317<sub>±0.000</sub> |     1.82<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     52.72<sub>±00.17</sub> |
|                     Rust |   1.543<sub>±0.000</sub> |     0.93<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     61.72<sub>±00.13</sub> |
|                     Java |   1.653<sub>±0.003</sub> |    38.82<sub>±00.08</sub> + 0.52<sub>±00.07</sub> |     64.26<sub>±00.17</sub> |
|                   D/ldc2 |   1.659<sub>±0.002</sub> |     3.01<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     66.27<sub>±00.17</sub> |
|                    D/gdc |   1.661<sub>±0.000</sub> |     6.55<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     68.36<sub>±00.15</sub> |
|                    C/gcc |   1.667<sub>±0.000</sub> |     0.88<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     64.53<sub>±00.04</sub> |
|                  C/clang |   1.670<sub>±0.001</sub> |     0.89<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     64.75<sub>±00.07</sub> |
|               Kotlin/JVM |   1.685<sub>±0.004</sub> |    41.48<sub>±00.18</sub> + 0.52<sub>±00.16</sub> |     65.51<sub>±00.18</sub> |
|              C++/clang++ |   1.738<sub>±0.000</sub> |     1.65<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     67.30<sub>±00.08</sub> |
|                      Zig |   1.747<sub>±0.001</sub> |     0.92<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     68.37<sub>±00.55</sub> |
|             C#/.NET Core |   1.902<sub>±0.001</sub> |    33.90<sub>±00.25</sub> + 0.00<sub>±00.00</sub> |     76.15<sub>±00.25</sub> |
|                       Go |   1.905<sub>±0.000</sub> |     2.98<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     73.64<sub>±00.17</sub> |
|                    OCaml |   1.911<sub>±0.000</sub> |     2.94<sub>±00.02</sub> + 1.98<sub>±00.00</sub> |     86.49<sub>±01.35</sub> |
|              Chez Scheme |   1.922<sub>±0.001</sub> |    24.78<sub>±00.04</sub> + 4.44<sub>±00.04</sub> |     79.39<sub>±00.12</sub> |
|             F#/.NET Core |   1.947<sub>±0.000</sub> |    37.85<sub>±00.03</sub> + 0.56<sub>±00.00</sub> |     77.86<sub>±00.20</sub> |
|                  Nim/gcc |   1.962<sub>±0.000</sub> |     2.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     76.63<sub>±00.69</sub> |
|                   Racket |   1.986<sub>±0.019</sub> |    92.88<sub>±00.44</sub> + 0.00<sub>±00.00</sub> |     80.21<sub>±01.03</sub> |
|                 Vala/gcc |   2.090<sub>±0.000</sub> |     4.24<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |     78.05<sub>±00.32</sub> |
|                Nim/clang |   2.112<sub>±0.001</sub> |     2.31<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     82.20<sub>±00.51</sub> |
|                 Go/gccgo |   2.176<sub>±0.001</sub> |    24.42<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     85.13<sub>±00.93</sub> |
|               Vala/clang |   2.266<sub>±0.000</sub> |     4.32<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     84.95<sub>±00.07</sub> |
|                    V/gcc |   2.277<sub>±0.000</sub> |     1.84<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     85.30<sub>±00.16</sub> |
|                  Crystal |   2.389<sub>±0.003</sub> |     3.00<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     95.36<sub>±00.43</sub> |
|                    Julia |   2.594<sub>±0.010</sub> |   224.45<sub>±00.16</sub> + 0.82<sub>±00.05</sub> |    101.61<sub>±00.91</sub> |
|                    MLton |   2.608<sub>±0.000</sub> |     0.96<sub>±00.02</sub> + 0.90<sub>±00.11</sub> |    102.94<sub>±00.56</sub> |
|                  C#/Mono |   2.686<sub>±0.001</sub> |    24.89<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    107.21<sub>±00.13</sub> |
|                  V/clang |   2.735<sub>±0.002</sub> |     1.84<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |    109.52<sub>±00.55</sub> |
|                    Scala |   3.230<sub>±0.009</sub> |  67.12<sub>±00.09</sub> + 140.81<sub>±00.29</sub> |    133.10<sub>±00.69</sub> |
|                    D/dmd |   3.267<sub>±0.000</sub> |     3.55<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    126.05<sub>±00.33</sub> |
|         Haskell (MArray) |   3.847<sub>±0.001</sub> |     5.87<sub>±00.03</sub> + 4.72<sub>±00.00</sub> |    154.08<sub>±00.43</sub> |
|                  Node.js |   4.154<sub>±0.004</sub> |    40.49<sub>±00.05</sub> + 2.16<sub>±00.09</sub> |    165.56<sub>±00.43</sub> |
|                    Swift |   5.443<sub>±0.000</sub> |    13.80<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    203.97<sub>±00.37</sub> |
|               Lua/luajit |   6.484<sub>±0.015</sub> |     2.50<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    258.91<sub>±00.55</sub> |
|         Ruby/truffleruby |   7.494<sub>±0.115</sub> | 297.28<sub>±00.54</sub> + 325.55<sub>±13.94</sub> |    359.91<sub>±05.75</sub> |
| Ruby/truffleruby (--jvm) |   8.155<sub>±0.067</sub> | 352.85<sub>±06.71</sub> + 467.10<sub>±19.50</sub> |    392.34<sub>±02.92</sub> |
|              Python/pypy |  12.188<sub>±0.033</sub> |   64.54<sub>±00.19</sub> + 30.13<sub>±00.10</sub> |    508.85<sub>±01.68</sub> |
|                  Haskell |  15.028<sub>±0.010</sub> |     3.85<sub>±00.03</sub> + 7.19<sub>±00.04</sub> |    679.80<sub>±08.07</sub> |
|             Ruby (--jit) |  40.868<sub>±0.197</sub> |   270.47<sub>±00.06</sub> + 0.29<sub>±00.04</sub> |   1629.77<sub>±07.26</sub> |
|                   Elixir |  43.616<sub>±0.243</sub> |    71.01<sub>±00.79</sub> + 0.00<sub>±00.00</sub> |   1985.56<sub>±21.21</sub> |
|                      Lua |  50.967<sub>±0.319</sub> |     2.30<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1955.29<sub>±18.53</sub> |
|               Ruby/jruby |  79.375<sub>±0.729</sub> |  184.40<sub>±01.19</sub> + 85.06<sub>±01.03</sub> |   3537.33<sub>±31.99</sub> |
|                     Ruby |  86.488<sub>±0.104</sub> |    13.93<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3604.94<sub>±19.73</sub> |
|                   Python | 196.223<sub>±1.810</sub> |    10.42<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  7843.33<sub>±119.79</sub> |
|                 Tcl (FP) | 270.436<sub>±1.400</sub> |     3.93<sub>±00.09</sub> + 0.00<sub>±00.00</sub> | 11307.54<sub>±134.33</sub> |
|                     Perl | 327.444<sub>±1.656</sub> |     6.86<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |  13065.18<sub>±71.67</sub> |
|                Tcl (OOP) | 523.177<sub>±2.973</sub> |     3.94<sub>±00.07</sub> + 0.00<sub>±00.00</sub> | 22061.79<sub>±158.35</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | -----------------------: |
|                  C++/g++ |  10.468<sub>±0.023</sub> |     1.80<sub>±00.02</sub> + 2.24<sub>±00.02</sub> |  420.90<sub>±01.56</sub> |
|  Racket (Syntax Objects) |  13.780<sub>±0.048</sub> |  111.45<sub>±00.23</sub> + 67.60<sub>±00.23</sub> |  550.03<sub>±02.02</sub> |
|                    C/gcc |  14.200<sub>±0.006</sub> |     0.88<sub>±00.03</sub> + 0.87<sub>±00.03</sub> |  560.07<sub>±04.35</sub> |
|               Kotlin/JVM |  14.292<sub>±0.005</sub> |    41.54<sub>±00.18</sub> + 1.45<sub>±00.21</sub> |  586.01<sub>±00.99</sub> |
|                    V/gcc |  14.450<sub>±0.014</sub> |     2.22<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |  577.29<sub>±00.51</sub> |
|                     Rust |  14.787<sub>±0.016</sub> |     0.92<sub>±00.01</sub> + 1.36<sub>±00.06</sub> |  577.08<sub>±03.69</sub> |
|                   D/ldc2 |  14.924<sub>±0.016</sub> |     2.94<sub>±00.01</sub> + 0.88<sub>±00.02</sub> |  595.92<sub>±00.62</sub> |
|              C++/clang++ |  15.012<sub>±0.015</sub> |     1.65<sub>±00.02</sub> + 1.93<sub>±00.03</sub> |  605.63<sub>±03.12</sub> |
|                  C/clang |  15.122<sub>±0.013</sub> |     0.90<sub>±00.02</sub> + 0.90<sub>±00.00</sub> |  647.01<sub>±03.97</sub> |
|                    D/gdc |  15.238<sub>±0.081</sub> |     6.56<sub>±00.03</sub> + 1.46<sub>±00.03</sub> |  636.13<sub>±04.29</sub> |
|                       Go |  16.157<sub>±0.020</sub> |     3.20<sub>±00.28</sub> + 1.27<sub>±00.00</sub> |  630.11<sub>±03.88</sub> |
|                      Zig |  16.239<sub>±0.020</sub> |     0.92<sub>±00.01</sub> + 1.31<sub>±00.00</sub> |  658.86<sub>±05.07</sub> |
|                     Java |  18.294<sub>±0.023</sub> |    37.71<sub>±00.35</sub> + 1.88<sub>±00.36</sub> |  730.92<sub>±00.72</sub> |
|                 Vala/gcc |  18.477<sub>±0.005</sub> |     4.20<sub>±00.03</sub> + 1.77<sub>±00.04</sub> |  691.66<sub>±00.34</sub> |
|             C#/.NET Core |  18.540<sub>±0.014</sub> |    34.15<sub>±00.06</sub> + 0.83<sub>±00.01</sub> |  740.52<sub>±01.15</sub> |
|                    Swift |  18.784<sub>±0.259</sub> |    14.69<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  759.12<sub>±07.38</sub> |
|                 Go/gccgo |  19.905<sub>±0.008</sub> |    24.46<sub>±00.03</sub> + 1.27<sub>±00.00</sub> |  832.05<sub>±07.20</sub> |
|               Vala/clang |  20.240<sub>±0.005</sub> |     4.20<sub>±00.06</sub> + 1.73<sub>±00.08</sub> |  783.19<sub>±00.73</sub> |
|                    Scala |  20.996<sub>±0.005</sub> |  68.20<sub>±00.10</sub> + 141.33<sub>±00.29</sub> |  877.97<sub>±01.76</sub> |
|                  V/clang |  21.510<sub>±0.078</sub> |     1.85<sub>±00.10</sub> + 1.15<sub>±00.09</sub> |  895.32<sub>±04.49</sub> |
|                  Crystal |  21.595<sub>±0.016</sub> |     2.98<sub>±00.01</sub> + 0.69<sub>±00.05</sub> |  860.94<sub>±02.42</sub> |
|                  Nim/gcc |  22.723<sub>±0.079</sub> |     1.98<sub>±00.02</sub> + 0.52<sub>±00.00</sub> |  924.02<sub>±07.74</sub> |
|                Nim/clang |  25.539<sub>±0.346</sub> |     2.26<sub>±00.01</sub> + 0.57<sub>±00.00</sub> | 1016.20<sub>±13.25</sub> |
|                    OCaml |  26.225<sub>±0.042</sub> |     3.45<sub>±00.03</sub> + 3.79<sub>±00.09</sub> | 1240.48<sub>±07.92</sub> |
|              Chez Scheme |  26.957<sub>±0.030</sub> |    25.36<sub>±00.02</sub> + 3.93<sub>±00.02</sub> | 1185.16<sub>±06.29</sub> |
|                    Julia |  29.496<sub>±0.070</sub> |   224.62<sub>±00.21</sub> + 0.70<sub>±00.02</sub> | 1123.15<sub>±08.96</sub> |
|                  Node.js |  29.634<sub>±1.152</sub> |    40.57<sub>±00.07</sub> + 5.37<sub>±00.00</sub> | 1250.41<sub>±55.67</sub> |
|                  C#/Mono |  31.506<sub>±0.033</sub> |    24.90<sub>±00.04</sub> + 0.82<sub>±00.00</sub> | 1336.77<sub>±01.87</sub> |
|               Lua/luajit |  35.074<sub>±0.035</sub> |     2.44<sub>±00.02</sub> + 0.44<sub>±00.00</sub> | 1399.61<sub>±01.60</sub> |
|                   Racket |  36.947<sub>±0.162</sub> |    92.71<sub>±00.16</sub> + 0.00<sub>±00.00</sub> | 1628.73<sub>±10.79</sub> |
|         Haskell (MArray) |  37.326<sub>±0.013</sub> |     5.67<sub>±00.03</sub> + 6.11<sub>±00.00</sub> | 1513.15<sub>±08.31</sub> |
|                    D/dmd |  37.466<sub>±0.010</sub> |     3.45<sub>±00.03</sub> + 1.00<sub>±00.03</sub> | 1356.90<sub>±00.40</sub> |
|             F#/.NET Core |  38.363<sub>±0.057</sub> |    37.86<sub>±00.05</sub> + 2.10<sub>±00.01</sub> | 1573.80<sub>±03.64</sub> |
|                    MLton |  44.875<sub>±0.023</sub> |     1.30<sub>±00.33</sub> + 4.40<sub>±00.28</sub> | 1941.05<sub>±19.16</sub> |
|              Python/pypy |  48.764<sub>±0.207</sub> |   64.23<sub>±00.13</sub> + 30.59<sub>±00.09</sub> | 2076.08<sub>±25.96</sub> |
| Ruby/truffleruby (--jvm) |  80.275<sub>±0.588</sub> | 353.40<sub>±01.09</sub> + 500.07<sub>±40.27</sub> | 3327.63<sub>±27.96</sub> |
|         Ruby/truffleruby |  81.176<sub>±0.481</sub> | 298.34<sub>±00.74</sub> + 257.92<sub>±09.34</sub> | 3973.50<sub>±42.02</sub> |
|                  Haskell | 186.181<sub>±0.556</sub> |    5.58<sub>±00.06</sub> + 30.55<sub>±00.00</sub> | 8151.90<sub>±99.97</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.101<sub>±0.000</sub> |     2.10<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   4.83<sub>±00.05</sub> |
|          C/clang (aklomp) |  0.101<sub>±0.000</sub> |     2.16<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   4.81<sub>±00.05</sub> |
|                      Rust |  0.970<sub>±0.000</sub> |     2.38<sub>±00.03</sub> + 0.12<sub>±00.01</sub> |  39.54<sub>±00.34</sub> |
|                   C/clang |  0.997<sub>±0.000</sub> |     2.09<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  37.34<sub>±00.26</sub> |
|                     C/gcc |  1.010<sub>±0.000</sub> |     2.11<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  37.86<sub>±00.12</sub> |
|                    D/ldc2 |  1.042<sub>±0.001</sub> |     3.09<sub>±00.04</sub> + 3.96<sub>±00.01</sub> |  43.46<sub>±00.77</sub> |
|                   Nim/gcc |  1.081<sub>±0.001</sub> |     1.63<sub>±00.05</sub> + 5.39<sub>±00.05</sub> |  42.90<sub>±00.19</sub> |
|                 Nim/clang |  1.094<sub>±0.001</sub> |     1.90<sub>±00.04</sub> + 5.29<sub>±00.03</sub> |  43.77<sub>±00.20</sub> |
|                   V/clang |  1.160<sub>±0.007</sub> |     2.36<sub>±00.03</sub> + 1.29<sub>±00.15</sub> |  47.30<sub>±00.49</sub> |
|                   Crystal |  1.181<sub>±0.002</sub> |     3.49<sub>±00.04</sub> + 1.45<sub>±00.01</sub> |  52.75<sub>±00.34</sub> |
|                     V/gcc |  1.205<sub>±0.003</sub> |     2.37<sub>±00.03</sub> + 1.24<sub>±00.03</sub> |  49.50<sub>±00.32</sub> |
|              Ruby (--jit) |  1.384<sub>±0.001</sub> |  270.92<sub>±00.07</sub> + 67.47<sub>±00.05</sub> |  53.89<sub>±00.29</sub> |
|                      Ruby |  1.389<sub>±0.001</sub> |   14.63<sub>±00.02</sub> + 58.67<sub>±00.03</sub> |  53.74<sub>±00.10</sub> |
|                      Java |  1.520<sub>±0.012</sub> |  39.39<sub>±00.04</sub> + 262.48<sub>±23.42</sub> |  62.01<sub>±00.87</sub> |
|                     Scala |  1.603<sub>±0.003</sub> |  65.19<sub>±00.12</sub> + 326.86<sub>±15.35</sub> |  68.76<sub>±00.66</sub> |
|                Kotlin/JVM |  1.635<sub>±0.002</sub> |  42.55<sub>±00.07</sub> + 249.61<sub>±00.86</sub> |  69.68<sub>±00.77</sub> |
|                  Vala/gcc |  1.643<sub>±0.001</sub> |     5.46<sub>±00.04</sub> + 0.19<sub>±00.04</sub> |  63.47<sub>±00.70</sub> |
|                Vala/clang |  1.643<sub>±0.001</sub> |     5.46<sub>±00.04</sub> + 0.22<sub>±00.05</sub> |  62.97<sub>±00.40</sub> |
|   C++/clang++ (libcrypto) |  1.715<sub>±0.002</sub> |     5.10<sub>±00.11</sub> + 0.65<sub>±00.03</sub> |  69.12<sub>±00.99</sub> |
|       C++/g++ (libcrypto) |  1.715<sub>±0.004</sub> |     5.64<sub>±00.07</sub> + 0.66<sub>±00.03</sub> |  69.34<sub>±00.49</sub> |
|                   Node.js |  1.743<sub>±0.006</sub> |   40.76<sub>±00.04</sub> + 38.86<sub>±00.02</sub> |  70.15<sub>±00.65</sub> |
|       Perl (MIME::Base64) |  1.791<sub>±0.000</sub> |    14.70<sub>±00.08</sub> + 0.16<sub>±00.03</sub> |  70.22<sub>±00.07</sub> |
|                        Go |  1.875<sub>±0.002</sub> |     4.22<sub>±00.05</sub> + 3.92<sub>±00.05</sub> |  80.99<sub>±00.49</sub> |
|                       PHP |  2.093<sub>±0.017</sub> |    17.79<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  84.17<sub>±00.78</sub> |
|                     D/gdc |  2.442<sub>±0.003</sub> |     7.52<sub>±00.07</sub> + 3.35<sub>±00.00</sub> | 108.08<sub>±00.52</sub> |
|                     D/dmd |  2.739<sub>±0.001</sub> |     3.60<sub>±00.12</sub> + 3.85<sub>±00.05</sub> | 119.70<sub>±00.68</sub> |
|                    Python |  2.837<sub>±0.002</sub> |    10.36<sub>±00.03</sub> + 0.55<sub>±00.06</sub> | 112.86<sub>±00.30</sub> |
|                       Zig |  3.067<sub>±0.002</sub> |     1.45<sub>±00.04</sub> + 0.00<sub>±00.00</sub> | 122.43<sub>±00.62</sub> |
|              F#/.NET Core |  3.202<sub>±0.052</sub> |   38.62<sub>±00.06</sub> + 18.68<sub>±00.71</sub> | 120.67<sub>±01.23</sub> |
|               Python/pypy |  3.581<sub>±0.002</sub> |   64.67<sub>±00.11</sub> + 31.23<sub>±00.04</sub> | 156.54<sub>±00.94</sub> |
|                       Tcl |  3.685<sub>±0.078</sub> |     5.12<sub>±00.06</sub> + 0.00<sub>±00.00</sub> | 149.06<sub>±01.75</sub> |
|                  Go/gccgo |  3.720<sub>±0.002</sub> |    25.62<sub>±00.20</sub> + 7.71<sub>±00.17</sub> | 168.84<sub>±00.90</sub> |
|              C#/.NET Core |  3.726<sub>±0.053</sub> |   34.67<sub>±00.08</sub> + 19.67<sub>±01.47</sub> | 137.24<sub>±01.33</sub> |
|                   C#/Mono |  4.692<sub>±0.014</sub> |   25.71<sub>±00.08</sub> + 18.74<sub>±00.03</sub> | 193.29<sub>±01.07</sub> |
|                     Julia |  5.023<sub>±0.004</sub> |  244.56<sub>±00.09</sub> + 42.28<sub>±00.09</sub> | 188.22<sub>±02.10</sub> |
|  Ruby/truffleruby (--jvm) |  5.072<sub>±0.012</sub> | 347.27<sub>±07.42</sub> + 168.12<sub>±27.78</sub> | 234.49<sub>±01.53</sub> |
|                Ruby/jruby | 10.519<sub>±0.026</sub> |  182.81<sub>±02.29</sub> + 80.63<sub>±02.65</sub> | 420.63<sub>±01.57</sub> |
| Perl (MIME::Base64::Perl) | 13.790<sub>±0.053</sub> |    16.12<sub>±00.07</sub> + 0.31<sub>±00.04</sub> | 573.08<sub>±04.60</sub> |
|          Ruby/truffleruby | 16.232<sub>±0.006</sub> | 295.77<sub>±00.95</sub> + 232.04<sub>±04.13</sub> | 647.25<sub>±03.84</sub> |

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
|        C++/g++ (simdjson On-Demand) |  0.066<sub>±0.000</sub> |   112.99<sub>±00.03</sub> + 60.31<sub>±00.07</sub> |    2.73<sub>±00.01</sub> |
|    C++/clang++ (simdjson On-Demand) |  0.067<sub>±0.000</sub> |   112.43<sub>±00.04</sub> + 60.36<sub>±00.00</sub> |    2.77<sub>±00.02</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.070<sub>±0.001</sub> |    112.39<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    2.94<sub>±00.04</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.080<sub>±0.000</sub> |    113.28<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    3.23<sub>±00.03</sub> |
|             C++/g++ (DAW JSON Link) |  0.081<sub>±0.000</sub> |    113.21<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    3.41<sub>±00.02</sub> |
|         C++/clang++ (DAW JSON Link) |  0.096<sub>±0.000</sub> |    112.46<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |    3.93<sub>±00.01</sub> |
|                 Rust (Serde Custom) |  0.106<sub>±0.000</sub> |    111.64<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    4.42<sub>±00.04</sub> |
|                  Rust (Serde Typed) |  0.107<sub>±0.000</sub> |   111.64<sub>±00.02</sub> + 11.52<sub>±00.08</sub> |    4.48<sub>±00.03</sub> |
|                     C++/g++ (gason) |  0.132<sub>±0.000</sub> |   113.06<sub>±00.11</sub> + 96.85<sub>±00.06</sub> |    5.23<sub>±00.01</sub> |
|              C++/g++ (simdjson DOM) |  0.139<sub>±0.000</sub> |  113.21<sub>±00.24</sub> + 175.85<sub>±00.23</sub> |    5.92<sub>±00.03</sub> |
|                 C++/clang++ (gason) |  0.143<sub>±0.000</sub> |   112.40<sub>±00.06</sub> + 96.97<sub>±00.06</sub> |    5.82<sub>±00.03</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.145<sub>±0.000</sub> |   112.66<sub>±00.02</sub> + 61.26<sub>±00.01</sub> |    6.03<sub>±00.02</sub> |
|          C++/clang++ (simdjson DOM) |  0.146<sub>±0.000</sub> |  112.47<sub>±00.03</sub> + 177.15<sub>±00.00</sub> |    6.27<sub>±00.02</sub> |
|                 C++/g++ (RapidJSON) |  0.166<sub>±0.000</sub> |  113.32<sub>±00.02</sub> + 127.47<sub>±00.30</sub> |    7.05<sub>±00.07</sub> |
|             C++/clang++ (RapidJSON) |  0.235<sub>±0.000</sub> |  112.36<sub>±00.07</sub> + 129.04<sub>±00.05</sub> |    9.77<sub>±00.03</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.236<sub>±0.000</sub> |   112.79<sub>±00.03</sub> + 80.70<sub>±00.00</sub> |    9.81<sub>±00.06</sub> |
|         C++/g++ (RapidJSON Precise) |  0.244<sub>±0.000</sub> |  113.29<sub>±00.02</sub> + 128.82<sub>±00.05</sub> |   10.24<sub>±00.08</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.314<sub>±0.000</sub> |  112.47<sub>±00.04</sub> + 129.00<sub>±00.03</sub> |   13.49<sub>±00.06</sub> |
|                C++/g++ (Boost.JSON) |  0.396<sub>±0.000</sub> |  113.32<sub>±00.04</sub> + 435.82<sub>±00.06</sub> |   16.56<sub>±00.02</sub> |
|             C++/g++ (RapidJSON SAX) |  0.404<sub>±0.001</sub> |    113.05<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   16.62<sub>±00.05</sub> |
|                   Nim/clang (jsony) |  0.414<sub>±0.000</sub> |   111.40<sub>±00.04</sub> + 42.97<sub>±00.11</sub> |   17.73<sub>±00.19</sub> |
|            C++/clang++ (Boost.JSON) |  0.418<sub>±0.001</sub> |  112.46<sub>±00.05</sub> + 436.25<sub>±00.06</sub> |   17.59<sub>±00.14</sub> |
|                     Nim/gcc (jsony) |  0.422<sub>±0.000</sub> |   111.10<sub>±00.04</sub> + 42.84<sub>±00.14</sub> |   17.75<sub>±00.15</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.448<sub>±0.000</sub> |    113.07<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   19.27<sub>±00.06</sub> |
|                       Go (jsoniter) |  0.520<sub>±0.002</sub> |    231.00<sub>±00.12</sub> + 1.24<sub>±00.08</sub> |   22.19<sub>±00.12</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.531<sub>±0.000</sub> |    194.70<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   20.74<sub>±00.17</sub> |
|                             Node.js |  0.546<sub>±0.006</sub> |  150.03<sub>±00.06</sub> + 188.93<sub>±00.13</sub> |   25.76<sub>±00.18</sub> |
|                     Java (DSL-JSON) |  0.619<sub>±0.012</sub> |  259.64<sub>±00.07</sub> + 188.73<sub>±08.78</sub> |   31.78<sub>±00.67</sub> |
|                               V/gcc |  0.633<sub>±0.001</sub> |  111.91<sub>±00.05</sub> + 496.18<sub>±00.09</sub> |   26.27<sub>±00.15</sub> |
|                Rust (Serde Untyped) |  0.639<sub>±0.001</sub> |  111.61<sub>±00.03</sub> + 840.27<sub>±00.03</sub> |   25.87<sub>±00.18</sub> |
|                             V/clang |  0.639<sub>±0.000</sub> |  111.97<sub>±00.03</sub> + 496.24<sub>±00.03</sub> |   26.66<sub>±00.17</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.663<sub>±0.000</sub> |    194.66<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   26.73<sub>±00.14</sub> |
|                         Python/pypy |  0.671<sub>±0.002</sub> |  284.89<sub>±00.05</sub> + 125.41<sub>±00.04</sub> |   29.24<sub>±00.15</sub> |
|                      Crystal (Pull) |  0.687<sub>±0.005</sub> |   113.42<sub>±00.04</sub> + 18.47<sub>±00.03</sub> |   30.07<sub>±00.17</sub> |
|                    Crystal (Schema) |  0.704<sub>±0.002</sub> |   113.40<sub>±00.02</sub> + 48.88<sub>±00.03</sub> |   30.82<sub>±00.16</sub> |
|     C#/.NET Core (System.Text.Json) |  0.755<sub>±0.001</sub> |  479.22<sub>±00.04</sub> + 138.55<sub>±00.00</sub> |   32.96<sub>±00.31</sub> |
|                                 Zig |  0.773<sub>±0.000</sub> |   110.75<sub>±00.01</sub> + 11.98<sub>±00.06</sub> |   30.65<sub>±00.20</sub> |
|                       Julia (JSON3) |  0.818<sub>±0.005</sub> |  452.44<sub>±00.58</sub> + 212.54<sub>±02.91</sub> |   33.11<sub>±00.26</sub> |
|             Perl (Cpanel::JSON::XS) |  0.834<sub>±0.009</sub> |  125.32<sub>±00.10</sub> + 402.87<sub>±00.02</sub> |   34.85<sub>±00.42</sub> |
|                                  Go |  0.856<sub>±0.001</sub> |   117.22<sub>±00.02</sub> + 79.73<sub>±00.22</sub> |   35.32<sub>±00.31</sub> |
|                             Crystal |  0.937<sub>±0.005</sub> |  113.52<sub>±00.08</sub> + 392.53<sub>±00.03</sub> |   42.18<sub>±00.34</sub> |
|                                 PHP |  0.982<sub>±0.002</sub> |  126.96<sub>±00.04</sub> + 682.21<sub>±00.00</sub> |   40.85<sub>±00.23</sub> |
|              Nim/clang (Packedjson) |  0.991<sub>±0.002</sub> |  112.15<sub>±00.02</sub> + 294.42<sub>±00.00</sub> |   40.83<sub>±00.21</sub> |
|                Nim/gcc (Packedjson) |  1.010<sub>±0.001</sub> |  111.83<sub>±00.03</sub> + 294.42<sub>±00.00</sub> |   41.89<sub>±00.21</sub> |
|                C++/clang++ (json-c) |  1.194<sub>±0.007</sub> | 112.71<sub>±00.07</sub> + 1216.11<sub>±00.03</sub> |   50.61<sub>±00.93</sub> |
|                    C++/g++ (json-c) |  1.198<sub>±0.006</sub> | 113.43<sub>±00.04</sub> + 1215.96<sub>±00.00</sub> |   50.29<sub>±00.65</sub> |
|                             Clojure |  1.199<sub>±0.019</sub> |  464.27<sub>±05.08</sub> + 541.31<sub>±49.27</sub> |   61.77<sub>±00.48</sub> |
|                            Go/gccgo |  1.259<sub>±0.003</sub> |   138.89<sub>±00.04</sub> + 83.65<sub>±00.10</sub> |   53.56<sub>±00.15</sub> |
|                        C#/.NET Core |  1.283<sub>±0.013</sub> |  486.95<sub>±00.07</sub> + 294.45<sub>±00.04</sub> |   55.84<sub>±00.54</sub> |
|              C++/clang++ (Nlohmann) |  1.297<sub>±0.002</sub> |  112.59<sub>±00.05</sub> + 360.20<sub>±00.00</sub> |   54.81<sub>±00.28</sub> |
|                             Nim/gcc |  1.345<sub>±0.002</sub> |  111.87<sub>±00.04</sub> + 919.93<sub>±00.00</sub> |   56.30<sub>±00.21</sub> |
|                           Nim/clang |  1.351<sub>±0.004</sub> |  112.15<sub>±00.01</sub> + 925.03<sub>±00.00</sub> |   55.67<sub>±00.23</sub> |
|                        Ruby (--jit) |  1.448<sub>±0.007</sub> |  380.59<sub>±00.07</sub> + 263.15<sub>±00.02</sub> |   61.92<sub>±00.27</sub> |
|                                Ruby |  1.449<sub>±0.003</sub> |  123.97<sub>±00.02</sub> + 263.11<sub>±00.01</sub> |   61.61<sub>±00.15</sub> |
|                 CPython (UltraJSON) |  1.454<sub>±0.003</sub> |  122.26<sub>±00.13</sub> + 545.52<sub>±01.28</sub> |   54.11<sub>±00.21</sub> |
|                  C++/g++ (Nlohmann) |  1.489<sub>±0.008</sub> |  113.32<sub>±00.03</sub> + 447.92<sub>±00.04</sub> |   60.89<sub>±00.57</sub> |
|                              Python |  1.524<sub>±0.002</sub> |  120.30<sub>±00.07</sub> + 375.25<sub>±00.03</sub> |   60.10<sub>±00.20</sub> |
|     F#/.NET Core (System.Text.Json) |  1.777<sub>±0.004</sub> |  486.91<sub>±00.03</sub> + 345.26<sub>±00.13</sub> |   77.46<sub>±00.66</sub> |
|                         Ruby (YAJL) |  1.962<sub>±0.010</sub> |  123.89<sub>±00.04</sub> + 282.64<sub>±00.07</sub> |   82.95<sub>±00.75</sub> |
|                     Scala (uPickle) |  1.997<sub>±0.014</sub> |  292.25<sub>±00.16</sub> + 702.97<sub>±67.44</sub> |   97.94<sub>±00.61</sub> |
|                              D/ldc2 |  2.079<sub>±0.003</sub> |  112.90<sub>±00.05</sub> + 680.22<sub>±00.05</sub> |   86.04<sub>±00.27</sub> |
|                             C#/Mono |  2.306<sub>±0.027</sub> |   252.32<sub>±00.07</sub> + 31.57<sub>±00.02</sub> |   97.12<sub>±00.89</sub> |
|                             Haskell |  2.590<sub>±0.005</sub> |  117.60<sub>±00.03</sub> + 713.50<sub>±00.38</sub> |  110.05<sub>±01.10</sub> |
|                           Rust (jq) |  2.804<sub>±0.003</sub> |  113.77<sub>±00.06</sub> + 779.44<sub>±00.39</sub> |  116.12<sub>±00.64</sub> |
|                          Ruby/jruby |  2.998<sub>±0.038</sub> | 442.51<sub>±05.84</sub> + 930.50<sub>±130.38</sub> |  151.53<sub>±02.51</sub> |
|    C++/clang++ (Boost.PropertyTree) |  3.158<sub>±0.003</sub> | 194.88<sub>±00.03</sub> + 1232.84<sub>±00.06</sub> |  131.20<sub>±00.85</sub> |
|        C++/g++ (Boost.PropertyTree) |  3.341<sub>±0.009</sub> | 113.17<sub>±00.03</sub> + 1440.16<sub>±00.03</sub> |  139.24<sub>±00.94</sub> |
|                            Vala/gcc |  3.613<sub>±0.008</sub> |  114.75<sub>±00.06</sub> + 940.52<sub>±00.09</sub> |  155.10<sub>±01.78</sub> |
|                          Vala/clang |  3.613<sub>±0.014</sub> |  114.75<sub>±00.03</sub> + 940.56<sub>±00.09</sub> |  153.78<sub>±01.48</sub> |
|                               D/gdc |  3.656<sub>±0.008</sub> |  116.62<sub>±00.05</sub> + 681.22<sub>±00.10</sub> |  156.04<sub>±00.72</sub> |
|                               D/dmd |  4.534<sub>±0.004</sub> |  113.23<sub>±00.04</sub> + 680.46<sub>±00.04</sub> |  181.45<sub>±00.78</sub> |
|                   Perl (JSON::Tiny) |  9.712<sub>±0.100</sub> |  125.59<sub>±00.03</sub> + 529.02<sub>±00.02</sub> |  427.10<sub>±03.42</sub> |
|            Ruby/truffleruby (--jvm) | 14.091<sub>±0.265</sub> | 477.27<sub>±14.72</sub> + 2545.17<sub>±68.47</sub> |  912.67<sub>±20.03</sub> |
|                    Ruby/truffleruby | 24.040<sub>±0.350</sub> | 433.56<sub>±02.08</sub> + 2561.27<sub>±48.17</sub> | 1194.76<sub>±20.42</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                  Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |   0.043<sub>±0.000</sub> |    5.84<sub>±00.04</sub> + 57.97<sub>±00.13</sub> |      4.42<sub>±00.02</sub> |
|    Nim/gcc (Arraymancer) |   0.063<sub>±0.006</sub> |    4.69<sub>±00.01</sub> + 58.04<sub>±00.15</sub> |      5.21<sub>±00.52</sub> |
|           Python (NumPy) |   0.067<sub>±0.000</sub> |   30.06<sub>±00.12</sub> + 58.10<sub>±00.03</sub> |      6.29<sub>±00.03</sub> |
|      C++/clang++ (Eigen) |   0.071<sub>±0.003</sub> |   38.76<sub>±05.62</sub> + 51.51<sub>±05.67</sub> |      5.87<sub>±00.10</sub> |
|          C++/g++ (Eigen) |   0.076<sub>±0.003</sub> |   19.72<sub>±13.35</sub> + 70.30<sub>±13.23</sub> |      5.60<sub>±00.15</sub> |
|              Java (ND4J) |   0.078<sub>±0.001</sub> |  105.22<sub>±01.56</sub> + 92.03<sub>±00.12</sub> |      6.13<sub>±00.06</sub> |
|  Nim/clang (Arraymancer) |   0.081<sub>±0.012</sub> |    6.19<sub>±00.07</sub> + 57.45<sub>±00.14</sub> |      6.87<sub>±00.69</sub> |
|           Rust (ndarray) |   0.090<sub>±0.001</sub> |    2.24<sub>±00.04</sub> + 68.74<sub>±00.05</sub> |      6.11<sub>±00.07</sub> |
|       Julia (threads: 2) |   0.101<sub>±0.000</sub> |  265.20<sub>±00.47</sub> + 46.43<sub>±00.13</sub> |      5.91<sub>±00.02</sub> |
|       Julia (threads: 1) |   0.151<sub>±0.000</sub> |  265.55<sub>±00.25</sub> + 45.95<sub>±00.09</sub> |      7.30<sub>±00.08</sub> |
|          Julia (no BLAS) |   1.041<sub>±0.012</sub> |  243.90<sub>±00.24</sub> + 51.54<sub>±00.04</sub> |     46.04<sub>±00.54</sub> |
|                   D/ldc2 |   1.715<sub>±0.002</sub> |    3.17<sub>±00.02</sub> + 70.49<sub>±00.03</sub> |     63.20<sub>±00.18</sub> |
|                    D/gdc |   1.870<sub>±0.002</sub> |    6.81<sub>±00.06</sub> + 70.97<sub>±00.03</sub> |     73.21<sub>±00.21</sub> |
|                    D/dmd |   1.874<sub>±0.002</sub> |    3.42<sub>±00.02</sub> + 70.48<sub>±00.01</sub> |     71.16<sub>±00.16</sub> |
|                    C/gcc |   3.029<sub>±0.000</sub> |    1.50<sub>±00.02</sub> + 68.74<sub>±00.04</sub> |    111.52<sub>±00.91</sub> |
|               Vala/clang |   3.060<sub>±0.001</sub> |    3.93<sub>±00.08</sub> + 69.88<sub>±00.07</sub> |    105.78<sub>±00.97</sub> |
|                  C/clang |   3.062<sub>±0.000</sub> |    1.51<sub>±00.01</sub> + 68.73<sub>±00.03</sub> |    104.53<sub>±00.17</sub> |
|                     Rust |   3.064<sub>±0.001</sub> |    2.02<sub>±00.04</sub> + 68.88<sub>±00.07</sub> |    104.85<sub>±00.19</sub> |
|                      Zig |   3.085<sub>±0.001</sub> |    1.40<sub>±00.02</sub> + 68.89<sub>±00.00</sub> |    109.21<sub>±00.14</sub> |
|                  Nim/gcc |   3.089<sub>±0.002</sub> |    2.51<sub>±00.03</sub> + 66.26<sub>±00.00</sub> |    114.20<sub>±00.48</sub> |
|                     Java |   3.089<sub>±0.001</sub> |   38.29<sub>±00.04</sub> + 68.54<sub>±00.13</sub> |    109.95<sub>±00.09</sub> |
|                    Swift |   3.096<sub>±0.000</sub> |    6.55<sub>±00.03</sub> + 68.91<sub>±00.01</sub> |    110.89<sub>±01.35</sub> |
|                Nim/clang |   3.121<sub>±0.001</sub> |    2.78<sub>±00.01</sub> + 66.00<sub>±00.00</sub> |    107.00<sub>±00.38</sub> |
|                 Vala/gcc |   3.125<sub>±0.001</sub> |    3.95<sub>±00.03</sub> + 69.91<sub>±00.04</sub> |    114.45<sub>±00.11</sub> |
|                       Go |   3.152<sub>±0.000</sub> |    3.70<sub>±00.04</sub> + 73.19<sub>±00.09</sub> |    115.16<sub>±01.26</sub> |
|                  Crystal |   3.160<sub>±0.001</sub> |    3.78<sub>±00.05</sub> + 59.96<sub>±00.01</sub> |    115.82<sub>±00.30</sub> |
|                 Go/gccgo |   3.160<sub>±0.001</sub> |   24.82<sub>±00.03</sub> + 73.40<sub>±00.05</sub> |    112.08<sub>±00.21</sub> |
|                    V/gcc |   3.188<sub>±0.001</sub> |    2.53<sub>±00.12</sub> + 70.90<sub>±00.00</sub> |    120.57<sub>±00.24</sub> |
|                  V/clang |   3.194<sub>±0.001</sub> |    2.89<sub>±00.03</sub> + 70.90<sub>±00.00</sub> |    120.75<sub>±00.24</sub> |
|                  Node.js |   3.208<sub>±0.001</sub> |   45.17<sub>±00.06</sub> + 70.61<sub>±00.00</sub> |    129.36<sub>±00.17</sub> |
|              Python/pypy |   3.268<sub>±0.002</sub> |   65.50<sub>±00.07</sub> + 68.63<sub>±00.04</sub> |    135.95<sub>±00.19</sub> |
|                    Scala |   3.334<sub>±0.004</sub> |  66.21<sub>±00.12</sub> + 147.70<sub>±00.19</sub> |    122.19<sub>±00.14</sub> |
|               Kotlin/JVM |   3.680<sub>±0.003</sub> |   40.47<sub>±00.06</sub> + 68.25<sub>±00.27</sub> |    153.89<sub>±00.35</sub> |
|             C#/.NET Core |   4.874<sub>±0.000</sub> |   34.59<sub>±00.03</sub> + 69.33<sub>±00.00</sub> |    195.95<sub>±00.22</sub> |
|                  C#/Mono |   7.413<sub>±0.000</sub> |   25.43<sub>±00.10</sub> + 69.52<sub>±00.02</sub> |    302.00<sub>±03.34</sub> |
|         Ruby/truffleruby |  16.489<sub>±2.861</sub> | 366.34<sub>±03.56</sub> + 447.40<sub>±65.47</sub> |    625.65<sub>±94.01</sub> |
| Ruby/truffleruby (--jvm) |  25.232<sub>±0.159</sub> | 460.71<sub>±32.48</sub> + 446.25<sub>±62.62</sub> |    903.31<sub>±14.59</sub> |
|                     Ruby | 199.779<sub>±1.180</sub> |   14.88<sub>±00.07</sub> + 69.07<sub>±00.02</sub> |   8676.55<sub>±45.16</sub> |
|             Ruby (--jit) | 202.181<sub>±2.040</sub> |  271.47<sub>±00.04</sub> + 69.06<sub>±00.02</sub> |  8655.57<sub>±187.67</sub> |
|                     Perl | 222.566<sub>±1.469</sub> |   9.40<sub>±00.08</sub> + 599.71<sub>±00.03</sub> |   9015.71<sub>±75.12</sub> |
|                   Python | 280.771<sub>±2.283</sub> |   10.62<sub>±00.07</sub> + 69.09<sub>±00.06</sub> |  11178.73<sub>±44.71</sub> |
|                      Tcl | 334.061<sub>±2.020</sub> |   7.35<sub>±00.06</sub> + 400.18<sub>±00.00</sub> |  13954.09<sub>±93.63</sub> |
|               Ruby/jruby | 389.418<sub>±5.069</sub> | 268.52<sub>±02.25</sub> + 709.04<sub>±17.44</sub> | 16350.68<sub>±256.43</sub> |

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
|                      Zig | 0.059<sub>±0.000</sub> |    0.92<sub>±00.01</sub> + 48.53<sub>±00.03</sub> |   2.45<sub>±00.02</sub> |
|                  Crystal | 0.122<sub>±0.000</sub> |    2.98<sub>±00.03</sub> + 90.71<sub>±00.17</sub> |   5.52<sub>±00.08</sub> |
|                     Rust | 0.140<sub>±0.000</sub> |    0.94<sub>±00.01</sub> + 74.26<sub>±00.07</sub> |   5.35<sub>±00.05</sub> |
|                     Java | 0.159<sub>±0.002</sub> |  38.27<sub>±00.10</sub> + 153.79<sub>±03.78</sub> |   8.89<sub>±00.06</sub> |
|                  C++/g++ | 0.189<sub>±0.000</sub> |   2.70<sub>±00.90</sub> + 116.66<sub>±00.81</sub> |   7.76<sub>±00.05</sub> |
|              C++/clang++ | 0.198<sub>±0.000</sub> |    1.66<sub>±00.02</sub> + 87.96<sub>±00.02</sub> |   7.79<sub>±00.09</sub> |
|                  V/clang | 0.212<sub>±0.001</sub> |   1.91<sub>±00.05</sub> + 203.15<sub>±01.44</sub> |   8.46<sub>±00.06</sub> |
|                  Node.js | 0.227<sub>±0.002</sub> |  39.10<sub>±00.03</sub> + 150.29<sub>±00.26</sub> |  11.37<sub>±00.12</sub> |
|                    V/gcc | 0.240<sub>±0.001</sub> |   2.28<sub>±00.14</sub> + 219.14<sub>±00.67</sub> |   9.66<sub>±00.12</sub> |
|                Nim/clang | 0.297<sub>±0.001</sub> |   2.07<sub>±00.01</sub> + 601.73<sub>±02.96</sub> |  11.72<sub>±00.09</sub> |
|               Lua/luajit | 0.338<sub>±0.002</sub> |   1.20<sub>±00.02</sub> + 157.12<sub>±01.04</sub> |  13.22<sub>±00.05</sub> |
|                  Nim/gcc | 0.352<sub>±0.003</sub> |   1.68<sub>±00.07</sub> + 614.62<sub>±00.26</sub> |  13.48<sub>±00.20</sub> |
|                    Scala | 0.362<sub>±0.006</sub> |  67.43<sub>±00.09</sub> + 248.88<sub>±11.36</sub> |  18.63<sub>±00.35</sub> |
|                    Julia | 0.597<sub>±0.001</sub> | 246.42<sub>±00.24</sub> + 374.41<sub>±01.06</sub> |  23.23<sub>±00.15</sub> |
|              Python/pypy | 0.880<sub>±0.002</sub> |  63.32<sub>±00.09</sub> + 250.27<sub>±00.06</sub> |  34.53<sub>±00.42</sub> |
| Ruby/truffleruby (--jvm) | 1.381<sub>±0.026</sub> | 354.60<sub>±10.97</sub> + 552.99<sub>±34.23</sub> |  90.57<sub>±01.36</sub> |
|             Ruby (--jit) | 1.442<sub>±0.004</sub> | 270.43<sub>±00.06</sub> + 147.02<sub>±00.05</sub> |  59.14<sub>±00.99</sub> |
|                      Lua | 1.478<sub>±0.004</sub> |   2.28<sub>±00.01</sub> + 283.04<sub>±00.48</sub> |  57.46<sub>±00.33</sub> |
|         Ruby/truffleruby | 1.506<sub>±0.007</sub> | 294.80<sub>±00.82</sub> + 410.90<sub>±26.05</sub> |  84.20<sub>±00.88</sub> |
|               Ruby/jruby | 1.990<sub>±0.057</sub> | 182.28<sub>±01.67</sub> + 538.37<sub>±28.70</sub> | 102.14<sub>±03.48</sub> |
|                     Ruby | 2.057<sub>±0.005</sub> |  13.87<sub>±00.07</sub> + 147.07<sub>±00.04</sub> |  82.80<sub>±00.47</sub> |
|                   Python | 5.022<sub>±0.024</sub> |  10.49<sub>±00.04</sub> + 234.72<sub>±00.26</sub> | 196.62<sub>±02.52</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Xeon(R) E-2324G

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 6.0.401                         |
| C#/.NET Core     | 4.3.0-3.22415.1 (8301d484)      |
| C#/Mono          | 6.8.0.105                       |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.11.1"                        |
| Crystal          | 1.5.1                           |
| D/dmd            | v2.100.2                        |
| D/gdc            | 12.2.0                          |
| D/ldc2           | 1.30.0                          |
| Elixir           | 1.12.2                          |
| F#/.NET Core     | 12.0.4.0 for F# 6.0             |
| Go               | go1.19.1                        |
| Go/gccgo         | 12.2.0                          |
| Haskell          | 9.4.2                           |
| Java             | 19                              |
| Julia            | v"1.8.1"                        |
| Kotlin           | 1.7.10                          |
| Lua              | 5.4.4                           |
| Lua/luajit       | 2.1.0-beta3                     |
| MLton            | 20210117                        |
| Nim              | 1.6.6                           |
| Node.js          | v18.9.0                         |
| OCaml            | 4.14.0                          |
| PHP              | 8.1.5                           |
| Perl             | v5.34.0                         |
| Python           | 3.10.7                          |
| Python/pypy      | 7.3.9-final0 for Python 3.9.12  |
| Racket           | "8.6"                           |
| Ruby             | 3.1.2p20                        |
| Ruby/jruby       | 9.3.8.0                         |
| Ruby/truffleruby | 22.2.0                          |
| Rust             | 1.64.0                          |
| Scala            | 3.2.0                           |
| Swift            | 5.7                             |
| Tcl              | 8.6                             |
| V                | 0.3.1 993802f                   |
| Vala             | 0.56.3                          |
| Zig              | 0.9.1                           |
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
