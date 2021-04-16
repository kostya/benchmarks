<!-- md-toc-begin -->
# Table of Content
* [Overview](#overview)
  * [Measurements](#measurements)
* [Test Cases](#test-cases)
  * [Brainfuck](#brainfuck)
    * [bench.b](#benchb)
    * [mandel.b](#mandelb)
  * [Base64](#base64)
  * [Json](#json)
  * [Matmul](#matmul)
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
<!-- md-toc-end -->

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
 - energy consumption of the CPU (PP0 package) during the benchmark.

All values are presented as: `median`<sub>±`median absolute deviation`</sub>.

UPDATE: 2021-03-31

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).
Supports two mode:

 - Verbose (default). Prints the output immediately.
 - Quiet (if QUIET environment variable is set). Accumulates the output using Fletcher-16 checksum, and prints it out after the benchmark.

[Brainfuck](brainfuck)

### bench.b

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|                  C++/g++ |    0.869<sub>±0.026</sub> |     1.49<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     17.64<sub>±00.80</sub> |
|  Racket (Syntax Objects) |    1.359<sub>±0.026</sub> |   110.04<sub>±00.14</sub> + 0.00<sub>±00.00</sub> |     30.17<sub>±02.55</sub> |
|                   D/ldc2 |    1.760<sub>±0.063</sub> |     3.03<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     37.80<sub>±02.69</sub> |
|                    C/gcc |    1.801<sub>±0.049</sub> |     0.50<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     40.08<sub>±01.11</sub> |
|                   Kotlin |    1.801<sub>±0.037</sub> |    38.80<sub>±00.08</sub> + 1.55<sub>±00.05</sub> |     33.84<sub>±01.23</sub> |
|                     Rust |    1.879<sub>±0.028</sub> |     2.04<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     41.72<sub>±00.69</sub> |
|                  Nim/gcc |    1.883<sub>±0.037</sub> |     1.87<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     35.48<sub>±01.09</sub> |
|                    D/gdc |    1.973<sub>±0.054</sub> |     6.21<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     36.87<sub>±01.67</sub> |
|                Nim/clang |    2.029<sub>±0.017</sub> |     2.31<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     38.04<sub>±01.06</sub> |
|                   Racket |    2.177<sub>±0.067</sub> |   115.81<sub>±00.28</sub> + 2.06<sub>±00.26</sub> |     46.44<sub>±04.18</sub> |
|                  C/clang |    2.215<sub>±0.026</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     44.48<sub>±02.20</sub> |
|                     Java |    2.218<sub>±0.035</sub> |    38.11<sub>±00.38</sub> + 1.15<sub>±00.44</sub> |     40.62<sub>±00.80</sub> |
|                    OCaml |    2.220<sub>±0.094</sub> |     2.61<sub>±00.05</sub> + 2.51<sub>±00.06</sub> |     44.80<sub>±03.57</sub> |
|                 Vala/gcc |    2.233<sub>±0.016</sub> |     3.75<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     48.63<sub>±00.41</sub> |
|                       Go |    2.235<sub>±0.083</sub> |     3.52<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     44.49<sub>±03.21</sub> |
|             F#/.NET Core |    2.327<sub>±0.050</sub> |    36.85<sub>±00.02</sub> + 0.39<sub>±00.03</sub> |     42.33<sub>±00.87</sub> |
|             C#/.NET Core |    2.365<sub>±0.029</sub> |    34.32<sub>±00.07</sub> + 0.01<sub>±00.00</sub> |     44.26<sub>±00.24</sub> |
|                 Go/gccgo |    2.385<sub>±0.171</sub> |    21.22<sub>±00.55</sub> + 0.00<sub>±00.00</sub> |     49.82<sub>±06.85</sub> |
|               Vala/clang |    2.445<sub>±0.128</sub> |     3.77<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     53.48<sub>±03.97</sub> |
|                    V/gcc |    2.448<sub>±0.027</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     46.70<sub>±00.85</sub> |
|                  Crystal |    2.519<sub>±0.106</sub> |     3.37<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     48.58<sub>±04.11</sub> |
|                    MLton |    2.603<sub>±0.026</sub> |     1.41<sub>±00.03</sub> + 0.25<sub>±00.00</sub> |     63.72<sub>±00.63</sub> |
|              Chez Scheme |    2.797<sub>±0.035</sub> |    24.89<sub>±00.04</sub> + 4.17<sub>±00.03</sub> |     64.46<sub>±01.42</sub> |
|                  V/clang |    2.859<sub>±0.092</sub> |     0.87<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     54.72<sub>±01.94</sub> |
|                    Julia |    3.061<sub>±0.114</sub> |   200.19<sub>±00.07</sub> + 0.61<sub>±00.03</sub> |     60.56<sub>±03.38</sub> |
|                    D/dmd |    3.520<sub>±0.106</sub> |     3.66<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     76.86<sub>±01.48</sub> |
|                    Scala |    3.540<sub>±0.033</sub> |   73.42<sub>±00.77</sub> + 44.39<sub>±02.28</sub> |     71.26<sub>±01.80</sub> |
|                  Node.js |    3.979<sub>±0.169</sub> |    29.37<sub>±00.04</sub> + 3.04<sub>±00.02</sub> |     85.86<sub>±06.41</sub> |
|                  C#/Mono |    4.303<sub>±0.098</sub> |    20.15<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     79.91<sub>±05.24</sub> |
|         Haskell (MArray) |    4.336<sub>±0.081</sub> |     3.67<sub>±00.04</sub> + 1.17<sub>±00.00</sub> |    102.58<sub>±03.61</sub> |
|               Lua/luajit |    6.412<sub>±0.229</sub> |     2.94<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    124.39<sub>±10.77</sub> |
|         Ruby/truffleruby |    9.842<sub>±0.389</sub> | 251.05<sub>±00.06</sub> + 751.21<sub>±17.20</sub> |    235.12<sub>±08.18</sub> |
| Ruby/truffleruby (--jvm) |   10.334<sub>±0.765</sub> | 563.35<sub>±06.15</sub> + 527.56<sub>±90.35</sub> |    352.45<sub>±13.58</sub> |
|              Python/pypy |   15.089<sub>±0.154</sub> |   63.10<sub>±00.03</sub> + 45.42<sub>±00.05</sub> |    309.13<sub>±03.52</sub> |
|                  Haskell |   15.832<sub>±0.547</sub> |     3.93<sub>±00.04</sub> + 0.88<sub>±00.00</sub> |    358.33<sub>±22.45</sub> |
|             Ruby (--jit) |   57.981<sub>±1.755</sub> |    14.06<sub>±00.06</sub> + 0.23<sub>±00.00</sub> |   1168.59<sub>±88.86</sub> |
|                      Lua |   58.537<sub>±0.243</sub> |     3.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   1104.39<sub>±09.98</sub> |
|                     Ruby |   88.135<sub>±4.503</sub> |    14.02<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   1829.33<sub>±99.56</sub> |
|               Ruby/jruby |  101.699<sub>±3.872</sub> | 202.58<sub>±04.58</sub> + 148.40<sub>±05.02</sub> |   2061.38<sub>±76.31</sub> |
|                   Elixir |  114.287<sub>±2.093</sub> |    56.70<sub>±01.01</sub> + 0.00<sub>±00.00</sub> |   2557.73<sub>±92.41</sub> |
|                   Python |  226.544<sub>±5.674</sub> |    10.44<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  4744.63<sub>±300.21</sub> |
|                 Tcl (FP) |  270.504<sub>±2.277</sub> |     4.23<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  5930.55<sub>±310.39</sub> |
|                     Perl |  357.665<sub>±9.538</sub> |     6.52<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  7318.76<sub>±401.40</sub> |
|                Tcl (OOP) | 537.000<sub>±14.265</sub> |     4.28<sub>±00.03</sub> + 0.00<sub>±00.00</sub> | 11271.28<sub>±673.92</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                    C/gcc |  12.383<sub>±0.348</sub> |     0.50<sub>±00.00</sub> + 1.13<sub>±00.00</sub> |   270.70<sub>±08.07</sub> |
|                  C++/g++ |  12.992<sub>±0.578</sub> |     1.50<sub>±00.01</sub> + 2.16<sub>±00.04</sub> |   274.10<sub>±18.31</sub> |
|                   D/ldc2 |  13.292<sub>±0.387</sub> |     3.05<sub>±00.04</sub> + 0.77<sub>±00.00</sub> |   279.58<sub>±13.52</sub> |
|                    D/gdc |  13.633<sub>±0.362</sub> |     6.61<sub>±00.07</sub> + 0.52<sub>±00.00</sub> |   283.07<sub>±16.67</sub> |
|                    V/gcc |  14.633<sub>±0.254</sub> |     1.17<sub>±00.66</sub> + 1.33<sub>±00.56</sub> |   265.64<sub>±07.53</sub> |
|                   Kotlin |  15.867<sub>±0.777</sub> |    38.79<sub>±00.07</sub> + 1.86<sub>±00.17</sub> |   323.60<sub>±25.09</sub> |
|                  Nim/gcc |  15.904<sub>±0.191</sub> |     1.84<sub>±00.04</sub> + 0.51<sub>±00.00</sub> |   361.97<sub>±11.82</sub> |
|  Racket (Syntax Objects) |  16.212<sub>±0.257</sub> |  109.92<sub>±00.07</sub> + 70.90<sub>±00.13</sub> |   371.05<sub>±13.45</sub> |
|                       Go |  16.522<sub>±0.483</sub> |     3.46<sub>±00.06</sub> + 1.28<sub>±00.01</sub> |   315.59<sub>±23.17</sub> |
|                     Rust |  17.527<sub>±0.598</sub> |     2.02<sub>±00.04</sub> + 0.28<sub>±00.03</sub> |   353.56<sub>±20.41</sub> |
|                  C/clang |  18.029<sub>±0.435</sub> |     0.50<sub>±00.00</sub> + 1.13<sub>±00.01</sub> |   342.99<sub>±03.97</sub> |
|             C#/.NET Core |  18.246<sub>±0.654</sub> |    34.30<sub>±00.10</sub> + 1.00<sub>±00.00</sub> |   365.05<sub>±36.17</sub> |
|                  V/clang |  20.060<sub>±0.249</sub> |     0.90<sub>±00.04</sub> + 2.01<sub>±00.14</sub> |   376.36<sub>±18.04</sub> |
|                Nim/clang |  20.566<sub>±0.634</sub> |     2.33<sub>±00.05</sub> + 0.51<sub>±00.00</sub> |   418.74<sub>±31.06</sub> |
|                     Java |  20.839<sub>±0.898</sub> |    38.23<sub>±00.13</sub> + 1.36<sub>±00.12</sub> |   440.21<sub>±16.34</sub> |
|                 Vala/gcc |  20.914<sub>±0.228</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   459.53<sub>±19.89</sub> |
|               Vala/clang |  21.290<sub>±0.799</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   455.16<sub>±34.67</sub> |
|                    Scala |  22.897<sub>±0.563</sub> |   72.32<sub>±00.22</sub> + 26.75<sub>±01.40</sub> |   516.08<sub>±19.78</sub> |
|                  Crystal |  23.608<sub>±0.416</sub> |     3.33<sub>±00.04</sub> + 0.40<sub>±00.02</sub> |   539.40<sub>±15.54</sub> |
|                 Go/gccgo |  24.522<sub>±0.908</sub> |    21.14<sub>±00.31</sub> + 1.29<sub>±00.00</sub> |   488.16<sub>±34.77</sub> |
|             F#/.NET Core |  33.330<sub>±1.009</sub> |    36.77<sub>±00.04</sub> + 2.09<sub>±00.04</sub> |   681.90<sub>±52.56</sub> |
|                    OCaml |  38.006<sub>±0.753</sub> |     3.88<sub>±00.01</sub> + 9.91<sub>±02.58</sub> |   717.17<sub>±20.45</sub> |
|                   Racket |  38.550<sub>±1.518</sub> |   116.04<sub>±00.23</sub> + 1.68<sub>±00.13</sub> |   750.19<sub>±34.12</sub> |
|              Chez Scheme |  38.818<sub>±0.638</sub> |    25.46<sub>±00.04</sub> + 3.65<sub>±00.02</sub> |   910.22<sub>±23.50</sub> |
|                  C#/Mono |  43.721<sub>±0.335</sub> |    20.25<sub>±00.11</sub> + 0.89<sub>±00.00</sub> |   969.42<sub>±28.34</sub> |
|                    D/dmd |  46.476<sub>±0.750</sub> |     3.63<sub>±00.02</sub> + 0.77<sub>±00.00</sub> |   893.25<sub>±39.57</sub> |
|                  Node.js |  46.617<sub>±0.270</sub> |    29.34<sub>±00.07</sub> + 5.59<sub>±00.07</sub> |   851.80<sub>±15.55</sub> |
|                    MLton |  50.512<sub>±0.466</sub> |     1.44<sub>±00.03</sub> + 4.11<sub>±00.00</sub> |  1146.03<sub>±39.15</sub> |
|         Haskell (MArray) |  59.919<sub>±0.420</sub> |     3.64<sub>±00.04</sub> + 2.68<sub>±00.00</sub> |  1372.34<sub>±22.61</sub> |
|              Python/pypy |  65.541<sub>±0.416</sub> |   63.29<sub>±00.08</sub> + 45.95<sub>±00.07</sub> |  1543.03<sub>±13.74</sub> |
|                    Julia |  77.758<sub>±0.837</sub> |   200.70<sub>±00.17</sub> + 0.64<sub>±00.03</sub> |  1465.14<sub>±25.78</sub> |
| Ruby/truffleruby (--jvm) | 127.673<sub>±2.631</sub> | 567.51<sub>±03.09</sub> + 460.57<sub>±11.26</sub> |  2748.11<sub>±26.80</sub> |
|         Ruby/truffleruby | 166.906<sub>±3.606</sub> | 251.32<sub>±00.11</sub> + 764.61<sub>±05.09</sub> | 3378.39<sub>±183.24</sub> |
|                  Haskell | 210.405<sub>±3.191</sub> |    3.88<sub>±00.07</sub> + 26.16<sub>±00.00</sub> | 4662.28<sub>±238.57</sub> |
|               Lua/luajit | 235.639<sub>±2.049</sub> |     2.89<sub>±00.04</sub> + 0.86<sub>±00.00</sub> |  5308.03<sub>±75.14</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.153<sub>±0.002</sub> |     1.91<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   3.58<sub>±00.07</sub> |
|                      Rust |  1.163<sub>±0.016</sub> |     2.54<sub>±00.05</sub> + 0.01<sub>±00.00</sub> |  23.27<sub>±01.58</sub> |
|                     C/gcc |  1.179<sub>±0.014</sub> |     1.91<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  23.10<sub>±01.62</sub> |
|                     V/gcc |  1.570<sub>±0.072</sub> |     1.49<sub>±00.01</sub> + 0.64<sub>±00.05</sub> |  29.90<sub>±02.52</sub> |
|                   Crystal |  1.592<sub>±0.007</sub> |     3.77<sub>±00.04</sub> + 1.82<sub>±00.04</sub> |  37.17<sub>±00.21</sub> |
|                 Nim/clang |  1.792<sub>±0.027</sub> |     2.73<sub>±00.03</sub> + 4.44<sub>±00.03</sub> |  32.45<sub>±00.45</sub> |
|                   V/clang |  1.876<sub>±0.048</sub> |     2.01<sub>±00.03</sub> + 0.48<sub>±00.05</sub> |  34.20<sub>±00.85</sub> |
|                   Nim/gcc |  1.947<sub>±0.012</sub> |     2.21<sub>±00.04</sub> + 4.44<sub>±00.00</sub> |  45.11<sub>±00.74</sub> |
|                    D/ldc2 |  1.964<sub>±0.017</sub> |     3.47<sub>±00.04</sub> + 3.67<sub>±00.00</sub> |  35.71<sub>±00.39</sub> |
|                     D/gdc |  2.014<sub>±0.088</sub> |     6.94<sub>±00.06</sub> + 3.40<sub>±00.01</sub> |  41.01<sub>±03.52</sub> |
|                Vala/clang |  2.045<sub>±0.013</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  46.59<sub>±00.56</sub> |
|                  Vala/gcc |  2.097<sub>±0.078</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  43.87<sub>±04.60</sub> |
|                      Java |  2.168<sub>±0.028</sub> |  38.77<sub>±00.57</sub> + 292.57<sub>±09.58</sub> |  49.56<sub>±01.80</sub> |
|                      Ruby |  2.260<sub>±0.044</sub> |   14.44<sub>±00.07</sub> + 56.96<sub>±01.09</sub> |  52.10<sub>±01.34</sub> |
|              Ruby (--jit) |  2.277<sub>±0.044</sub> |   14.45<sub>±00.04</sub> + 57.15<sub>±01.26</sub> |  54.06<sub>±01.35</sub> |
|                    Kotlin |  2.340<sub>±0.073</sub> |  39.65<sub>±00.09</sub> + 324.10<sub>±06.73</sub> |  52.17<sub>±02.65</sub> |
|       C++/g++ (libcrypto) |  2.437<sub>±0.042</sub> |     5.40<sub>±00.02</sub> + 0.07<sub>±00.00</sub> |  55.85<sub>±02.79</sub> |
|                     Scala |  2.481<sub>±0.027</sub> |   72.77<sub>±00.72</sub> + 53.74<sub>±00.71</sub> |  47.64<sub>±01.32</sub> |
|                        Go |  2.577<sub>±0.007</sub> |     4.60<sub>±00.02</sub> + 5.39<sub>±00.18</sub> |  47.73<sub>±00.23</sub> |
|       Perl (MIME::Base64) |  3.011<sub>±0.065</sub> |    14.11<sub>±00.05</sub> + 0.02<sub>±00.00</sub> |  54.11<sub>±02.29</sub> |
|                   Node.js |  3.012<sub>±0.042</sub> | 29.81<sub>±00.03</sub> + 1029.18<sub>±00.09</sub> |  61.62<sub>±02.69</sub> |
|                       PHP |  3.138<sub>±0.012</sub> |    15.75<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |  55.64<sub>±00.51</sub> |
|                  Go/gccgo |  3.511<sub>±0.006</sub> |    21.92<sub>±00.07</sub> + 7.32<sub>±00.08</sub> |  72.72<sub>±01.09</sub> |
|                    Python |  3.773<sub>±0.019</sub> |    10.14<sub>±00.02</sub> + 0.18<sub>±00.00</sub> |  86.88<sub>±01.61</sub> |
|                     D/dmd |  3.796<sub>±0.063</sub> |     3.75<sub>±00.07</sub> + 3.67<sub>±00.06</sub> |  67.42<sub>±01.89</sub> |
|                       Tcl |  4.490<sub>±0.060</sub> |     4.80<sub>±00.10</sub> + 0.19<sub>±00.01</sub> | 100.65<sub>±02.32</sub> |
|               Python/pypy |  4.958<sub>±0.117</sub> |   63.26<sub>±00.03</sub> + 45.74<sub>±00.03</sub> |  87.84<sub>±01.85</sub> |
|              F#/.NET Core |  5.444<sub>±0.095</sub> |   37.08<sub>±00.01</sub> + 36.06<sub>±08.40</sub> | 102.98<sub>±04.55</sub> |
|              C#/.NET Core |  5.743<sub>±0.018</sub> |   34.61<sub>±00.11</sub> + 45.08<sub>±03.51</sub> | 109.02<sub>±00.99</sub> |
|  Ruby/truffleruby (--jvm) |  5.981<sub>±0.126</sub> | 563.95<sub>±05.11</sub> + 298.42<sub>±40.13</sub> | 118.70<sub>±03.89</sub> |
|                     Julia |  6.196<sub>±0.182</sub> |  230.53<sub>±00.16</sub> + 63.93<sub>±00.07</sub> | 125.55<sub>±07.91</sub> |
|                   C#/Mono |  7.623<sub>±0.027</sub> |   20.73<sub>±00.04</sub> + 18.48<sub>±00.03</sub> | 145.13<sub>±01.95</sub> |
|                Ruby/jruby | 10.310<sub>±0.514</sub> | 188.91<sub>±04.86</sub> + 136.05<sub>±03.99</sub> | 220.35<sub>±16.58</sub> |
| Perl (MIME::Base64::Perl) | 16.248<sub>±0.556</sub> |    15.50<sub>±00.07</sub> + 0.20<sub>±00.07</sub> | 341.96<sub>±31.97</sub> |
|          Ruby/truffleruby | 21.205<sub>±0.687</sub> | 241.12<sub>±00.59</sub> + 418.93<sub>±00.56</sub> | 430.14<sub>±34.34</sub> |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|                        Language |                 Time, s |                                         Memory, MiB |               Energy, J |
| :------------------------------ | ----------------------: | --------------------------------------------------: | ----------------------: |
|         C++/g++ (DAW JSON Link) |  0.079<sub>±0.002</sub> |     109.22<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   1.92<sub>±00.07</sub> |
|    C++/g++ (simdjson On-Demand) |  0.081<sub>±0.002</sub> |    109.87<sub>±00.03</sub> + 59.55<sub>±00.00</sub> |   1.86<sub>±00.11</sub> |
|                    D/gdc (fast) |  0.099<sub>±0.002</sub> |    219.96<sub>±00.07</sub> + 11.34<sub>±00.00</sub> |   2.44<sub>±00.17</sub> |
|              Rust (Serde Typed) |  0.125<sub>±0.004</sub> |    108.53<sub>±00.05</sub> + 11.77<sub>±00.26</sub> |   3.01<sub>±00.16</sub> |
|             Rust (Serde Custom) |  0.133<sub>±0.002</sub> |     108.38<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   2.53<sub>±00.12</sub> |
|          C++/g++ (simdjson DOM) |  0.148<sub>±0.004</sub> |   109.88<sub>±00.04</sub> + 176.60<sub>±00.00</sub> |   3.69<sub>±00.23</sub> |
|                 C++/g++ (gason) |  0.159<sub>±0.001</sub> |    109.21<sub>±00.01</sub> + 97.17<sub>±00.03</sub> |   3.02<sub>±00.09</sub> |
|             C++/g++ (RapidJSON) |  0.221<sub>±0.006</sub> |   109.24<sub>±00.01</sub> + 128.82<sub>±00.00</sub> |   4.56<sub>±00.48</sub> |
|            C++/g++ (Boost.JSON) |  0.498<sub>±0.009</sub> |   109.81<sub>±00.03</sub> + 435.70<sub>±00.00</sub> |  10.00<sub>±00.67</sub> |
|                            Java |  0.508<sub>±0.007</sub> |    253.80<sub>±00.21</sub> + 70.03<sub>±01.09</sub> |  13.87<sub>±00.33</sub> |
|         C++/g++ (RapidJSON SAX) |  0.530<sub>±0.008</sub> |     109.44<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  11.80<sub>±00.31</sub> |
|                           Scala |  0.589<sub>±0.011</sub> |    307.27<sub>±00.60</sub> + 74.61<sub>±00.32</sub> |  14.93<sub>±00.66</sub> |
|                         Node.js |  0.632<sub>±0.026</sub> |   242.44<sub>±00.02</sub> + 185.01<sub>±00.27</sub> |  15.47<sub>±01.24</sub> |
|                   Go (jsoniter) |  0.653<sub>±0.014</sub> |    224.31<sub>±00.14</sub> + 13.69<sub>±00.22</sub> |  14.46<sub>±00.73</sub> |
|                Crystal (Schema) |  0.783<sub>±0.008</sub> |    110.38<sub>±00.03</sub> + 46.88<sub>±00.11</sub> |  12.28<sub>±00.20</sub> |
|            Rust (Serde Untyped) |  0.796<sub>±0.010</sub> |   108.39<sub>±00.04</sub> + 839.98<sub>±00.00</sub> |  18.27<sub>±00.23</sub> |
|                  Crystal (Pull) |  0.796<sub>±0.021</sub> |    110.35<sub>±00.04</sub> + 18.24<sub>±00.02</sub> |  12.41<sub>±00.16</sub> |
|                     Python/pypy |  0.798<sub>±0.031</sub> |   276.56<sub>±00.05</sub> + 127.96<sub>±00.00</sub> |  17.11<sub>±01.65</sub> |
|                         V/clang |  0.829<sub>±0.011</sub> |   108.35<sub>±00.02</sub> + 484.14<sub>±00.06</sub> |  15.42<sub>±00.61</sub> |
|                           V/gcc |  0.857<sub>±0.009</sub> |   107.64<sub>±00.26</sub> + 484.30<sub>±00.21</sub> |  20.14<sub>±00.74</sub> |
|                   Julia (JSON3) |  0.881<sub>±0.014</sub> |   388.56<sub>±05.34</sub> + 356.37<sub>±01.07</sub> |  20.51<sub>±00.45</sub> |
|         Perl (Cpanel::JSON::XS) |  0.949<sub>±0.024</sub> |   121.29<sub>±00.05</sub> + 402.72<sub>±00.00</sub> |  21.81<sub>±01.24</sub> |
| C#/.NET Core (System.Text.Json) |  0.969<sub>±0.028</sub> |   465.40<sub>±00.09</sub> + 135.69<sub>±00.00</sub> |  23.44<sub>±00.51</sub> |
|                         Crystal |  1.055<sub>±0.035</sub> |   110.32<sub>±00.08</sub> + 393.34<sub>±00.02</sub> |  19.27<sub>±00.67</sub> |
|                              Go |  1.143<sub>±0.032</sub> |    113.77<sub>±00.07</sub> + 83.23<sub>±00.08</sub> |  20.41<sub>±00.39</sub> |
|                             PHP |  1.196<sub>±0.016</sub> |   121.52<sub>±00.13</sub> + 682.01<sub>±00.00</sub> |  25.31<sub>±00.62</sub> |
|                C++/g++ (json-c) |  1.450<sub>±0.021</sub> |  109.39<sub>±00.03</sub> + 1216.07<sub>±00.00</sub> |  34.03<sub>±00.76</sub> |
|            Nim/gcc (Packedjson) |  1.456<sub>±0.049</sub> |   108.80<sub>±00.01</sub> + 290.55<sub>±00.00</sub> |  31.80<sub>±02.20</sub> |
|          Nim/clang (Packedjson) |  1.466<sub>±0.033</sub> |   109.27<sub>±00.02</sub> + 290.55<sub>±00.00</sub> |  33.38<sub>±02.31</sub> |
|                        Go/gccgo |  1.495<sub>±0.023</sub> |    132.58<sub>±00.29</sub> + 95.95<sub>±00.18</sub> |  26.33<sub>±00.47</sub> |
|                         Clojure |  1.556<sub>±0.049</sub> |   469.16<sub>±04.90</sub> + 621.88<sub>±14.07</sub> |  41.43<sub>±02.27</sub> |
|                    C#/.NET Core |  1.696<sub>±0.045</sub> |   474.30<sub>±00.10</sub> + 288.66<sub>±00.06</sub> |  31.59<sub>±01.35</sub> |
|                          Python |  1.736<sub>±0.044</sub> |   116.73<sub>±00.01</sub> + 377.21<sub>±00.00</sub> |  37.67<sub>±02.41</sub> |
|             CPython (UltraJSON) |  1.758<sub>±0.036</sub> |   118.43<sub>±00.03</sub> + 545.83<sub>±02.36</sub> |  38.09<sub>±01.08</sub> |
|                         Haskell |  1.856<sub>±0.018</sub> |       4.70<sub>±00.10</sub> + 5.02<sub>±00.03</sub> |  34.28<sub>±00.63</sub> |
|                         Nim/gcc |  1.904<sub>±0.042</sub> |   108.78<sub>±00.04</sub> + 919.36<sub>±00.00</sub> |  35.23<sub>±00.52</sub> |
|                       Nim/clang |  1.918<sub>±0.024</sub> |   109.19<sub>±00.05</sub> + 919.42<sub>±00.06</sub> |  35.93<sub>±01.38</sub> |
|                         C#/Mono |  2.060<sub>±0.055</sub> |     462.41<sub>±00.12</sub> + 0.18<sub>±00.00</sub> |  46.42<sub>±03.45</sub> |
|              C++/g++ (Nlohmann) |  2.173<sub>±0.030</sub> |   109.33<sub>±00.01</sub> + 448.31<sub>±00.00</sub> |  43.76<sub>±00.42</sub> |
|                           D/gdc |  2.227<sub>±0.080</sub> |   113.16<sub>±00.06</sub> + 600.28<sub>±00.00</sub> |  49.11<sub>±01.80</sub> |
|                            Ruby |  2.286<sub>±0.056</sub> |   120.58<sub>±00.03</sub> + 410.67<sub>±00.01</sub> |  43.07<sub>±01.79</sub> |
|                     Ruby (YAJL) |  2.300<sub>±0.082</sub> |   120.54<sub>±00.02</sub> + 281.58<sub>±00.00</sub> |  48.46<sub>±03.87</sub> |
|                    Ruby (--jit) |  2.335<sub>±0.039</sub> |   120.62<sub>±00.01</sub> + 410.78<sub>±00.01</sub> |  45.49<sub>±01.11</sub> |
| F#/.NET Core (System.Text.Json) |  2.526<sub>±0.051</sub> |   471.70<sub>±00.09</sub> + 444.59<sub>±02.96</sub> |  46.50<sub>±00.91</sub> |
|                          D/ldc2 |  2.541<sub>±0.050</sub> |   109.54<sub>±00.09</sub> + 680.29<sub>±00.02</sub> |  49.03<sub>±01.87</sub> |
|                       Rust (jq) |  3.587<sub>±0.033</sub> |   110.40<sub>±00.02</sub> + 774.21<sub>±00.64</sub> |  75.45<sub>±03.74</sub> |
|                      Ruby/jruby |  3.641<sub>±0.138</sub> |  466.71<sub>±05.85</sub> + 1379.71<sub>±38.93</sub> |  98.45<sub>±07.39</sub> |
|    C++/g++ (Boost.PropertyTree) |  4.423<sub>±0.178</sub> |  109.56<sub>±00.06</sub> + 1440.06<sub>±00.00</sub> |  97.00<sub>±07.24</sub> |
|                           D/dmd |  4.910<sub>±0.071</sub> |   110.14<sub>±00.04</sub> + 680.23<sub>±00.04</sub> |  99.16<sub>±03.16</sub> |
|                      Vala/clang |  5.199<sub>±0.194</sub> |       0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 107.75<sub>±09.21</sub> |
|                        Vala/gcc |  5.574<sub>±0.040</sub> |       0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 101.91<sub>±02.17</sub> |
|               Perl (JSON::Tiny) | 11.829<sub>±0.286</sub> |   121.94<sub>±00.07</sub> + 525.12<sub>±00.04</sub> | 245.67<sub>±18.39</sub> |
|        Ruby/truffleruby (--jvm) | 18.258<sub>±0.459</sub> |  735.70<sub>±12.50</sub> + 1571.42<sub>±34.39</sub> | 490.21<sub>±05.42</sub> |
|                Ruby/truffleruby | 40.868<sub>±0.786</sub> | 732.58<sub>±00.25</sub> + 2351.77<sub>±120.46</sub> | 916.47<sub>±58.11</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.060<sub>±0.001</sub> |    6.71<sub>±00.07</sub> + 55.45<sub>±00.17</sub> |      3.45<sub>±00.02</sub> |
|           Python (NumPy) |    0.094<sub>±0.001</sub> |   27.66<sub>±00.09</sub> + 57.68<sub>±00.03</sub> |      5.36<sub>±00.09</sub> |
|  Nim/clang (Arraymancer) |    0.147<sub>±0.041</sub> |    6.51<sub>±00.06</sub> + 57.42<sub>±00.01</sub> |      7.35<sub>±01.78</sub> |
|    Nim/gcc (Arraymancer) |    0.167<sub>±0.011</sub> |    5.78<sub>±00.04</sub> + 57.66<sub>±00.02</sub> |      8.97<sub>±00.55</sub> |
|          C++/g++ (Eigen) |    0.186<sub>±0.003</sub> |    3.54<sub>±00.03</sub> + 85.25<sub>±00.00</sub> |      3.76<sub>±00.03</sub> |
|              Java (ND4J) |    0.220<sub>±0.013</sub> |  144.24<sub>±01.82</sub> + 87.51<sub>±00.10</sub> |      9.75<sub>±00.49</sub> |
|       Julia (threads: 8) |    0.293<sub>±0.003</sub> |  238.60<sub>±00.19</sub> + 30.86<sub>±00.23</sub> |     15.18<sub>±00.33</sub> |
|       Julia (threads: 1) |    0.632<sub>±0.005</sub> |  238.50<sub>±00.24</sub> + 31.28<sub>±00.17</sub> |     11.72<sub>±00.30</sub> |
|          Julia (no BLAS) |    1.144<sub>±0.014</sub> |  217.53<sub>±00.18</sub> + 51.52<sub>±00.00</sub> |     32.78<sub>±01.92</sub> |
|                   D/ldc2 |    1.960<sub>±0.006</sub> |    3.60<sub>±00.02</sub> + 70.11<sub>±00.00</sub> |     44.05<sub>±00.87</sub> |
|                    D/dmd |    2.092<sub>±0.007</sub> |    3.55<sub>±00.04</sub> + 70.12<sub>±00.00</sub> |     46.98<sub>±00.40</sub> |
|                    D/gdc |    2.137<sub>±0.044</sub> |    6.64<sub>±00.08</sub> + 70.71<sub>±00.00</sub> |     45.60<sub>±00.88</sub> |
|                     Java |    3.304<sub>±0.007</sub> |   38.96<sub>±00.19</sub> + 80.65<sub>±00.49</sub> |     74.63<sub>±00.99</sub> |
|                    C/gcc |    3.307<sub>±0.009</sub> |    2.04<sub>±00.06</sub> + 68.06<sub>±00.00</sub> |     76.20<sub>±01.67</sub> |
|                 Vala/gcc |    3.336<sub>±0.006</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     67.47<sub>±01.18</sub> |
|                    Scala |    3.356<sub>±0.049</sub> |   74.48<sub>±00.12</sub> + 74.42<sub>±03.95</sub> |     77.03<sub>±00.89</sub> |
|               Vala/clang |    3.430<sub>±0.006</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     72.64<sub>±00.29</sub> |
|                  Nim/gcc |    3.435<sub>±0.014</sub> |    2.58<sub>±00.05</sub> + 71.29<sub>±01.16</sub> |     71.30<sub>±01.11</sub> |
|                Nim/clang |    3.473<sub>±0.011</sub> |    3.08<sub>±00.05</sub> + 75.54<sub>±04.77</sub> |     71.60<sub>±01.04</sub> |
|                     Rust |    3.519<sub>±0.062</sub> |    2.62<sub>±00.06</sub> + 68.32<sub>±00.00</sub> |     69.38<sub>±01.98</sub> |
|                       Go |    3.572<sub>±0.018</sub> |    3.85<sub>±00.06</sub> + 73.33<sub>±00.18</sub> |     76.52<sub>±01.11</sub> |
|                 Go/gccgo |    3.628<sub>±0.054</sub> |   21.31<sub>±00.19</sub> + 72.99<sub>±00.12</sub> |     75.17<sub>±00.55</sub> |
|                  Crystal |    3.637<sub>±0.022</sub> |    4.18<sub>±00.12</sub> + 59.66<sub>±00.06</sub> |     78.60<sub>±01.31</sub> |
|                    Swift |    3.694<sub>±0.083</sub> |  149.00<sub>±00.05</sub> + 59.61<sub>±00.14</sub> |     83.34<sub>±04.81</sub> |
|                  V/clang |    3.724<sub>±0.041</sub> |    2.43<sub>±00.03</sub> + 68.84<sub>±00.00</sub> |     72.42<sub>±00.88</sub> |
|                  Node.js |    3.759<sub>±0.033</sub> |   32.61<sub>±00.12</sub> + 71.54<sub>±00.52</sub> |     87.65<sub>±01.63</sub> |
|                    V/gcc |    4.503<sub>±0.195</sub> |    1.97<sub>±00.03</sub> + 68.84<sub>±00.00</sub> |     90.31<sub>±09.35</sub> |
|                   Kotlin |    4.679<sub>±0.044</sub> |   38.06<sub>±00.15</sub> + 81.76<sub>±00.44</sub> |     84.14<sub>±02.52</sub> |
|              Python/pypy |    6.009<sub>±0.117</sub> |   63.57<sub>±00.05</sub> + 69.19<sub>±00.03</sub> |    136.97<sub>±07.06</sub> |
|             C#/.NET Core |    7.288<sub>±0.069</sub> |   33.94<sub>±00.09</sub> + 69.14<sub>±00.03</sub> |    127.95<sub>±02.59</sub> |
|                  C#/Mono |   10.932<sub>±0.482</sub> |   20.16<sub>±00.05</sub> + 69.02<sub>±00.03</sub> |    223.53<sub>±26.05</sub> |
|         Ruby/truffleruby |   50.394<sub>±0.352</sub> | 523.19<sub>±00.38</sub> + 740.58<sub>±02.09</sub> |   1028.56<sub>±24.84</sub> |
| Ruby/truffleruby (--jvm) |   70.186<sub>±1.667</sub> | 616.78<sub>±05.98</sub> + 388.84<sub>±31.46</sub> |  1909.49<sub>±118.30</sub> |
|                     Ruby |  215.251<sub>±5.288</sub> |   15.17<sub>±00.07</sub> + 68.64<sub>±00.00</sub> |  4613.14<sub>±297.52</sub> |
|             Ruby (--jit) |  217.599<sub>±4.770</sub> |   15.19<sub>±00.04</sub> + 68.93<sub>±00.00</sub> |  4331.21<sub>±327.51</sub> |
|                   Python |  228.084<sub>±1.992</sub> |   10.49<sub>±00.02</sub> + 68.58<sub>±00.00</sub> |  5019.49<sub>±156.86</sub> |
|                      Tcl |  360.078<sub>±7.343</sub> |   7.24<sub>±00.04</sub> + 400.44<sub>±00.03</sub> |  7082.90<sub>±189.15</sub> |
|                     Perl |  401.651<sub>±3.072</sub> |   9.02<sub>±00.04</sub> + 599.67<sub>±00.08</sub> |  8472.06<sub>±125.86</sub> |
|               Ruby/jruby | 498.988<sub>±15.692</sub> | 276.05<sub>±05.82</sub> + 983.96<sub>±46.89</sub> | 10919.65<sub>±202.22</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.104                         |
| C#/.NET Core     | 3.8.0-5.20604.10 (9ed4b774)     |
| C#/Mono          | 6.12.0.122                      |
| C/clang          | 11.0.1                          |
| C/gcc            | 10.2.1                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.3"                        |
| Crystal          | 1.0.0                           |
| D/dmd            | v2.096.0                        |
| D/gdc            | 10.2.1                          |
| D/ldc2           | 1.25.1                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 11.0.0.0 for F# 5.0             |
| Go               | go1.16.2                        |
| Go/gccgo         | 10.2.1                          |
| Haskell          | 8.10.4                          |
| Java             | 16                              |
| Julia            | v"1.6.0"                        |
| Kotlin           | 1.4.31                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20210117                        |
| Nim              | 1.4.4                           |
| Node.js          | v15.12.0                        |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.15                          |
| Perl             | v5.32.1                         |
| Python           | 3.9.2                           |
| Python/pypy      | 7.3.3-beta0 for Python 3.7.9    |
| Racket           | "8.0"                           |
| Ruby             | 3.0.0p0                         |
| Ruby/jruby       | 9.2.16.0                        |
| Ruby/truffleruby | 21.0.0.2                        |
| Rust             | 1.51.0                          |
| Scala            | 2.13.5                          |
| Swift            | swift-5.3.3-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.2.2                           |
| Vala             | 0.48.14                         |

## Using Docker

Build the image:

    $ docker build docker/ -t benchmarks

Run the image:

    $ docker run -it --rm -v $(pwd):/src benchmarks <cmd>

where <cmd> is:

 - `versions` (print installed language versions);
 - `shell` (start the shell);
 - `brainfuck bench` (build and run Brainfuck bench.b benchmarks);
 - `brainfuck mandel` (build and run Brainfuck mandel.b benchmarks);
 - `base64` (build and run Base64 benchmarks);
 - `json` (build and run Json benchmarks);
 - `matmul` (build and run Matmul benchmarks);

Please note that the actual measurements provided in the project are taken semi-manually (via `shell`) as the full update takes days and could have occassional issues in Docker.

There is a `Makefile` that could be used to simlify Docker usage:

 - `make build` (build the image);
 - `make versions` (run the image with the `versions` command);
 - `make shell` (run the image with the `shell' command);
 - `make toc` (utility rule to update ToC in this README, requires
[git-markdown-toc](https://github.com/ildar-shaimordanov/git-markdown-toc)
available in `PATH`).

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

For Lua:

 - [LuaRocks](https://luarocks.org/) for installing dependencies
(Debian package `luarocks`).

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
