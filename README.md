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

UPDATE: 2021-09-09

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
|  Racket (Syntax Objects) |   1.404<sub>±0.026</sub> |   111.62<sub>±00.16</sub> + 0.00<sub>±00.00</sub> |     32.75<sub>±01.55</sub> |
|                  C++/g++ |   1.446<sub>±0.015</sub> |     1.51<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     31.37<sub>±00.58</sub> |
|                   Kotlin |   1.774<sub>±0.048</sub> |    40.81<sub>±00.11</sub> + 0.52<sub>±00.04</sub> |     38.17<sub>±01.72</sub> |
|                     Rust |   1.799<sub>±0.015</sub> |     1.97<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     36.99<sub>±00.51</sub> |
|                      Zig |   1.865<sub>±0.020</sub> |     0.94<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     41.67<sub>±00.54</sub> |
|                   D/ldc2 |   1.876<sub>±0.075</sub> |     3.10<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     34.05<sub>±02.45</sub> |
|                    D/gdc |   1.911<sub>±0.065</sub> |     6.73<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     41.62<sub>±02.28</sub> |
|                    C/gcc |   1.953<sub>±0.032</sub> |     0.50<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     35.71<sub>±02.32</sub> |
|                  Nim/gcc |   1.959<sub>±0.025</sub> |     1.89<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     43.84<sub>±00.94</sub> |
|                       Go |   1.976<sub>±0.029</sub> |     3.37<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     43.91<sub>±01.56</sub> |
|                 Vala/gcc |   2.115<sub>±0.010</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     46.75<sub>±01.23</sub> |
|                     Java |   2.157<sub>±0.092</sub> |    38.01<sub>±00.15</sub> + 0.79<sub>±00.15</sub> |     43.32<sub>±03.28</sub> |
|                 Go/gccgo |   2.163<sub>±0.165</sub> |    22.34<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |     43.79<sub>±06.58</sub> |
|             F#/.NET Core |   2.244<sub>±0.025</sub> |    36.84<sub>±00.05</sub> + 0.30<sub>±00.00</sub> |     48.65<sub>±00.53</sub> |
|                Nim/clang |   2.251<sub>±0.081</sub> |     2.29<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     46.90<sub>±04.04</sub> |
|             C#/.NET Core |   2.285<sub>±0.022</sub> |    34.50<sub>±00.08</sub> + 0.01<sub>±00.00</sub> |     51.83<sub>±01.17</sub> |
|                    OCaml |   2.293<sub>±0.047</sub> |     2.58<sub>±00.02</sub> + 2.50<sub>±00.01</sub> |     41.93<sub>±02.14</sub> |
|                   Racket |   2.295<sub>±0.115</sub> |    95.89<sub>±00.34</sub> + 0.00<sub>±00.00</sub> |     52.29<sub>±01.78</sub> |
|                  C/clang |   2.307<sub>±0.049</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     42.99<sub>±01.28</sub> |
|               Vala/clang |   2.381<sub>±0.059</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     54.80<sub>±01.56</sub> |
|                    V/gcc |   2.457<sub>±0.014</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     56.30<sub>±00.93</sub> |
|                  Crystal |   2.582<sub>±0.078</sub> |     3.41<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     46.54<sub>±04.33</sub> |
|                    MLton |   2.653<sub>±0.027</sub> |     1.44<sub>±00.04</sub> + 0.25<sub>±00.00</sub> |     62.54<sub>±01.96</sub> |
|              Chez Scheme |   2.964<sub>±0.119</sub> |    24.90<sub>±00.03</sub> + 4.23<sub>±00.06</sub> |     59.80<sub>±06.38</sub> |
|                  V/clang |   3.121<sub>±0.030</sub> |     0.55<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     68.63<sub>±02.39</sub> |
|                    Julia |   3.300<sub>±0.069</sub> |   199.35<sub>±00.15</sub> + 0.84<sub>±00.00</sub> |     52.82<sub>±01.15</sub> |
|                    D/dmd |   3.468<sub>±0.058</sub> |     3.71<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     75.26<sub>±01.59</sub> |
|                    Scala |   3.635<sub>±0.105</sub> |  55.66<sub>±00.09</sub> + 276.16<sub>±07.97</sub> |     73.67<sub>±04.83</sub> |
|                  Node.js |   4.233<sub>±0.227</sub> |    31.89<sub>±00.08</sub> + 1.39<sub>±00.00</sub> |     76.37<sub>±04.22</sub> |
|                  C#/Mono |   4.459<sub>±0.022</sub> |    20.27<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     75.09<sub>±02.77</sub> |
|         Haskell (MArray) |   5.023<sub>±0.091</sub> |     3.81<sub>±00.03</sub> + 1.11<sub>±00.00</sub> |     93.41<sub>±01.80</sub> |
|                    Swift |   5.814<sub>±0.148</sub> |    13.20<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    124.36<sub>±03.45</sub> |
|               Lua/luajit |   6.609<sub>±0.141</sub> |     2.20<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    139.84<sub>±06.53</sub> |
| Ruby/truffleruby (--jvm) |   6.774<sub>±0.451</sub> | 627.62<sub>±13.07</sub> + 557.04<sub>±18.72</sub> |    257.19<sub>±16.15</sub> |
|         Ruby/truffleruby |   9.451<sub>±0.287</sub> | 434.84<sub>±01.43</sub> + 576.89<sub>±11.61</sub> |    243.73<sub>±08.03</sub> |
|              Python/pypy |  14.256<sub>±0.236</sub> |   65.37<sub>±00.12</sub> + 45.65<sub>±00.11</sub> |    351.35<sub>±09.41</sub> |
|                  Haskell |  16.138<sub>±0.393</sub> |     3.88<sub>±00.13</sub> + 0.82<sub>±00.03</sub> |    353.66<sub>±17.70</sub> |
|                      Lua |  58.225<sub>±1.973</sub> |     2.94<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   1185.67<sub>±78.13</sub> |
|             Ruby (--jit) |  61.436<sub>±1.051</sub> |    14.09<sub>±00.08</sub> + 0.24<sub>±00.00</sub> |   1095.41<sub>±11.70</sub> |
|                   Elixir |  61.635<sub>±0.330</sub> |    76.38<sub>±00.79</sub> + 0.08<sub>±00.08</sub> |   1075.24<sub>±17.58</sub> |
|                     Ruby |  98.127<sub>±2.169</sub> |    14.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   2048.86<sub>±47.72</sub> |
|               Ruby/jruby | 110.606<sub>±1.951</sub> | 186.16<sub>±03.75</sub> + 144.43<sub>±05.61</sub> |   2250.98<sub>±68.33</sub> |
|                   Python | 236.129<sub>±5.773</sub> |    10.22<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  4576.59<sub>±171.41</sub> |
|                 Tcl (FP) | 274.398<sub>±3.211</sub> |     4.24<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  5942.43<sub>±309.70</sub> |
|                     Perl | 350.999<sub>±3.863</sub> |     6.54<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |   7697.59<sub>±76.67</sub> |
|                Tcl (OOP) | 544.444<sub>±8.730</sub> |     4.30<sub>±00.01</sub> + 0.00<sub>±00.00</sub> | 11838.47<sub>±313.36</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                  C++/g++ |  11.375<sub>±0.214</sub> |     2.41<sub>±00.89</sub> + 1.38<sub>±00.86</sub> |   231.20<sub>±09.59</sub> |
|                    C/gcc |  12.380<sub>±0.250</sub> |     0.55<sub>±00.01</sub> + 1.11<sub>±00.02</sub> |   269.00<sub>±13.83</sub> |
|                     Rust |  13.491<sub>±0.112</sub> |     1.98<sub>±00.01</sub> + 0.25<sub>±00.00</sub> |   260.44<sub>±03.52</sub> |
|                   D/ldc2 |  14.011<sub>±0.056</sub> |     3.12<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |   305.63<sub>±09.08</sub> |
|                    D/gdc |  16.307<sub>±0.312</sub> |     6.81<sub>±00.07</sub> + 0.57<sub>±00.00</sub> |   359.78<sub>±10.41</sub> |
|                    V/gcc |  16.537<sub>±0.549</sub> |     1.63<sub>±00.05</sub> + 1.03<sub>±00.00</sub> |   335.96<sub>±26.43</sub> |
|                   Kotlin |  16.718<sub>±0.752</sub> |    40.71<sub>±00.10</sub> + 1.14<sub>±00.21</sub> |   332.15<sub>±22.31</sub> |
|  Racket (Syntax Objects) |  16.890<sub>±0.778</sub> |  111.58<sub>±00.11</sub> + 68.96<sub>±00.13</sub> |   337.21<sub>±25.89</sub> |
|                  C/clang |  17.631<sub>±0.389</sub> |     0.55<sub>±00.01</sub> + 1.11<sub>±00.03</sub> |   413.69<sub>±10.81</sub> |
|                       Go |  17.707<sub>±0.271</sub> |     3.39<sub>±00.05</sub> + 1.26<sub>±00.00</sub> |   328.24<sub>±19.00</sub> |
|                      Zig |  17.780<sub>±0.354</sub> |     1.87<sub>±00.02</sub> + 0.52<sub>±00.00</sub> |   320.96<sub>±20.18</sub> |
|             C#/.NET Core |  18.056<sub>±0.322</sub> |    34.42<sub>±00.12</sub> + 1.00<sub>±00.00</sub> |   389.70<sub>±10.63</sub> |
|                  Nim/gcc |  19.117<sub>±0.441</sub> |     1.84<sub>±00.05</sub> + 0.51<sub>±00.00</sub> |   350.19<sub>±32.59</sub> |
|                 Vala/gcc |  19.343<sub>±0.561</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   415.62<sub>±25.24</sub> |
|               Vala/clang |  21.283<sub>±0.868</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   471.07<sub>±15.67</sub> |
|                 Go/gccgo |  21.479<sub>±0.730</sub> |    22.47<sub>±00.15</sub> + 1.28<sub>±00.00</sub> |   423.49<sub>±35.96</sub> |
|                Nim/clang |  21.736<sub>±0.277</sub> |     2.35<sub>±00.06</sub> + 0.55<sub>±00.04</sub> |   494.32<sub>±14.56</sub> |
|                    Swift |  23.634<sub>±0.452</sub> |    14.06<sub>±00.20</sub> + 0.00<sub>±00.00</sub> |   422.42<sub>±13.10</sub> |
|                     Java |  23.684<sub>±1.300</sub> |    37.34<sub>±00.31</sub> + 2.01<sub>±00.41</sub> |   500.23<sub>±47.94</sub> |
|                  Crystal |  23.739<sub>±0.143</sub> |     3.35<sub>±00.03</sub> + 0.46<sub>±00.02</sub> |   548.02<sub>±09.02</sub> |
|                    Scala |  24.912<sub>±0.462</sub> |  55.56<sub>±00.09</sub> + 190.41<sub>±01.53</sub> |   521.85<sub>±12.89</sub> |
|                  V/clang |  26.954<sub>±0.421</sub> |     1.70<sub>±00.03</sub> + 1.03<sub>±00.00</sub> |   489.42<sub>±13.86</sub> |
|             F#/.NET Core |  33.012<sub>±0.544</sub> |    37.03<sub>±00.08</sub> + 2.05<sub>±00.01</sub> |   737.86<sub>±11.26</sub> |
|                    OCaml |  38.151<sub>±0.579</sub> |     3.91<sub>±00.02</sub> + 4.24<sub>±01.80</sub> |   719.63<sub>±17.60</sub> |
|                   Racket |  38.760<sub>±0.819</sub> |    95.57<sub>±00.37</sub> + 0.25<sub>±00.02</sub> |   887.55<sub>±25.58</sub> |
|              Chez Scheme |  39.615<sub>±0.671</sub> |    25.47<sub>±00.06</sub> + 3.66<sub>±00.03</sub> |   910.31<sub>±37.23</sub> |
|               Lua/luajit |  39.938<sub>±1.246</sub> |     2.19<sub>±00.01</sub> + 0.44<sub>±00.00</sub> |   759.18<sub>±59.27</sub> |
|                  Node.js |  40.497<sub>±1.306</sub> |    31.90<sub>±00.08</sub> + 5.90<sub>±00.00</sub> |   793.72<sub>±61.55</sub> |
|                    D/dmd |  44.321<sub>±1.351</sub> |     3.68<sub>±00.02</sub> + 0.77<sub>±00.00</sub> |   900.34<sub>±66.48</sub> |
|                  C#/Mono |  45.258<sub>±1.491</sub> |    20.38<sub>±00.05</sub> + 0.89<sub>±00.00</sub> |   930.94<sub>±65.26</sub> |
|                    MLton |  53.455<sub>±0.980</sub> |     1.50<sub>±00.06</sub> + 4.11<sub>±00.00</sub> |  1005.95<sub>±16.37</sub> |
|              Python/pypy |  60.737<sub>±0.507</sub> |   65.44<sub>±00.15</sub> + 46.44<sub>±00.05</sub> |  1420.15<sub>±52.24</sub> |
|         Haskell (MArray) |  62.838<sub>±1.524</sub> |     3.75<sub>±00.04</sub> + 2.62<sub>±00.01</sub> |  1242.50<sub>±83.81</sub> |
|                    Julia |  76.724<sub>±2.780</sub> |   199.83<sub>±00.21</sub> + 0.92<sub>±00.15</sub> | 1570.55<sub>±112.80</sub> |
| Ruby/truffleruby (--jvm) | 109.244<sub>±5.070</sub> | 628.22<sub>±19.83</sub> + 525.14<sub>±35.69</sub> | 2226.25<sub>±173.66</sub> |
|         Ruby/truffleruby | 124.609<sub>±8.689</sub> | 433.90<sub>±01.58</sub> + 599.91<sub>±07.72</sub> | 2616.63<sub>±181.36</sub> |
|                  Haskell | 218.209<sub>±1.429</sub> |    3.85<sub>±00.09</sub> + 26.16<sub>±00.00</sub> |  5028.89<sub>±59.92</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.143<sub>±0.003</sub> |     1.89<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.48<sub>±00.12</sub> |
|                      Rust |  1.117<sub>±0.018</sub> |     2.46<sub>±00.09</sub> + 0.07<sub>±00.06</sub> |  24.21<sub>±01.51</sub> |
|                     C/gcc |  1.181<sub>±0.034</sub> |     1.87<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  24.32<sub>±01.36</sub> |
|                   V/clang |  1.253<sub>±0.038</sub> |     1.69<sub>±00.06</sub> + 0.17<sub>±00.12</sub> |  27.29<sub>±01.44</sub> |
|                   Nim/gcc |  1.344<sub>±0.036</sub> |     2.26<sub>±00.04</sub> + 4.44<sub>±00.00</sub> |  24.70<sub>±01.26</sub> |
|                     V/gcc |  1.419<sub>±0.037</sub> |     2.12<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  31.28<sub>±01.15</sub> |
|                 Nim/clang |  1.730<sub>±0.022</sub> |     2.76<sub>±00.06</sub> + 4.38<sub>±00.00</sub> |  31.06<sub>±01.47</sub> |
|                   Crystal |  1.731<sub>±0.003</sub> |     3.75<sub>±00.02</sub> + 1.84<sub>±00.05</sub> |  36.14<sub>±00.19</sub> |
|                    D/ldc2 |  1.978<sub>±0.009</sub> |     3.57<sub>±00.04</sub> + 3.67<sub>±00.00</sub> |  35.56<sub>±00.80</sub> |
|                  Vala/gcc |  2.088<sub>±0.028</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  46.61<sub>±01.25</sub> |
|              Ruby (--jit) |  2.094<sub>±0.076</sub> |   14.47<sub>±00.04</sub> + 54.36<sub>±01.53</sub> |  44.47<sub>±04.35</sub> |
|                      Ruby |  2.142<sub>±0.064</sub> |   14.48<sub>±00.03</sub> + 54.75<sub>±01.87</sub> |  39.50<sub>±03.31</sub> |
|                      Java |  2.164<sub>±0.062</sub> |  38.51<sub>±00.32</sub> + 318.97<sub>±39.40</sub> |  49.59<sub>±01.34</sub> |
|                Vala/clang |  2.239<sub>±0.043</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  39.40<sub>±00.68</sub> |
|                    Kotlin |  2.332<sub>±0.063</sub> |  40.71<sub>±00.14</sub> + 335.77<sub>±10.77</sub> |  49.36<sub>±03.99</sub> |
|                     Scala |  2.364<sub>±0.040</sub> |  53.91<sub>±00.07</sub> + 337.67<sub>±13.53</sub> |  54.24<sub>±02.59</sub> |
|                        Go |  2.587<sub>±0.005</sub> |     4.43<sub>±00.01</sub> + 5.25<sub>±00.12</sub> |  51.93<sub>±00.40</sub> |
|       C++/g++ (libcrypto) |  2.733<sub>±0.108</sub> |     5.55<sub>±00.12</sub> + 0.07<sub>±00.00</sub> |  55.73<sub>±05.46</sub> |
|                   Node.js |  2.814<sub>±0.064</sub> |   32.30<sub>±00.08</sub> + 36.36<sub>±00.07</sub> |  60.55<sub>±02.04</sub> |
|       Perl (MIME::Base64) |  2.896<sub>±0.030</sub> |    14.11<sub>±00.03</sub> + 0.08<sub>±00.00</sub> |  63.67<sub>±01.34</sub> |
|                       PHP |  2.960<sub>±0.049</sub> |    15.72<sub>±00.14</sub> + 0.00<sub>±00.00</sub> |  50.69<sub>±01.54</sub> |
|                  Go/gccgo |  3.233<sub>±0.021</sub> |    23.39<sub>±00.25</sub> + 8.76<sub>±00.33</sub> |  66.18<sub>±00.67</sub> |
|                     D/gdc |  3.295<sub>±0.102</sub> |     7.04<sub>±00.02</sub> + 3.52<sub>±00.00</sub> |  69.16<sub>±03.88</sub> |
|                     D/dmd |  3.718<sub>±0.055</sub> |     3.80<sub>±00.07</sub> + 3.61<sub>±00.00</sub> |  73.01<sub>±02.23</sub> |
|                    Python |  3.971<sub>±0.063</sub> |     9.96<sub>±00.03</sub> + 0.18<sub>±00.00</sub> |  90.62<sub>±01.60</sub> |
|                       Zig |  4.513<sub>±0.010</sub> |     1.88<sub>±00.02</sub> + 0.34<sub>±00.03</sub> |  82.21<sub>±01.46</sub> |
|               Python/pypy |  4.886<sub>±0.142</sub> |   65.43<sub>±00.06</sub> + 45.75<sub>±00.09</sub> |  98.29<sub>±08.01</sub> |
|                       Tcl |  4.998<sub>±0.026</sub> |     4.88<sub>±00.06</sub> + 0.19<sub>±00.03</sub> |  84.07<sub>±02.10</sub> |
|                     Julia |  5.740<sub>±0.202</sub> |  218.99<sub>±00.04</sub> + 63.02<sub>±00.17</sub> | 128.13<sub>±09.02</sub> |
|              F#/.NET Core |  5.765<sub>±0.045</sub> |   37.22<sub>±00.04</sub> + 36.42<sub>±08.18</sub> | 106.41<sub>±01.19</sub> |
|              C#/.NET Core |  5.847<sub>±0.015</sub> |   34.66<sub>±00.04</sub> + 40.93<sub>±07.60</sub> | 108.42<sub>±01.65</sub> |
|  Ruby/truffleruby (--jvm) |  6.302<sub>±0.210</sub> | 625.38<sub>±09.77</sub> + 141.33<sub>±42.46</sub> | 137.95<sub>±11.33</sub> |
|                   C#/Mono |  7.259<sub>±0.059</sub> |   20.84<sub>±00.03</sub> + 18.49<sub>±00.07</sub> | 174.00<sub>±01.80</sub> |
|                Ruby/jruby | 11.868<sub>±0.308</sub> | 184.35<sub>±02.57</sub> + 138.17<sub>±15.24</sub> | 266.12<sub>±09.10</sub> |
| Perl (MIME::Base64::Perl) | 15.947<sub>±0.181</sub> |    15.51<sub>±00.03</sub> + 0.16<sub>±00.06</sub> | 362.72<sub>±04.38</sub> |
|          Ruby/truffleruby | 20.507<sub>±0.491</sub> | 427.34<sub>±01.69</sub> + 320.53<sub>±02.44</sub> | 367.59<sub>±20.16</sub> |

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

|                        Language |                 Time, s |                                        Memory, MiB |                Energy, J |
| :------------------------------ | ----------------------: | -------------------------------------------------: | -----------------------: |
| C++/g++ (DAW JSON Link NoCheck) |  0.095<sub>±0.004</sub> |    112.69<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |    2.60<sub>±00.21</sub> |
|    C++/g++ (simdjson On-Demand) |  0.098<sub>±0.002</sub> |   113.27<sub>±00.03</sub> + 59.81<sub>±00.00</sub> |    2.14<sub>±00.05</sub> |
|         C++/g++ (DAW JSON Link) |  0.115<sub>±0.003</sub> |    112.93<sub>±00.23</sub> + 0.00<sub>±00.00</sub> |    2.59<sub>±00.23</sub> |
|             Rust (Serde Custom) |  0.146<sub>±0.005</sub> |    111.84<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    2.72<sub>±00.12</sub> |
|              Rust (Serde Typed) |  0.156<sub>±0.006</sub> |   111.84<sub>±00.06</sub> + 11.89<sub>±00.20</sub> |    2.99<sub>±00.27</sub> |
|                 C++/g++ (gason) |  0.162<sub>±0.002</sub> |   113.08<sub>±00.05</sub> + 96.74<sub>±00.03</sub> |    3.81<sub>±00.20</sub> |
|          C++/g++ (simdjson DOM) |  0.171<sub>±0.002</sub> |  113.32<sub>±00.08</sub> + 176.60<sub>±00.00</sub> |    3.56<sub>±00.10</sub> |
|             C++/g++ (RapidJSON) |  0.224<sub>±0.007</sub> |  113.17<sub>±00.08</sub> + 128.71<sub>±00.00</sub> |    4.76<sub>±00.40</sub> |
|           D/ldc2 (Mir Asdf DOM) |  0.233<sub>±0.003</sub> |   112.80<sub>±00.04</sub> + 61.35<sub>±00.00</sub> |    5.09<sub>±00.16</sub> |
|   D/ldc2 (Mir Amazon's Ion DOM) |  0.236<sub>±0.004</sub> |   112.76<sub>±00.09</sub> + 16.27<sub>±00.01</sub> |    4.83<sub>±00.14</sub> |
|     C++/g++ (RapidJSON Precise) |  0.290<sub>±0.008</sub> |  113.14<sub>±00.00</sub> + 128.67<sub>±00.02</sub> |    6.75<sub>±00.57</sub> |
|            C++/g++ (Boost.JSON) |  0.492<sub>±0.008</sub> |  113.21<sub>±00.02</sub> + 435.70<sub>±00.00</sub> |   12.20<sub>±00.41</sub> |
|         C++/g++ (RapidJSON SAX) |  0.566<sub>±0.025</sub> |    112.88<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   11.46<sub>±00.89</sub> |
| C++/g++ (RapidJSON SAX Precise) |  0.662<sub>±0.018</sub> |    112.93<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   11.71<sub>±00.48</sub> |
|                   Go (jsoniter) |  0.668<sub>±0.007</sub> |    230.95<sub>±00.09</sub> + 1.56<sub>±00.19</sub> |   14.76<sub>±00.51</sub> |
|                  Crystal (Pull) |  0.741<sub>±0.031</sub> |   113.73<sub>±00.04</sub> + 18.24<sub>±00.02</sub> |   14.91<sub>±01.31</sub> |
|                Crystal (Schema) |  0.760<sub>±0.028</sub> |   113.75<sub>±00.04</sub> + 47.32<sub>±00.03</sub> |   15.47<sub>±00.77</sub> |
|                 Java (DSL-JSON) |  0.775<sub>±0.031</sub> |  263.87<sub>±00.20</sub> + 307.21<sub>±07.03</sub> |   20.01<sub>±00.44</sub> |
|                     Python/pypy |  0.804<sub>±0.004</sub> |  285.44<sub>±00.06</sub> + 121.42<sub>±00.01</sub> |   19.69<sub>±00.52</sub> |
|            Rust (Serde Untyped) |  0.824<sub>±0.029</sub> |  111.80<sub>±00.09</sub> + 839.98<sub>±00.03</sub> |   17.70<sub>±01.38</sub> |
|                           V/gcc |  0.862<sub>±0.021</sub> |  111.15<sub>±00.01</sub> + 496.15<sub>±00.06</sub> |   18.53<sub>±01.74</sub> |
|                         V/clang |  0.890<sub>±0.036</sub> |  111.14<sub>±00.02</sub> + 496.15<sub>±00.03</sub> |   16.99<sub>±01.13</sub> |
|                   Julia (JSON3) |  0.923<sub>±0.031</sub> |  385.29<sub>±00.09</sub> + 362.25<sub>±00.08</sub> |   19.65<sub>±01.96</sub> |
|                         Node.js |  0.941<sub>±0.017</sub> |   251.53<sub>±00.08</sub> + 78.39<sub>±01.19</sub> |   20.03<sub>±01.04</sub> |
|         Perl (Cpanel::JSON::XS) |  0.991<sub>±0.007</sub> |  124.71<sub>±00.05</sub> + 402.78<sub>±00.03</sub> |   23.43<sub>±00.68</sub> |
|                         Crystal |  0.994<sub>±0.036</sub> |  113.76<sub>±00.04</sub> + 396.41<sub>±00.04</sub> |   21.72<sub>±00.95</sub> |
| C#/.NET Core (System.Text.Json) |  1.031<sub>±0.022</sub> |  479.18<sub>±00.10</sub> + 139.04<sub>±00.00</sub> |   24.64<sub>±00.82</sub> |
|                             Zig |  1.046<sub>±0.021</sub> |   111.40<sub>±00.03</sub> + 11.92<sub>±00.00</sub> |   19.30<sub>±01.12</sub> |
|                              Go |  1.078<sub>±0.048</sub> |   117.18<sub>±00.09</sub> + 83.31<sub>±00.23</sub> |   19.86<sub>±01.45</sub> |
|          Nim/clang (Packedjson) |  1.142<sub>±0.046</sub> |  112.63<sub>±00.02</sub> + 293.91<sub>±00.00</sub> |   23.84<sub>±02.08</sub> |
|            Nim/gcc (Packedjson) |  1.289<sub>±0.050</sub> |  112.03<sub>±00.10</sub> + 293.91<sub>±00.00</sub> |   26.95<sub>±02.65</sub> |
|                             PHP |  1.316<sub>±0.051</sub> |  125.27<sub>±00.13</sub> + 682.01<sub>±00.00</sub> |   24.44<sub>±01.21</sub> |
|                C++/g++ (json-c) |  1.537<sub>±0.024</sub> | 113.07<sub>±00.18</sub> + 1216.12<sub>±00.21</sub> |   36.54<sub>±01.06</sub> |
|                         Clojure |  1.599<sub>±0.042</sub> |  453.79<sub>±06.58</sub> + 609.33<sub>±30.41</sub> |   42.01<sub>±02.15</sub> |
|                        Go/gccgo |  1.634<sub>±0.049</sub> |   137.56<sub>±00.11</sub> + 83.46<sub>±00.07</sub> |   35.31<sub>±02.63</sub> |
|                       Nim/clang |  1.683<sub>±0.036</sub> |  112.64<sub>±00.04</sub> + 924.79<sub>±00.02</sub> |   36.07<sub>±03.85</sub> |
|                         Nim/gcc |  1.708<sub>±0.026</sub> |  112.09<sub>±00.04</sub> + 919.68<sub>±00.00</sub> |   39.71<sub>±03.16</sub> |
|              C++/g++ (Nlohmann) |  1.744<sub>±0.036</sub> |  113.27<sub>±00.03</sub> + 447.88<sub>±00.00</sub> |   40.77<sub>±02.40</sub> |
|                    C#/.NET Core |  1.805<sub>±0.028</sub> |  488.19<sub>±00.18</sub> + 294.07<sub>±00.00</sub> |   33.37<sub>±01.17</sub> |
|             CPython (UltraJSON) |  1.819<sub>±0.027</sub> |  121.53<sub>±00.04</sub> + 547.67<sub>±01.59</sub> |   38.87<sub>±01.51</sub> |
|                          Python |  1.874<sub>±0.014</sub> |  119.95<sub>±00.02</sub> + 377.21<sub>±00.00</sub> |   37.41<sub>±00.37</sub> |
|                         Haskell |  2.085<sub>±0.027</sub> |      4.39<sub>±00.03</sub> + 4.56<sub>±00.01</sub> |   49.34<sub>±02.93</sub> |
|                         C#/Mono |  2.327<sub>±0.071</sub> |    476.35<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |   43.87<sub>±02.47</sub> |
|                     Ruby (YAJL) |  2.380<sub>±0.065</sub> |  123.90<sub>±00.03</sub> + 283.34<sub>±00.01</sub> |   55.27<sub>±00.98</sub> |
|                            Ruby |  2.418<sub>±0.026</sub> |  123.99<sub>±00.03</sub> + 410.70<sub>±00.02</sub> |   56.46<sub>±02.06</sub> |
|                 Scala (uPickle) |  2.518<sub>±0.069</sub> |  284.50<sub>±00.07</sub> + 850.99<sub>±29.10</sub> |   60.49<sub>±01.57</sub> |
|                    Ruby (--jit) |  2.569<sub>±0.103</sub> |  123.95<sub>±00.05</sub> + 410.82<sub>±00.03</sub> |   56.08<sub>±04.29</sub> |
| F#/.NET Core (System.Text.Json) |  2.601<sub>±0.117</sub> |  485.46<sub>±00.05</sub> + 457.58<sub>±01.48</sub> |   54.45<sub>±05.98</sub> |
|                          D/ldc2 |  2.608<sub>±0.055</sub> |  113.05<sub>±00.04</sub> + 680.21<sub>±00.03</sub> |   53.38<sub>±02.42</sub> |
|                           D/gdc |  3.198<sub>±0.064</sub> |  116.75<sub>±00.06</sub> + 600.61<sub>±00.06</sub> |   56.66<sub>±01.63</sub> |
|                       Rust (jq) |  3.732<sub>±0.153</sub> |  113.79<sub>±00.03</sub> + 778.50<sub>±00.52</sub> |   75.11<sub>±07.38</sub> |
|                      Ruby/jruby |  3.823<sub>±0.094</sub> | 449.89<sub>±03.21</sub> + 1444.92<sub>±47.92</sub> |  108.91<sub>±08.61</sub> |
|    C++/g++ (Boost.PropertyTree) |  4.449<sub>±0.138</sub> | 113.04<sub>±00.01</sub> + 1440.00<sub>±00.00</sub> |  104.07<sub>±05.37</sub> |
|                           D/dmd |  5.203<sub>±0.057</sub> |  113.60<sub>±00.02</sub> + 680.12<sub>±00.04</sub> |   99.42<sub>±03.44</sub> |
|                      Vala/clang |  5.526<sub>±0.226</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  102.69<sub>±05.50</sub> |
|                        Vala/gcc |  5.565<sub>±0.036</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   96.89<sub>±04.05</sub> |
|               Perl (JSON::Tiny) | 12.633<sub>±0.331</sub> |  125.37<sub>±00.03</sub> + 528.57<sub>±00.02</sub> |  227.05<sub>±07.69</sub> |
|        Ruby/truffleruby (--jvm) | 23.649<sub>±0.386</sub> | 799.89<sub>±23.71</sub> + 1332.93<sub>±15.77</sub> |  658.54<sub>±09.27</sub> |
|                Ruby/truffleruby | 50.714<sub>±1.663</sub> | 761.31<sub>±01.65</sub> + 2440.31<sub>±39.32</sub> | 1067.44<sub>±82.34</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                        Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | -------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.083<sub>±0.001</sub> |     6.75<sub>±00.06</sub> + 56.12<sub>±00.11</sub> |      4.41<sub>±00.04</sub> |
|           Python (NumPy) |    0.108<sub>±0.001</sub> |    27.39<sub>±00.04</sub> + 57.53<sub>±00.01</sub> |      5.71<sub>±00.10</sub> |
|              Java (ND4J) |    0.114<sub>±0.004</sub> |   151.47<sub>±00.42</sub> + 90.77<sub>±00.00</sub> |      5.19<sub>±00.14</sub> |
|  Nim/clang (Arraymancer) |    0.129<sub>±0.019</sub> |     6.74<sub>±00.39</sub> + 55.09<sub>±00.27</sub> |      6.37<sub>±00.94</sub> |
|    Nim/gcc (Arraymancer) |    0.133<sub>±0.012</sub> |     5.45<sub>±00.20</sub> + 55.40<sub>±00.24</sub> |      6.38<sub>±00.54</sub> |
|          C++/g++ (Eigen) |    0.215<sub>±0.005</sub> |     3.70<sub>±00.08</sub> + 85.25<sub>±00.00</sub> |      4.46<sub>±00.19</sub> |
|       Julia (threads: 8) |    0.235<sub>±0.003</sub> |   238.84<sub>±00.11</sub> + 37.18<sub>±00.19</sub> |     11.82<sub>±00.19</sub> |
|       Julia (threads: 1) |    0.576<sub>±0.027</sub> |   238.33<sub>±00.11</sub> + 37.86<sub>±00.06</sub> |     13.18<sub>±01.22</sub> |
|          Julia (no BLAS) |    1.233<sub>±0.014</sub> |   217.07<sub>±00.18</sub> + 51.55<sub>±00.09</sub> |     29.92<sub>±01.44</sub> |
|                   D/ldc2 |    1.985<sub>±0.010</sub> |     3.62<sub>±00.08</sub> + 70.11<sub>±00.00</sub> |     45.44<sub>±00.54</sub> |
|                    D/gdc |    2.105<sub>±0.008</sub> |     7.11<sub>±00.06</sub> + 70.08<sub>±00.01</sub> |     52.70<sub>±00.90</sub> |
|                    D/dmd |    2.137<sub>±0.020</sub> |     3.53<sub>±00.01</sub> + 70.13<sub>±00.00</sub> |     48.66<sub>±02.18</sub> |
|                     Java |    3.280<sub>±0.072</sub> |    38.45<sub>±00.06</sub> + 80.98<sub>±00.32</sub> |     75.23<sub>±01.08</sub> |
|                 Vala/gcc |    3.332<sub>±0.038</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     79.61<sub>±01.33</sub> |
|                    C/gcc |    3.349<sub>±0.049</sub> |     2.03<sub>±00.01</sub> + 68.06<sub>±00.00</sub> |     78.89<sub>±01.04</sub> |
|               Vala/clang |    3.412<sub>±0.024</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     74.88<sub>±01.03</sub> |
|                  Nim/gcc |    3.425<sub>±0.095</sub> |     2.59<sub>±00.02</sub> + 65.48<sub>±00.00</sub> |     77.92<sub>±01.77</sub> |
|                    Scala |    3.438<sub>±0.063</sub> |   55.85<sub>±00.23</sub> + 153.86<sub>±00.15</sub> |     80.67<sub>±01.01</sub> |
|                      Zig |    3.468<sub>±0.018</sub> |     2.14<sub>±00.03</sub> + 68.58<sub>±00.00</sub> |     73.12<sub>±00.85</sub> |
|                     Rust |    3.468<sub>±0.017</sub> |     2.71<sub>±00.03</sub> + 68.32<sub>±00.00</sub> |     73.72<sub>±01.23</sub> |
|                    Swift |    3.480<sub>±0.012</sub> |     7.74<sub>±00.09</sub> + 68.91<sub>±00.07</sub> |     70.97<sub>±00.84</sub> |
|                Nim/clang |    3.494<sub>±0.037</sub> |     3.08<sub>±00.03</sub> + 66.00<sub>±00.13</sub> |     74.08<sub>±01.21</sub> |
|                 Go/gccgo |    3.526<sub>±0.024</sub> |    22.76<sub>±00.12</sub> + 73.50<sub>±00.16</sub> |     77.93<sub>±01.31</sub> |
|                  Crystal |    3.652<sub>±0.027</sub> |     4.13<sub>±00.08</sub> + 59.71<sub>±00.04</sub> |     82.10<sub>±04.74</sub> |
|                    V/gcc |    3.694<sub>±0.073</sub> |     1.98<sub>±00.12</sub> + 68.58<sub>±00.13</sub> |     87.26<sub>±02.33</sub> |
|                       Go |    3.733<sub>±0.115</sub> |     3.53<sub>±00.07</sub> + 73.11<sub>±00.14</sub> |     76.53<sub>±01.43</sub> |
|                  V/clang |    3.750<sub>±0.064</sub> |     2.16<sub>±00.06</sub> + 68.84<sub>±00.00</sub> |     82.17<sub>±01.53</sub> |
|                   Kotlin |    3.817<sub>±0.046</sub> |    38.91<sub>±00.08</sub> + 80.19<sub>±00.43</sub> |     88.99<sub>±06.26</sub> |
|                  Node.js |    3.874<sub>±0.086</sub> |    35.21<sub>±00.19</sub> + 72.29<sub>±00.41</sub> |     87.24<sub>±06.76</sub> |
|              Python/pypy |    6.136<sub>±0.152</sub> |    65.95<sub>±00.17</sub> + 68.98<sub>±00.06</sub> |    139.27<sub>±04.09</sub> |
|             C#/.NET Core |    7.476<sub>±0.204</sub> |    34.07<sub>±00.08</sub> + 69.11<sub>±00.01</sub> |    131.78<sub>±11.30</sub> |
|                  C#/Mono |   10.843<sub>±0.225</sub> |    20.31<sub>±00.04</sub> + 69.05<sub>±00.02</sub> |    241.33<sub>±09.84</sub> |
|         Ruby/truffleruby |   33.494<sub>±0.414</sub> |  678.82<sub>±01.66</sub> + 608.88<sub>±03.84</sub> |    775.25<sub>±74.77</sub> |
| Ruby/truffleruby (--jvm) |   50.476<sub>±1.709</sub> |  723.24<sub>±21.84</sub> + 392.56<sub>±62.84</sub> |  1235.37<sub>±105.75</sub> |
|                     Ruby |  227.359<sub>±7.682</sub> |    15.19<sub>±00.05</sub> + 68.64<sub>±00.00</sub> |  4152.18<sub>±191.04</sub> |
|             Ruby (--jit) |  239.284<sub>±5.426</sub> |    15.20<sub>±00.02</sub> + 68.90<sub>±00.00</sub> |  4666.33<sub>±396.30</sub> |
|                   Python |  249.008<sub>±4.728</sub> |    10.41<sub>±00.02</sub> + 68.58<sub>±00.00</sub> |  4709.83<sub>±417.93</sub> |
|                      Tcl |  367.150<sub>±8.908</sub> |    7.19<sub>±00.05</sub> + 400.21<sub>±00.04</sub> |  7187.58<sub>±566.87</sub> |
|                     Perl |  410.279<sub>±4.917</sub> |    8.98<sub>±00.07</sub> + 599.64<sub>±00.07</sub> |  8340.13<sub>±307.18</sub> |
|               Ruby/jruby | 505.828<sub>±15.742</sub> | 267.76<sub>±07.91</sub> + 1038.75<sub>±63.89</sub> | 11300.13<sub>±628.20</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                      Zig | 0.076<sub>±0.001</sub> |    1.81<sub>±00.02</sub> + 50.66<sub>±00.78</sub> |   1.81<sub>±00.08</sub> |
|                  Crystal | 0.164<sub>±0.001</sub> |    3.38<sub>±00.06</sub> + 92.26<sub>±01.39</sub> |   3.75<sub>±00.11</sub> |
|                     Java | 0.183<sub>±0.004</sub> |  36.52<sub>±00.42</sub> + 104.61<sub>±00.57</sub> |   5.68<sub>±00.11</sub> |
|                  C++/g++ | 0.187<sub>±0.007</sub> |    3.30<sub>±00.03</sub> + 84.80<sub>±00.11</sub> |   3.94<sub>±00.20</sub> |
|                     Rust | 0.220<sub>±0.005</sub> |    2.38<sub>±00.06</sub> + 72.88<sub>±00.24</sub> |   4.36<sub>±00.08</sub> |
|                  Node.js | 0.293<sub>±0.003</sub> |  31.66<sub>±00.05</sub> + 175.72<sub>±00.29</sub> |   7.87<sub>±00.22</sub> |
|                    Scala | 0.434<sub>±0.017</sub> |  55.49<sub>±00.12</sub> + 244.83<sub>±03.49</sub> |  14.38<sub>±00.21</sub> |
|               Lua/luajit | 0.510<sub>±0.004</sub> |   2.39<sub>±00.01</sub> + 129.85<sub>±01.03</sub> |  10.18<sub>±00.27</sub> |
|              Python/pypy | 0.973<sub>±0.025</sub> |  65.06<sub>±00.16</sub> + 248.80<sub>±00.07</sub> |  20.95<sub>±01.43</sub> |
|         Ruby/truffleruby | 1.304<sub>±0.030</sub> | 427.81<sub>±02.30</sub> + 436.43<sub>±03.88</sub> |  31.93<sub>±03.05</sub> |
| Ruby/truffleruby (--jvm) | 1.809<sub>±0.028</sub> | 630.19<sub>±03.19</sub> + 316.02<sub>±21.17</sub> |  63.68<sub>±01.92</sub> |
|                      Lua | 1.954<sub>±0.017</sub> |   2.39<sub>±00.17</sub> + 373.41<sub>±00.14</sub> |  41.16<sub>±00.54</sub> |
|             Ruby (--jit) | 2.410<sub>±0.042</sub> |  14.05<sub>±00.04</sub> + 150.19<sub>±00.04</sub> |  59.79<sub>±01.90</sub> |
|                     Ruby | 2.483<sub>±0.088</sub> |  14.01<sub>±00.04</sub> + 149.89<sub>±00.00</sub> |  54.41<sub>±02.20</sub> |
|               Ruby/jruby | 2.975<sub>±0.108</sub> | 186.65<sub>±01.87</sub> + 363.04<sub>±22.97</sub> |  74.15<sub>±04.66</sub> |
|                   Python | 5.732<sub>±0.250</sub> |  10.19<sub>±00.13</sub> + 236.09<sub>±00.64</sub> | 108.17<sub>±10.92</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.400                         |
| C#/.NET Core     | 3.11.0-4.21373.6 (bb3bdbbe)     |
| C#/Mono          | 6.12.0.122                      |
| C/clang          | 11.0.1                          |
| C/gcc            | 11.2.0                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.3"                        |
| Crystal          | 1.1.1                           |
| D/dmd            | v2.097.2                        |
| D/gdc            | 11.2.0                          |
| D/ldc2           | 1.27.1                          |
| Elixir           | 1.12.2                          |
| F#/.NET Core     | 11.4.2.0 for F# 5.0             |
| Go               | go1.17                          |
| Go/gccgo         | 11.2.0                          |
| Haskell          | 9.0.1                           |
| Java             | 16.0.2                          |
| Julia            | v"1.6.2"                        |
| Kotlin           | 1.5.30                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20210117                        |
| Nim              | 1.4.8                           |
| Node.js          | v16.9.0                         |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.21                          |
| Perl             | v5.32.1                         |
| Python           | 3.9.7                           |
| Python/pypy      | 7.3.5-final0 for Python 3.7.10  |
| Racket           | "8.2"                           |
| Ruby             | 3.0.2p107                       |
| Ruby/jruby       | 9.2.19.0                        |
| Ruby/truffleruby | 21.2.0.1                        |
| Rust             | 1.54.0                          |
| Scala            | 3.0.2                           |
| Swift            | swift-5.5-RELEASE               |
| Tcl              | 8.6                             |
| V                | 0.2.4 b72a2de                   |
| Vala             | 0.48.18                         |
| Zig              | 0.8.1                           |

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
 - `primes` (build and run Primes benchmarks);

Please note that the actual measurements provided in the project are taken semi-manually (via `shell`) as the full update takes days and could have occassional issues in Docker.

There is a `Makefile` that could be used to simplify Docker usage:

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
