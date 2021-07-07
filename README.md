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

UPDATE: 2021-07-06

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
|                  C++/g++ |    0.921<sub>±0.033</sub> |     1.48<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     16.51<sub>±00.81</sub> |
|  Racket (Syntax Objects) |    1.433<sub>±0.053</sub> |   106.75<sub>±00.60</sub> + 0.00<sub>±00.00</sub> |     30.49<sub>±02.37</sub> |
|                    C/gcc |    1.848<sub>±0.076</sub> |     0.54<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     39.40<sub>±02.59</sub> |
|                     Rust |    1.864<sub>±0.062</sub> |     2.12<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     35.78<sub>±02.93</sub> |
|                   D/ldc2 |    1.864<sub>±0.041</sub> |     2.97<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     34.74<sub>±01.31</sub> |
|                   Kotlin |    1.868<sub>±0.048</sub> |    40.89<sub>±00.11</sub> + 0.48<sub>±00.10</sub> |     35.11<sub>±02.66</sub> |
|                    D/gdc |    1.943<sub>±0.066</sub> |     6.30<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     41.12<sub>±02.40</sub> |
|                    OCaml |    2.140<sub>±0.055</sub> |     2.59<sub>±00.04</sub> + 2.51<sub>±00.06</sub> |     48.78<sub>±02.79</sub> |
|                 Go/gccgo |    2.175<sub>±0.057</sub> |    20.99<sub>±00.34</sub> + 0.00<sub>±00.00</sub> |     39.59<sub>±03.11</sub> |
|                       Go |    2.201<sub>±0.075</sub> |     3.45<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     48.46<sub>±01.43</sub> |
|                     Java |    2.216<sub>±0.047</sub> |    38.08<sub>±00.10</sub> + 0.88<sub>±00.14</sub> |     43.65<sub>±02.77</sub> |
|                 Vala/gcc |    2.251<sub>±0.068</sub> |     3.72<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     48.29<sub>±01.13</sub> |
|                  C/clang |    2.288<sub>±0.040</sub> |     0.52<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     43.78<sub>±03.77</sub> |
|                   Racket |    2.300<sub>±0.156</sub> |   115.73<sub>±00.07</sub> + 2.32<sub>±00.52</sub> |     52.15<sub>±04.19</sub> |
|                  Nim/gcc |    2.364<sub>±0.029</sub> |     1.78<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     47.10<sub>±04.60</sub> |
|                    V/gcc |    2.366<sub>±0.027</sub> |     0.55<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     54.95<sub>±01.10</sub> |
|                Nim/clang |    2.367<sub>±0.070</sub> |     2.32<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     48.55<sub>±04.79</sub> |
|             C#/.NET Core |    2.374<sub>±0.107</sub> |    34.24<sub>±00.03</sub> + 0.01<sub>±00.00</sub> |     49.14<sub>±04.65</sub> |
|             F#/.NET Core |    2.403<sub>±0.046</sub> |    36.74<sub>±00.05</sub> + 0.30<sub>±00.00</sub> |     42.82<sub>±02.23</sub> |
|                  Crystal |    2.491<sub>±0.116</sub> |     3.30<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     51.84<sub>±03.87</sub> |
|               Vala/clang |    2.500<sub>±0.048</sub> |     3.64<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     48.54<sub>±02.57</sub> |
|                    MLton |    2.822<sub>±0.058</sub> |     1.42<sub>±00.04</sub> + 0.25<sub>±00.00</sub> |     53.42<sub>±03.42</sub> |
|              Chez Scheme |    2.996<sub>±0.129</sub> |    24.88<sub>±00.00</sub> + 4.21<sub>±00.03</sub> |     62.23<sub>±04.69</sub> |
|                    Julia |    3.264<sub>±0.050</sub> |   197.94<sub>±00.14</sub> + 0.75<sub>±00.03</sub> |     54.51<sub>±01.22</sub> |
|                    D/dmd |    3.535<sub>±0.134</sub> |     3.67<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     77.38<sub>±03.85</sub> |
|                    Scala |    3.599<sub>±0.080</sub> |  55.41<sub>±00.10</sub> + 261.15<sub>±04.05</sub> |     77.90<sub>±03.26</sub> |
|                  Node.js |    4.208<sub>±0.175</sub> |    31.23<sub>±00.07</sub> + 1.33<sub>±00.00</sub> |     82.18<sub>±10.81</sub> |
|                  C#/Mono |    4.421<sub>±0.031</sub> |    20.01<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     77.47<sub>±03.78</sub> |
|                  V/clang |    4.676<sub>±0.107</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |    102.59<sub>±03.08</sub> |
|         Haskell (MArray) |    4.885<sub>±0.166</sub> |     3.72<sub>±00.09</sub> + 1.11<sub>±00.00</sub> |     95.27<sub>±06.35</sub> |
|               Lua/luajit |    6.784<sub>±0.057</sub> |     2.88<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    116.90<sub>±02.65</sub> |
| Ruby/truffleruby (--jvm) |    7.802<sub>±0.288</sub> | 580.68<sub>±15.18</sub> + 679.16<sub>±31.19</sub> |    281.34<sub>±11.68</sub> |
|         Ruby/truffleruby |    8.594<sub>±0.336</sub> | 414.21<sub>±00.64</sub> + 541.02<sub>±07.15</sub> |    209.20<sub>±18.29</sub> |
|              Python/pypy |   15.296<sub>±0.235</sub> |   65.41<sub>±00.09</sub> + 45.49<sub>±00.04</sub> |    301.08<sub>±09.78</sub> |
|                  Haskell |   15.741<sub>±0.410</sub> |     3.79<sub>±00.06</sub> + 0.82<sub>±00.00</sub> |    372.84<sub>±21.06</sub> |
|                      Lua |   57.706<sub>±2.164</sub> |     2.93<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   1175.05<sub>±76.72</sub> |
|             Ruby (--jit) |   61.809<sub>±1.173</sub> |    14.08<sub>±00.02</sub> + 0.23<sub>±00.00</sub> |   1150.99<sub>±65.44</sub> |
|                     Ruby |   96.539<sub>±2.652</sub> |    13.99<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   1747.94<sub>±66.65</sub> |
|               Ruby/jruby |  107.399<sub>±2.175</sub> | 186.72<sub>±01.62</sub> + 130.70<sub>±19.19</sub> |   2264.93<sub>±46.81</sub> |
|                   Elixir |  118.727<sub>±4.015</sub> |    56.04<sub>±01.20</sub> + 0.00<sub>±00.00</sub> |  2381.04<sub>±129.88</sub> |
|                   Python |  227.596<sub>±5.242</sub> |    10.34<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  5115.22<sub>±223.46</sub> |
|                 Tcl (FP) |  285.989<sub>±6.642</sub> |     4.25<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  5522.61<sub>±280.47</sub> |
|                     Perl |  354.862<sub>±6.481</sub> |     6.51<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  7796.46<sub>±338.55</sub> |
|                Tcl (OOP) | 544.022<sub>±11.873</sub> |     4.25<sub>±00.08</sub> + 0.00<sub>±00.00</sub> | 11120.82<sub>±814.03</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                        Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | -------------------------------------------------: | ------------------------: |
|                    C/gcc |  12.423<sub>±0.162</sub> |      0.50<sub>±00.00</sub> + 1.11<sub>±00.02</sub> |   271.35<sub>±04.89</sub> |
|                     Rust |  12.586<sub>±0.178</sub> |      2.03<sub>±00.06</sub> + 0.25<sub>±00.00</sub> |   284.06<sub>±11.12</sub> |
|                  C++/g++ |  12.910<sub>±0.304</sub> |      3.18<sub>±00.05</sub> + 0.47<sub>±00.00</sub> |   289.47<sub>±07.83</sub> |
|                    D/gdc |  13.800<sub>±0.242</sub> |      6.72<sub>±00.05</sub> + 0.52<sub>±00.00</sub> |   281.84<sub>±12.34</sub> |
|                   D/ldc2 |  13.902<sub>±0.141</sub> |      3.02<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |   239.36<sub>±05.93</sub> |
|                       Go |  16.117<sub>±0.330</sub> |      3.40<sub>±00.08</sub> + 1.27<sub>±00.01</sub> |   339.69<sub>±16.91</sub> |
|                   Kotlin |  17.034<sub>±0.522</sub> |     40.80<sub>±00.12</sub> + 0.88<sub>±00.32</sub> |   329.00<sub>±31.56</sub> |
|                    V/gcc |  17.119<sub>±0.737</sub> |      1.67<sub>±00.04</sub> + 1.03<sub>±00.00</sub> |   359.04<sub>±34.09</sub> |
|  Racket (Syntax Objects) |  17.444<sub>±0.120</sub> |   106.37<sub>±00.15</sub> + 72.70<sub>±00.26</sub> |   332.19<sub>±21.78</sub> |
|                  C/clang |  18.167<sub>±0.686</sub> |      0.54<sub>±00.01</sub> + 1.09<sub>±00.03</sub> |   381.14<sub>±34.92</sub> |
|             C#/.NET Core |  19.055<sub>±0.224</sub> |     34.28<sub>±00.02</sub> + 0.96<sub>±00.00</sub> |   341.42<sub>±04.72</sub> |
|                 Vala/gcc |  19.880<sub>±0.559</sub> |      3.52<sub>±00.04</sub> + 2.06<sub>±00.02</sub> |   375.90<sub>±18.11</sub> |
|               Vala/clang |  22.178<sub>±0.405</sub> |      3.54<sub>±00.03</sub> + 2.06<sub>±00.03</sub> |   424.87<sub>±38.99</sub> |
|                Nim/clang |  22.230<sub>±0.709</sub> |      2.31<sub>±00.05</sub> + 0.51<sub>±00.00</sub> |   449.93<sub>±38.24</sub> |
|                  Nim/gcc |  23.202<sub>±0.447</sub> |      1.81<sub>±00.06</sub> + 0.51<sub>±00.00</sub> |   414.37<sub>±29.55</sub> |
|                     Java |  23.358<sub>±0.630</sub> |     38.01<sub>±00.15</sub> + 1.42<sub>±00.13</sub> |   438.99<sub>±41.36</sub> |
|                 Go/gccgo |  23.445<sub>±0.761</sub> |     21.30<sub>±00.07</sub> + 1.28<sub>±00.00</sub> |   443.45<sub>±33.96</sub> |
|                  Crystal |  24.551<sub>±1.035</sub> |      3.32<sub>±00.01</sub> + 0.46<sub>±00.01</sub> |   518.51<sub>±24.86</sub> |
|                    Scala |  25.120<sub>±0.293</sub> |   55.66<sub>±00.09</sub> + 141.27<sub>±00.33</sub> |   530.51<sub>±11.43</sub> |
|                  V/clang |  26.637<sub>±1.047</sub> |      1.66<sub>±00.03</sub> + 1.03<sub>±00.00</sub> |   538.04<sub>±45.94</sub> |
|             F#/.NET Core |  35.454<sub>±0.288</sub> |     36.79<sub>±00.07</sub> + 2.03<sub>±00.03</sub> |   634.62<sub>±03.81</sub> |
|                    OCaml |  37.562<sub>±1.228</sub> |      3.84<sub>±00.01</sub> + 6.69<sub>±00.39</sub> |   775.01<sub>±52.89</sub> |
|                   Racket |  38.757<sub>±0.747</sub> |    115.69<sub>±00.28</sub> + 2.32<sub>±00.26</sub> |   732.68<sub>±17.03</sub> |
|                  Node.js |  39.350<sub>±0.187</sub> |     31.24<sub>±00.05</sub> + 5.94<sub>±00.01</sub> |   846.41<sub>±04.74</sub> |
|              Chez Scheme |  40.316<sub>±1.217</sub> |     25.48<sub>±00.04</sub> + 3.64<sub>±00.00</sub> |   863.75<sub>±64.05</sub> |
|                    D/dmd |  42.491<sub>±0.121</sub> |      3.70<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |   963.22<sub>±03.28</sub> |
|                  C#/Mono |  44.502<sub>±0.272</sub> |     20.11<sub>±00.02</sub> + 0.88<sub>±00.00</sub> |   980.24<sub>±07.72</sub> |
|                    MLton |  51.981<sub>±1.646</sub> |      1.41<sub>±00.04</sub> + 4.11<sub>±00.00</sub> |  1104.45<sub>±82.03</sub> |
|         Haskell (MArray) |  60.083<sub>±0.844</sub> |      3.78<sub>±00.10</sub> + 2.50<sub>±00.13</sub> |  1379.17<sub>±42.02</sub> |
|              Python/pypy |  64.354<sub>±0.552</sub> |    65.38<sub>±00.11</sub> + 46.41<sub>±00.02</sub> |  1229.14<sub>±09.32</sub> |
|                    Julia |  78.738<sub>±1.108</sub> |    198.50<sub>±00.04</sub> + 0.78<sub>±00.05</sub> |  1455.76<sub>±70.63</sub> |
| Ruby/truffleruby (--jvm) | 134.368<sub>±3.421</sub> | 584.53<sub>±06.57</sub> + 745.92<sub>±114.67</sub> |  2794.06<sub>±50.97</sub> |
|         Ruby/truffleruby | 150.668<sub>±4.223</sub> |  414.64<sub>±00.37</sub> + 585.68<sub>±11.10</sub> | 2907.83<sub>±122.54</sub> |
|                  Haskell | 226.291<sub>±4.319</sub> |     4.04<sub>±00.10</sub> + 25.91<sub>±00.00</sub> | 4356.11<sub>±160.91</sub> |
|               Lua/luajit | 247.906<sub>±5.559</sub> |      2.87<sub>±00.03</sub> + 0.86<sub>±00.00</sub> | 4766.50<sub>±239.31</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.155<sub>±0.004</sub> |     1.90<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.70<sub>±00.39</sub> |
|                      Rust |  1.186<sub>±0.015</sub> |     2.49<sub>±00.10</sub> + 0.01<sub>±00.00</sub> |  21.80<sub>±01.84</sub> |
|                     C/gcc |  1.218<sub>±0.036</sub> |     1.85<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  23.94<sub>±01.91</sub> |
|                   Nim/gcc |  1.254<sub>±0.014</sub> |     2.17<sub>±00.05</sub> + 4.44<sub>±00.00</sub> |  28.92<sub>±01.10</sub> |
|                   Crystal |  1.612<sub>±0.015</sub> |     3.72<sub>±00.03</sub> + 1.83<sub>±00.04</sub> |  39.30<sub>±00.13</sub> |
|                 Nim/clang |  1.634<sub>±0.061</sub> |     2.67<sub>±00.07</sub> + 4.38<sub>±00.00</sub> |  36.75<sub>±01.05</sub> |
|                    D/ldc2 |  1.967<sub>±0.032</sub> |     3.41<sub>±00.03</sub> + 3.66<sub>±00.00</sub> |  35.17<sub>±00.62</sub> |
|                     V/gcc |  1.977<sub>±0.035</sub> |     2.12<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  34.56<sub>±02.67</sub> |
|                      Ruby |  2.019<sub>±0.072</sub> |   14.45<sub>±00.03</sub> + 56.75<sub>±00.53</sub> |  46.19<sub>±02.13</sub> |
|              Ruby (--jit) |  2.132<sub>±0.068</sub> |   14.43<sub>±00.04</sub> + 57.92<sub>±00.42</sub> |  41.36<sub>±02.27</sub> |
|                Vala/clang |  2.142<sub>±0.078</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  46.28<sub>±02.00</sub> |
|                     D/gdc |  2.175<sub>±0.121</sub> |     7.06<sub>±00.04</sub> + 3.46<sub>±00.00</sub> |  44.27<sub>±06.07</sub> |
|                      Java |  2.202<sub>±0.066</sub> |  38.90<sub>±00.21</sub> + 280.75<sub>±09.17</sub> |  49.87<sub>±01.20</sub> |
|                  Vala/gcc |  2.224<sub>±0.030</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  42.00<sub>±03.33</sub> |
|                     Scala |  2.403<sub>±0.086</sub> |  53.57<sub>±00.13</sub> + 312.97<sub>±21.36</sub> |  52.53<sub>±02.64</sub> |
|                    Kotlin |  2.406<sub>±0.019</sub> |  40.63<sub>±00.16</sub> + 327.63<sub>±29.57</sub> |  48.30<sub>±01.93</sub> |
|       C++/g++ (libcrypto) |  2.408<sub>±0.048</sub> |     5.39<sub>±00.03</sub> + 0.07<sub>±00.00</sub> |  56.94<sub>±01.66</sub> |
|                        Go |  2.558<sub>±0.017</sub> |     4.56<sub>±00.04</sub> + 5.61<sub>±00.26</sub> |  49.88<sub>±00.83</sub> |
|                   V/clang |  2.782<sub>±0.142</sub> |     1.82<sub>±00.26</sub> + 0.13<sub>±00.13</sub> |  55.12<sub>±04.19</sub> |
|                   Node.js |  2.792<sub>±0.046</sub> |   31.72<sub>±00.05</sub> + 36.18<sub>±00.07</sub> |  58.48<sub>±03.21</sub> |
|       Perl (MIME::Base64) |  2.901<sub>±0.089</sub> |    14.12<sub>±00.06</sub> + 0.08<sub>±00.06</sub> |  62.70<sub>±02.33</sub> |
|                       PHP |  3.062<sub>±0.142</sub> |    15.76<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |  64.64<sub>±06.94</sub> |
|                  Go/gccgo |  3.514<sub>±0.009</sub> |    22.45<sub>±00.34</sub> + 7.19<sub>±00.29</sub> |  74.67<sub>±01.32</sub> |
|                     D/dmd |  3.825<sub>±0.098</sub> |     3.80<sub>±00.02</sub> + 3.69<sub>±00.03</sub> |  72.83<sub>±04.90</sub> |
|                    Python |  4.112<sub>±0.145</sub> |    10.09<sub>±00.02</sub> + 0.18<sub>±00.00</sub> |  81.15<sub>±07.44</sub> |
|                       Tcl |  4.896<sub>±0.097</sub> |     4.83<sub>±00.04</sub> + 0.19<sub>±00.01</sub> |  87.33<sub>±03.34</sub> |
|               Python/pypy |  5.049<sub>±0.052</sub> |   65.29<sub>±00.02</sub> + 45.68<sub>±00.04</sub> |  97.29<sub>±08.84</sub> |
|              F#/.NET Core |  5.668<sub>±0.090</sub> |   37.03<sub>±00.03</sub> + 37.94<sub>±05.46</sub> | 107.68<sub>±02.72</sub> |
|              C#/.NET Core |  5.707<sub>±0.069</sub> |   34.48<sub>±00.09</sub> + 39.49<sub>±06.99</sub> | 112.02<sub>±02.38</sub> |
|                     Julia |  6.078<sub>±0.079</sub> |  216.92<sub>±00.09</sub> + 52.82<sub>±10.27</sub> | 120.10<sub>±06.44</sub> |
|  Ruby/truffleruby (--jvm) |  6.928<sub>±0.193</sub> | 568.10<sub>±10.90</sub> + 364.17<sub>±12.04</sub> | 138.24<sub>±11.06</sub> |
|                   C#/Mono |  7.404<sub>±0.387</sub> |   20.56<sub>±00.04</sub> + 18.48<sub>±00.02</sub> | 174.66<sub>±04.52</sub> |
|                Ruby/jruby | 12.104<sub>±0.464</sub> | 188.35<sub>±04.81</sub> + 134.19<sub>±05.02</sub> | 247.75<sub>±11.06</sub> |
| Perl (MIME::Base64::Perl) | 16.679<sub>±0.661</sub> |    15.40<sub>±00.10</sub> + 0.22<sub>±00.06</sub> | 363.19<sub>±30.65</sub> |
|          Ruby/truffleruby | 23.147<sub>±0.630</sub> | 405.72<sub>±01.19</sub> + 305.43<sub>±02.85</sub> | 441.44<sub>±36.07</sub> |

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

|                        Language |                 Time, s |                                        Memory, MiB |               Energy, J |
| :------------------------------ | ----------------------: | -------------------------------------------------: | ----------------------: |
| C++/g++ (DAW JSON Link NoCheck) |  0.088<sub>±0.001</sub> |    113.03<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1.92<sub>±00.02</sub> |
|    C++/g++ (simdjson On-Demand) |  0.096<sub>±0.001</sub> |   113.21<sub>±00.05</sub> + 59.81<sub>±00.00</sub> |   2.07<sub>±00.08</sub> |
|         C++/g++ (DAW JSON Link) |  0.124<sub>±0.002</sub> |    112.80<sub>±00.20</sub> + 0.00<sub>±00.00</sub> |   2.56<sub>±00.05</sub> |
|             Rust (Serde Custom) |  0.137<sub>±0.003</sub> |    111.86<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   3.15<sub>±00.18</sub> |
|              Rust (Serde Typed) |  0.151<sub>±0.004</sub> |   111.87<sub>±00.07</sub> + 11.70<sub>±00.19</sub> |   2.73<sub>±00.18</sub> |
|                 C++/g++ (gason) |  0.158<sub>±0.002</sub> |   113.00<sub>±00.04</sub> + 96.74<sub>±00.06</sub> |   4.07<sub>±00.20</sub> |
|          C++/g++ (simdjson DOM) |  0.172<sub>±0.003</sub> |  113.28<sub>±00.03</sub> + 176.60<sub>±00.00</sub> |   3.68<sub>±00.16</sub> |
|             C++/g++ (RapidJSON) |  0.227<sub>±0.006</sub> |  113.03<sub>±00.03</sub> + 128.77<sub>±00.06</sub> |   5.02<sub>±00.42</sub> |
|   D/ldc2 (Mir Amazon's Ion DOM) |  0.229<sub>±0.006</sub> |   112.70<sub>±00.04</sub> + 16.26<sub>±00.00</sub> |   4.97<sub>±00.44</sub> |
|           D/ldc2 (Mir Asdf DOM) |  0.243<sub>±0.004</sub> |   112.67<sub>±00.07</sub> + 61.34<sub>±00.01</sub> |   5.26<sub>±00.16</sub> |
|     C++/g++ (RapidJSON Precise) |  0.290<sub>±0.009</sub> |  113.07<sub>±00.02</sub> + 128.71<sub>±00.05</sub> |   6.29<sub>±00.56</sub> |
|            C++/g++ (Boost.JSON) |  0.529<sub>±0.020</sub> |  113.13<sub>±00.01</sub> + 435.76<sub>±00.06</sub> |  11.13<sub>±00.67</sub> |
|         C++/g++ (RapidJSON SAX) |  0.589<sub>±0.009</sub> |    112.79<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  10.30<sub>±00.40</sub> |
| C++/g++ (RapidJSON SAX Precise) |  0.638<sub>±0.025</sub> |    112.82<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  13.41<sub>±01.12</sub> |
|                   Go (jsoniter) |  0.695<sub>±0.023</sub> |   231.35<sub>±00.02</sub> + 13.59<sub>±00.28</sub> |  14.49<sub>±01.06</sub> |
|                Crystal (Schema) |  0.733<sub>±0.011</sub> |   113.73<sub>±00.03</sub> + 42.72<sub>±00.03</sub> |  15.30<sub>±00.17</sub> |
|                  Crystal (Pull) |  0.764<sub>±0.019</sub> |   113.71<sub>±00.04</sub> + 18.18<sub>±00.03</sub> |  15.22<sub>±01.41</sub> |
|                 Java (DSL-JSON) |  0.803<sub>±0.025</sub> |  264.04<sub>±00.14</sub> + 311.38<sub>±02.85</sub> |  20.76<sub>±00.82</sub> |
|                         V/clang |  0.847<sub>±0.020</sub> |  111.15<sub>±00.01</sub> + 496.21<sub>±00.06</sub> |  19.21<sub>±01.32</sub> |
|            Rust (Serde Untyped) |  0.854<sub>±0.029</sub> |  111.89<sub>±00.08</sub> + 839.98<sub>±00.00</sub> |  17.32<sub>±01.54</sub> |
|                     Python/pypy |  0.862<sub>±0.012</sub> |  285.48<sub>±00.09</sub> + 121.42<sub>±00.01</sub> |  16.56<sub>±00.28</sub> |
|                         Node.js |  0.877<sub>±0.025</sub> |   251.02<sub>±00.03</sub> + 78.29<sub>±00.74</sub> |  23.02<sub>±00.55</sub> |
|                   Julia (JSON3) |  0.960<sub>±0.024</sub> |  368.78<sub>±00.22</sub> + 373.25<sub>±00.20</sub> |  18.56<sub>±00.83</sub> |
|                           V/gcc |  0.969<sub>±0.043</sub> |  111.14<sub>±00.01</sub> + 496.21<sub>±00.00</sub> |  20.08<sub>±02.07</sub> |
|         Perl (Cpanel::JSON::XS) |  0.986<sub>±0.010</sub> |  124.67<sub>±00.04</sub> + 402.78<sub>±00.03</sub> |  23.49<sub>±00.48</sub> |
|                         Crystal |  1.030<sub>±0.023</sub> |  113.72<sub>±00.02</sub> + 396.45<sub>±00.01</sub> |  22.79<sub>±00.49</sub> |
| C#/.NET Core (System.Text.Json) |  1.060<sub>±0.043</sub> |  479.22<sub>±00.09</sub> + 138.78<sub>±00.06</sub> |  22.38<sub>±02.34</sub> |
|                              Go |  1.097<sub>±0.036</sub> |   117.39<sub>±00.05</sub> + 83.29<sub>±00.11</sub> |  24.76<sub>±01.61</sub> |
|          Nim/clang (Packedjson) |  1.103<sub>±0.041</sub> |  112.47<sub>±00.10</sub> + 294.04<sub>±00.13</sub> |  25.56<sub>±01.59</sub> |
|                             PHP |  1.277<sub>±0.046</sub> |  124.99<sub>±00.14</sub> + 682.01<sub>±00.00</sub> |  25.92<sub>±02.27</sub> |
|            Nim/gcc (Packedjson) |  1.380<sub>±0.057</sub> |  112.05<sub>±00.11</sub> + 293.91<sub>±00.00</sub> |  28.11<sub>±02.64</sub> |
|                        Go/gccgo |  1.450<sub>±0.030</sub> |   142.23<sub>±06.08</sub> + 96.03<sub>±00.17</sub> |  33.10<sub>±01.45</sub> |
|                C++/g++ (json-c) |  1.532<sub>±0.030</sub> | 112.93<sub>±00.19</sub> + 1216.23<sub>±00.11</sub> |  36.83<sub>±01.14</sub> |
|                         Clojure |  1.609<sub>±0.044</sub> |  473.39<sub>±03.17</sub> + 576.83<sub>±05.74</sub> |  46.18<sub>±01.78</sub> |
|                       Nim/clang |  1.610<sub>±0.055</sub> |  112.41<sub>±00.08</sub> + 925.03<sub>±00.06</sub> |  37.65<sub>±01.70</sub> |
|                    C#/.NET Core |  1.730<sub>±0.058</sub> |  487.98<sub>±00.04</sub> + 294.07<sub>±00.00</sub> |  37.47<sub>±04.26</sub> |
|             CPython (UltraJSON) |  1.780<sub>±0.023</sub> |  121.82<sub>±00.03</sub> + 549.14<sub>±01.04</sub> |  41.90<sub>±00.80</sub> |
|              C++/g++ (Nlohmann) |  1.809<sub>±0.011</sub> |  113.15<sub>±00.02</sub> + 447.88<sub>±00.00</sub> |  34.77<sub>±01.10</sub> |
|                         Nim/gcc |  1.844<sub>±0.070</sub> |  112.11<sub>±00.04</sub> + 923.29<sub>±00.06</sub> |  37.81<sub>±03.36</sub> |
|                          Python |  1.848<sub>±0.014</sub> |  120.09<sub>±00.03</sub> + 377.21<sub>±00.00</sub> |  38.12<sub>±01.17</sub> |
|                         C#/Mono |  2.236<sub>±0.089</sub> |    476.19<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  47.40<sub>±05.36</sub> |
|                           D/gdc |  2.283<sub>±0.028</sub> |  116.61<sub>±00.04</sub> + 600.39<sub>±00.00</sub> |  53.37<sub>±00.83</sub> |
|                         Haskell |  2.291<sub>±0.051</sub> |      4.44<sub>±00.05</sub> + 4.68<sub>±00.03</sub> |  43.10<sub>±02.05</sub> |
|                            Ruby |  2.426<sub>±0.058</sub> |  124.00<sub>±00.02</sub> + 410.69<sub>±00.01</sub> |  57.63<sub>±01.08</sub> |
|                     Ruby (YAJL) |  2.443<sub>±0.047</sub> |  123.96<sub>±00.02</sub> + 283.34<sub>±00.00</sub> |  44.94<sub>±01.47</sub> |
|                 Scala (uPickle) |  2.516<sub>±0.053</sub> |  284.73<sub>±00.09</sub> + 762.67<sub>±23.36</sub> |  59.39<sub>±04.12</sub> |
| F#/.NET Core (System.Text.Json) |  2.577<sub>±0.083</sub> |  485.37<sub>±00.05</sub> + 453.88<sub>±02.45</sub> |  55.03<sub>±05.61</sub> |
|                          D/ldc2 |  2.585<sub>±0.057</sub> |  112.92<sub>±00.06</sub> + 680.09<sub>±00.04</sub> |  53.34<sub>±01.70</sub> |
|                    Ruby (--jit) |  2.589<sub>±0.022</sub> |  124.01<sub>±00.06</sub> + 410.80<sub>±00.04</sub> |  50.43<sub>±00.51</sub> |
|                       Rust (jq) |  3.584<sub>±0.035</sub> |  113.83<sub>±00.02</sub> + 777.86<sub>±00.90</sub> |  81.39<sub>±00.50</sub> |
|                      Ruby/jruby |  3.876<sub>±0.084</sub> | 453.42<sub>±10.48</sub> + 1412.22<sub>±28.09</sub> | 108.53<sub>±06.77</sub> |
|    C++/g++ (Boost.PropertyTree) |  4.483<sub>±0.118</sub> | 112.95<sub>±00.04</sub> + 1440.06<sub>±00.00</sub> | 101.16<sub>±06.49</sub> |
|                           D/dmd |  5.098<sub>±0.075</sub> |  113.65<sub>±00.06</sub> + 680.25<sub>±00.01</sub> | 102.46<sub>±04.22</sub> |
|                        Vala/gcc |  5.473<sub>±0.217</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  99.96<sub>±06.82</sub> |
|                      Vala/clang |  5.602<sub>±0.056</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 101.12<sub>±00.60</sub> |
|               Perl (JSON::Tiny) | 12.605<sub>±0.279</sub> |  125.30<sub>±00.07</sub> + 528.63<sub>±00.10</sub> | 231.12<sub>±05.85</sub> |
|        Ruby/truffleruby (--jvm) | 16.962<sub>±0.464</sub> | 721.72<sub>±12.93</sub> + 1648.53<sub>±20.81</sub> | 479.86<sub>±14.66</sub> |
|                Ruby/truffleruby | 40.369<sub>±0.806</sub> | 738.03<sub>±01.52</sub> + 2269.36<sub>±70.04</sub> | 834.73<sub>±65.49</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                  Time, s |                                        Memory, MiB |                  Energy, J |
| :----------------------- | -----------------------: | -------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |   0.081<sub>±0.002</sub> |     6.70<sub>±00.06</sub> + 55.90<sub>±00.12</sub> |      4.58<sub>±00.10</sub> |
|           Python (NumPy) |   0.107<sub>±0.003</sub> |    27.62<sub>±00.10</sub> + 57.64<sub>±00.06</sub> |      6.15<sub>±00.19</sub> |
|    Nim/gcc (Arraymancer) |   0.158<sub>±0.029</sub> |     5.58<sub>±00.10</sub> + 57.28<sub>±00.12</sub> |      8.16<sub>±00.82</sub> |
|  Nim/clang (Arraymancer) |   0.189<sub>±0.019</sub> |     6.44<sub>±00.14</sub> + 57.38<sub>±00.07</sub> |      9.07<sub>±00.70</sub> |
|          C++/g++ (Eigen) |   0.203<sub>±0.007</sub> |     3.60<sub>±00.08</sub> + 85.24<sub>±00.01</sub> |      4.59<sub>±00.22</sub> |
|       Julia (threads: 8) |   0.207<sub>±0.004</sub> |   251.36<sub>±00.42</sub> + 53.11<sub>±00.12</sub> |     11.03<sub>±00.42</sub> |
|              Java (ND4J) |   0.232<sub>±0.023</sub> |   145.25<sub>±01.16</sub> + 87.50<sub>±00.13</sub> |     10.77<sub>±00.94</sub> |
|       Julia (threads: 1) |   0.552<sub>±0.013</sub> |   251.36<sub>±00.14</sub> + 53.60<sub>±00.09</sub> |     12.89<sub>±00.44</sub> |
|          Julia (no BLAS) |   1.220<sub>±0.018</sub> |   216.72<sub>±00.33</sub> + 51.63<sub>±00.00</sub> |     30.49<sub>±00.74</sub> |
|                   D/ldc2 |   1.991<sub>±0.007</sub> |     3.52<sub>±00.12</sub> + 70.11<sub>±00.13</sub> |     44.85<sub>±00.56</sub> |
|                    D/dmd |   2.135<sub>±0.024</sub> |     3.59<sub>±00.10</sub> + 70.12<sub>±00.01</sub> |     48.46<sub>±00.73</sub> |
|                    D/gdc |   2.149<sub>±0.057</sub> |     6.63<sub>±00.06</sub> + 70.71<sub>±00.02</sub> |     48.48<sub>±03.20</sub> |
|                     Java |   3.244<sub>±0.025</sub> |    38.55<sub>±00.15</sub> + 80.66<sub>±00.55</sub> |     76.29<sub>±01.27</sub> |
|                 Vala/gcc |   3.271<sub>±0.021</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     79.07<sub>±01.08</sub> |
|                    C/gcc |   3.357<sub>±0.055</sub> |     2.01<sub>±00.04</sub> + 68.06<sub>±00.00</sub> |     77.55<sub>±01.22</sub> |
|                  Nim/gcc |   3.372<sub>±0.017</sub> |     2.56<sub>±00.05</sub> + 65.48<sub>±00.00</sub> |     81.10<sub>±03.55</sub> |
|               Vala/clang |   3.426<sub>±0.024</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     74.62<sub>±00.92</sub> |
|                     Rust |   3.445<sub>±0.016</sub> |     2.71<sub>±00.12</sub> + 68.32<sub>±00.00</sub> |     74.65<sub>±00.66</sub> |
|                    Scala |   3.449<sub>±0.027</sub> |   55.68<sub>±00.06</sub> + 153.92<sub>±00.12</sub> |     82.04<sub>±01.31</sub> |
|                Nim/clang |   3.469<sub>±0.050</sub> |     3.07<sub>±00.02</sub> + 66.00<sub>±00.00</sub> |     74.45<sub>±00.98</sub> |
|                       Go |   3.543<sub>±0.023</sub> |     3.80<sub>±00.06</sub> + 73.36<sub>±00.09</sub> |     80.52<sub>±03.47</sub> |
|                 Go/gccgo |   3.564<sub>±0.086</sub> |    21.59<sub>±00.18</sub> + 72.83<sub>±00.16</sub> |     78.12<sub>±02.04</sub> |
|                    Swift |   3.646<sub>±0.088</sub> |   145.20<sub>±00.05</sub> + 60.64<sub>±00.03</sub> |     86.24<sub>±03.09</sub> |
|                  Crystal |   3.728<sub>±0.104</sub> |     4.15<sub>±00.12</sub> + 59.64<sub>±00.11</sub> |     79.90<sub>±06.27</sub> |
|                    V/gcc |   3.757<sub>±0.127</sub> |     1.87<sub>±00.05</sub> + 69.35<sub>±00.00</sub> |     80.73<sub>±07.42</sub> |
|                   Kotlin |   3.800<sub>±0.048</sub> |    38.80<sub>±00.22</sub> + 80.71<sub>±00.38</sub> |     88.40<sub>±02.97</sub> |
|                  Node.js |   4.925<sub>±0.088</sub> |    35.33<sub>±00.09</sub> + 71.08<sub>±00.44</sub> |     89.30<sub>±04.41</sub> |
|              Python/pypy |   6.541<sub>±0.073</sub> |    65.85<sub>±00.13</sub> + 69.18<sub>±00.04</sub> |    114.00<sub>±03.02</sub> |
|             C#/.NET Core |   7.373<sub>±0.141</sub> |    33.97<sub>±00.11</sub> + 69.11<sub>±00.00</sub> |    133.57<sub>±05.53</sub> |
|                  C#/Mono |  10.775<sub>±0.202</sub> |    20.06<sub>±00.06</sub> + 69.06<sub>±00.02</sub> |    240.98<sub>±06.98</sub> |
|                  V/clang |  14.013<sub>±0.187</sub> |     2.20<sub>±00.04</sub> + 69.35<sub>±00.00</sub> |    237.08<sub>±15.02</sub> |
|         Ruby/truffleruby |  46.906<sub>±0.541</sub> |  573.59<sub>±00.56</sub> + 684.43<sub>±03.60</sub> |   1118.10<sub>±14.38</sub> |
| Ruby/truffleruby (--jvm) |  68.521<sub>±0.596</sub> |  650.54<sub>±12.48</sub> + 693.22<sub>±91.72</sub> |   1633.85<sub>±53.37</sub> |
|                     Ruby | 220.767<sub>±2.987</sub> |    15.22<sub>±00.05</sub> + 68.61<sub>±00.03</sub> |   4856.07<sub>±55.59</sub> |
|             Ruby (--jit) | 225.230<sub>±3.868</sub> |    15.20<sub>±00.02</sub> + 68.91<sub>±00.03</sub> |   4870.24<sub>±53.87</sub> |
|                   Python | 241.153<sub>±8.661</sub> |    10.53<sub>±00.02</sub> + 68.58<sub>±00.00</sub> |  4955.07<sub>±173.31</sub> |
|                      Tcl | 358.681<sub>±3.162</sub> |    7.26<sub>±00.03</sub> + 400.44<sub>±00.00</sub> |  6919.11<sub>±392.92</sub> |
|                     Perl | 405.584<sub>±6.809</sub> |    9.00<sub>±00.06</sub> + 599.62<sub>±00.04</sub> |  9364.17<sub>±679.44</sub> |
|               Ruby/jruby | 500.132<sub>±7.243</sub> | 263.98<sub>±04.55</sub> + 1004.45<sub>±37.01</sub> | 10848.95<sub>±221.31</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                  Crystal | 0.161<sub>±0.002</sub> |    3.36<sub>±00.04</sub> + 88.63<sub>±01.41</sub> |   3.94<sub>±00.16</sub> |
|                     Java | 0.178<sub>±0.002</sub> |  36.02<sub>±00.30</sub> + 106.19<sub>±01.54</sub> |   5.60<sub>±00.32</sub> |
|                     Rust | 0.219<sub>±0.004</sub> |    2.40<sub>±00.06</sub> + 72.88<sub>±00.00</sub> |   4.34<sub>±00.03</sub> |
|                  Node.js | 0.286<sub>±0.003</sub> |  31.00<sub>±00.09</sub> + 175.57<sub>±00.20</sub> |   8.05<sub>±00.04</sub> |
|                  C++/g++ | 0.288<sub>±0.012</sub> |   3.19<sub>±00.03</sub> + 125.23<sub>±00.00</sub> |   5.43<sub>±00.35</sub> |
|                    Scala | 0.423<sub>±0.016</sub> |  55.71<sub>±00.12</sub> + 246.59<sub>±02.58</sub> |  14.71<sub>±00.50</sub> |
|              Python/pypy | 0.963<sub>±0.039</sub> |  64.83<sub>±00.13</sub> + 248.84<sub>±00.14</sub> |  22.58<sub>±00.69</sub> |
|         Ruby/truffleruby | 1.254<sub>±0.028</sub> | 397.95<sub>±01.44</sub> + 446.35<sub>±01.65</sub> |  33.48<sub>±02.20</sub> |
| Ruby/truffleruby (--jvm) | 1.717<sub>±0.045</sub> | 577.56<sub>±19.73</sub> + 370.37<sub>±41.51</sub> |  63.60<sub>±03.08</sub> |
|             Ruby (--jit) | 2.396<sub>±0.029</sub> |  14.12<sub>±00.07</sub> + 150.16<sub>±00.00</sub> |  64.55<sub>±02.34</sub> |
|                     Ruby | 2.619<sub>±0.066</sub> |  14.01<sub>±00.08</sub> + 149.86<sub>±00.03</sub> |  46.43<sub>±04.26</sub> |
|               Ruby/jruby | 2.876<sub>±0.080</sub> | 190.72<sub>±06.08</sub> + 362.10<sub>±07.62</sub> |  78.15<sub>±02.13</sub> |
|                   Python | 5.477<sub>±0.123</sub> |  10.16<sub>±00.05</sub> + 236.08<sub>±00.60</sub> | 114.04<sub>±06.36</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.204                         |
| C#/.NET Core     | 3.9.0-6.21160.10 (59eedc33)     |
| C#/Mono          | 6.12.0.122                      |
| C/clang          | 11.0.1                          |
| C/gcc            | 10.2.1                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.3"                        |
| Crystal          | 1.0.0                           |
| D/dmd            | v2.097.0                        |
| D/gdc            | 10.2.1                          |
| D/ldc2           | 1.26.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 11.3.2.0 for F# 5.0             |
| Go               | go1.16.5                        |
| Go/gccgo         | 10.2.1                          |
| Haskell          | 9.0.1                           |
| Java             | 16.0.1                          |
| Julia            | v"1.6.1"                        |
| Kotlin           | 1.5.20                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20210117                        |
| Nim              | 1.4.8                           |
| Node.js          | v16.4.0                         |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.15                          |
| Perl             | v5.32.1                         |
| Python           | 3.9.2                           |
| Python/pypy      | 7.3.5-final0 for Python 3.7.10  |
| Racket           | "8.1"                           |
| Ruby             | 3.0.1p64                        |
| Ruby/jruby       | 9.2.19.0                        |
| Ruby/truffleruby | 21.1.0                          |
| Rust             | 1.53.0                          |
| Scala            | 3.0.0                           |
| Swift            | swift-5.4.2-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.2.2                           |
| Vala             | 0.48.17                         |

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
