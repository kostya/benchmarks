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

UPDATE: 2021-02-27

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
|                  C++/g++ |    0.892<sub>±0.022</sub> |     1.48<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     18.30<sub>±00.92</sub> |
|  Racket (Syntax Objects) |    1.375<sub>±0.019</sub> |   110.18<sub>±00.36</sub> + 0.00<sub>±00.00</sub> |     32.45<sub>±00.85</sub> |
|                   Kotlin |    1.887<sub>±0.053</sub> |    38.23<sub>±00.10</sub> + 1.96<sub>±00.09</sub> |     35.30<sub>±01.29</sub> |
|                   D/ldc2 |    1.888<sub>±0.048</sub> |     3.03<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     37.00<sub>±02.52</sub> |
|                  Nim/gcc |    1.900<sub>±0.091</sub> |     1.83<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     40.81<sub>±02.49</sub> |
|                    C/gcc |    1.939<sub>±0.048</sub> |     0.54<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     37.72<sub>±03.49</sub> |
|                    D/gdc |    1.940<sub>±0.124</sub> |     6.26<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     40.74<sub>±03.16</sub> |
|                Nim/clang |    2.061<sub>±0.099</sub> |     2.32<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     42.74<sub>±02.22</sub> |
|                     Rust |    2.156<sub>±0.030</sub> |     2.02<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |     42.93<sub>±03.62</sub> |
|                     Java |    2.184<sub>±0.076</sub> |    37.52<sub>±00.13</sub> + 1.03<sub>±00.07</sub> |     40.80<sub>±02.68</sub> |
|                   Racket |    2.194<sub>±0.194</sub> |   116.20<sub>±00.06</sub> + 1.55<sub>±00.26</sub> |     55.26<sub>±02.86</sub> |
|                  C/clang |    2.270<sub>±0.083</sub> |     0.49<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     44.68<sub>±03.72</sub> |
|                    OCaml |    2.270<sub>±0.056</sub> |     2.56<sub>±00.02</sub> + 2.51<sub>±00.03</sub> |     46.23<sub>±05.72</sub> |
|                       Go |    2.344<sub>±0.073</sub> |     3.47<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     44.27<sub>±02.99</sub> |
|             C#/.NET Core |    2.362<sub>±0.124</sub> |    34.19<sub>±00.04</sub> + 0.01<sub>±00.00</sub> |     46.54<sub>±02.62</sub> |
|             F#/.NET Core |    2.371<sub>±0.009</sub> |    36.78<sub>±00.11</sub> + 0.33<sub>±00.03</sub> |     42.51<sub>±00.85</sub> |
|                 Vala/gcc |    2.419<sub>±0.098</sub> |     3.72<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     45.36<sub>±02.88</sub> |
|               Vala/clang |    2.492<sub>±0.076</sub> |     3.67<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     51.39<sub>±04.14</sub> |
|                    V/gcc |    2.536<sub>±0.121</sub> |     0.55<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     54.42<sub>±03.05</sub> |
|                  Crystal |    2.560<sub>±0.156</sub> |     3.29<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     51.61<sub>±03.00</sub> |
|                 Go/gccgo |    2.638<sub>±0.310</sub> |    20.96<sub>±00.17</sub> + 0.00<sub>±00.00</sub> |     52.51<sub>±08.29</sub> |
|                    MLton |    2.774<sub>±0.134</sub> |     1.40<sub>±00.02</sub> + 0.25<sub>±00.00</sub> |     61.95<sub>±05.52</sub> |
|                  V/clang |    2.916<sub>±0.086</sub> |     0.85<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     62.00<sub>±03.80</sub> |
|                    Julia |    3.033<sub>±0.098</sub> |   168.18<sub>±00.20</sub> + 0.00<sub>±00.00</sub> |     54.70<sub>±04.97</sub> |
|              Chez Scheme |    3.059<sub>±0.083</sub> |    24.83<sub>±00.02</sub> + 4.21<sub>±00.04</sub> |     59.99<sub>±04.24</sub> |
|                    Scala |    3.520<sub>±0.024</sub> |   80.43<sub>±00.92</sub> + 60.43<sub>±06.97</sub> |     69.69<sub>±01.65</sub> |
|                    D/dmd |    3.590<sub>±0.105</sub> |     3.60<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     70.59<sub>±04.10</sub> |
|                  Node.js |    4.171<sub>±0.210</sub> |    30.23<sub>±00.05</sub> + 1.76<sub>±00.00</sub> |     82.40<sub>±06.47</sub> |
|                  C#/Mono |    4.475<sub>±0.155</sub> |    20.05<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |     81.10<sub>±04.83</sub> |
|         Haskell (MArray) |    4.648<sub>±0.172</sub> |     3.59<sub>±00.05</sub> + 1.16<sub>±00.00</sub> |     99.26<sub>±07.84</sub> |
|               Lua/luajit |    6.416<sub>±0.322</sub> |     2.89<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    134.68<sub>±08.07</sub> |
|         Ruby/truffleruby |    9.975<sub>±0.264</sub> | 250.78<sub>±00.14</sub> + 750.14<sub>±20.30</sub> |    263.86<sub>±19.35</sub> |
| Ruby/truffleruby (--jvm) |   10.610<sub>±0.952</sub> | 565.07<sub>±05.30</sub> + 546.80<sub>±66.01</sub> |    352.97<sub>±15.23</sub> |
|              Python/pypy |   15.790<sub>±0.416</sub> |   63.91<sub>±00.24</sub> + 45.38<sub>±00.03</sub> |    344.53<sub>±22.25</sub> |
|                  Haskell |   16.417<sub>±0.706</sub> |     3.79<sub>±00.04</sub> + 0.88<sub>±00.00</sub> |    386.41<sub>±14.14</sub> |
|             Ruby (--jit) |   58.145<sub>±1.318</sub> |    14.03<sub>±00.02</sub> + 0.23<sub>±00.00</sub> |   1285.44<sub>±28.51</sub> |
|                      Lua |   59.404<sub>±1.460</sub> |     2.97<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   1116.62<sub>±41.84</sub> |
|                     Ruby |   92.382<sub>±2.821</sub> |    14.01<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   1720.82<sub>±80.86</sub> |
|               Ruby/jruby |  107.728<sub>±5.276</sub> | 205.96<sub>±05.72</sub> + 235.28<sub>±04.00</sub> |   2175.45<sub>±74.60</sub> |
|                   Elixir |  119.456<sub>±3.617</sub> |    58.12<sub>±01.06</sub> + 0.00<sub>±00.00</sub> |   2606.79<sub>±74.01</sub> |
|                   Python |  234.997<sub>±6.281</sub> |    10.29<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   5337.77<sub>±84.07</sub> |
|                 Tcl (FP) |  291.608<sub>±2.969</sub> |     4.28<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  5530.18<sub>±199.13</sub> |
|                     Perl |  362.950<sub>±6.122</sub> |     6.48<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  7931.43<sub>±195.67</sub> |
|                Tcl (OOP) | 560.871<sub>±14.755</sub> |     4.27<sub>±00.03</sub> + 0.00<sub>±00.00</sub> | 11993.69<sub>±613.22</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                    C/gcc |  13.477<sub>±0.263</sub> |     0.52<sub>±00.03</sub> + 1.09<sub>±00.03</sub> |   234.35<sub>±09.07</sub> |
|                  C++/g++ |  14.001<sub>±0.347</sub> |     3.14<sub>±00.15</sub> + 0.50<sub>±00.03</sub> |   244.94<sub>±11.59</sub> |
|                   D/ldc2 |  14.281<sub>±0.213</sub> |     3.03<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |   258.85<sub>±14.10</sub> |
|                    D/gdc |  14.499<sub>±0.398</sub> |     6.69<sub>±00.08</sub> + 0.52<sub>±00.00</sub> |   253.16<sub>±10.80</sub> |
|                  Nim/gcc |  16.364<sub>±0.246</sub> |     1.84<sub>±00.01</sub> + 0.57<sub>±00.00</sub> |   361.69<sub>±08.01</sub> |
|                    V/gcc |  16.535<sub>±0.686</sub> |     0.52<sub>±00.03</sub> + 1.89<sub>±00.05</sub> |   310.30<sub>±22.82</sub> |
|                   Kotlin |  16.541<sub>±0.824</sub> |    38.35<sub>±00.21</sub> + 2.50<sub>±00.25</sub> |   331.29<sub>±19.28</sub> |
|                       Go |  16.760<sub>±0.574</sub> |     3.43<sub>±00.09</sub> + 1.29<sub>±00.00</sub> |   339.53<sub>±29.47</sub> |
|  Racket (Syntax Objects) |  17.710<sub>±0.900</sub> |  110.05<sub>±00.13</sub> + 70.90<sub>±00.13</sub> |   362.86<sub>±07.00</sub> |
|                  C/clang |  18.400<sub>±0.625</sub> |     0.49<sub>±00.00</sub> + 1.11<sub>±00.01</sub> |   363.79<sub>±24.65</sub> |
|             C#/.NET Core |  18.763<sub>±0.666</sub> |    34.35<sub>±00.06</sub> + 1.00<sub>±00.00</sub> |   368.78<sub>±32.99</sub> |
|                     Rust |  19.505<sub>±0.307</sub> |     2.00<sub>±00.04</sub> + 0.28<sub>±00.03</sub> |   456.28<sub>±08.89</sub> |
|                  Crystal |  19.997<sub>±0.534</sub> |     3.32<sub>±00.04</sub> + 0.45<sub>±00.02</sub> |   393.08<sub>±28.54</sub> |
|                Nim/clang |  20.742<sub>±0.529</sub> |     2.28<sub>±00.02</sub> + 0.51<sub>±00.00</sub> |   461.96<sub>±15.63</sub> |
|                  V/clang |  20.757<sub>±0.507</sub> |     0.88<sub>±00.04</sub> + 1.97<sub>±00.12</sub> |   371.94<sub>±14.40</sub> |
|                     Java |  22.209<sub>±0.362</sub> |    37.52<sub>±00.16</sub> + 1.53<sub>±00.35</sub> |   425.77<sub>±44.37</sub> |
|               Vala/clang |  22.473<sub>±0.543</sub> |     3.56<sub>±00.05</sub> + 2.04<sub>±00.04</sub> |   428.17<sub>±22.08</sub> |
|                 Vala/gcc |  22.723<sub>±0.469</sub> |     3.55<sub>±00.01</sub> + 2.05<sub>±00.01</sub> |   412.52<sub>±16.09</sub> |
|                    Scala |  24.482<sub>±0.359</sub> |   80.25<sub>±00.58</sub> + 37.60<sub>±04.43</sub> |   465.50<sub>±11.89</sub> |
|                 Go/gccgo |  26.182<sub>±0.458</sub> |    21.37<sub>±00.17</sub> + 1.28<sub>±00.04</sub> |   450.94<sub>±10.14</sub> |
|             F#/.NET Core |  35.402<sub>±0.212</sub> |    36.83<sub>±00.06</sub> + 2.07<sub>±00.03</sub> |   643.00<sub>±19.63</sub> |
|                    OCaml |  38.179<sub>±1.251</sub> |     3.85<sub>±00.02</sub> + 6.95<sub>±00.26</sub> |   748.19<sub>±52.73</sub> |
|              Chez Scheme |  40.251<sub>±0.542</sub> |    25.43<sub>±00.04</sub> + 3.67<sub>±00.02</sub> |   926.00<sub>±36.88</sub> |
|                   Racket |  42.992<sub>±4.642</sub> |   115.71<sub>±00.18</sub> + 2.06<sub>±00.26</sub> |   832.01<sub>±71.41</sub> |
|                    D/dmd |  45.225<sub>±1.074</sub> |     3.56<sub>±00.05</sub> + 0.77<sub>±00.00</sub> |   869.87<sub>±28.71</sub> |
|                  C#/Mono |  46.198<sub>±1.532</sub> |    20.13<sub>±00.05</sub> + 0.88<sub>±00.00</sub> |   907.03<sub>±76.61</sub> |
|                  Node.js |  47.642<sub>±0.449</sub> |    30.20<sub>±00.13</sub> + 5.83<sub>±00.11</sub> |   866.33<sub>±21.85</sub> |
|                    MLton |  50.890<sub>±0.252</sub> |     1.40<sub>±00.03</sub> + 4.11<sub>±00.00</sub> |  1172.41<sub>±15.11</sub> |
|                    Julia |  60.716<sub>±1.267</sub> |   168.63<sub>±00.28</sub> + 0.00<sub>±00.00</sub> |  1067.66<sub>±10.06</sub> |
|         Haskell (MArray) |  61.456<sub>±0.617</sub> |     3.62<sub>±00.04</sub> + 2.68<sub>±00.00</sub> |  1401.97<sub>±26.08</sub> |
|              Python/pypy |  67.820<sub>±0.658</sub> |   63.93<sub>±00.19</sub> + 45.90<sub>±00.04</sub> |  1575.02<sub>±56.96</sub> |
| Ruby/truffleruby (--jvm) | 131.730<sub>±2.236</sub> | 565.14<sub>±10.92</sub> + 466.56<sub>±18.77</sub> |  2725.46<sub>±62.38</sub> |
|         Ruby/truffleruby | 172.357<sub>±4.092</sub> | 251.03<sub>±00.30</sub> + 758.66<sub>±17.72</sub> | 3768.42<sub>±226.62</sub> |
|                  Haskell | 216.273<sub>±4.050</sub> |    3.83<sub>±00.05</sub> + 26.16<sub>±00.00</sub> | 4543.18<sub>±330.08</sub> |
|               Lua/luajit | 253.096<sub>±2.212</sub> |     2.86<sub>±00.02</sub> + 0.86<sub>±00.00</sub> |  4740.45<sub>±61.20</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.159<sub>±0.003</sub> |     1.89<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   3.60<sub>±00.20</sub> |
|                     C/gcc |  1.240<sub>±0.029</sub> |     1.84<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  24.80<sub>±01.95</sub> |
|                      Rust |  1.328<sub>±0.049</sub> |     2.46<sub>±00.09</sub> + 0.01<sub>±00.00</sub> |  25.90<sub>±02.42</sub> |
|                   Crystal |  1.605<sub>±0.008</sub> |     3.73<sub>±00.03</sub> + 1.78<sub>±00.04</sub> |  38.41<sub>±00.34</sub> |
|                   V/clang |  1.659<sub>±0.032</sub> |     1.96<sub>±00.02</sub> + 0.49<sub>±00.02</sub> |  34.24<sub>±03.03</sub> |
|                 Nim/clang |  1.728<sub>±0.016</sub> |     2.68<sub>±00.03</sub> + 4.38<sub>±00.00</sub> |  38.53<sub>±01.02</sub> |
|                     V/gcc |  1.782<sub>±0.012</sub> |     1.71<sub>±00.04</sub> + 0.25<sub>±00.03</sub> |  29.96<sub>±00.45</sub> |
|                    D/ldc2 |  2.000<sub>±0.008</sub> |     3.41<sub>±00.04</sub> + 3.66<sub>±00.00</sub> |  36.64<sub>±00.17</sub> |
|                   Nim/gcc |  2.013<sub>±0.030</sub> |     2.21<sub>±00.06</sub> + 4.44<sub>±00.00</sub> |  44.68<sub>±01.82</sub> |
|                     D/gdc |  2.023<sub>±0.077</sub> |     7.08<sub>±00.05</sub> + 3.46<sub>±00.00</sub> |  43.81<sub>±02.27</sub> |
|                  Vala/gcc |  2.148<sub>±0.051</sub> |     4.94<sub>±00.05</sub> + 0.57<sub>±00.02</sub> |  42.36<sub>±01.27</sub> |
|                Vala/clang |  2.198<sub>±0.055</sub> |     4.97<sub>±00.05</sub> + 0.56<sub>±00.06</sub> |  41.69<sub>±01.05</sub> |
|                      Ruby |  2.312<sub>±0.028</sub> |   14.38<sub>±00.04</sub> + 55.53<sub>±01.54</sub> |  52.98<sub>±01.37</sub> |
|                      Java |  2.328<sub>±0.051</sub> |  38.46<sub>±00.09</sub> + 288.62<sub>±20.60</sub> |  47.21<sub>±02.47</sub> |
|              Ruby (--jit) |  2.342<sub>±0.054</sub> |   14.46<sub>±00.06</sub> + 57.15<sub>±01.20</sub> |  53.35<sub>±01.29</sub> |
|       C++/g++ (libcrypto) |  2.512<sub>±0.052</sub> |     5.40<sub>±00.04</sub> + 0.07<sub>±00.00</sub> |  56.47<sub>±02.30</sub> |
|                    Kotlin |  2.528<sub>±0.030</sub> |  39.38<sub>±00.19</sub> + 322.70<sub>±07.06</sub> |  50.71<sub>±02.27</sub> |
|                        Go |  2.619<sub>±0.006</sub> |     4.55<sub>±00.03</sub> + 5.38<sub>±00.14</sub> |  49.05<sub>±00.38</sub> |
|                     Scala |  2.625<sub>±0.015</sub> |   80.13<sub>±00.22</sub> + 75.91<sub>±04.39</sub> |  49.38<sub>±00.74</sub> |
|                   Node.js |  2.959<sub>±0.045</sub> | 30.82<sub>±00.05</sub> + 1029.46<sub>±00.16</sub> |  63.51<sub>±01.44</sub> |
|       Perl (MIME::Base64) |  2.983<sub>±0.101</sub> |    14.13<sub>±00.04</sub> + 0.08<sub>±00.00</sub> |  58.70<sub>±05.39</sub> |
|                       PHP |  3.003<sub>±0.021</sub> |    15.71<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  64.76<sub>±02.97</sub> |
|                  Go/gccgo |  3.539<sub>±0.003</sub> |    22.06<sub>±00.16</sub> + 7.32<sub>±00.30</sub> |  76.84<sub>±00.67</sub> |
|                     D/dmd |  4.120<sub>±0.106</sub> |     3.69<sub>±00.05</sub> + 3.67<sub>±00.05</sub> |  72.67<sub>±02.73</sub> |
|                    Python |  4.341<sub>±0.115</sub> |    10.03<sub>±00.03</sub> + 0.18<sub>±00.00</sub> |  77.43<sub>±02.75</sub> |
|                       Tcl |  4.636<sub>±0.021</sub> |     4.78<sub>±00.10</sub> + 0.16<sub>±00.04</sub> | 102.14<sub>±00.80</sub> |
|               Python/pypy |  5.054<sub>±0.128</sub> |   63.94<sub>±00.25</sub> + 45.73<sub>±00.05</sub> |  91.32<sub>±05.98</sub> |
|              F#/.NET Core |  5.635<sub>±0.067</sub> |   37.12<sub>±00.04</sub> + 33.62<sub>±05.08</sub> |  91.11<sub>±01.41</sub> |
|              C#/.NET Core |  5.836<sub>±0.042</sub> |   34.61<sub>±00.02</sub> + 33.39<sub>±06.29</sub> | 109.33<sub>±00.58</sub> |
|  Ruby/truffleruby (--jvm) |  5.861<sub>±0.075</sub> | 552.67<sub>±06.94</sub> + 310.16<sub>±25.83</sub> | 132.20<sub>±05.92</sub> |
|                     Julia |  6.010<sub>±0.117</sub> |  200.46<sub>±00.11</sub> + 42.95<sub>±00.25</sub> | 111.63<sub>±04.11</sub> |
|                   C#/Mono |  7.334<sub>±0.082</sub> |   20.58<sub>±00.03</sub> + 18.49<sub>±00.02</sub> | 170.73<sub>±08.58</sub> |
|                Ruby/jruby | 10.684<sub>±0.600</sub> | 199.34<sub>±04.86</sub> + 144.29<sub>±15.58</sub> | 219.12<sub>±15.61</sub> |
| Perl (MIME::Base64::Perl) | 16.395<sub>±0.314</sub> |    15.38<sub>±00.05</sub> + 0.27<sub>±00.03</sub> | 368.17<sub>±11.67</sub> |
|          Ruby/truffleruby | 22.208<sub>±0.143</sub> | 241.24<sub>±00.84</sub> + 397.56<sub>±00.08</sub> | 388.75<sub>±04.64</sub> |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|                        Language |                 Time, s |                                        Memory, MiB |               Energy, J |
| :------------------------------ | ----------------------: | -------------------------------------------------: | ----------------------: |
|         C++/g++ (DAW JSON Link) |  0.086<sub>±0.003</sub> |    109.27<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   1.83<sub>±00.18</sub> |
|    C++/g++ (simdjson On-Demand) |  0.087<sub>±0.002</sub> |   109.90<sub>±00.05</sub> + 59.55<sub>±00.00</sub> |   1.75<sub>±00.09</sub> |
|                    D/gdc (fast) |  0.111<sub>±0.005</sub> |   220.07<sub>±00.05</sub> + 11.09<sub>±00.26</sub> |   2.54<sub>±00.28</sub> |
|              Rust (Serde Typed) |  0.145<sub>±0.003</sub> |   108.46<sub>±00.08</sub> + 11.80<sub>±00.26</sub> |   2.96<sub>±00.23</sub> |
|             Rust (Serde Custom) |  0.151<sub>±0.005</sub> |    108.45<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   2.54<sub>±00.09</sub> |
|                 C++/g++ (gason) |  0.156<sub>±0.004</sub> |   109.25<sub>±00.01</sub> + 97.11<sub>±00.03</sub> |   3.47<sub>±00.26</sub> |
|          C++/g++ (simdjson DOM) |  0.160<sub>±0.002</sub> |  109.84<sub>±00.00</sub> + 176.60<sub>±00.00</sub> |   3.09<sub>±00.07</sub> |
|             C++/g++ (RapidJSON) |  0.223<sub>±0.008</sub> |  109.25<sub>±00.02</sub> + 128.82<sub>±00.00</sub> |   4.52<sub>±00.42</sub> |
|            C++/g++ (Boost.JSON) |  0.525<sub>±0.006</sub> |  109.80<sub>±00.04</sub> + 435.70<sub>±00.00</sub> |   9.51<sub>±00.26</sub> |
|                            Java |  0.536<sub>±0.018</sub> |   252.55<sub>±00.08</sub> + 74.23<sub>±00.79</sub> |  13.49<sub>±00.61</sub> |
|         C++/g++ (RapidJSON SAX) |  0.574<sub>±0.018</sub> |    109.46<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  10.10<sub>±00.65</sub> |
|                           Scala |  0.611<sub>±0.023</sub> |   328.42<sub>±02.85</sub> + 74.73<sub>±00.60</sub> |  15.73<sub>±01.00</sub> |
|                         Node.js |  0.637<sub>±0.018</sub> |  243.66<sub>±00.03</sub> + 184.96<sub>±00.56</sub> |  15.88<sub>±01.14</sub> |
|                   Go (jsoniter) |  0.662<sub>±0.009</sub> |   224.46<sub>±00.06</sub> + 13.61<sub>±00.13</sub> |  14.68<sub>±00.65</sub> |
|                Crystal (Schema) |  0.766<sub>±0.022</sub> |   110.30<sub>±00.05</sub> + 47.04<sub>±00.10</sub> |  11.98<sub>±00.30</sub> |
|                  Crystal (Pull) |  0.811<sub>±0.034</sub> |   110.33<sub>±00.06</sub> + 18.23<sub>±00.02</sub> |  14.91<sub>±01.35</sub> |
|                     Python/pypy |  0.843<sub>±0.019</sub> |  277.49<sub>±00.19</sub> + 127.96<sub>±00.00</sub> |  15.94<sub>±00.94</sub> |
|                   Julia (JSON3) |  0.848<sub>±0.009</sub> |  361.87<sub>±00.43</sub> + 360.75<sub>±00.26</sub> |  19.82<sub>±00.29</sub> |
|            Rust (Serde Untyped) |  0.851<sub>±0.035</sub> |  108.45<sub>±00.12</sub> + 839.98<sub>±00.00</sub> |  17.46<sub>±01.63</sub> |
|                         V/clang |  0.859<sub>±0.016</sub> |  108.36<sub>±00.05</sub> + 484.15<sub>±00.06</sub> |  15.38<sub>±00.63</sub> |
|                           V/gcc |  0.885<sub>±0.013</sub> |  107.44<sub>±00.06</sub> + 484.48<sub>±00.12</sub> |  20.51<sub>±00.87</sub> |
|         Perl (Cpanel::JSON::XS) |  0.961<sub>±0.023</sub> |  121.30<sub>±00.05</sub> + 402.72<sub>±00.00</sub> |  22.33<sub>±00.43</sub> |
|                         Crystal |  0.969<sub>±0.005</sub> |  110.34<sub>±00.06</sub> + 393.37<sub>±00.02</sub> |  21.43<sub>±00.18</sub> |
| C#/.NET Core (System.Text.Json) |  1.005<sub>±0.040</sub> |  465.42<sub>±00.09</sub> + 135.68<sub>±00.01</sub> |  23.01<sub>±01.06</sub> |
|                              Go |  1.093<sub>±0.011</sub> |   113.98<sub>±00.09</sub> + 83.29<sub>±00.02</sub> |  24.16<sub>±00.38</sub> |
|                             PHP |  1.281<sub>±0.042</sub> |  121.76<sub>±00.14</sub> + 682.01<sub>±00.00</sub> |  23.12<sub>±01.01</sub> |
|                        Go/gccgo |  1.437<sub>±0.035</sub> |   132.60<sub>±00.13</sub> + 95.90<sub>±00.10</sub> |  31.82<sub>±00.91</sub> |
|          Nim/clang (Packedjson) |  1.548<sub>±0.064</sub> |  109.17<sub>±00.10</sub> + 290.55<sub>±00.00</sub> |  31.93<sub>±03.05</sub> |
|            Nim/gcc (Packedjson) |  1.550<sub>±0.029</sub> |  108.74<sub>±00.06</sub> + 290.55<sub>±00.00</sub> |  26.92<sub>±00.46</sub> |
|                C++/g++ (json-c) |  1.551<sub>±0.044</sub> | 109.42<sub>±00.04</sub> + 1216.08<sub>±00.00</sub> |  31.40<sub>±03.00</sub> |
|                         Clojure |  1.643<sub>±0.028</sub> |  488.62<sub>±04.90</sub> + 525.49<sub>±14.00</sub> |  42.18<sub>±01.44</sub> |
|                    C#/.NET Core |  1.734<sub>±0.053</sub> |  474.26<sub>±00.03</sub> + 288.66<sub>±00.06</sub> |  34.47<sub>±04.08</sub> |
|                         Haskell |  1.760<sub>±0.040</sub> |      4.70<sub>±00.09</sub> + 4.61<sub>±00.02</sub> |  40.29<sub>±02.69</sub> |
|                          Python |  1.763<sub>±0.056</sub> |  116.74<sub>±00.03</sub> + 377.21<sub>±00.00</sub> |  37.76<sub>±02.63</sub> |
|             CPython (UltraJSON) |  1.783<sub>±0.029</sub> |  118.39<sub>±00.03</sub> + 543.47<sub>±00.39</sub> |  37.20<sub>±01.46</sub> |
|                       Nim/clang |  1.833<sub>±0.026</sub> |  109.24<sub>±00.05</sub> + 919.42<sub>±00.03</sub> |  42.46<sub>±00.75</sub> |
|                         Nim/gcc |  1.941<sub>±0.097</sub> |  108.78<sub>±00.04</sub> + 919.42<sub>±00.03</sub> |  38.89<sub>±03.82</sub> |
|                            Ruby |  2.206<sub>±0.028</sub> |  120.55<sub>±00.03</sub> + 410.67<sub>±00.01</sub> |  50.84<sub>±00.77</sub> |
|                         C#/Mono |  2.207<sub>±0.113</sub> |    462.64<sub>±00.09</sub> + 0.17<sub>±00.02</sub> |  46.36<sub>±07.33</sub> |
|                           D/gdc |  2.220<sub>±0.020</sub> |  113.29<sub>±00.02</sub> + 600.33<sub>±00.00</sub> |  50.18<sub>±00.93</sub> |
|                     Ruby (YAJL) |  2.272<sub>±0.030</sub> |  120.47<sub>±00.05</sub> + 281.58<sub>±00.01</sub> |  52.01<sub>±01.72</sub> |
|                    Ruby (--jit) |  2.300<sub>±0.076</sub> |  120.61<sub>±00.02</sub> + 410.78<sub>±00.01</sub> |  50.17<sub>±03.75</sub> |
| F#/.NET Core (System.Text.Json) |  2.555<sub>±0.005</sub> |  471.58<sub>±00.05</sub> + 444.65<sub>±01.22</sub> |  52.43<sub>±00.74</sub> |
|                          D/ldc2 |  2.561<sub>±0.025</sub> |  109.59<sub>±00.05</sub> + 680.20<sub>±00.04</sub> |  51.38<sub>±01.48</sub> |
|                       Rust (jq) |  3.744<sub>±0.097</sub> |  110.43<sub>±00.03</sub> + 775.37<sub>±00.52</sub> |  69.39<sub>±05.88</sub> |
|                      Ruby/jruby |  3.887<sub>±0.102</sub> | 475.26<sub>±04.27</sub> + 1484.87<sub>±32.34</sub> | 114.97<sub>±02.68</sub> |
|    C++/g++ (Boost.PropertyTree) |  4.359<sub>±0.073</sub> | 109.61<sub>±00.03</sub> + 1440.06<sub>±00.00</sub> | 101.74<sub>±01.57</sub> |
|                           D/dmd |  5.029<sub>±0.101</sub> |  110.12<sub>±00.03</sub> + 680.10<sub>±00.03</sub> |  99.91<sub>±03.77</sub> |
|                        Vala/gcc |  5.843<sub>±0.172</sub> |  111.17<sub>±00.07</sub> + 997.54<sub>±00.01</sub> | 106.11<sub>±02.27</sub> |
|                      Vala/clang |  6.087<sub>±0.149</sub> |  111.21<sub>±00.05</sub> + 932.12<sub>±00.12</sub> | 108.56<sub>±03.08</sub> |
|               Perl (JSON::Tiny) | 11.675<sub>±0.170</sub> |  122.00<sub>±00.04</sub> + 525.16<sub>±00.03</sub> | 266.13<sub>±04.55</sub> |
|        Ruby/truffleruby (--jvm) | 18.393<sub>±0.585</sub> | 748.21<sub>±14.35</sub> + 1639.80<sub>±63.41</sub> | 495.50<sub>±12.87</sub> |
|                Ruby/truffleruby | 42.147<sub>±1.079</sub> | 733.18<sub>±00.41</sub> + 2342.65<sub>±22.01</sub> | 885.48<sub>±36.81</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                  Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |   0.063<sub>±0.001</sub> |    6.69<sub>±00.08</sub> + 55.58<sub>±00.12</sub> |      3.61<sub>±00.06</sub> |
|           Python (NumPy) |   0.099<sub>±0.002</sub> |   27.64<sub>±00.10</sub> + 57.64<sub>±00.05</sub> |      5.56<sub>±00.20</sub> |
|    Nim/gcc (Arraymancer) |   0.165<sub>±0.014</sub> |    5.81<sub>±00.13</sub> + 57.56<sub>±00.12</sub> |      8.76<sub>±00.77</sub> |
|  Nim/clang (Arraymancer) |   0.185<sub>±0.049</sub> |    6.56<sub>±00.08</sub> + 57.41<sub>±00.05</sub> |      8.78<sub>±01.76</sub> |
|              Java (ND4J) |   0.186<sub>±0.010</sub> |  133.12<sub>±02.85</sub> + 87.35<sub>±00.00</sub> |      8.69<sub>±00.63</sub> |
|       Julia (threads: 8) |   0.197<sub>±0.003</sub> |  221.88<sub>±00.31</sub> + 52.84<sub>±00.11</sub> |     10.82<sub>±00.17</sub> |
|       Julia (threads: 1) |   0.571<sub>±0.024</sub> |  222.09<sub>±00.17</sub> + 52.64<sub>±00.00</sub> |     11.01<sub>±00.46</sub> |
|                   D/ldc2 |   1.998<sub>±0.008</sub> |    3.58<sub>±00.05</sub> + 70.11<sub>±00.00</sub> |     45.53<sub>±00.46</sub> |
|                    D/gdc |   2.130<sub>±0.033</sub> |    6.71<sub>±00.05</sub> + 70.71<sub>±00.01</sub> |     49.98<sub>±03.33</sub> |
|                    D/dmd |   2.190<sub>±0.029</sub> |    3.51<sub>±00.06</sub> + 70.11<sub>±00.00</sub> |     47.95<sub>±01.36</sub> |
|                    C/gcc |   3.361<sub>±0.032</sub> |    1.96<sub>±00.03</sub> + 68.06<sub>±00.00</sub> |     76.13<sub>±00.66</sub> |
|                     Java |   3.375<sub>±0.028</sub> |   37.91<sub>±00.30</sub> + 77.38<sub>±00.18</sub> |     76.02<sub>±01.56</sub> |
|                    Scala |   3.405<sub>±0.045</sub> |   78.83<sub>±04.99</sub> + 74.79<sub>±07.60</sub> |     79.08<sub>±02.97</sub> |
|                     Rust |   3.438<sub>±0.015</sub> |    2.59<sub>±00.10</sub> + 68.32<sub>±00.00</sub> |     72.45<sub>±01.23</sub> |
|                  Nim/gcc |   3.499<sub>±0.024</sub> |    2.58<sub>±00.03</sub> + 70.12<sub>±00.13</sub> |     72.89<sub>±00.79</sub> |
|                Nim/clang |   3.515<sub>±0.020</sub> |    3.10<sub>±00.03</sub> + 79.66<sub>±05.67</sub> |     74.79<sub>±01.28</sub> |
|          Julia (no BLAS) |   3.568<sub>±0.025</sub> |  178.15<sub>±00.22</sub> + 69.76<sub>±00.06</sub> |     75.14<sub>±01.00</sub> |
|                 Vala/gcc |   3.622<sub>±0.063</sub> |    5.24<sub>±00.06</sub> + 68.32<sub>±00.00</sub> |     63.98<sub>±02.77</sub> |
|                       Go |   3.638<sub>±0.029</sub> |    3.86<sub>±00.09</sub> + 73.33<sub>±00.09</sub> |     79.54<sub>±02.48</sub> |
|               Vala/clang |   3.639<sub>±0.028</sub> |    5.25<sub>±00.04</sub> + 68.32<sub>±00.00</sub> |     70.06<sub>±02.38</sub> |
|                 Go/gccgo |   3.708<sub>±0.049</sub> |   21.82<sub>±00.58</sub> + 72.61<sub>±00.14</sub> |     75.63<sub>±01.61</sub> |
|                    Swift |   3.720<sub>±0.060</sub> |  149.02<sub>±00.05</sub> + 59.60<sub>±00.06</sub> |     91.24<sub>±03.75</sub> |
|                  Crystal |   3.766<sub>±0.092</sub> |    4.10<sub>±00.04</sub> + 59.69<sub>±00.04</sub> |     77.91<sub>±05.35</sub> |
|                  Node.js |   3.840<sub>±0.070</sub> |   33.98<sub>±00.09</sub> + 71.64<sub>±00.62</sub> |     89.11<sub>±05.38</sub> |
|                  V/clang |   3.924<sub>±0.056</sub> |    2.40<sub>±00.05</sub> + 68.84<sub>±00.00</sub> |     79.38<sub>±05.46</sub> |
|                   Kotlin |   4.024<sub>±0.118</sub> |   37.85<sub>±00.05</sub> + 77.94<sub>±00.17</sub> |     76.73<sub>±06.10</sub> |
|                    V/gcc |   4.941<sub>±0.036</sub> |    1.99<sub>±00.03</sub> + 68.84<sub>±00.00</sub> |     89.41<sub>±03.53</sub> |
|              Python/pypy |   6.420<sub>±0.201</sub> |   64.28<sub>±00.20</sub> + 69.19<sub>±00.04</sub> |    115.52<sub>±05.81</sub> |
|             C#/.NET Core |   6.819<sub>±0.095</sub> |   34.00<sub>±00.08</sub> + 69.11<sub>±00.00</sub> |    163.77<sub>±05.84</sub> |
|                  C#/Mono |  11.473<sub>±0.327</sub> |   20.26<sub>±00.03</sub> + 69.01<sub>±00.00</sub> |    207.27<sub>±12.91</sub> |
|         Ruby/truffleruby |  48.858<sub>±0.386</sub> | 522.26<sub>±01.54</sub> + 739.45<sub>±02.45</sub> |   1166.40<sub>±23.30</sub> |
| Ruby/truffleruby (--jvm) |  71.053<sub>±1.215</sub> | 613.42<sub>±10.65</sub> + 420.01<sub>±15.34</sub> |   1928.18<sub>±51.69</sub> |
|             Ruby (--jit) | 211.146<sub>±4.444</sub> |   15.21<sub>±00.05</sub> + 68.87<sub>±00.00</sub> |   4762.60<sub>±75.89</sub> |
|                     Ruby | 214.346<sub>±3.747</sub> |   15.15<sub>±00.05</sub> + 68.64<sub>±00.00</sub> |  4777.97<sub>±185.82</sub> |
|                   Python | 234.810<sub>±4.264</sub> |   10.51<sub>±00.05</sub> + 68.58<sub>±00.00</sub> |   5089.89<sub>±88.84</sub> |
|                      Tcl | 355.979<sub>±9.911</sub> |   7.18<sub>±00.06</sub> + 400.44<sub>±00.06</sub> |  7466.13<sub>±555.27</sub> |
|                     Perl | 396.637<sub>±4.175</sub> |   8.99<sub>±00.07</sub> + 599.63<sub>±00.03</sub> |  9832.31<sub>±213.24</sub> |
|               Ruby/jruby | 500.411<sub>±9.064</sub> | 279.30<sub>±03.56</sub> + 663.99<sub>±11.11</sub> | 11013.14<sub>±240.57</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.103                         |
| C#/.NET Core     | 3.8.0-5.20604.10 (9ed4b774)     |
| C#/Mono          | 6.12.0.107                      |
| C/clang          | 11.0.1                          |
| C/gcc            | 10.2.1                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.2"                        |
| Crystal          | 0.36.1                          |
| D/dmd            | v2.095.1                        |
| D/gdc            | 10.2.1                          |
| D/ldc2           | 1.25.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 11.0.0.0 for F# 5.0             |
| Go               | go1.16                          |
| Go/gccgo         | 10.2.1                          |
| Haskell          | 8.10.4                          |
| Java             | 15.0.2                          |
| Julia            | v"1.5.3"                        |
| Kotlin           | 1.4.30                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20210117                        |
| Nim              | 1.4.4                           |
| Node.js          | v15.10.0                        |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.15                          |
| Perl             | v5.32.1                         |
| Python           | 3.9.1+                          |
| Python/pypy      | 7.3.3-beta0 for Python 3.7.9    |
| Racket           | "8.0"                           |
| Ruby             | 3.0.0p0                         |
| Ruby/jruby       | 9.2.14.0                        |
| Ruby/truffleruby | 21.0.0.2                        |
| Rust             | 1.50.0                          |
| Scala            | 2.13.5                          |
| Swift            | swift-5.3.3-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.2.2                           |
| Vala             | 0.48.13                         |

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
