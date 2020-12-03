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

UPDATE: 2020-12-01

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
|                  C++/g++ |    0.892<sub>±0.044</sub> |     1.49<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     15.92<sub>±00.43</sub> |
|                   Kotlin |    1.705<sub>±0.018</sub> |    38.11<sub>±00.12</sub> + 2.12<sub>±00.07</sub> |     38.89<sub>±00.47</sub> |
|                   D/ldc2 |    1.832<sub>±0.023</sub> |     3.06<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     33.17<sub>±01.19</sub> |
|                  Nim/gcc |    1.860<sub>±0.036</sub> |     1.92<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     35.16<sub>±02.06</sub> |
|                    C/gcc |    1.887<sub>±0.034</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     36.04<sub>±00.71</sub> |
|                     Rust |    1.921<sub>±0.036</sub> |     2.02<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     45.72<sub>±01.08</sub> |
|                Nim/clang |    1.921<sub>±0.036</sub> |     2.37<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     44.39<sub>±01.02</sub> |
|                    D/gdc |    1.950<sub>±0.034</sub> |     6.34<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     37.39<sub>±01.93</sub> |
|                  C/clang |    2.223<sub>±0.066</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     42.64<sub>±01.37</sub> |
|                    OCaml |    2.230<sub>±0.026</sub> |     2.60<sub>±00.02</sub> + 2.50<sub>±00.03</sub> |     42.50<sub>±01.23</sub> |
|                       Go |    2.241<sub>±0.034</sub> |     3.53<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     50.55<sub>±01.43</sub> |
|               Vala/clang |    2.316<sub>±0.026</sub> |     3.73<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     54.54<sub>±00.49</sub> |
|                  Crystal |    2.317<sub>±0.021</sub> |     3.37<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     55.93<sub>±01.28</sub> |
|                 Vala/gcc |    2.335<sub>±0.062</sub> |     3.77<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     47.62<sub>±01.82</sub> |
|             C#/.NET Core |    2.341<sub>±0.089</sub> |    34.18<sub>±00.04</sub> + 0.01<sub>±00.00</sub> |     48.13<sub>±04.76</sub> |
|                    V/gcc |    2.433<sub>±0.071</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     51.49<sub>±01.99</sub> |
|                     Java |    2.458<sub>±0.113</sub> |    37.49<sub>±00.09</sub> + 0.96<sub>±00.11</sub> |     48.82<sub>±02.17</sub> |
|                 Go/gccgo |    2.593<sub>±0.203</sub> |    20.93<sub>±00.22</sub> + 0.00<sub>±00.00</sub> |     52.43<sub>±10.28</sub> |
|                  V/clang |    2.742<sub>±0.106</sub> |     0.87<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     57.97<sub>±03.80</sub> |
|                    MLton |    2.841<sub>±0.030</sub> |     1.44<sub>±00.05</sub> + 0.25<sub>±00.00</sub> |     69.59<sub>±01.25</sub> |
|              Chez Scheme |    2.898<sub>±0.136</sub> |    24.87<sub>±00.05</sub> + 4.23<sub>±00.09</sub> |     61.62<sub>±05.33</sub> |
|                    Julia |    2.989<sub>±0.124</sub> |   169.09<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |     52.77<sub>±03.78</sub> |
|                    D/dmd |    3.398<sub>±0.051</sub> |     3.58<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     75.85<sub>±04.16</sub> |
|                    Scala |    3.451<sub>±0.036</sub> |   80.33<sub>±00.31</sub> + 63.22<sub>±04.38</sub> |     70.71<sub>±01.84</sub> |
|                  Node.js |    3.876<sub>±0.091</sub> |    29.72<sub>±00.07</sub> + 2.08<sub>±00.00</sub> |     86.57<sub>±02.19</sub> |
|                  C#/Mono |    4.047<sub>±0.035</sub> |    20.14<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     93.13<sub>±00.66</sub> |
|         Haskell (MArray) |    4.626<sub>±0.065</sub> |     3.52<sub>±00.05</sub> + 1.23<sub>±00.00</sub> |    104.52<sub>±07.29</sub> |
|             F#/.NET Core |    5.759<sub>±0.060</sub> |   36.41<sub>±00.05</sub> + 90.04<sub>±00.54</sub> |    122.34<sub>±02.41</sub> |
|               Lua/luajit |    6.266<sub>±0.056</sub> |     2.84<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    134.09<sub>±03.45</sub> |
|                   Racket |   10.241<sub>±0.263</sub> |   101.02<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    188.03<sub>±09.99</sub> |
| Ruby/truffleruby (--jvm) |   11.014<sub>±0.785</sub> | 554.32<sub>±05.28</sub> + 467.70<sub>±85.03</sub> |    346.21<sub>±23.03</sub> |
|              Python/pypy |   14.634<sub>±0.483</sub> |   63.77<sub>±00.19</sub> + 45.38<sub>±00.00</sub> |    348.98<sub>±16.82</sub> |
|                  Haskell |   16.436<sub>±0.405</sub> |     4.28<sub>±00.05</sub> + 1.00<sub>±00.00</sub> |    320.67<sub>±10.52</sub> |
|         Ruby/truffleruby |   21.492<sub>±1.470</sub> | 253.22<sub>±00.14</sub> + 638.77<sub>±14.18</sub> |    517.59<sub>±37.53</sub> |
|                      Lua |   57.236<sub>±1.627</sub> |     2.67<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   1122.15<sub>±58.58</sub> |
|             Ruby (--jit) |   61.421<sub>±1.978</sub> |    14.12<sub>±00.05</sub> + 0.15<sub>±00.00</sub> |   1122.68<sub>±34.10</sub> |
|                     Ruby |   82.449<sub>±3.315</sub> |    14.15<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  1706.93<sub>±130.86</sub> |
|               Ruby/jruby |  109.907<sub>±2.647</sub> | 202.52<sub>±03.44</sub> + 236.59<sub>±02.42</sub> |  2259.77<sub>±110.02</sub> |
|                   Elixir |  119.090<sub>±1.217</sub> |    56.61<sub>±01.01</sub> + 0.00<sub>±00.00</sub> |   2268.87<sub>±68.60</sub> |
|                   Python |  235.009<sub>±4.679</sub> |     9.45<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  4650.00<sub>±179.37</sub> |
|                 Tcl (FP) |  279.844<sub>±8.099</sub> |     4.28<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  5693.86<sub>±351.71</sub> |
|                     Perl |  361.937<sub>±3.237</sub> |     6.51<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  7386.06<sub>±407.83</sub> |
|                Tcl (OOP) | 540.958<sub>±12.988</sub> |     4.26<sub>±00.09</sub> + 0.00<sub>±00.00</sub> | 11178.46<sub>±579.19</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                   D/ldc2 |  13.023<sub>±0.141</sub> |     3.02<sub>±00.04</sub> + 0.77<sub>±00.00</sub> |   282.47<sub>±15.54</sub> |
|                    C/gcc |  13.024<sub>±0.044</sub> |     0.49<sub>±00.00</sub> + 1.11<sub>±00.02</sub> |   232.35<sub>±06.92</sub> |
|                  C++/g++ |  13.508<sub>±0.519</sub> |     3.14<sub>±00.14</sub> + 0.49<sub>±00.02</sub> |   243.38<sub>±14.88</sub> |
|                    D/gdc |  13.622<sub>±0.486</sub> |     6.68<sub>±00.06</sub> + 0.52<sub>±00.00</sub> |   284.97<sub>±15.61</sub> |
|                    V/gcc |  13.929<sub>±0.345</sub> |     0.55<sub>±00.05</sub> + 1.88<sub>±00.06</sub> |   263.36<sub>±22.17</sub> |
|                   Kotlin |  16.201<sub>±0.639</sub> |    37.89<sub>±00.20</sub> + 2.53<sub>±00.14</sub> |   333.78<sub>±12.48</sub> |
|                  C/clang |  17.205<sub>±0.296</sub> |     0.54<sub>±00.00</sub> + 1.09<sub>±00.02</sub> |   409.86<sub>±07.81</sub> |
|             C#/.NET Core |  17.817<sub>±0.206</sub> |    34.36<sub>±00.02</sub> + 1.06<sub>±00.06</sub> |   386.31<sub>±09.90</sub> |
|                  Nim/gcc |  18.909<sub>±0.573</sub> |     1.81<sub>±00.04</sub> + 0.57<sub>±00.05</sub> |   364.59<sub>±13.74</sub> |
|                     Rust |  19.121<sub>±0.365</sub> |     2.06<sub>±00.09</sub> + 0.28<sub>±00.03</sub> |   450.43<sub>±14.62</sub> |
|                  V/clang |  19.954<sub>±0.144</sub> |     0.85<sub>±00.00</sub> + 1.97<sub>±00.09</sub> |   354.30<sub>±05.02</sub> |
|                Nim/clang |  21.089<sub>±0.839</sub> |     2.31<sub>±00.07</sub> + 0.51<sub>±00.00</sub> |   427.52<sub>±33.64</sub> |
|               Vala/clang |  21.997<sub>±0.558</sub> |     3.54<sub>±00.04</sub> + 2.02<sub>±00.02</sub> |   417.73<sub>±08.93</sub> |
|                     Java |  22.059<sub>±2.009</sub> |    37.61<sub>±00.10</sub> + 1.41<sub>±00.15</sub> |   438.82<sub>±32.38</sub> |
|                 Vala/gcc |  22.339<sub>±0.495</sub> |     3.55<sub>±00.02</sub> + 2.05<sub>±00.02</sub> |   395.91<sub>±13.76</sub> |
|                    Scala |  22.953<sub>±0.280</sub> |   80.06<sub>±00.43</sub> + 38.70<sub>±03.31</sub> |   533.87<sub>±10.60</sub> |
|                 Go/gccgo |  23.761<sub>±0.456</sub> |    33.35<sub>±00.22</sub> + 1.28<sub>±00.05</sub> |   519.74<sub>±23.32</sub> |
|                  Crystal |  23.958<sub>±0.297</sub> |     3.32<sub>±00.04</sub> + 0.45<sub>±00.02</sub> |   442.85<sub>±08.69</sub> |
|                       Go |  34.576<sub>±0.772</sub> |     3.50<sub>±00.03</sub> + 1.24<sub>±00.01</sub> |   636.21<sub>±19.42</sub> |
|                    OCaml |  36.101<sub>±0.674</sub> |     3.81<sub>±00.03</sub> + 7.19<sub>±01.04</sub> |   809.54<sub>±37.56</sub> |
|              Chez Scheme |  38.361<sub>±0.223</sub> |    25.47<sub>±00.03</sub> + 3.65<sub>±00.02</sub> |   928.03<sub>±21.01</sub> |
|                    D/dmd |  40.256<sub>±0.116</sub> |     3.48<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   890.37<sub>±04.94</sub> |
|                  C#/Mono |  43.907<sub>±0.601</sub> |    20.06<sub>±00.06</sub> + 0.88<sub>±00.00</sub> |   974.19<sub>±30.51</sub> |
|                  Node.js |  45.577<sub>±1.511</sub> |    29.36<sub>±00.12</sub> + 6.12<sub>±00.01</sub> |   911.95<sub>±74.08</sub> |
|                    MLton |  52.145<sub>±0.592</sub> |     1.40<sub>±00.03</sub> + 4.11<sub>±00.00</sub> |   997.68<sub>±30.34</sub> |
|                    Julia |  57.873<sub>±1.982</sub> |   168.56<sub>±00.17</sub> + 0.00<sub>±00.00</sub> |  1150.80<sub>±96.19</sub> |
|              Python/pypy |  65.443<sub>±0.415</sub> |   64.01<sub>±00.25</sub> + 45.97<sub>±00.09</sub> |  1537.62<sub>±41.76</sub> |
|         Haskell (MArray) |  65.728<sub>±1.731</sub> |     3.56<sub>±00.09</sub> + 2.74<sub>±00.00</sub> |  1333.81<sub>±82.75</sub> |
| Ruby/truffleruby (--jvm) | 127.906<sub>±1.069</sub> | 553.57<sub>±06.48</sub> + 384.87<sub>±17.42</sub> |  2850.44<sub>±62.68</sub> |
|             F#/.NET Core | 133.896<sub>±0.448</sub> |   36.46<sub>±00.05</sub> + 91.62<sub>±00.12</sub> |  2663.72<sub>±08.90</sub> |
|                   Racket | 167.016<sub>±3.127</sub> |   100.96<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  3106.19<sub>±93.72</sub> |
|         Ruby/truffleruby | 169.044<sub>±5.202</sub> | 252.82<sub>±00.38</sub> + 661.55<sub>±17.47</sub> | 3637.07<sub>±123.52</sub> |
|                  Haskell | 224.213<sub>±5.516</sub> |    3.65<sub>±00.06</sub> + 26.29<sub>±00.00</sub> | 4739.08<sub>±376.63</sub> |
|               Lua/luajit | 246.285<sub>±3.792</sub> |     2.79<sub>±00.03</sub> + 0.86<sub>±00.00</sub> | 4889.38<sub>±291.91</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.156<sub>±0.002</sub> |     1.90<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   3.43<sub>±00.14</sub> |
|                     C/gcc |  1.190<sub>±0.010</sub> |     1.87<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  24.92<sub>±00.52</sub> |
|                      Rust |  1.246<sub>±0.045</sub> |     2.49<sub>±00.08</sub> + 0.01<sub>±00.01</sub> |  28.21<sub>±00.74</sub> |
|                   V/clang |  1.522<sub>±0.026</sub> |     1.98<sub>±00.04</sub> + 0.47<sub>±00.01</sub> |  32.63<sub>±00.77</sub> |
|                     V/gcc |  1.624<sub>±0.026</sub> |     1.73<sub>±00.03</sub> + 0.23<sub>±00.11</sub> |  35.31<sub>±01.20</sub> |
|                    D/ldc2 |  1.745<sub>±0.011</sub> |     3.40<sub>±00.02</sub> + 3.66<sub>±00.00</sub> |  30.74<sub>±00.31</sub> |
|                 Nim/clang |  1.877<sub>±0.024</sub> |     2.68<sub>±00.06</sub> + 4.41<sub>±00.03</sub> |  33.76<sub>±01.21</sub> |
|                   Nim/gcc |  2.048<sub>±0.075</sub> |     2.23<sub>±00.05</sub> + 4.44<sub>±00.00</sub> |  40.58<sub>±03.26</sub> |
|                   Crystal |  2.100<sub>±0.006</sub> |     3.76<sub>±00.03</sub> + 1.79<sub>±00.02</sub> |  43.08<sub>±00.46</sub> |
|                     D/gdc |  2.137<sub>±0.015</sub> |     7.05<sub>±00.09</sub> + 3.46<sub>±00.03</sub> |  37.92<sub>±01.38</sub> |
|                      Java |  2.248<sub>±0.032</sub> |  38.45<sub>±00.09</sub> + 360.35<sub>±13.12</sub> |  49.02<sub>±02.22</sub> |
|                      Ruby |  2.336<sub>±0.029</sub> |   14.41<sub>±00.05</sub> + 42.64<sub>±00.90</sub> |  53.21<sub>±02.22</sub> |
|              Ruby (--jit) |  2.364<sub>±0.048</sub> |   14.47<sub>±00.04</sub> + 43.78<sub>±00.06</sub> |  53.65<sub>±03.10</sub> |
|                    Kotlin |  2.411<sub>±0.058</sub> |  39.43<sub>±00.08</sub> + 339.98<sub>±11.43</sub> |  51.45<sub>±02.95</sub> |
|                        Go |  2.497<sub>±0.007</sub> |     4.52<sub>±00.13</sub> + 4.89<sub>±00.10</sub> |  47.17<sub>±00.43</sub> |
|                     Scala |  2.518<sub>±0.028</sub> |   81.21<sub>±00.46</sub> + 64.54<sub>±07.62</sub> |  55.40<sub>±03.45</sub> |
|       C++/g++ (libcrypto) |  2.623<sub>±0.028</sub> |     5.34<sub>±00.04</sub> + 0.07<sub>±00.00</sub> |  47.14<sub>±00.51</sub> |
|                       PHP |  2.945<sub>±0.031</sub> |    15.72<sub>±00.15</sub> + 0.00<sub>±00.00</sub> |  51.14<sub>±01.07</sub> |
|       Perl (MIME::Base64) |  3.024<sub>±0.046</sub> |    14.16<sub>±00.03</sub> + 0.02<sub>±00.00</sub> |  52.92<sub>±01.33</sub> |
|                   Node.js |  3.065<sub>±0.030</sub> | 29.89<sub>±00.05</sub> + 1029.92<sub>±00.07</sub> |  67.19<sub>±00.76</sub> |
|                  Go/gccgo |  3.660<sub>±0.012</sub> |    22.07<sub>±00.12</sub> + 7.22<sub>±00.22</sub> |  72.73<sub>±00.40</sub> |
|                     D/dmd |  3.663<sub>±0.066</sub> |     3.67<sub>±00.06</sub> + 3.67<sub>±00.00</sub> |  74.06<sub>±00.79</sub> |
|                       Tcl |  4.295<sub>±0.072</sub> |     5.06<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  73.76<sub>±01.81</sub> |
|               Python/pypy |  4.723<sub>±0.117</sub> |   64.13<sub>±00.15</sub> + 45.72<sub>±00.05</sub> | 101.07<sub>±05.88</sub> |
|              C#/.NET Core |  5.213<sub>±0.062</sub> |   34.44<sub>±00.09</sub> + 41.21<sub>±01.20</sub> |  97.46<sub>±00.94</sub> |
|                    Python |  5.448<sub>±0.112</sub> |     9.21<sub>±00.05</sub> + 0.18<sub>±00.00</sub> | 102.56<sub>±05.80</sub> |
|                     Julia |  5.857<sub>±0.093</sub> |  200.32<sub>±00.21</sub> + 42.88<sub>±00.21</sub> | 111.74<sub>±01.76</sub> |
|  Ruby/truffleruby (--jvm) |  5.859<sub>±0.142</sub> | 551.20<sub>±02.74</sub> + 399.55<sub>±28.65</sub> | 124.74<sub>±07.81</sub> |
|                   C#/Mono |  7.565<sub>±0.204</sub> |   20.60<sub>±00.05</sub> + 18.49<sub>±00.06</sub> | 144.66<sub>±05.48</sub> |
|                Ruby/jruby |  9.862<sub>±0.279</sub> | 192.07<sub>±03.01</sub> + 173.11<sub>±05.41</sub> | 213.68<sub>±16.56</sub> |
| Perl (MIME::Base64::Perl) | 16.414<sub>±0.631</sub> |    15.46<sub>±00.05</sub> + 0.20<sub>±00.08</sub> | 353.95<sub>±27.54</sub> |
|          Ruby/truffleruby | 20.647<sub>±0.721</sub> | 253.00<sub>±00.31</sub> + 404.70<sub>±00.60</sub> | 413.32<sub>±37.94</sub> |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|                        Language |                  Time, s |                                        Memory, MiB |                  Energy, J |
| :------------------------------ | -----------------------: | -------------------------------------------------: | -------------------------: |
|         C++/g++ (DAW JSON Link) |   0.082<sub>±0.002</sub> |    109.27<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |      1.95<sub>±00.06</sub> |
|    C++/g++ (simdjson On-Demand) |   0.088<sub>±0.001</sub> |   109.90<sub>±00.05</sub> + 59.81<sub>±00.12</sub> |      1.69<sub>±00.08</sub> |
|                    D/gdc (fast) |   0.100<sub>±0.001</sub> |   219.98<sub>±00.03</sub> + 11.12<sub>±00.03</sub> |      2.58<sub>±00.06</sub> |
|             Rust (Serde Custom) |   0.141<sub>±0.001</sub> |    108.43<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |      2.35<sub>±00.04</sub> |
|              Rust (Serde Typed) |   0.150<sub>±0.001</sub> |   108.48<sub>±00.07</sub> + 11.51<sub>±00.03</sub> |      2.52<sub>±00.04</sub> |
|                 C++/g++ (gason) |   0.158<sub>±0.004</sub> |   109.25<sub>±00.01</sub> + 97.17<sub>±00.03</sub> |      3.19<sub>±00.21</sub> |
|          C++/g++ (simdjson DOM) |   0.160<sub>±0.003</sub> |  109.81<sub>±00.03</sub> + 176.60<sub>±00.00</sub> |      3.21<sub>±00.08</sub> |
|             C++/g++ (RapidJSON) |   0.229<sub>±0.006</sub> |  109.27<sub>±00.02</sub> + 128.82<sub>±00.00</sub> |      4.51<sub>±00.35</sub> |
|            C++/g++ (Boost.JSON) |   0.481<sub>±0.009</sub> |  109.80<sub>±00.03</sub> + 435.70<sub>±00.00</sub> |     11.83<sub>±00.26</sub> |
|                            Java |   0.528<sub>±0.018</sub> |   252.65<sub>±00.15</sub> + 75.46<sub>±01.57</sub> |     14.53<sub>±00.36</sub> |
|         C++/g++ (RapidJSON SAX) |   0.534<sub>±0.021</sub> |    109.46<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     11.52<sub>±00.82</sub> |
|                           Scala |   0.609<sub>±0.009</sub> |   327.43<sub>±00.58</sub> + 73.55<sub>±00.87</sub> |     16.90<sub>±00.72</sub> |
|                   Julia (JSON3) |   0.615<sub>±0.031</sub> |  355.85<sub>±00.44</sub> + 243.86<sub>±02.20</sub> |     13.47<sub>±01.25</sub> |
|                         Node.js |   0.663<sub>±0.010</sub> |  242.73<sub>±00.08</sub> + 185.38<sub>±00.82</sub> |     14.04<sub>±00.37</sub> |
|                   Go (jsoniter) |   0.702<sub>±0.019</sub> |   224.79<sub>±00.11</sub> + 13.89<sub>±00.15</sub> |     14.43<sub>±01.07</sub> |
|                     Python/pypy |   0.780<sub>±0.013</sub> |  277.41<sub>±00.16</sub> + 127.96<sub>±00.00</sub> |     18.71<sub>±00.61</sub> |
|            Rust (Serde Untyped) |   0.846<sub>±0.013</sub> |  108.48<sub>±00.08</sub> + 839.98<sub>±00.00</sub> |     19.09<sub>±00.42</sub> |
|                  Crystal (Pull) |   0.856<sub>±0.033</sub> |   110.30<sub>±00.05</sub> + 18.20<sub>±00.04</sub> |     16.65<sub>±01.40</sub> |
|                Crystal (Schema) |   0.883<sub>±0.039</sub> |   110.36<sub>±00.03</sub> + 47.05<sub>±00.08</sub> |     15.84<sub>±01.53</sub> |
| C#/.NET Core (System.Text.Json) |   0.982<sub>±0.018</sub> |  465.50<sub>±00.22</sub> + 135.59<sub>±00.10</sub> |     22.84<sub>±00.75</sub> |
|         Perl (Cpanel::JSON::XS) |   1.000<sub>±0.013</sub> |  121.32<sub>±00.06</sub> + 402.72<sub>±00.00</sub> |     18.21<sub>±00.37</sub> |
|                              Go |   1.129<sub>±0.049</sub> |   113.94<sub>±00.19</sub> + 95.95<sub>±00.02</sub> |     22.35<sub>±01.99</sub> |
|                         Crystal |   1.147<sub>±0.029</sub> |  110.37<sub>±00.03</sub> + 393.35<sub>±00.03</sub> |     24.00<sub>±01.31</sub> |
|                             PHP |   1.204<sub>±0.012</sub> |  121.69<sub>±00.09</sub> + 682.01<sub>±00.00</sub> |     25.92<sub>±00.20</sub> |
|                           V/gcc |   1.211<sub>±0.024</sub> |  107.42<sub>±00.01</sub> + 484.49<sub>±00.03</sub> |     27.41<sub>±00.83</sub> |
|                         V/clang |   1.299<sub>±0.021</sub> |  107.91<sub>±00.03</sub> + 484.48<sub>±00.03</sub> |     29.13<sub>±01.05</sub> |
|                        Go/gccgo |   1.401<sub>±0.025</sub> |   132.57<sub>±00.07</sub> + 95.88<sub>±00.20</sub> |     31.66<sub>±01.06</sub> |
|            Nim/gcc (Packedjson) |   1.458<sub>±0.035</sub> |  108.80<sub>±00.04</sub> + 290.55<sub>±00.00</sub> |     31.04<sub>±03.08</sub> |
|          Nim/clang (Packedjson) |   1.459<sub>±0.028</sub> |  109.22<sub>±00.08</sub> + 290.55<sub>±00.00</sub> |     34.02<sub>±00.59</sub> |
|                C++/g++ (json-c) |   1.561<sub>±0.044</sub> | 109.42<sub>±00.01</sub> + 1216.08<sub>±00.00</sub> |     30.31<sub>±01.55</sub> |
|                         Clojure |   1.561<sub>±0.053</sub> |  491.24<sub>±08.10</sub> + 516.55<sub>±22.59</sub> |     42.27<sub>±01.67</sub> |
|                         Haskell |   1.701<sub>±0.024</sub> |      4.41<sub>±00.03</sub> + 5.42<sub>±00.03</sub> |     39.66<sub>±00.56</sub> |
|                    C#/.NET Core |   1.736<sub>±0.054</sub> |  474.19<sub>±00.09</sub> + 288.64<sub>±00.06</sub> |     30.96<sub>±01.72</sub> |
|                         Nim/gcc |   1.756<sub>±0.020</sub> |  108.60<sub>±00.11</sub> + 915.75<sub>±00.06</sub> |     39.94<sub>±01.11</sub> |
|                          Python |   1.757<sub>±0.056</sub> |  115.88<sub>±00.05</sub> + 377.23<sub>±00.01</sub> |     38.02<sub>±02.48</sub> |
|             CPython (UltraJSON) |   1.771<sub>±0.020</sub> |  117.52<sub>±00.04</sub> + 545.45<sub>±00.90</sub> |     35.77<sub>±00.50</sub> |
|                       Nim/clang |   1.880<sub>±0.064</sub> |  109.09<sub>±00.10</sub> + 915.65<sub>±00.10</sub> |     37.34<sub>±03.24</sub> |
|                         C#/Mono |   2.072<sub>±0.048</sub> |    462.61<sub>±00.08</sub> + 0.18<sub>±00.00</sub> |     45.96<sub>±02.08</sub> |
|                     Ruby (YAJL) |   2.183<sub>±0.061</sub> |  120.60<sub>±00.03</sub> + 286.98<sub>±00.01</sub> |     50.23<sub>±02.52</sub> |
|                           D/gdc |   2.234<sub>±0.115</sub> |  113.23<sub>±00.03</sub> + 600.32<sub>±00.00</sub> |     44.22<sub>±05.07</sub> |
|                            Ruby |   2.260<sub>±0.067</sub> |  120.71<sub>±00.04</sub> + 277.85<sub>±00.04</sub> |     42.18<sub>±01.80</sub> |
|                    Ruby (--jit) |   2.316<sub>±0.029</sub> |  120.76<sub>±00.02</sub> + 277.83<sub>±00.05</sub> |     44.52<sub>±01.57</sub> |
|                          D/ldc2 |   2.503<sub>±0.033</sub> |  109.55<sub>±00.05</sub> + 680.07<sub>±00.04</sub> |     50.98<sub>±01.91</sub> |
|                       Rust (jq) |   3.578<sub>±0.063</sub> |  110.41<sub>±00.05</sub> + 774.34<sub>±00.26</sub> |     79.25<sub>±01.55</sub> |
|                      Ruby/jruby |   3.815<sub>±0.041</sub> | 473.21<sub>±08.23</sub> + 1448.93<sub>±24.01</sub> |    116.78<sub>±04.54</sub> |
|    C++/g++ (Boost.PropertyTree) |   4.353<sub>±0.127</sub> | 109.59<sub>±00.02</sub> + 1440.00<sub>±00.00</sub> |     94.67<sub>±07.97</sub> |
|                           D/dmd |   4.867<sub>±0.131</sub> |  110.06<sub>±00.04</sub> + 680.07<sub>±00.03</sub> |    101.12<sub>±04.66</sub> |
|               Perl (JSON::Tiny) |  11.508<sub>±0.117</sub> |  121.91<sub>±00.06</sub> + 525.12<sub>±00.04</sub> |    268.69<sub>±04.33</sub> |
|        Ruby/truffleruby (--jvm) | 101.930<sub>±0.923</sub> | 730.89<sub>±03.57</sub> + 1288.83<sub>±43.61</sub> |   2876.01<sub>±27.44</sub> |
|                Ruby/truffleruby | 582.209<sub>±8.945</sub> | 731.12<sub>±00.18</sub> + 2184.33<sub>±38.93</sub> | 11738.06<sub>±202.59</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.069<sub>±0.001</sub> |    6.70<sub>±00.04</sub> + 56.29<sub>±00.19</sub> |      3.83<sub>±00.09</sub> |
|  Nim/clang (Arraymancer) |    0.070<sub>±0.001</sub> |    5.54<sub>±00.04</sub> + 57.67<sub>±00.02</sub> |      4.02<sub>±00.04</sub> |
|    Nim/gcc (Arraymancer) |    0.079<sub>±0.001</sub> |    5.53<sub>±00.04</sub> + 57.67<sub>±00.02</sub> |      4.70<sub>±00.09</sub> |
|           Python (NumPy) |    0.105<sub>±0.002</sub> |   27.06<sub>±00.06</sub> + 57.64<sub>±00.04</sub> |      5.91<sub>±00.21</sub> |
|       Julia (threads: 8) |    0.190<sub>±0.003</sub> |  222.04<sub>±00.19</sub> + 52.41<sub>±00.12</sub> |     11.08<sub>±00.30</sub> |
|              Java (ND4J) |    0.206<sub>±0.026</sub> |  137.15<sub>±03.95</sub> + 87.35<sub>±00.00</sub> |      9.96<sub>±01.00</sub> |
|       Julia (threads: 1) |    0.531<sub>±0.019</sub> |  221.56<sub>±00.17</sub> + 53.05<sub>±00.22</sub> |     12.27<sub>±00.94</sub> |
|                   D/ldc2 |    1.965<sub>±0.010</sub> |    3.55<sub>±00.04</sub> + 70.11<sub>±00.00</sub> |     43.78<sub>±00.60</sub> |
|                    D/gdc |    2.072<sub>±0.027</sub> |    6.68<sub>±00.05</sub> + 70.70<sub>±00.01</sub> |     48.59<sub>±01.94</sub> |
|                    D/dmd |    2.103<sub>±0.009</sub> |    3.75<sub>±00.11</sub> + 70.43<sub>±00.13</sub> |     48.29<sub>±00.94</sub> |
|                     Java |    3.289<sub>±0.020</sub> |   37.92<sub>±00.04</sub> + 77.27<sub>±00.26</sub> |     74.62<sub>±01.52</sub> |
|                    C/gcc |    3.320<sub>±0.010</sub> |    2.03<sub>±00.02</sub> + 68.06<sub>±00.00</sub> |     76.08<sub>±01.58</sub> |
|                     Rust |    3.363<sub>±0.008</sub> |    2.62<sub>±00.06</sub> + 68.32<sub>±00.00</sub> |     69.32<sub>±01.58</sub> |
|                  Nim/gcc |    3.434<sub>±0.017</sub> |    2.63<sub>±00.03</sub> + 74.77<sub>±04.64</sub> |     70.32<sub>±01.18</sub> |
|                    Scala |    3.451<sub>±0.039</sub> |   84.70<sub>±00.88</sub> + 80.50<sub>±03.14</sub> |     80.20<sub>±01.58</sub> |
|                Nim/clang |    3.454<sub>±0.009</sub> |    3.08<sub>±00.06</sub> + 72.19<sub>±02.19</sub> |     71.54<sub>±01.32</sub> |
|          Julia (no BLAS) |    3.503<sub>±0.024</sub> |  177.98<sub>±00.14</sub> + 69.74<sub>±00.07</sub> |     74.83<sub>±00.82</sub> |
|                       Go |    3.572<sub>±0.037</sub> |    3.79<sub>±00.08</sub> + 73.24<sub>±00.23</sub> |     76.44<sub>±01.29</sub> |
|                 Go/gccgo |    3.604<sub>±0.055</sub> |   27.67<sub>±06.19</sub> + 72.72<sub>±00.21</sub> |     75.21<sub>±00.74</sub> |
|                  Crystal |    3.623<sub>±0.010</sub> |    4.13<sub>±00.04</sub> + 59.70<sub>±00.01</sub> |     90.24<sub>±00.72</sub> |
|                  V/clang |    3.642<sub>±0.054</sub> |    2.44<sub>±00.01</sub> + 68.84<sub>±00.00</sub> |     90.91<sub>±01.22</sub> |
|                    Swift |    3.657<sub>±0.059</sub> |  148.95<sub>±00.13</sub> + 59.76<sub>±00.07</sub> |     87.78<sub>±02.97</sub> |
|                  Node.js |    3.891<sub>±0.137</sub> |   33.06<sub>±00.01</sub> + 71.18<sub>±00.03</sub> |     82.84<sub>±05.76</sub> |
|                   Kotlin |    3.990<sub>±0.016</sub> |   37.62<sub>±00.11</sub> + 77.96<sub>±00.23</sub> |     69.54<sub>±02.71</sub> |
|                    V/gcc |    4.671<sub>±0.152</sub> |    1.97<sub>±00.05</sub> + 68.84<sub>±00.00</sub> |     97.06<sub>±09.17</sub> |
|              Python/pypy |    6.261<sub>±0.256</sub> |   64.29<sub>±00.20</sub> + 69.20<sub>±00.03</sub> |    122.94<sub>±12.29</sub> |
|             C#/.NET Core |    6.892<sub>±0.269</sub> |   33.88<sub>±00.14</sub> + 69.12<sub>±00.01</sub> |    150.17<sub>±12.02</sub> |
|                  C#/Mono |   10.532<sub>±0.078</sub> |   20.20<sub>±00.05</sub> + 69.02<sub>±00.01</sub> |    237.07<sub>±02.79</sub> |
|         Ruby/truffleruby |   38.623<sub>±0.297</sub> | 527.69<sub>±00.21</sub> + 482.24<sub>±03.58</sub> |    706.62<sub>±15.54</sub> |
| Ruby/truffleruby (--jvm) |   70.781<sub>±1.848</sub> | 597.32<sub>±03.94</sub> + 330.89<sub>±08.63</sub> |   1912.53<sub>±68.70</sub> |
|             Ruby (--jit) |  213.343<sub>±7.607</sub> |   15.17<sub>±00.04</sub> + 69.09<sub>±00.00</sub> |   4581.51<sub>±99.85</sub> |
|                     Ruby |  214.756<sub>±3.892</sub> |   15.14<sub>±00.04</sub> + 68.89<sub>±00.00</sub> |  4585.32<sub>±103.55</sub> |
|                   Python |  240.588<sub>±5.667</sub> |    9.84<sub>±00.04</sub> + 68.58<sub>±00.00</sub> |  5021.18<sub>±181.03</sub> |
|                      Tcl |  348.604<sub>±8.750</sub> |   7.25<sub>±00.03</sub> + 400.41<sub>±00.03</sub> |  7605.35<sub>±576.78</sub> |
|                     Perl |  402.099<sub>±5.437</sub> |   8.96<sub>±00.08</sub> + 599.69<sub>±00.05</sub> |  8531.93<sub>±821.67</sub> |
|               Ruby/jruby | 488.395<sub>±11.851</sub> | 279.21<sub>±05.66</sub> + 658.86<sub>±07.14</sub> | 10808.74<sub>±390.65</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.100                         |
| C#/.NET Core     | 3.8.0-5.20519.18 (4c195c3a)     |
| C#/Mono          | 6.12.0.90                       |
| C/clang          | 11.0.0                          |
| C/gcc            | 10.2.0                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.1"                        |
| Crystal          | 0.35.1                          |
| D/dmd            | v2.094.2                        |
| D/gdc            | 10.2.0                          |
| D/ldc2           | 1.24.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 11.0.0.0 for F# 5.0             |
| Go               | go1.15.5                        |
| Go/gccgo         | 10.2.0                          |
| Haskell          | 8.10.2                          |
| Java             | 15.0.1                          |
| Julia            | v"1.5.3"                        |
| Kotlin           | 1.4.20                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20201002                        |
| Nim              | 1.4.0                           |
| Node.js          | v15.3.0                         |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.11                          |
| Perl             | v5.32.0                         |
| Python           | 3.8.6                           |
| Python/pypy      | 7.3.3-beta0 for Python 3.7.9    |
| Racket           | "7.9"                           |
| Ruby             | 2.7.2p137                       |
| Ruby/jruby       | 9.2.13.0                        |
| Ruby/truffleruby | 20.3.0                          |
| Rust             | 1.48.0                          |
| Scala            | 2.13.4                          |
| Swift            | swift-5.3.1-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.1.30                          |
| Vala             | 0.48.12                         |

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
