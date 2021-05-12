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

UPDATE: 2021-05-11

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
|                  C++/g++ |    0.887<sub>±0.040</sub> |     1.49<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     18.53<sub>±01.68</sub> |
|  Racket (Syntax Objects) |    1.435<sub>±0.058</sub> |   106.70<sub>±00.51</sub> + 0.00<sub>±00.00</sub> |     30.78<sub>±03.00</sub> |
|                   D/ldc2 |    1.719<sub>±0.021</sub> |     3.02<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     38.72<sub>±00.76</sub> |
|                   Kotlin |    1.722<sub>±0.024</sub> |    39.23<sub>±00.11</sub> + 1.63<sub>±00.21</sub> |     38.90<sub>±00.37</sub> |
|                     Rust |    1.785<sub>±0.053</sub> |     2.06<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |     37.35<sub>±03.03</sub> |
|                    C/gcc |    1.893<sub>±0.050</sub> |     0.53<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     37.71<sub>±03.11</sub> |
|                    D/gdc |    2.002<sub>±0.046</sub> |     6.41<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     36.57<sub>±02.44</sub> |
|                 Go/gccgo |    2.047<sub>±0.026</sub> |    21.44<sub>±00.58</sub> + 0.00<sub>±00.00</sub> |     43.83<sub>±02.01</sub> |
|                       Go |    2.130<sub>±0.014</sub> |     3.49<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     48.08<sub>±01.27</sub> |
|                     Java |    2.132<sub>±0.075</sub> |    38.29<sub>±00.22</sub> + 0.86<sub>±00.28</sub> |     46.92<sub>±01.37</sub> |
|                Nim/clang |    2.248<sub>±0.071</sub> |     2.34<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     46.76<sub>±03.92</sub> |
|                 Vala/gcc |    2.265<sub>±0.062</sub> |     3.76<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     46.50<sub>±02.06</sub> |
|                  C/clang |    2.272<sub>±0.032</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     42.58<sub>±01.16</sub> |
|                    OCaml |    2.298<sub>±0.053</sub> |     2.61<sub>±00.02</sub> + 2.50<sub>±00.06</sub> |     41.39<sub>±01.07</sub> |
|                   Racket |    2.333<sub>±0.041</sub> |   115.66<sub>±00.14</sub> + 2.32<sub>±00.13</sub> |     46.43<sub>±05.60</sub> |
|               Vala/clang |    2.354<sub>±0.034</sub> |     3.70<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     54.43<sub>±00.72</sub> |
|                  Nim/gcc |    2.362<sub>±0.035</sub> |     1.85<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     43.36<sub>±01.52</sub> |
|             F#/.NET Core |    2.393<sub>±0.032</sub> |    36.88<sub>±00.12</sub> + 0.29<sub>±00.00</sub> |     41.65<sub>±00.69</sub> |
|             C#/.NET Core |    2.401<sub>±0.044</sub> |    34.35<sub>±00.02</sub> + 0.07<sub>±00.00</sub> |     44.16<sub>±02.20</sub> |
|                    V/gcc |    2.427<sub>±0.082</sub> |     0.53<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     53.52<sub>±02.12</sub> |
|                  Crystal |    2.528<sub>±0.077</sub> |     3.32<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     44.50<sub>±01.79</sub> |
|                    MLton |    2.744<sub>±0.104</sub> |     1.44<sub>±00.01</sub> + 0.25<sub>±00.00</sub> |     57.60<sub>±05.55</sub> |
|              Chez Scheme |    3.041<sub>±0.033</sub> |    24.85<sub>±00.04</sub> + 4.23<sub>±00.02</sub> |     54.88<sub>±01.72</sub> |
|                    Julia |    3.161<sub>±0.136</sub> |   198.80<sub>±00.11</sub> + 0.78<sub>±00.05</sub> |     62.44<sub>±04.55</sub> |
|                    Scala |    3.554<sub>±0.028</sub> |   72.40<sub>±00.34</sub> + 41.53<sub>±01.76</sub> |     70.90<sub>±01.48</sub> |
|                    D/dmd |    3.703<sub>±0.086</sub> |     3.61<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     67.36<sub>±02.74</sub> |
|                  Node.js |    4.109<sub>±0.159</sub> |    30.70<sub>±00.06</sub> + 3.57<sub>±00.00</sub> |     79.59<sub>±06.43</sub> |
|                  C#/Mono |    4.358<sub>±0.119</sub> |    20.14<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     77.73<sub>±01.73</sub> |
|         Haskell (MArray) |    4.415<sub>±0.084</sub> |     3.68<sub>±00.08</sub> + 1.17<sub>±00.00</sub> |    100.95<sub>±03.60</sub> |
|                  V/clang |    4.787<sub>±0.124</sub> |     0.86<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     84.16<sub>±01.42</sub> |
|               Lua/luajit |    6.388<sub>±0.145</sub> |     2.92<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    133.80<sub>±03.92</sub> |
| Ruby/truffleruby (--jvm) |    7.893<sub>±0.199</sub> | 589.00<sub>±09.65</sub> + 662.55<sub>±30.78</sub> |    279.51<sub>±12.92</sub> |
|         Ruby/truffleruby |    8.783<sub>±0.230</sub> | 415.87<sub>±00.79</sub> + 530.82<sub>±09.17</sub> |    183.15<sub>±04.44</sub> |
|              Python/pypy |   14.000<sub>±0.092</sub> |   65.46<sub>±00.15</sub> + 45.49<sub>±00.04</sub> |    352.08<sub>±04.58</sub> |
|                  Haskell |   16.657<sub>±0.379</sub> |     3.95<sub>±00.18</sub> + 0.88<sub>±00.06</sub> |    319.16<sub>±16.43</sub> |
|                      Lua |   56.144<sub>±1.050</sub> |     2.98<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1235.91<sub>±12.80</sub> |
|             Ruby (--jit) |   59.070<sub>±1.495</sub> |    14.10<sub>±00.04</sub> + 0.20<sub>±00.03</sub> |   1270.83<sub>±51.58</sub> |
|                     Ruby |   95.925<sub>±1.875</sub> |    14.04<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   1730.34<sub>±29.57</sub> |
|               Ruby/jruby |  108.937<sub>±3.967</sub> | 193.93<sub>±06.81</sub> + 125.95<sub>±14.62</sub> |   2207.07<sub>±64.21</sub> |
|                   Elixir |  118.452<sub>±3.773</sub> |    56.35<sub>±00.67</sub> + 0.00<sub>±00.00</sub> |  2452.64<sub>±141.77</sub> |
|                   Python |  225.315<sub>±5.268</sub> |    10.44<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  4895.03<sub>±190.72</sub> |
|                 Tcl (FP) |  278.444<sub>±4.141</sub> |     4.30<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  5439.77<sub>±216.82</sub> |
|                     Perl |  362.032<sub>±4.739</sub> |     6.55<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  7367.39<sub>±522.22</sub> |
|                Tcl (OOP) | 542.918<sub>±12.654</sub> |     4.31<sub>±00.05</sub> + 0.00<sub>±00.00</sub> | 11537.98<sub>±493.52</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                    C/gcc |  12.811<sub>±0.524</sub> |     0.55<sub>±00.01</sub> + 1.11<sub>±00.03</sub> |   250.85<sub>±22.32</sub> |
|                   D/ldc2 |  13.156<sub>±0.445</sub> |     3.07<sub>±00.07</sub> + 0.77<sub>±00.00</sub> |   263.50<sub>±24.22</sub> |
|                  C++/g++ |  13.376<sub>±0.399</sub> |     3.28<sub>±00.06</sub> + 0.47<sub>±00.01</sub> |   268.43<sub>±14.79</sub> |
|                     Rust |  13.398<sub>±0.182</sub> |     2.19<sub>±00.04</sub> + 0.25<sub>±00.00</sub> |   247.32<sub>±04.90</sub> |
|                    D/gdc |  14.413<sub>±0.763</sub> |     6.74<sub>±00.04</sub> + 0.52<sub>±00.00</sub> |   251.18<sub>±05.29</sub> |
|                    V/gcc |  14.937<sub>±0.295</sub> |     1.64<sub>±00.16</sub> + 0.77<sub>±00.00</sub> |   338.31<sub>±06.91</sub> |
|                       Go |  16.134<sub>±0.570</sub> |     3.46<sub>±00.06</sub> + 1.28<sub>±00.00</sub> |   348.96<sub>±19.77</sub> |
|  Racket (Syntax Objects) |  16.351<sub>±0.443</sub> |  107.54<sub>±00.70</sub> + 71.80<sub>±00.77</sub> |   371.05<sub>±16.84</sub> |
|                   Kotlin |  17.211<sub>±0.691</sub> |    39.24<sub>±00.11</sub> + 1.97<sub>±00.11</sub> |   336.46<sub>±31.35</sub> |
|                  C/clang |  18.082<sub>±0.838</sub> |     0.50<sub>±00.00</sub> + 1.13<sub>±00.02</sub> |   387.24<sub>±37.25</sub> |
|             C#/.NET Core |  19.026<sub>±0.221</sub> |    34.34<sub>±00.06</sub> + 1.00<sub>±00.00</sub> |   340.41<sub>±16.20</sub> |
|                 Vala/gcc |  19.971<sub>±0.342</sub> |     3.59<sub>±00.04</sub> + 2.07<sub>±00.02</sub> |   375.26<sub>±10.38</sub> |
|               Vala/clang |  20.499<sub>±0.344</sub> |     3.57<sub>±00.04</sub> + 2.07<sub>±00.04</sub> |   485.68<sub>±16.04</sub> |
|                     Java |  21.451<sub>±1.883</sub> |    38.19<sub>±00.28</sub> + 1.43<sub>±00.28</sub> |   450.02<sub>±68.42</sub> |
|                  Nim/gcc |  22.354<sub>±0.356</sub> |     1.77<sub>±00.02</sub> + 0.51<sub>±00.00</sub> |   503.29<sub>±12.39</sub> |
|                 Go/gccgo |  22.808<sub>±0.725</sub> |    21.28<sub>±00.11</sub> + 1.28<sub>±00.00</sub> |   451.37<sub>±38.88</sub> |
|                Nim/clang |  22.928<sub>±0.263</sub> |     2.31<sub>±00.05</sub> + 0.54<sub>±00.03</sub> |   421.51<sub>±07.00</sub> |
|                    Scala |  23.253<sub>±0.610</sub> |   73.66<sub>±00.63</sub> + 25.55<sub>±01.13</sub> |   499.19<sub>±33.30</sub> |
|                  V/clang |  23.440<sub>±0.355</sub> |     2.10<sub>±00.09</sub> + 0.77<sub>±00.00</sub> |   527.86<sub>±19.44</sub> |
|                  Crystal |  24.435<sub>±0.992</sub> |     3.38<sub>±00.02</sub> + 0.38<sub>±00.04</sub> |   515.33<sub>±46.04</sub> |
|             F#/.NET Core |  34.565<sub>±0.365</sub> |    36.82<sub>±00.07</sub> + 2.03<sub>±00.00</sub> |   628.43<sub>±12.89</sub> |
|                    OCaml |  38.112<sub>±0.568</sub> |     3.91<sub>±00.02</sub> + 6.82<sub>±00.13</sub> |   709.65<sub>±06.96</sub> |
|                   Racket |  38.938<sub>±2.272</sub> |   115.69<sub>±00.18</sub> + 1.80<sub>±00.39</sub> |   828.00<sub>±47.60</sub> |
|              Chez Scheme |  39.375<sub>±0.585</sub> |    25.49<sub>±00.03</sub> + 3.72<sub>±00.09</sub> |   922.02<sub>±20.81</sub> |
|                  Node.js |  41.226<sub>±0.585</sub> |    30.64<sub>±00.08</sub> + 6.71<sub>±00.00</sub> |   732.87<sub>±13.37</sub> |
|                    D/dmd |  45.699<sub>±1.554</sub> |     3.64<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   921.76<sub>±67.84</sub> |
|                  C#/Mono |  46.799<sub>±0.359</sub> |    20.19<sub>±00.06</sub> + 0.88<sub>±00.00</sub> |   839.48<sub>±10.27</sub> |
|                    MLton |  50.137<sub>±0.535</sub> |     1.47<sub>±00.07</sub> + 4.11<sub>±00.00</sub> |  1158.49<sub>±27.73</sub> |
|              Python/pypy |  61.742<sub>±1.887</sub> |   65.24<sub>±00.03</sub> + 46.46<sub>±00.04</sub> |  1326.92<sub>±93.95</sub> |
|         Haskell (MArray) |  62.608<sub>±1.184</sub> |     3.90<sub>±00.12</sub> + 2.43<sub>±00.00</sub> | 1227.45<sub>±111.27</sub> |
|                    Julia |  78.557<sub>±0.670</sub> |   199.37<sub>±00.28</sub> + 0.96<sub>±00.12</sub> |  1441.81<sub>±25.06</sub> |
| Ruby/truffleruby (--jvm) | 131.607<sub>±0.986</sub> | 575.77<sub>±07.17</sub> + 670.21<sub>±93.59</sub> |  2784.49<sub>±53.43</sub> |
|         Ruby/truffleruby | 145.901<sub>±2.851</sub> | 416.55<sub>±00.22</sub> + 616.44<sub>±13.62</sub> |  3272.93<sub>±77.01</sub> |
|                  Haskell | 209.454<sub>±2.302</sub> |    4.12<sub>±00.07</sub> + 25.91<sub>±00.00</sub> | 4779.38<sub>±134.86</sub> |
|               Lua/luajit | 234.265<sub>±0.831</sub> |     2.95<sub>±00.05</sub> + 0.86<sub>±00.00</sub> |  5391.35<sub>±54.50</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.158<sub>±0.003</sub> |     1.92<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   3.62<sub>±00.19</sub> |
|                      Rust |  1.099<sub>±0.012</sub> |     2.50<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  25.03<sub>±00.53</sub> |
|                     C/gcc |  1.204<sub>±0.018</sub> |     1.88<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  25.03<sub>±00.92</sub> |
|                   Nim/gcc |  1.315<sub>±0.051</sub> |     2.17<sub>±00.03</sub> + 4.44<sub>±00.00</sub> |  26.36<sub>±02.48</sub> |
|                     V/gcc |  1.536<sub>±0.025</sub> |     1.53<sub>±00.07</sub> + 0.57<sub>±00.15</sub> |  32.53<sub>±00.75</sub> |
|                   Crystal |  1.607<sub>±0.005</sub> |     3.78<sub>±00.02</sub> + 1.73<sub>±00.05</sub> |  38.11<sub>±00.24</sub> |
|                 Nim/clang |  1.737<sub>±0.034</sub> |     2.74<sub>±00.02</sub> + 4.38<sub>±00.00</sub> |  30.87<sub>±01.37</sub> |
|                     D/gdc |  1.986<sub>±0.055</sub> |     7.10<sub>±00.07</sub> + 3.46<sub>±00.00</sub> |  43.83<sub>±01.58</sub> |
|                      Ruby |  2.000<sub>±0.015</sub> |   14.45<sub>±00.06</sub> + 55.93<sub>±00.77</sub> |  45.58<sub>±00.47</sub> |
|                    D/ldc2 |  2.016<sub>±0.030</sub> |     3.41<sub>±00.03</sub> + 3.67<sub>±00.00</sub> |  34.76<sub>±00.15</sub> |
|              Ruby (--jit) |  2.142<sub>±0.038</sub> |   14.48<sub>±00.04</sub> + 55.32<sub>±02.00</sub> |  40.99<sub>±01.63</sub> |
|                Vala/clang |  2.166<sub>±0.102</sub> |     2.48<sub>±02.48</sub> + 0.21<sub>±00.21</sub> |  42.94<sub>±04.07</sub> |
|                      Java |  2.205<sub>±0.032</sub> |  38.88<sub>±00.12</sub> + 284.38<sub>±12.77</sub> |  49.29<sub>±02.07</sub> |
|                  Vala/gcc |  2.222<sub>±0.066</sub> |     2.50<sub>±02.50</sub> + 0.23<sub>±00.23</sub> |  40.61<sub>±03.76</sub> |
|                   V/clang |  2.366<sub>±0.022</sub> |     2.01<sub>±00.09</sub> + 0.57<sub>±00.14</sub> |  40.70<sub>±01.40</sub> |
|                    Kotlin |  2.374<sub>±0.055</sub> |  40.35<sub>±00.09</sub> + 305.36<sub>±35.98</sub> |  47.30<sub>±01.56</sub> |
|                     Scala |  2.456<sub>±0.022</sub> |   73.41<sub>±00.80</sub> + 53.48<sub>±02.23</sub> |  48.65<sub>±02.17</sub> |
|       C++/g++ (libcrypto) |  2.589<sub>±0.115</sub> |     5.46<sub>±00.02</sub> + 0.07<sub>±00.00</sub> |  50.14<sub>±05.06</sub> |
|                        Go |  2.592<sub>±0.010</sub> |     4.59<sub>±00.04</sub> + 5.38<sub>±00.19</sub> |  49.56<sub>±00.40</sub> |
|                   Node.js |  2.797<sub>±0.060</sub> |   31.26<sub>±00.06</sub> + 36.71<sub>±00.05</sub> |  59.13<sub>±02.46</sub> |
|                       PHP |  2.976<sub>±0.073</sub> |    15.94<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |  64.81<sub>±02.71</sub> |
|       Perl (MIME::Base64) |  3.104<sub>±0.042</sub> |    14.17<sub>±00.04</sub> + 0.05<sub>±00.03</sub> |  53.43<sub>±01.27</sub> |
|                  Go/gccgo |  3.527<sub>±0.007</sub> |    22.10<sub>±00.04</sub> + 7.29<sub>±00.24</sub> |  76.38<sub>±00.50</sub> |
|                    Python |  3.842<sub>±0.072</sub> |    10.13<sub>±00.04</sub> + 0.18<sub>±00.00</sub> |  86.80<sub>±02.81</sub> |
|                     D/dmd |  3.899<sub>±0.016</sub> |     3.74<sub>±00.06</sub> + 3.67<sub>±00.06</sub> |  65.86<sub>±02.37</sub> |
|                       Tcl |  4.931<sub>±0.052</sub> |     4.82<sub>±00.07</sub> + 0.16<sub>±00.03</sub> |  85.15<sub>±01.71</sub> |
|               Python/pypy |  5.388<sub>±0.153</sub> |   65.31<sub>±00.10</sub> + 45.70<sub>±00.04</sub> | 113.66<sub>±07.17</sub> |
|              F#/.NET Core |  5.447<sub>±0.057</sub> |   37.06<sub>±00.05</sub> + 33.01<sub>±05.24</sub> | 101.12<sub>±01.56</sub> |
|              C#/.NET Core |  5.762<sub>±0.016</sub> |   34.55<sub>±00.06</sub> + 36.93<sub>±05.76</sub> | 109.67<sub>±00.38</sub> |
|                     Julia |  5.900<sub>±0.107</sub> |  218.04<sub>±00.18</sub> + 63.00<sub>±00.12</sub> | 112.60<sub>±03.40</sub> |
|  Ruby/truffleruby (--jvm) |  6.878<sub>±0.224</sub> | 572.64<sub>±10.58</sub> + 326.01<sub>±39.19</sub> | 140.38<sub>±10.99</sub> |
|                   C#/Mono |  7.497<sub>±0.315</sub> |   20.73<sub>±00.04</sub> + 18.41<sub>±00.02</sub> | 152.30<sub>±12.76</sub> |
|                Ruby/jruby | 10.952<sub>±0.463</sub> | 190.82<sub>±05.19</sub> + 137.54<sub>±19.63</sub> | 229.20<sub>±12.38</sub> |
| Perl (MIME::Base64::Perl) | 17.158<sub>±0.337</sub> |    15.48<sub>±00.02</sub> + 0.20<sub>±00.04</sub> | 308.64<sub>±18.30</sub> |
|          Ruby/truffleruby | 22.370<sub>±0.368</sub> | 406.16<sub>±01.08</sub> + 304.93<sub>±01.49</sub> | 471.43<sub>±17.59</sub> |

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
| C++/g++ (DAW JSON Link NoCheck) |  0.091<sub>±0.002</sub> |    113.03<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   2.24<sub>±00.21</sub> |
|    C++/g++ (simdjson On-Demand) |  0.107<sub>±0.008</sub> |   113.22<sub>±00.06</sub> + 59.81<sub>±00.00</sub> |   2.60<sub>±00.51</sub> |
|         C++/g++ (DAW JSON Link) |  0.130<sub>±0.003</sub> |    113.04<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.32<sub>±00.13</sub> |
|             Rust (Serde Custom) |  0.148<sub>±0.006</sub> |    111.84<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   3.23<sub>±00.24</sub> |
|              Rust (Serde Typed) |  0.160<sub>±0.004</sub> |   111.92<sub>±00.07</sub> + 11.54<sub>±00.13</sub> |   3.96<sub>±00.31</sub> |
|                 C++/g++ (gason) |  0.173<sub>±0.003</sub> |   113.03<sub>±00.08</sub> + 96.77<sub>±00.03</sub> |   4.06<sub>±00.26</sub> |
|          C++/g++ (simdjson DOM) |  0.184<sub>±0.002</sub> |  113.21<sub>±00.06</sub> + 176.60<sub>±00.00</sub> |   4.46<sub>±00.21</sub> |
|             C++/g++ (RapidJSON) |  0.234<sub>±0.003</sub> |  113.06<sub>±00.06</sub> + 128.71<sub>±00.06</sub> |   5.61<sub>±00.15</sub> |
|   D/ldc2 (Mir Amazon's Ion DOM) |  0.239<sub>±0.003</sub> |   112.74<sub>±00.04</sub> + 16.27<sub>±00.01</sub> |   5.47<sub>±00.28</sub> |
|           D/ldc2 (Mir Asdf DOM) |  0.261<sub>±0.004</sub> |   112.59<sub>±00.02</sub> + 61.34<sub>±00.00</sub> |   6.06<sub>±00.19</sub> |
|     C++/g++ (RapidJSON Precise) |  0.314<sub>±0.007</sub> |  113.09<sub>±00.04</sub> + 128.71<sub>±00.06</sub> |   7.22<sub>±00.35</sub> |
|            C++/g++ (Boost.JSON) |  0.562<sub>±0.015</sub> |  113.15<sub>±00.01</sub> + 435.70<sub>±00.00</sub> |  12.84<sub>±00.15</sub> |
|         C++/g++ (RapidJSON SAX) |  0.602<sub>±0.016</sub> |    112.83<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  12.69<sub>±00.56</sub> |
| C++/g++ (RapidJSON SAX Precise) |  0.700<sub>±0.024</sub> |    112.85<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  14.49<sub>±00.47</sub> |
|                   Go (jsoniter) |  0.727<sub>±0.008</sub> |    231.44<sub>±00.12</sub> + 1.96<sub>±00.46</sub> |  16.74<sub>±00.33</sub> |
|                  Crystal (Pull) |  0.794<sub>±0.010</sub> |   113.73<sub>±00.04</sub> + 18.24<sub>±00.02</sub> |  17.82<sub>±01.17</sub> |
|                Crystal (Schema) |  0.807<sub>±0.021</sub> |   113.76<sub>±00.02</sub> + 47.30<sub>±00.08</sub> |  17.04<sub>±00.92</sub> |
|                            Java |  0.808<sub>±0.015</sub> |  264.22<sub>±00.12</sub> + 282.49<sub>±20.79</sub> |  22.96<sub>±01.85</sub> |
|            Rust (Serde Untyped) |  0.894<sub>±0.022</sub> |  111.84<sub>±00.09</sub> + 839.98<sub>±00.00</sub> |  18.87<sub>±00.60</sub> |
|                           Scala |  0.907<sub>±0.014</sub> |  314.85<sub>±01.13</sub> + 118.21<sub>±01.29</sub> |  23.79<sub>±00.54</sub> |
|                         V/clang |  0.933<sub>±0.022</sub> |  111.53<sub>±00.05</sub> + 484.37<sub>±00.00</sub> |  20.55<sub>±01.05</sub> |
|                     Python/pypy |  0.938<sub>±0.011</sub> |  285.41<sub>±00.06</sub> + 121.42<sub>±00.00</sub> |  20.32<sub>±00.84</sub> |
|                         Node.js |  0.972<sub>±0.034</sub> |   250.74<sub>±00.08</sub> + 79.32<sub>±00.49</sub> |  23.35<sub>±00.97</sub> |
|                           V/gcc |  1.026<sub>±0.026</sub> |  111.04<sub>±00.02</sub> + 484.36<sub>±00.06</sub> |  22.22<sub>±00.83</sub> |
|                   Julia (JSON3) |  1.028<sub>±0.023</sub> |  384.45<sub>±02.15</sub> + 360.66<sub>±00.06</sub> |  22.65<sub>±00.58</sub> |
|         Perl (Cpanel::JSON::XS) |  1.113<sub>±0.033</sub> |  124.71<sub>±00.04</sub> + 402.72<sub>±00.00</sub> |  24.41<sub>±00.54</sub> |
|                         Crystal |  1.125<sub>±0.023</sub> |  113.69<sub>±00.05</sub> + 396.44<sub>±00.04</sub> |  25.19<sub>±01.06</sub> |
| C#/.NET Core (System.Text.Json) |  1.147<sub>±0.025</sub> |  479.06<sub>±00.07</sub> + 139.03<sub>±00.03</sub> |  23.70<sub>±00.72</sub> |
|                              Go |  1.176<sub>±0.027</sub> |   117.28<sub>±00.09</sub> + 83.24<sub>±00.13</sub> |  26.45<sub>±01.29</sub> |
|          Nim/clang (Packedjson) |  1.184<sub>±0.019</sub> |  112.49<sub>±00.14</sub> + 294.04<sub>±00.13</sub> |  27.24<sub>±00.78</sub> |
|                             PHP |  1.345<sub>±0.044</sub> |  125.09<sub>±00.11</sub> + 682.01<sub>±00.00</sub> |  29.04<sub>±01.66</sub> |
|            Nim/gcc (Packedjson) |  1.436<sub>±0.016</sub> |  111.89<sub>±00.06</sub> + 294.16<sub>±00.00</sub> |  31.25<sub>±01.11</sub> |
|                        Go/gccgo |  1.575<sub>±0.047</sub> |   135.92<sub>±00.11</sub> + 96.10<sub>±00.14</sub> |  33.84<sub>±02.52</sub> |
|                C++/g++ (json-c) |  1.695<sub>±0.044</sub> | 113.25<sub>±00.04</sub> + 1215.90<sub>±00.00</sub> |  36.72<sub>±01.15</sub> |
|                         Clojure |  1.700<sub>±0.005</sub> |  467.34<sub>±08.42</sub> + 592.99<sub>±38.56</sub> |  48.41<sub>±02.76</sub> |
|                    C#/.NET Core |  1.759<sub>±0.026</sub> |  487.88<sub>±00.19</sub> + 294.07<sub>±00.00</sub> |  41.30<sub>±01.21</sub> |
|                       Nim/clang |  1.781<sub>±0.038</sub> |  112.57<sub>±00.07</sub> + 924.83<sub>±00.06</sub> |  38.88<sub>±00.81</sub> |
|              C++/g++ (Nlohmann) |  1.910<sub>±0.037</sub> |  113.18<sub>±00.04</sub> + 447.88<sub>±00.00</sub> |  39.83<sub>±01.09</sub> |
|                         Nim/gcc |  1.931<sub>±0.014</sub> |  111.90<sub>±00.16</sub> + 923.48<sub>±00.06</sub> |  42.11<sub>±00.68</sub> |
|                          Python |  1.990<sub>±0.034</sub> |  120.11<sub>±00.03</sub> + 377.21<sub>±00.00</sub> |  42.55<sub>±01.15</sub> |
|             CPython (UltraJSON) |  2.064<sub>±0.009</sub> |  121.82<sub>±00.02</sub> + 547.12<sub>±01.15</sub> |  42.96<sub>±00.63</sub> |
|                         Haskell |  2.382<sub>±0.025</sub> |      4.63<sub>±00.04</sub> + 4.57<sub>±00.05</sub> |  54.14<sub>±02.72</sub> |
|                           D/gdc |  2.484<sub>±0.058</sub> |  116.69<sub>±00.01</sub> + 600.52<sub>±00.00</sub> |  52.78<sub>±01.35</sub> |
|                         C#/Mono |  2.494<sub>±0.046</sub> |    476.32<sub>±00.12</sub> + 0.20<sub>±00.01</sub> |  52.99<sub>±00.25</sub> |
|                     Ruby (YAJL) |  2.611<sub>±0.061</sub> |  123.93<sub>±00.06</sub> + 283.33<sub>±00.01</sub> |  55.46<sub>±01.13</sub> |
|                            Ruby |  2.655<sub>±0.053</sub> |  123.94<sub>±00.06</sub> + 410.70<sub>±00.01</sub> |  57.79<sub>±00.69</sub> |
|                          D/ldc2 |  2.714<sub>±0.045</sub> |  112.99<sub>±00.06</sub> + 680.10<sub>±00.03</sub> |  60.21<sub>±00.73</sub> |
|                    Ruby (--jit) |  2.748<sub>±0.051</sub> |  123.99<sub>±00.06</sub> + 410.83<sub>±00.02</sub> |  59.68<sub>±02.08</sub> |
| F#/.NET Core (System.Text.Json) |  2.782<sub>±0.083</sub> |  485.25<sub>±00.02</sub> + 458.10<sub>±01.50</sub> |  59.53<sub>±00.97</sub> |
|                       Rust (jq) |  3.851<sub>±0.043</sub> |  113.80<sub>±00.03</sub> + 778.63<sub>±00.52</sub> |  83.66<sub>±02.95</sub> |
|                      Ruby/jruby |  4.053<sub>±0.093</sub> | 475.28<sub>±09.71</sub> + 1453.79<sub>±63.41</sub> | 120.61<sub>±06.17</sub> |
|    C++/g++ (Boost.PropertyTree) |  4.762<sub>±0.020</sub> | 112.97<sub>±00.01</sub> + 1440.06<sub>±00.00</sub> | 109.12<sub>±01.30</sub> |
|                           D/dmd |  5.441<sub>±0.048</sub> |  113.53<sub>±00.04</sub> + 680.07<sub>±00.01</sub> | 116.61<sub>±02.35</sub> |
|                      Vala/clang |  5.744<sub>±0.190</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 119.93<sub>±07.93</sub> |
|                        Vala/gcc |  5.753<sub>±0.114</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 115.87<sub>±04.04</sub> |
|               Perl (JSON::Tiny) | 12.644<sub>±0.097</sub> |  125.30<sub>±00.05</sub> + 528.60<sub>±00.03</sub> | 296.84<sub>±02.64</sub> |
|        Ruby/truffleruby (--jvm) | 17.785<sub>±0.455</sub> | 730.51<sub>±19.39</sub> + 1732.07<sub>±55.80</sub> | 507.44<sub>±26.29</sub> |
|                Ruby/truffleruby | 42.449<sub>±1.081</sub> | 739.47<sub>±02.07</sub> + 2276.93<sub>±53.78</sub> | 929.08<sub>±21.76</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                        Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | -------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.085<sub>±0.002</sub> |     6.76<sub>±00.04</sub> + 55.76<sub>±00.22</sub> |      4.61<sub>±00.04</sub> |
|           Python (NumPy) |    0.108<sub>±0.004</sub> |    27.63<sub>±00.07</sub> + 57.60<sub>±00.05</sub> |      5.84<sub>±00.20</sub> |
|    Nim/gcc (Arraymancer) |    0.173<sub>±0.018</sub> |     5.78<sub>±00.19</sub> + 57.17<sub>±00.24</sub> |      8.71<sub>±00.71</sub> |
|  Nim/clang (Arraymancer) |    0.198<sub>±0.031</sub> |     6.51<sub>±00.15</sub> + 57.29<sub>±00.14</sub> |      9.23<sub>±01.58</sub> |
|          C++/g++ (Eigen) |    0.206<sub>±0.003</sub> |     3.63<sub>±00.12</sub> + 85.24<sub>±00.00</sub> |      4.39<sub>±00.27</sub> |
|       Julia (threads: 8) |    0.207<sub>±0.003</sub> |   250.73<sub>±00.19</sub> + 53.19<sub>±00.18</sub> |     11.05<sub>±00.52</sub> |
|              Java (ND4J) |    0.240<sub>±0.020</sub> |   143.60<sub>±01.12</sub> + 87.67<sub>±00.19</sub> |     10.00<sub>±00.44</sub> |
|       Julia (threads: 1) |    0.576<sub>±0.029</sub> |   250.56<sub>±00.21</sub> + 53.47<sub>±00.03</sub> |     12.40<sub>±01.44</sub> |
|          Julia (no BLAS) |    1.232<sub>±0.015</sub> |   215.90<sub>±00.38</sub> + 51.62<sub>±00.00</sub> |     34.45<sub>±01.82</sub> |
|                   D/ldc2 |    2.018<sub>±0.012</sub> |     3.62<sub>±00.10</sub> + 70.11<sub>±00.00</sub> |     45.11<sub>±00.98</sub> |
|                    D/dmd |    2.176<sub>±0.027</sub> |     3.60<sub>±00.04</sub> + 70.12<sub>±00.01</sub> |     47.58<sub>±02.18</sub> |
|                    D/gdc |    2.187<sub>±0.061</sub> |     6.70<sub>±00.05</sub> + 70.69<sub>±00.01</sub> |     47.76<sub>±03.85</sub> |
|                     Java |    3.242<sub>±0.017</sub> |    38.79<sub>±00.11</sub> + 81.31<sub>±00.26</sub> |     75.38<sub>±01.65</sub> |
|                 Vala/gcc |    3.307<sub>±0.028</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     76.50<sub>±01.42</sub> |
|                    C/gcc |    3.312<sub>±0.051</sub> |     2.02<sub>±00.02</sub> + 68.06<sub>±00.00</sub> |     75.63<sub>±01.60</sub> |
|                    Scala |    3.358<sub>±0.037</sub> |    74.55<sub>±00.14</sub> + 80.89<sub>±02.57</sub> |     77.15<sub>±01.21</sub> |
|                  Nim/gcc |    3.397<sub>±0.032</sub> |     2.54<sub>±00.05</sub> + 65.48<sub>±00.00</sub> |     81.01<sub>±02.16</sub> |
|               Vala/clang |    3.444<sub>±0.037</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     73.45<sub>±00.73</sub> |
|                     Rust |    3.470<sub>±0.027</sub> |     2.66<sub>±00.10</sub> + 68.32<sub>±00.00</sub> |     72.74<sub>±02.13</sub> |
|                Nim/clang |    3.521<sub>±0.064</sub> |     3.07<sub>±00.04</sub> + 66.00<sub>±00.00</sub> |     73.80<sub>±02.69</sub> |
|                    Swift |    3.592<sub>±0.062</sub> |   144.95<sub>±00.04</sub> + 60.57<sub>±00.11</sub> |     77.32<sub>±05.12</sub> |
|                 Go/gccgo |    3.625<sub>±0.107</sub> |    21.46<sub>±00.17</sub> + 72.64<sub>±00.21</sub> |     73.94<sub>±01.38</sub> |
|                       Go |    3.703<sub>±0.069</sub> |     3.81<sub>±00.06</sub> + 73.15<sub>±00.16</sub> |     76.23<sub>±03.19</sub> |
|                  Crystal |    3.846<sub>±0.079</sub> |     4.35<sub>±00.22</sub> + 59.43<sub>±00.20</sub> |     74.47<sub>±04.61</sub> |
|                   Kotlin |    4.004<sub>±0.049</sub> |    38.59<sub>±00.08</sub> + 81.24<sub>±00.28</sub> |     71.75<sub>±02.87</sub> |
|                    V/gcc |    4.441<sub>±0.091</sub> |     2.03<sub>±00.01</sub> + 68.84<sub>±00.00</sub> |    101.20<sub>±04.11</sub> |
|                  Node.js |    4.880<sub>±0.066</sub> |    34.66<sub>±00.06</sub> + 71.10<sub>±00.51</sub> |     89.68<sub>±01.26</sub> |
|              Python/pypy |    6.520<sub>±0.261</sub> |    65.66<sub>±00.04</sub> + 69.17<sub>±00.01</sub> |    125.66<sub>±14.87</sub> |
|             C#/.NET Core |    7.198<sub>±0.360</sub> |    33.93<sub>±00.07</sub> + 69.10<sub>±00.00</sub> |    158.94<sub>±15.34</sub> |
|                  C#/Mono |   11.556<sub>±0.242</sub> |    20.18<sub>±00.06</sub> + 69.02<sub>±00.01</sub> |    203.32<sub>±11.33</sub> |
|                  V/clang |   14.623<sub>±0.382</sub> |     2.45<sub>±00.01</sub> + 68.84<sub>±00.00</sub> |    252.93<sub>±19.56</sub> |
|         Ruby/truffleruby |   47.034<sub>±0.518</sub> |  574.51<sub>±01.27</sub> + 685.30<sub>±03.53</sub> |   1115.82<sub>±23.55</sub> |
| Ruby/truffleruby (--jvm) |   68.261<sub>±0.796</sub> | 661.51<sub>±19.86</sub> + 687.85<sub>±116.10</sub> |   1692.73<sub>±54.29</sub> |
|                     Ruby |  222.057<sub>±4.896</sub> |    15.23<sub>±00.05</sub> + 68.61<sub>±00.03</sub> |   4820.25<sub>±86.57</sub> |
|             Ruby (--jit) |  228.150<sub>±7.978</sub> |    15.22<sub>±00.03</sub> + 68.94<sub>±00.00</sub> |  4876.27<sub>±300.59</sub> |
|                   Python |  242.022<sub>±9.447</sub> |    10.51<sub>±00.05</sub> + 68.58<sub>±00.00</sub> |   5144.17<sub>±38.18</sub> |
|                      Tcl | 349.675<sub>±11.664</sub> |    7.21<sub>±00.04</sub> + 400.41<sub>±00.03</sub> |  7854.20<sub>±662.18</sub> |
|                     Perl |  399.854<sub>±8.563</sub> |    8.97<sub>±00.06</sub> + 599.61<sub>±00.06</sub> |  9746.56<sub>±110.95</sub> |
|               Ruby/jruby | 517.823<sub>±21.836</sub> |  275.39<sub>±08.95</sub> + 992.21<sub>±54.98</sub> | 11588.29<sub>±403.88</sub> |

## Primes

Testing:

 - generating primes (using fast algorithm described by [Abd Elhakeem Abd Elnabya, A.H. El-Baz](https://doi.org/10.1016/j.eij.2020.05.002) );
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|    Language |                Time, s |                                      Memory, MiB |              Energy, J |
| :---------- | ---------------------: | -----------------------------------------------: | ---------------------: |
|        Rust | 0.790<sub>±0.008</sub> |   1.96<sub>±00.04</sub> + 87.42<sub>±00.51</sub> | 17.78<sub>±00.37</sub> |
|        Java | 0.853<sub>±0.006</sub> | 37.99<sub>±00.09</sub> + 490.97<sub>±02.15</sub> | 23.37<sub>±00.28</sub> |
|       Scala | 0.883<sub>±0.016</sub> | 73.20<sub>±00.49</sub> + 250.63<sub>±01.38</sub> | 27.75<sub>±00.54</sub> |
|     C++/g++ | 0.991<sub>±0.008</sub> |  1.50<sub>±00.00</sub> + 172.02<sub>±00.01</sub> | 21.64<sub>±00.44</sub> |
| Python/pypy | 1.441<sub>±0.026</sub> | 64.62<sub>±00.16</sub> + 224.43<sub>±00.02</sub> | 32.26<sub>±01.42</sub> |
|     Node.js | 1.747<sub>±0.009</sub> | 30.74<sub>±00.04</sub> + 308.15<sub>±00.54</sub> | 38.11<sub>±00.26</sub> |
|      Python | 4.708<sub>±0.127</sub> |  9.71<sub>±00.03</sub> + 327.08<sub>±00.39</sub> | 99.31<sub>±06.07</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.202                         |
| C#/.NET Core     | 3.9.0-6.21160.10 (59eedc33)     |
| C#/Mono          | 6.12.0.122                      |
| C/clang          | 11.0.1                          |
| C/gcc            | 10.2.1                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.3"                        |
| Crystal          | 1.0.0                           |
| D/dmd            | v2.096.1                        |
| D/gdc            | 10.2.1                          |
| D/ldc2           | 1.26.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 11.3.2.0 for F# 5.0             |
| Go               | go1.16.4                        |
| Go/gccgo         | 10.2.1                          |
| Haskell          | 8.10.4                          |
| Java             | 16                              |
| Julia            | v"1.6.1"                        |
| Kotlin           | 1.5.0                           |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20210117                        |
| Nim              | 1.4.6                           |
| Node.js          | v16.1.0                         |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.15                          |
| Perl             | v5.32.1                         |
| Python           | 3.9.2                           |
| Python/pypy      | 7.3.4-final0 for Python 3.7.10  |
| Racket           | "8.1"                           |
| Ruby             | 3.0.1p64                        |
| Ruby/jruby       | 9.2.17.0                        |
| Ruby/truffleruby | 21.1.0                          |
| Rust             | 1.52.0                          |
| Scala            | 2.13.5                          |
| Swift            | swift-5.4-RELEASE               |
| Tcl              | 8.6                             |
| V                | 0.2.2                           |
| Vala             | 0.48.15                         |

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
