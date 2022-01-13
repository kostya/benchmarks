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

UPDATE: 2021-12-22

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
|  Racket (Syntax Objects) |   1.417<sub>±0.057</sub> |   111.37<sub>±00.30</sub> + 0.00<sub>±00.00</sub> |     29.17<sub>±01.71</sub> |
|                  C++/g++ |   1.549<sub>±0.118</sub> |     1.68<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     30.33<sub>±00.98</sub> |
|                  C/clang |   1.834<sub>±0.004</sub> |     0.73<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     40.82<sub>±01.04</sub> |
|                     Rust |   1.834<sub>±0.086</sub> |     2.12<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |     36.41<sub>±02.69</sub> |
|                   D/ldc2 |   1.862<sub>±0.028</sub> |     3.18<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     33.17<sub>±00.90</sub> |
|                    D/gdc |   1.891<sub>±0.063</sub> |     6.81<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     40.42<sub>±01.22</sub> |
|                    C/gcc |   1.936<sub>±0.041</sub> |     0.73<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     36.34<sub>±02.03</sub> |
|                   Kotlin |   1.944<sub>±0.149</sub> |    39.47<sub>±00.18</sub> + 1.91<sub>±00.16</sub> |     37.58<sub>±01.39</sub> |
|              C++/clang++ |   1.956<sub>±0.080</sub> |     1.46<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     35.74<sub>±01.40</sub> |
|                  Nim/gcc |   1.956<sub>±0.028</sub> |     1.90<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     41.14<sub>±02.08</sub> |
|                       Go |   1.976<sub>±0.020</sub> |     3.29<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     42.79<sub>±01.49</sub> |
|                      Zig |   2.033<sub>±0.037</sub> |     0.78<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     35.63<sub>±00.43</sub> |
|                 Go/gccgo |   2.082<sub>±0.075</sub> |    22.33<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |     38.42<sub>±03.92</sub> |
|                    OCaml |   2.158<sub>±0.060</sub> |     2.63<sub>±00.03</sub> + 2.48<sub>±00.00</sub> |     47.02<sub>±02.59</sub> |
|                 Vala/gcc |   2.191<sub>±0.067</sub> |     4.38<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     44.33<sub>±03.02</sub> |
|                     Java |   2.214<sub>±0.068</sub> |    37.14<sub>±00.19</sub> + 1.57<sub>±00.17</sub> |     39.43<sub>±01.23</sub> |
|             C#/.NET Core |   2.219<sub>±0.074</sub> |    34.20<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |     44.72<sub>±01.70</sub> |
|                Nim/clang |   2.234<sub>±0.093</sub> |     2.43<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     45.08<sub>±04.15</sub> |
|                   Racket |   2.313<sub>±0.080</sub> |    95.95<sub>±00.20</sub> + 0.00<sub>±00.00</sub> |     43.31<sub>±02.68</sub> |
|                  Crystal |   2.423<sub>±0.049</sub> |     3.44<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     50.95<sub>±00.85</sub> |
|               Vala/clang |   2.452<sub>±0.098</sub> |     4.36<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     47.88<sub>±02.79</sub> |
|             F#/.NET Core |   2.504<sub>±0.090</sub> |    37.75<sub>±00.08</sub> + 0.62<sub>±00.00</sub> |     46.32<sub>±03.60</sub> |
|                    V/gcc |   2.533<sub>±0.083</sub> |     0.74<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     51.76<sub>±04.02</sub> |
|                    MLton |   2.822<sub>±0.071</sub> |     1.55<sub>±00.06</sub> + 0.25<sub>±00.00</sub> |     52.63<sub>±01.67</sub> |
|              Chez Scheme |   2.858<sub>±0.094</sub> |    24.84<sub>±00.05</sub> + 4.39<sub>±00.05</sub> |     63.23<sub>±03.01</sub> |
|                  V/clang |   2.950<sub>±0.048</sub> |     0.71<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     51.94<sub>±01.44</sub> |
|                    Julia |   3.287<sub>±0.072</sub> |   207.40<sub>±00.06</sub> + 0.86<sub>±00.03</sub> |     53.29<sub>±01.67</sub> |
|                  C#/Mono |   3.333<sub>±0.101</sub> |    20.19<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     64.50<sub>±05.13</sub> |
|                    D/dmd |   3.462<sub>±0.091</sub> |     3.78<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     72.56<sub>±02.47</sub> |
|                    Scala |   3.523<sub>±0.107</sub> |  75.45<sub>±00.29</sub> + 236.47<sub>±01.03</sub> |     74.83<sub>±04.27</sub> |
|                  Node.js |   4.471<sub>±0.165</sub> |    34.54<sub>±00.03</sub> + 1.75<sub>±00.06</sub> |     83.82<sub>±07.15</sub> |
|         Haskell (MArray) |   4.643<sub>±0.117</sub> |     3.84<sub>±00.12</sub> + 1.07<sub>±00.00</sub> |    102.80<sub>±05.83</sub> |
|                    Swift |   5.926<sub>±0.097</sub> |    13.29<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    109.98<sub>±02.90</sub> |
|               Lua/luajit |   7.801<sub>±0.240</sub> |     2.45<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |    155.67<sub>±07.16</sub> |
| Ruby/truffleruby (--jvm) |   8.157<sub>±1.000</sub> | 580.04<sub>±02.78</sub> + 642.83<sub>±55.39</sub> |    283.76<sub>±18.13</sub> |
|         Ruby/truffleruby |   9.309<sub>±0.247</sub> | 448.06<sub>±01.96</sub> + 564.69<sub>±15.11</sub> |    241.52<sub>±10.32</sub> |
|              Python/pypy |  14.256<sub>±0.142</sub> |   63.89<sub>±00.18</sub> + 46.98<sub>±00.10</sub> |    335.05<sub>±09.73</sub> |
|                  Haskell |  16.888<sub>±0.438</sub> |     4.03<sub>±00.16</sub> + 0.82<sub>±00.00</sub> |    364.48<sub>±20.99</sub> |
|                      Lua |  56.785<sub>±0.669</sub> |     2.28<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   1201.22<sub>±12.56</sub> |
|                   Elixir |  58.647<sub>±1.550</sub> |    76.20<sub>±00.88</sub> + 0.24<sub>±00.20</sub> |   1085.53<sub>±73.23</sub> |
|             Ruby (--jit) |  58.801<sub>±1.179</sub> |    14.22<sub>±00.03</sub> + 0.24<sub>±00.00</sub> |   1242.18<sub>±24.88</sub> |
|                     Ruby |  98.637<sub>±1.884</sub> |    14.17<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  1825.14<sub>±134.80</sub> |
|               Ruby/jruby | 111.328<sub>±2.431</sub> | 185.95<sub>±01.26</sub> + 109.54<sub>±03.11</sub> |   2225.11<sub>±45.95</sub> |
|                   Python | 239.635<sub>±7.290</sub> |    10.27<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  4600.97<sub>±157.90</sub> |
|                 Tcl (FP) | 284.120<sub>±0.892</sub> |     4.32<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   5235.87<sub>±84.17</sub> |
|                     Perl | 356.731<sub>±6.203</sub> |     6.60<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  7271.62<sub>±360.18</sub> |
|                Tcl (OOP) | 539.812<sub>±2.773</sub> |     4.29<sub>±00.03</sub> + 0.00<sub>±00.00</sub> | 11636.75<sub>±137.19</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                  C++/g++ |  10.932<sub>±0.199</sub> |     1.68<sub>±00.01</sub> + 2.34<sub>±00.05</sub> |   225.83<sub>±05.41</sub> |
|                     Rust |  13.704<sub>±0.488</sub> |     2.19<sub>±00.07</sub> + 0.24<sub>±00.00</sub> |   257.81<sub>±22.14</sub> |
|                    C/gcc |  13.797<sub>±0.147</sub> |     0.73<sub>±00.00</sub> + 0.98<sub>±00.05</sub> |   224.36<sub>±08.14</sub> |
|                   D/ldc2 |  14.336<sub>±0.525</sub> |     3.22<sub>±00.07</sub> + 0.77<sub>±00.00</sub> |   289.63<sub>±17.83</sub> |
|              C++/clang++ |  14.854<sub>±0.429</sub> |     1.47<sub>±00.06</sub> + 2.15<sub>±00.01</sub> |   262.24<sub>±12.29</sub> |
|                  C/clang |  15.834<sub>±0.224</sub> |     0.73<sub>±00.03</sub> + 0.99<sub>±00.03</sub> |   335.59<sub>±08.99</sub> |
|                    V/gcc |  16.251<sub>±0.549</sub> |     1.75<sub>±00.05</sub> + 1.03<sub>±00.00</sub> |   298.69<sub>±21.98</sub> |
|  Racket (Syntax Objects) |  16.585<sub>±0.295</sub> |  112.76<sub>±01.33</sub> + 69.35<sub>±00.39</sub> |   354.98<sub>±16.04</sub> |
|                       Go |  17.049<sub>±0.408</sub> |     3.25<sub>±00.03</sub> + 1.25<sub>±00.00</sub> |   346.99<sub>±18.08</sub> |
|                    D/gdc |  17.059<sub>±0.656</sub> |     6.82<sub>±00.05</sub> + 0.54<sub>±00.02</sub> |   322.18<sub>±26.82</sub> |
|                   Kotlin |  17.255<sub>±0.492</sub> |    39.56<sub>±00.33</sub> + 2.12<sub>±00.56</sub> |   319.86<sub>±25.34</sub> |
|                      Zig |  17.913<sub>±0.524</sub> |     1.56<sub>±00.01</sub> + 0.77<sub>±00.00</sub> |   366.02<sub>±20.50</sub> |
|                  Crystal |  18.032<sub>±0.229</sub> |     3.47<sub>±00.04</sub> + 0.38<sub>±00.02</sub> |   374.53<sub>±09.29</sub> |
|                  Nim/gcc |  19.242<sub>±0.616</sub> |     1.95<sub>±00.07</sub> + 0.54<sub>±00.03</sub> |   322.23<sub>±07.81</sub> |
|                 Vala/gcc |  19.883<sub>±0.831</sub> |     4.22<sub>±00.08</sub> + 1.96<sub>±00.02</sub> |   391.40<sub>±35.74</sub> |
|                     Java |  20.599<sub>±0.671</sub> |    37.11<sub>±00.16</sub> + 2.11<sub>±00.18</sub> |   412.17<sub>±23.31</sub> |
|               Vala/clang |  21.014<sub>±0.725</sub> |     4.19<sub>±00.05</sub> + 1.81<sub>±00.05</sub> |   419.14<sub>±29.38</sub> |
|                 Go/gccgo |  21.430<sub>±0.299</sub> |    22.33<sub>±00.11</sub> + 1.28<sub>±00.01</sub> |   417.51<sub>±18.90</sub> |
|                    Swift |  21.983<sub>±0.323</sub> |    14.11<sub>±00.11</sub> + 0.00<sub>±00.00</sub> |   475.14<sub>±08.64</sub> |
|                  V/clang |  22.298<sub>±0.232</sub> |     1.66<sub>±00.11</sub> + 0.77<sub>±00.00</sub> |   462.65<sub>±12.62</sub> |
|                Nim/clang |  22.933<sub>±0.279</sub> |     2.43<sub>±00.02</sub> + 0.54<sub>±00.03</sub> |   403.75<sub>±09.81</sub> |
|             C#/.NET Core |  23.099<sub>±0.468</sub> |    33.84<sub>±00.17</sub> + 1.14<sub>±00.06</sub> |   452.36<sub>±24.09</sub> |
|                    Scala |  25.662<sub>±0.355</sub> |  76.25<sub>±00.49</sub> + 126.77<sub>±00.15</sub> |   525.21<sub>±12.19</sub> |
|                    OCaml |  37.151<sub>±0.732</sub> |     3.91<sub>±00.07</sub> + 7.05<sub>±00.25</sub> |   782.03<sub>±38.94</sub> |
|                   Racket |  39.353<sub>±0.459</sub> |    96.41<sub>±01.05</sub> + 0.13<sub>±00.13</sub> |   724.92<sub>±67.64</sub> |
|              Chez Scheme |  40.947<sub>±1.064</sub> |    25.63<sub>±00.06</sub> + 3.71<sub>±00.04</sub> |   819.36<sub>±54.21</sub> |
|                    D/dmd |  42.822<sub>±0.684</sub> |     3.81<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   915.79<sub>±24.16</sub> |
|             F#/.NET Core |  43.657<sub>±0.294</sub> |    37.89<sub>±00.06</sub> + 2.15<sub>±00.00</sub> |   924.88<sub>±04.91</sub> |
|                  C#/Mono |  44.231<sub>±1.507</sub> |    20.22<sub>±00.03</sub> + 0.88<sub>±00.00</sub> |   831.32<sub>±62.72</sub> |
|               Lua/luajit |  45.326<sub>±1.163</sub> |     2.42<sub>±00.06</sub> + 0.44<sub>±00.00</sub> |   839.58<sub>±58.77</sub> |
|                  Node.js |  48.492<sub>±1.182</sub> |    35.43<sub>±00.04</sub> + 5.22<sub>±00.02</sub> |   982.69<sub>±47.24</sub> |
|                    MLton |  52.723<sub>±1.259</sub> |     1.56<sub>±00.06</sub> + 4.11<sub>±00.00</sub> |  1039.11<sub>±69.20</sub> |
|              Python/pypy |  65.210<sub>±1.483</sub> |   63.59<sub>±00.09</sub> + 47.69<sub>±00.03</sub> |  1400.93<sub>±49.89</sub> |
|         Haskell (MArray) |  66.932<sub>±1.044</sub> |     3.96<sub>±00.13</sub> + 2.59<sub>±00.00</sub> |  1262.52<sub>±34.14</sub> |
|                    Julia |  67.907<sub>±2.015</sub> |   208.07<sub>±00.15</sub> + 0.83<sub>±00.03</sub> |  1425.38<sub>±59.31</sub> |
| Ruby/truffleruby (--jvm) | 124.262<sub>±3.714</sub> | 582.11<sub>±02.68</sub> + 539.13<sub>±12.88</sub> |  2525.70<sub>±91.42</sub> |
|         Ruby/truffleruby | 137.807<sub>±5.544</sub> | 447.55<sub>±04.24</sub> + 565.55<sub>±25.32</sub> | 2474.62<sub>±150.11</sub> |
|                  Haskell | 234.604<sub>±2.184</sub> |    3.88<sub>±00.06</sub> + 26.15<sub>±00.06</sub> | 4352.85<sub>±150.57</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.138<sub>±0.006</sub> |     2.03<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.39<sub>±00.11</sub> |
|          C/clang (aklomp) |  0.139<sub>±0.006</sub> |     2.05<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   3.28<sub>±00.14</sub> |
|                      Rust |  1.179<sub>±0.040</sub> |     2.58<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |  22.94<sub>±01.95</sub> |
|                     C/gcc |  1.277<sub>±0.029</sub> |     2.00<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  20.30<sub>±00.76</sub> |
|                   C/clang |  1.292<sub>±0.023</sub> |     2.00<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  21.24<sub>±01.02</sub> |
|                   Nim/gcc |  1.358<sub>±0.022</sub> |     2.31<sub>±00.07</sub> + 4.12<sub>±00.03</sub> |  23.56<sub>±01.30</sub> |
|                   V/clang |  1.375<sub>±0.049</sub> |     1.92<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |  24.07<sub>±01.31</sub> |
|                     V/gcc |  1.501<sub>±0.036</sub> |     2.20<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |  25.28<sub>±00.90</sub> |
|                 Nim/clang |  1.606<sub>±0.053</sub> |     2.81<sub>±00.03</sub> + 4.38<sub>±00.00</sub> |  28.19<sub>±01.53</sub> |
|                    D/ldc2 |  1.917<sub>±0.013</sub> |     3.68<sub>±00.04</sub> + 3.63<sub>±00.00</sub> |  36.26<sub>±00.72</sub> |
|                   Crystal |  2.076<sub>±0.008</sub> |     3.89<sub>±00.05</sub> + 1.16<sub>±00.02</sub> |  54.37<sub>±00.66</sub> |
|                Vala/clang |  2.191<sub>±0.008</sub> |     5.58<sub>±00.03</sub> + 0.43<sub>±00.05</sub> |  47.75<sub>±00.28</sub> |
|                      Java |  2.191<sub>±0.068</sub> |  39.22<sub>±00.26</sub> + 286.26<sub>±28.64</sub> |  44.34<sub>±04.35</sub> |
|                      Ruby |  2.302<sub>±0.039</sub> |   14.55<sub>±00.03</sub> + 66.29<sub>±01.71</sub> |  49.55<sub>±01.94</sub> |
|                  Vala/gcc |  2.339<sub>±0.077</sub> |     5.53<sub>±00.05</sub> + 0.42<sub>±00.06</sub> |  40.39<sub>±01.67</sub> |
|                    Kotlin |  2.362<sub>±0.094</sub> |  40.81<sub>±00.35</sub> + 311.82<sub>±00.15</sub> |  48.62<sub>±03.29</sub> |
|              Ruby (--jit) |  2.400<sub>±0.069</sub> |   14.58<sub>±00.02</sub> + 67.93<sub>±02.68</sub> |  45.74<sub>±02.96</sub> |
|                     Scala |  2.415<sub>±0.041</sub> |  75.06<sub>±00.12</sub> + 231.62<sub>±08.09</sub> |  47.31<sub>±02.11</sub> |
|                        Go |  2.559<sub>±0.005</sub> |     4.43<sub>±00.05</sub> + 5.21<sub>±00.13</sub> |  52.15<sub>±00.54</sub> |
|       C++/g++ (libcrypto) |  2.684<sub>±0.035</sub> |     5.69<sub>±00.05</sub> + 0.08<sub>±00.00</sub> |  57.99<sub>±01.21</sub> |
|                       PHP |  2.795<sub>±0.045</sub> |    15.78<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  57.48<sub>±00.78</sub> |
|                   Node.js |  2.845<sub>±0.027</sub> |   35.02<sub>±00.02</sub> + 36.70<sub>±00.09</sub> |  60.60<sub>±00.59</sub> |
|   C++/clang++ (libcrypto) |  2.876<sub>±0.042</sub> |     5.18<sub>±00.06</sub> + 0.08<sub>±00.00</sub> |  48.94<sub>±01.27</sub> |
|                  Go/gccgo |  3.065<sub>±0.009</sub> |    23.27<sub>±00.04</sub> + 8.85<sub>±00.29</sub> |  66.04<sub>±00.42</sub> |
|       Perl (MIME::Base64) |  3.104<sub>±0.075</sub> |    14.22<sub>±00.07</sub> + 0.01<sub>±00.01</sub> |  51.62<sub>±02.06</sub> |
|                     D/gdc |  3.251<sub>±0.029</sub> |     7.19<sub>±00.04</sub> + 3.41<sub>±00.02</sub> |  68.60<sub>±00.39</sub> |
|                    Python |  3.629<sub>±0.160</sub> |    10.04<sub>±00.01</sub> + 0.18<sub>±00.00</sub> |  69.99<sub>±07.43</sub> |
|                     D/dmd |  3.638<sub>±0.018</sub> |     3.94<sub>±00.07</sub> + 3.62<sub>±00.00</sub> |  72.03<sub>±00.33</sub> |
|                       Zig |  4.163<sub>±0.068</sub> |     1.50<sub>±00.02</sub> + 0.37<sub>±00.00</sub> |  88.89<sub>±03.47</sub> |
|               Python/pypy |  4.927<sub>±0.131</sub> |   63.58<sub>±00.14</sub> + 47.60<sub>±00.05</sub> |  92.88<sub>±07.39</sub> |
|              F#/.NET Core |  5.165<sub>±0.025</sub> |   38.55<sub>±00.06</sub> + 30.58<sub>±01.23</sub> |  93.99<sub>±00.72</sub> |
|              C#/.NET Core |  5.209<sub>±0.013</sub> |   34.50<sub>±00.03</sub> + 36.86<sub>±06.25</sub> |  95.84<sub>±00.65</sub> |
|                       Tcl |  5.741<sub>±0.247</sub> |     4.85<sub>±00.01</sub> + 0.18<sub>±00.00</sub> | 110.94<sub>±10.01</sub> |
|                     Julia |  6.317<sub>±0.162</sub> |  226.69<sub>±00.10</sub> + 61.91<sub>±00.16</sub> | 142.27<sub>±08.35</sub> |
|  Ruby/truffleruby (--jvm) |  6.321<sub>±0.071</sub> | 580.84<sub>±03.60</sub> + 262.36<sub>±25.16</sub> | 125.82<sub>±04.45</sub> |
|                   C#/Mono |  7.746<sub>±0.155</sub> |   20.75<sub>±00.04</sub> + 18.49<sub>±00.03</sub> | 138.70<sub>±02.90</sub> |
|                Ruby/jruby | 12.336<sub>±0.332</sub> | 184.49<sub>±02.39</sub> + 132.51<sub>±03.28</sub> | 237.42<sub>±10.99</sub> |
| Perl (MIME::Base64::Perl) | 17.145<sub>±0.580</sub> |    15.52<sub>±00.08</sub> + 0.21<sub>±00.05</sub> | 362.20<sub>±17.27</sub> |
|          Ruby/truffleruby | 20.145<sub>±0.330</sub> | 442.57<sub>±03.88</sub> + 319.73<sub>±02.45</sub> | 408.44<sub>±09.21</sub> |

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

|                            Language |                 Time, s |                                        Memory, MiB |               Energy, J |
| :---------------------------------- | ----------------------: | -------------------------------------------------: | ----------------------: |
|        C++/g++ (simdjson On-Demand) |  0.086<sub>±0.002</sub> |   113.43<sub>±00.03</sub> + 59.81<sub>±00.00</sub> |   1.92<sub>±00.14</sub> |
| C++/clang++ (DAW JSON Link NoCheck) |  0.090<sub>±0.002</sub> |    112.50<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   2.38<sub>±00.03</sub> |
|    C++/clang++ (simdjson On-Demand) |  0.090<sub>±0.002</sub> |   113.05<sub>±00.04</sub> + 59.81<sub>±00.00</sub> |   2.19<sub>±00.17</sub> |
|     C++/g++ (DAW JSON Link NoCheck) |  0.091<sub>±0.002</sub> |    113.22<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   2.35<sub>±00.10</sub> |
|             C++/g++ (DAW JSON Link) |  0.102<sub>±0.003</sub> |    113.23<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |   2.64<sub>±00.07</sub> |
|         C++/clang++ (DAW JSON Link) |  0.113<sub>±0.002</sub> |    112.46<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   2.27<sub>±00.08</sub> |
|                  Rust (Serde Typed) |  0.150<sub>±0.009</sub> |   111.95<sub>±00.12</sub> + 11.66<sub>±00.39</sub> |   3.06<sub>±00.35</sub> |
|                 Rust (Serde Custom) |  0.151<sub>±0.005</sub> |    111.92<sub>±00.12</sub> + 0.00<sub>±00.00</sub> |   2.72<sub>±00.22</sub> |
|              C++/g++ (simdjson DOM) |  0.151<sub>±0.004</sub> |  113.44<sub>±00.03</sub> + 176.60<sub>±00.00</sub> |   3.85<sub>±00.29</sub> |
|          C++/clang++ (simdjson DOM) |  0.153<sub>±0.004</sub> |  113.07<sub>±00.04</sub> + 176.60<sub>±00.00</sub> |   3.41<sub>±00.13</sub> |
|                     C++/g++ (gason) |  0.168<sub>±0.007</sub> |   113.15<sub>±00.04</sub> + 96.77<sub>±00.03</sub> |   4.05<sub>±00.35</sub> |
|                 C++/clang++ (gason) |  0.190<sub>±0.003</sub> |   112.48<sub>±00.05</sub> + 96.97<sub>±00.00</sub> |   3.82<sub>±00.15</sub> |
|                 C++/g++ (RapidJSON) |  0.218<sub>±0.004</sub> |  113.26<sub>±00.04</sub> + 128.79<sub>±00.03</sub> |   4.42<sub>±00.31</sub> |
|       D/ldc2 (Mir Amazon's Ion DOM) |  0.241<sub>±0.006</sub> |   112.78<sub>±00.05</sub> + 16.25<sub>±00.01</sub> |   5.12<sub>±00.41</sub> |
|               D/ldc2 (Mir Asdf DOM) |  0.243<sub>±0.008</sub> |   112.78<sub>±00.04</sub> + 61.34<sub>±00.00</sub> |   4.85<sub>±00.38</sub> |
|             C++/clang++ (RapidJSON) |  0.273<sub>±0.009</sub> |  112.54<sub>±00.05</sub> + 129.00<sub>±00.02</sub> |   5.30<sub>±00.29</sub> |
|         C++/g++ (RapidJSON Precise) |  0.288<sub>±0.010</sub> |  113.21<sub>±00.04</sub> + 128.84<sub>±00.02</sub> |   6.24<sub>±00.61</sub> |
|     C++/clang++ (RapidJSON Precise) |  0.453<sub>±0.017</sub> |  112.52<sub>±00.08</sub> + 129.02<sub>±00.03</sub> |   8.94<sub>±00.66</sub> |
|                C++/g++ (Boost.JSON) |  0.511<sub>±0.011</sub> |  113.25<sub>±00.05</sub> + 435.82<sub>±00.06</sub> |  10.74<sub>±00.72</sub> |
|             C++/g++ (RapidJSON SAX) |  0.536<sub>±0.008</sub> |    112.97<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  12.05<sub>±00.49</sub> |
|            C++/clang++ (Boost.JSON) |  0.563<sub>±0.020</sub> |  112.47<sub>±00.04</sub> + 436.28<sub>±00.03</sub> |  11.59<sub>±00.66</sub> |
|         C++/clang++ (RapidJSON SAX) |  0.598<sub>±0.017</sub> |    194.74<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |  10.97<sub>±00.44</sub> |
|     C++/g++ (RapidJSON SAX Precise) |  0.644<sub>±0.018</sub> |    112.98<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  12.37<sub>±01.17</sub> |
|                       Go (jsoniter) |  0.646<sub>±0.009</sub> |   231.06<sub>±00.07</sub> + 13.54<sub>±00.62</sub> |  14.83<sub>±00.36</sub> |
|                             Node.js |  0.680<sub>±0.015</sub> |  145.46<sub>±00.05</sub> + 187.17<sub>±00.08</sub> |  15.28<sub>±00.69</sub> |
|                     Java (DSL-JSON) |  0.741<sub>±0.012</sub> |  263.98<sub>±00.18</sub> + 228.72<sub>±05.91</sub> |  20.13<sub>±00.28</sub> |
| C++/clang++ (RapidJSON SAX Precise) |  0.745<sub>±0.022</sub> |    194.78<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  16.48<sub>±01.35</sub> |
|                Rust (Serde Untyped) |  0.794<sub>±0.010</sub> |  112.00<sub>±00.05</sub> + 839.98<sub>±00.03</sub> |  18.06<sub>±00.21</sub> |
|                         Python/pypy |  0.831<sub>±0.028</sub> |  283.59<sub>±00.13</sub> + 123.29<sub>±00.00</sub> |  16.46<sub>±01.38</sub> |
|                                 Zig |  0.882<sub>±0.011</sub> |   110.79<sub>±00.01</sub> + 12.18<sub>±00.00</sub> |  20.14<sub>±00.56</sub> |
|                               V/gcc |  0.887<sub>±0.024</sub> |  111.23<sub>±00.04</sub> + 496.21<sub>±00.06</sub> |  17.23<sub>±01.58</sub> |
|                             V/clang |  0.895<sub>±0.022</sub> |  111.24<sub>±00.04</sub> + 496.21<sub>±00.06</sub> |  17.19<sub>±01.44</sub> |
|     C#/.NET Core (System.Text.Json) |  0.926<sub>±0.034</sub> |  478.78<sub>±00.07</sub> + 138.78<sub>±00.00</sub> |  19.05<sub>±01.58</sub> |
|                       Julia (JSON3) |  0.959<sub>±0.022</sub> |  442.05<sub>±00.28</sub> + 262.58<sub>±00.38</sub> |  18.47<sub>±00.51</sub> |
|                    Crystal (Schema) |  0.972<sub>±0.028</sub> |   113.79<sub>±00.01</sub> + 47.31<sub>±00.01</sub> |  19.59<sub>±01.14</sub> |
|                      Crystal (Pull) |  1.011<sub>±0.028</sub> |   113.82<sub>±00.01</sub> + 18.16<sub>±00.02</sub> |  16.15<sub>±00.87</sub> |
|             Perl (Cpanel::JSON::XS) |  1.050<sub>±0.065</sub> |  124.75<sub>±00.04</sub> + 402.72<sub>±00.00</sub> |  23.86<sub>±01.82</sub> |
|                                  Go |  1.057<sub>±0.022</sub> |   117.23<sub>±00.07</sub> + 83.05<sub>±00.22</sub> |  19.22<sub>±00.78</sub> |
|              Nim/clang (Packedjson) |  1.099<sub>±0.005</sub> |  112.71<sub>±00.05</sub> + 293.91<sub>±00.00</sub> |  26.19<sub>±00.39</sub> |
|                Nim/gcc (Packedjson) |  1.250<sub>±0.033</sub> |  112.19<sub>±00.04</sub> + 293.91<sub>±00.00</sub> |  23.56<sub>±00.91</sub> |
|                             Crystal |  1.252<sub>±0.052</sub> |  113.82<sub>±00.05</sub> + 398.95<sub>±00.01</sub> |  26.46<sub>±01.61</sub> |
|                                 PHP |  1.269<sub>±0.044</sub> |  125.03<sub>±00.06</sub> + 682.02<sub>±00.00</sub> |  25.92<sub>±01.22</sub> |
|                            Go/gccgo |  1.558<sub>±0.025</sub> |   137.42<sub>±00.11</sub> + 83.62<sub>±00.09</sub> |  36.49<sub>±00.57</sub> |
|                    C++/g++ (json-c) |  1.579<sub>±0.056</sub> | 113.31<sub>±00.06</sub> + 1215.97<sub>±00.06</sub> |  33.49<sub>±02.50</sub> |
|                             Clojure |  1.601<sub>±0.057</sub> |  457.04<sub>±04.39</sub> + 500.38<sub>±17.00</sub> |  44.61<sub>±00.80</sub> |
|                C++/clang++ (json-c) |  1.621<sub>±0.044</sub> | 112.71<sub>±00.04</sub> + 1216.10<sub>±00.04</sub> |  36.66<sub>±03.30</sub> |
|                           Nim/clang |  1.698<sub>±0.087</sub> |  112.63<sub>±00.08</sub> + 924.83<sub>±00.06</sub> |  36.52<sub>±02.82</sub> |
|                        C#/.NET Core |  1.714<sub>±0.039</sub> |  486.62<sub>±00.13</sub> + 294.26<sub>±00.06</sub> |  30.69<sub>±00.60</sub> |
|              C++/clang++ (Nlohmann) |  1.747<sub>±0.103</sub> |  112.63<sub>±00.06</sub> + 360.16<sub>±00.03</sub> |  35.72<sub>±03.35</sub> |
|                 CPython (UltraJSON) |  1.781<sub>±0.017</sub> |  121.66<sub>±00.03</sub> + 549.44<sub>±01.42</sub> |  37.02<sub>±00.88</sub> |
|                             Nim/gcc |  1.796<sub>±0.074</sub> |  112.18<sub>±00.07</sub> + 919.39<sub>±00.03</sub> |  37.71<sub>±02.60</sub> |
|                  C++/g++ (Nlohmann) |  1.810<sub>±0.054</sub> |  113.23<sub>±00.11</sub> + 447.94<sub>±00.02</sub> |  39.23<sub>±03.09</sub> |
|                              Python |  1.823<sub>±0.037</sub> |  120.04<sub>±00.03</sub> + 377.33<sub>±00.00</sub> |  42.37<sub>±01.44</sub> |
|     F#/.NET Core (System.Text.Json) |  2.208<sub>±0.022</sub> |  486.01<sub>±00.08</sub> + 456.59<sub>±03.35</sub> |  49.37<sub>±01.63</sub> |
|                             C#/Mono |  2.221<sub>±0.079</sub> |    476.31<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  45.85<sub>±04.05</sub> |
|                         Ruby (YAJL) |  2.411<sub>±0.054</sub> |  124.04<sub>±00.05</sub> + 281.67<sub>±00.28</sub> |  46.60<sub>±03.88</sub> |
|                                Ruby |  2.418<sub>±0.023</sub> |  124.44<sub>±00.04</sub> + 410.33<sub>±00.01</sub> |  55.61<sub>±01.44</sub> |
|                        Ruby (--jit) |  2.480<sub>±0.026</sub> |  124.45<sub>±00.04</sub> + 410.49<sub>±00.01</sub> |  59.05<sub>±00.65</sub> |
|                              D/ldc2 |  2.560<sub>±0.071</sub> |  113.02<sub>±00.08</sub> + 680.16<sub>±00.04</sub> |  52.06<sub>±01.66</sub> |
|                     Scala (uPickle) |  2.612<sub>±0.036</sub> |  304.31<sub>±00.22</sub> + 714.83<sub>±50.60</sub> |  54.41<sub>±03.45</sub> |
|                               D/gdc |  3.271<sub>±0.046</sub> |  116.93<sub>±00.03</sub> + 600.63<sub>±00.02</sub> |  55.67<sub>±02.06</sub> |
|                             Haskell |  3.560<sub>±0.072</sub> |  116.04<sub>±00.04</sub> + 715.29<sub>±00.10</sub> |  86.73<sub>±02.61</sub> |
|                           Rust (jq) |  3.831<sub>±0.067</sub> |  113.82<sub>±00.03</sub> + 779.54<sub>±00.39</sub> |  68.03<sub>±02.24</sub> |
|                          Ruby/jruby |  3.991<sub>±0.086</sub> | 457.12<sub>±01.81</sub> + 1060.00<sub>±16.29</sub> |  97.01<sub>±04.45</sub> |
|        C++/g++ (Boost.PropertyTree) |  4.154<sub>±0.147</sub> | 113.14<sub>±00.02</sub> + 1440.10<sub>±00.03</sub> |  85.63<sub>±08.44</sub> |
|    C++/clang++ (Boost.PropertyTree) |  4.156<sub>±0.064</sub> | 194.95<sub>±00.02</sub> + 1232.84<sub>±00.06</sub> |  78.24<sub>±03.04</sub> |
|                          Vala/clang |  5.015<sub>±0.095</sub> |  114.78<sub>±00.05</sub> + 932.42<sub>±00.01</sub> | 108.59<sub>±06.93</sub> |
|                            Vala/gcc |  5.059<sub>±0.196</sub> |  114.74<sub>±00.03</sub> + 996.92<sub>±00.03</sub> | 112.26<sub>±06.89</sub> |
|                               D/dmd |  5.660<sub>±0.120</sub> |  113.66<sub>±00.02</sub> + 680.23<sub>±00.03</sub> | 112.01<sub>±05.85</sub> |
|                   Perl (JSON::Tiny) | 12.609<sub>±0.139</sub> |  125.13<sub>±00.05</sub> + 528.83<sub>±00.06</sub> | 232.63<sub>±07.37</sub> |
|            Ruby/truffleruby (--jvm) | 16.083<sub>±0.324</sub> | 779.75<sub>±06.79</sub> + 1702.23<sub>±52.04</sub> | 514.39<sub>±14.84</sub> |
|                    Ruby/truffleruby | 22.709<sub>±0.647</sub> | 784.33<sub>±07.49</sub> + 1950.26<sub>±43.96</sub> | 506.48<sub>±19.41</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.076<sub>±0.001</sub> |    7.06<sub>±00.03</sub> + 55.93<sub>±00.22</sub> |      4.41<sub>±00.04</sub> |
|           Python (NumPy) |    0.101<sub>±0.002</sub> |   28.12<sub>±00.09</sub> + 57.62<sub>±00.04</sub> |      5.72<sub>±00.14</sub> |
|              Java (ND4J) |    0.105<sub>±0.003</sub> |  146.61<sub>±01.21</sub> + 90.75<sub>±00.04</sub> |      4.74<sub>±00.37</sub> |
|  Nim/clang (Arraymancer) |    0.107<sub>±0.013</sub> |    6.61<sub>±00.10</sub> + 55.50<sub>±00.28</sub> |      5.79<sub>±00.64</sub> |
|    Nim/gcc (Arraymancer) |    0.132<sub>±0.014</sub> |    5.85<sub>±00.26</sub> + 55.81<sub>±00.50</sub> |      6.95<sub>±00.64</sub> |
|       Julia (threads: 8) |    0.166<sub>±0.003</sub> |  234.35<sub>±00.27</sub> + 48.42<sub>±00.59</sub> |      9.23<sub>±00.32</sub> |
|      C++/clang++ (Eigen) |    0.197<sub>±0.006</sub> |    3.63<sub>±00.04</sub> + 85.26<sub>±00.00</sub> |      4.69<sub>±00.37</sub> |
|          C++/g++ (Eigen) |    0.201<sub>±0.004</sub> |    3.88<sub>±00.02</sub> + 85.26<sub>±00.00</sub> |      4.16<sub>±00.19</sub> |
|       Julia (threads: 1) |    0.225<sub>±0.007</sub> |  234.62<sub>±00.21</sub> + 48.99<sub>±00.98</sub> |      4.91<sub>±00.34</sub> |
|          Julia (no BLAS) |    1.210<sub>±0.046</sub> |  224.72<sub>±00.09</sub> + 51.37<sub>±00.07</sub> |     35.44<sub>±01.15</sub> |
|                   D/ldc2 |    1.961<sub>±0.007</sub> |    3.71<sub>±00.02</sub> + 70.13<sub>±00.00</sub> |     44.42<sub>±00.76</sub> |
|                    D/dmd |    2.141<sub>±0.050</sub> |    3.89<sub>±00.04</sub> + 70.15<sub>±00.00</sub> |     49.04<sub>±00.78</sub> |
|                    D/gdc |    2.174<sub>±0.035</sub> |    7.20<sub>±00.03</sub> + 70.06<sub>±00.02</sub> |     48.82<sub>±03.08</sub> |
|                    C/gcc |    3.230<sub>±0.016</sub> |    2.07<sub>±00.01</sub> + 68.06<sub>±00.00</sub> |     77.69<sub>±01.40</sub> |
|                 Vala/gcc |    3.284<sub>±0.025</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     79.40<sub>±02.16</sub> |
|               Vala/clang |    3.337<sub>±0.041</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     73.17<sub>±00.96</sub> |
|                  C/clang |    3.338<sub>±0.047</sub> |    2.07<sub>±00.04</sub> + 68.06<sub>±00.00</sub> |     70.98<sub>±01.80</sub> |
|                  Nim/gcc |    3.356<sub>±0.051</sub> |    2.60<sub>±00.05</sub> + 65.87<sub>±00.13</sub> |     79.61<sub>±01.43</sub> |
|                Nim/clang |    3.435<sub>±0.064</sub> |    3.15<sub>±00.03</sub> + 70.77<sub>±00.64</sub> |     72.21<sub>±01.41</sub> |
|                    Swift |    3.437<sub>±0.026</sub> |    7.93<sub>±00.09</sub> + 68.90<sub>±00.02</sub> |     73.14<sub>±01.01</sub> |
|                     Java |    3.437<sub>±0.014</sub> |   38.95<sub>±00.17</sub> + 79.11<sub>±01.97</sub> |     72.78<sub>±01.15</sub> |
|                      Zig |    3.451<sub>±0.014</sub> |    1.71<sub>±00.05</sub> + 68.58<sub>±00.00</sub> |     73.59<sub>±01.25</sub> |
|                     Rust |    3.453<sub>±0.058</sub> |    2.74<sub>±00.02</sub> + 68.32<sub>±00.00</sub> |     74.19<sub>±01.28</sub> |
|                 Go/gccgo |    3.502<sub>±0.042</sub> |   22.79<sub>±00.14</sub> + 73.36<sub>±00.07</sub> |     77.08<sub>±01.15</sub> |
|                    V/gcc |    3.645<sub>±0.058</sub> |    2.00<sub>±00.09</sub> + 68.84<sub>±00.00</sub> |     88.07<sub>±03.01</sub> |
|                       Go |    3.649<sub>±0.120</sub> |    3.51<sub>±00.02</sub> + 73.23<sub>±00.04</sub> |     79.97<sub>±04.09</sub> |
|                    Scala |    3.657<sub>±0.095</sub> |  75.57<sub>±00.17</sub> + 141.45<sub>±00.20</sub> |     82.51<sub>±02.53</sub> |
|                  Crystal |    3.796<sub>±0.202</sub> |    4.22<sub>±00.02</sub> + 59.66<sub>±00.03</sub> |     82.74<sub>±06.59</sub> |
|                  V/clang |    3.840<sub>±0.173</sub> |    2.30<sub>±00.07</sub> + 68.84<sub>±00.00</sub> |     85.83<sub>±06.70</sub> |
|                  Node.js |    4.074<sub>±0.154</sub> |   39.89<sub>±00.15</sub> + 71.28<sub>±00.04</sub> |     81.49<sub>±10.53</sub> |
|                   Kotlin |    4.107<sub>±0.147</sub> |   39.98<sub>±00.25</sub> + 80.26<sub>±00.23</sub> |     80.99<sub>±09.17</sub> |
|              Python/pypy |    6.567<sub>±0.395</sub> |   64.83<sub>±00.10</sub> + 69.15<sub>±00.04</sub> |    135.02<sub>±13.16</sub> |
|             C#/.NET Core |    7.438<sub>±0.278</sub> |   33.21<sub>±00.02</sub> + 69.46<sub>±00.00</sub> |    136.71<sub>±16.03</sub> |
|                  C#/Mono |   11.660<sub>±0.824</sub> |   20.16<sub>±00.08</sub> + 69.03<sub>±00.03</sub> |    233.42<sub>±30.72</sub> |
|         Ruby/truffleruby |   32.418<sub>±1.241</sub> | 685.60<sub>±06.48</sub> + 623.24<sub>±09.06</sub> |    719.39<sub>±49.87</sub> |
| Ruby/truffleruby (--jvm) |   43.371<sub>±1.352</sub> | 665.81<sub>±07.16</sub> + 630.95<sub>±50.72</sub> |   1099.17<sub>±20.94</sub> |
|                     Ruby |  224.493<sub>±2.206</sub> |   15.18<sub>±00.05</sub> + 69.35<sub>±00.00</sub> |  4046.86<sub>±171.10</sub> |
|             Ruby (--jit) |  237.567<sub>±3.308</sub> |   15.22<sub>±00.05</sub> + 69.63<sub>±00.01</sub> |  4595.30<sub>±427.12</sub> |
|                   Python |  258.332<sub>±8.540</sub> |   10.43<sub>±00.06</sub> + 68.58<sub>±00.00</sub> |  4916.76<sub>±455.37</sub> |
|                      Tcl |  373.531<sub>±6.183</sub> |   7.22<sub>±00.02</sub> + 400.38<sub>±00.00</sub> |  7359.29<sub>±526.02</sub> |
|                     Perl |  404.034<sub>±7.123</sub> |   8.98<sub>±00.03</sub> + 599.65<sub>±00.06</sub> |  8722.92<sub>±728.78</sub> |
|               Ruby/jruby | 514.557<sub>±12.889</sub> | 256.15<sub>±02.11</sub> + 705.17<sub>±63.25</sub> | 10976.79<sub>±294.97</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                      Zig | 0.078<sub>±0.001</sub> |    1.41<sub>±00.04</sub> + 53.14<sub>±00.39</sub> |   1.54<sub>±00.06</sub> |
|                     Rust | 0.119<sub>±0.001</sub> |    2.29<sub>±00.06</sub> + 77.14<sub>±00.13</sub> |   2.34<sub>±00.06</sub> |
|              C++/clang++ | 0.180<sub>±0.005</sub> |    3.12<sub>±00.03</sub> + 75.41<sub>±00.00</sub> |   3.67<sub>±00.19</sub> |
|                  Crystal | 0.185<sub>±0.002</sub> |    3.47<sub>±00.05</sub> + 86.26<sub>±02.35</sub> |   2.73<sub>±00.08</sub> |
|                  C++/g++ | 0.194<sub>±0.005</sub> |    3.55<sub>±00.04</sub> + 84.79<sub>±00.01</sub> |   4.00<sub>±00.28</sub> |
|                     Java | 0.220<sub>±0.008</sub> |   38.32<sub>±00.14</sub> + 99.12<sub>±01.11</sub> |   4.07<sub>±00.22</sub> |
|                  Node.js | 0.318<sub>±0.004</sub> |  34.76<sub>±00.02</sub> + 176.20<sub>±00.37</sub> |   5.76<sub>±00.09</sub> |
|               Lua/luajit | 0.421<sub>±0.010</sub> |   2.56<sub>±00.05</sub> + 156.30<sub>±00.83</sub> |   8.40<sub>±00.45</sub> |
|                    Scala | 0.457<sub>±0.037</sub> |  75.93<sub>±00.12</sub> + 241.74<sub>±07.94</sub> |   8.89<sub>±00.62</sub> |
|              Python/pypy | 0.931<sub>±0.012</sub> |  62.93<sub>±00.20</sub> + 251.41<sub>±00.23</sub> |  21.03<sub>±00.15</sub> |
|         Ruby/truffleruby | 1.508<sub>±0.035</sub> | 444.16<sub>±02.22</sub> + 485.67<sub>±03.88</sub> |  33.69<sub>±00.59</sub> |
|                      Lua | 1.787<sub>±0.031</sub> |   2.20<sub>±00.04</sub> + 284.04<sub>±00.35</sub> |  37.55<sub>±01.21</sub> |
| Ruby/truffleruby (--jvm) | 2.374<sub>±0.049</sub> | 577.70<sub>±03.57</sub> + 483.29<sub>±39.55</sub> |  49.17<sub>±01.25</sub> |
|             Ruby (--jit) | 2.690<sub>±0.048</sub> |  14.23<sub>±00.04</sub> + 145.21<sub>±00.64</sub> |  51.82<sub>±01.84</sub> |
|                     Ruby | 2.737<sub>±0.066</sub> |  14.17<sub>±00.05</sub> + 144.22<sub>±00.00</sub> |  46.58<sub>±02.31</sub> |
|               Ruby/jruby | 3.077<sub>±0.283</sub> | 188.30<sub>±01.59</sub> + 463.28<sub>±21.58</sub> |  60.82<sub>±04.50</sub> |
|                   Python | 5.583<sub>±0.148</sub> |   9.96<sub>±00.04</sub> + 235.96<sub>±00.55</sub> | 111.27<sub>±03.65</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 6.0.101                         |
| C#/.NET Core     | 4.0.1-1.21569.2 (52021b4b)      |
| C#/Mono          | 6.12.0.122                      |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.3"                        |
| Crystal          | 1.2.2                           |
| D/dmd            | v2.098.1                        |
| D/gdc            | 11.2.0                          |
| D/ldc2           | 1.28.0                          |
| Elixir           | 1.12.2                          |
| F#/.NET Core     | 12.0.0.0 for F# 6.0             |
| Go               | go1.17.5                        |
| Go/gccgo         | 11.2.0                          |
| Haskell          | 9.0.1                           |
| Java             | 17.0.1                          |
| Julia            | v"1.7.0"                        |
| Kotlin           | 1.6.10                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20210117                        |
| Nim              | 1.6.2                           |
| Node.js          | v17.3.0                         |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.26                          |
| Perl             | v5.32.1                         |
| Python           | 3.9.9                           |
| Python/pypy      | 7.3.7-final0 for Python 3.8.12  |
| Racket           | "8.3"                           |
| Ruby             | 3.0.3p157                       |
| Ruby/jruby       | 9.3.2.0                         |
| Ruby/truffleruby | 21.3.0                          |
| Rust             | 1.57.0                          |
| Scala            | 3.1.0                           |
| Swift            | swift-5.5.2-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.2.4 ed2d128                   |
| Vala             | 0.54.4                          |
| Zig              | 0.9.0                           |
| clang/clang++    | 13.0.0                          |
| gcc/g++          | 11.2.0                          |

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
