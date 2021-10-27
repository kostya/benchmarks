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

UPDATE: 2021-10-29

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
|  Racket (Syntax Objects) |   1.367<sub>±0.021</sub> |   111.52<sub>±00.23</sub> + 0.00<sub>±00.00</sub> |     31.96<sub>±01.66</sub> |
|                  C++/g++ |   1.561<sub>±0.063</sub> |     1.66<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     31.70<sub>±02.61</sub> |
|                     Rust |   1.735<sub>±0.039</sub> |     1.96<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     40.24<sub>±02.18</sub> |
|                   D/ldc2 |   1.780<sub>±0.069</sub> |     3.07<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     36.39<sub>±03.16</sub> |
|                  C/clang |   1.827<sub>±0.041</sub> |     0.63<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     41.53<sub>±00.81</sub> |
|                    D/gdc |   1.839<sub>±0.027</sub> |     6.70<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     42.66<sub>±01.05</sub> |
|                      Zig |   1.861<sub>±0.024</sub> |     1.01<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     41.37<sub>±00.72</sub> |
|                    C/gcc |   1.929<sub>±0.049</sub> |     0.63<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     35.99<sub>±02.36</sub> |
|              C++/clang++ |   1.992<sub>±0.056</sub> |     1.63<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     37.10<sub>±00.95</sub> |
|                  Nim/gcc |   2.019<sub>±0.095</sub> |     1.82<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     43.98<sub>±03.01</sub> |
|                   Kotlin |   2.026<sub>±0.109</sub> |    40.23<sub>±00.53</sub> + 1.32<sub>±00.56</sub> |     35.95<sub>±01.77</sub> |
|                     Java |   2.041<sub>±0.044</sub> |    36.85<sub>±00.18</sub> + 1.58<sub>±00.16</sub> |     45.91<sub>±01.49</sub> |
|                 Go/gccgo |   2.046<sub>±0.107</sub> |    22.23<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     38.40<sub>±03.55</sub> |
|                       Go |   2.064<sub>±0.067</sub> |     3.34<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     38.42<sub>±01.47</sub> |
|                 Vala/gcc |   2.129<sub>±0.029</sub> |     4.20<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     46.43<sub>±01.00</sub> |
|                Nim/clang |   2.181<sub>±0.082</sub> |     2.29<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     49.29<sub>±01.95</sub> |
|                    OCaml |   2.219<sub>±0.104</sub> |     2.61<sub>±00.02</sub> + 2.55<sub>±00.01</sub> |     44.37<sub>±04.54</sub> |
|             F#/.NET Core |   2.219<sub>±0.033</sub> |    36.94<sub>±00.01</sub> + 0.31<sub>±00.00</sub> |     48.44<sub>±00.62</sub> |
|                   Racket |   2.279<sub>±0.038</sub> |    95.53<sub>±00.33</sub> + 0.00<sub>±00.00</sub> |     44.95<sub>±06.21</sub> |
|             C#/.NET Core |   2.320<sub>±0.110</sub> |    34.47<sub>±00.04</sub> + 0.01<sub>±00.00</sub> |     47.14<sub>±04.77</sub> |
|                    V/gcc |   2.435<sub>±0.042</sub> |     0.64<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     55.66<sub>±00.38</sub> |
|                  Crystal |   2.448<sub>±0.111</sub> |     3.33<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     49.11<sub>±04.24</sub> |
|               Vala/clang |   2.523<sub>±0.047</sub> |     4.12<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     45.88<sub>±00.68</sub> |
|                    MLton |   2.639<sub>±0.059</sub> |     1.45<sub>±00.01</sub> + 0.25<sub>±00.00</sub> |     62.65<sub>±02.78</sub> |
|              Chez Scheme |   2.946<sub>±0.126</sub> |    24.81<sub>±00.07</sub> + 4.42<sub>±00.07</sub> |     58.58<sub>±05.35</sub> |
|                  V/clang |   3.104<sub>±0.096</sub> |     0.66<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     70.27<sub>±04.07</sub> |
|                    Julia |   3.135<sub>±0.132</sub> |   200.09<sub>±00.18</sub> + 0.62<sub>±00.05</sub> |     61.50<sub>±06.33</sub> |
|                    Scala |   3.573<sub>±0.061</sub> |  75.12<sub>±00.25</sub> + 151.67<sub>±25.23</sub> |     71.59<sub>±03.15</sub> |
|                    D/dmd |   3.695<sub>±0.087</sub> |     3.49<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     65.45<sub>±01.63</sub> |
|                  C#/Mono |   4.233<sub>±0.194</sub> |    20.01<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     83.25<sub>±08.12</sub> |
|                  Node.js |   4.643<sub>±0.037</sub> |    33.59<sub>±00.03</sub> + 3.48<sub>±00.00</sub> |     77.88<sub>±01.80</sub> |
|         Haskell (MArray) |   4.785<sub>±0.211</sub> |     3.72<sub>±00.05</sub> + 1.11<sub>±00.00</sub> |    100.19<sub>±08.87</sub> |
|                    Swift |   5.583<sub>±0.052</sub> |    13.12<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    124.99<sub>±02.94</sub> |
|               Lua/luajit |   7.501<sub>±0.228</sub> |     2.39<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |    162.99<sub>±05.74</sub> |
|         Ruby/truffleruby |   9.657<sub>±0.545</sub> | 445.46<sub>±01.50</sub> + 557.11<sub>±04.22</sub> |    242.54<sub>±22.15</sub> |
| Ruby/truffleruby (--jvm) |  10.199<sub>±0.252</sub> | 586.60<sub>±03.23</sub> + 619.78<sub>±16.04</sub> |    339.90<sub>±20.43</sub> |
|              Python/pypy |  13.991<sub>±0.086</sub> |   63.50<sub>±00.10</sub> + 46.97<sub>±00.07</sub> |    349.52<sub>±05.04</sub> |
|                  Haskell |  16.044<sub>±0.573</sub> |     3.83<sub>±00.03</sub> + 0.81<sub>±00.00</sub> |    344.33<sub>±29.95</sub> |
|                      Lua |  56.323<sub>±0.979</sub> |     2.24<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |   1208.76<sub>±54.77</sub> |
|             Ruby (--jit) |  59.255<sub>±1.875</sub> |    14.03<sub>±00.02</sub> + 0.23<sub>±00.00</sub> |  1259.13<sub>±100.01</sub> |
|                   Elixir |  59.969<sub>±1.827</sub> |    76.49<sub>±01.32</sub> + 0.11<sub>±00.11</sub> |   1126.02<sub>±79.12</sub> |
|                     Ruby | 100.234<sub>±2.333</sub> |    14.00<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  1918.36<sub>±139.45</sub> |
|               Ruby/jruby | 108.515<sub>±2.453</sub> | 185.95<sub>±01.93</sub> + 143.09<sub>±18.56</sub> |   2243.51<sub>±30.54</sub> |
|                   Python | 234.584<sub>±6.561</sub> |    10.34<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  4765.63<sub>±355.18</sub> |
|                 Tcl (FP) | 279.893<sub>±8.152</sub> |     4.22<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  5532.51<sub>±299.02</sub> |
|                     Perl | 365.708<sub>±2.632</sub> |     6.46<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |   6818.18<sub>±58.85</sub> |
|                Tcl (OOP) | 565.010<sub>±8.616</sub> |     4.23<sub>±00.04</sub> + 0.00<sub>±00.00</sub> | 10590.30<sub>±209.71</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                  C++/g++ |  11.561<sub>±0.257</sub> |     2.53<sub>±00.89</sub> + 1.37<sub>±00.86</sub> |   198.02<sub>±10.79</sub> |
|                    C/gcc |  12.638<sub>±0.191</sub> |     0.63<sub>±00.00</sub> + 1.00<sub>±00.04</sub> |   271.04<sub>±04.12</sub> |
|                     Rust |  13.468<sub>±0.603</sub> |     1.99<sub>±00.07</sub> + 0.31<sub>±00.03</sub> |   260.96<sub>±26.40</sub> |
|                  C/clang |  14.006<sub>±0.558</sub> |     0.69<sub>±00.00</sub> + 0.95<sub>±00.03</sub> |   277.62<sub>±28.40</sub> |
|                   D/ldc2 |  14.538<sub>±0.642</sub> |     3.07<sub>±00.06</sub> + 0.77<sub>±00.00</sub> |   281.89<sub>±29.43</sub> |
|              C++/clang++ |  14.970<sub>±0.181</sub> |     3.39<sub>±00.07</sub> + 0.52<sub>±00.00</sub> |   257.64<sub>±03.78</sub> |
|                    V/gcc |  16.302<sub>±0.336</sub> |     1.70<sub>±00.05</sub> + 1.03<sub>±00.00</sub> |   354.12<sub>±17.95</sub> |
|                    D/gdc |  16.355<sub>±0.228</sub> |     6.66<sub>±00.03</sub> + 0.52<sub>±00.01</sub> |   352.08<sub>±04.88</sub> |
|                   Kotlin |  16.475<sub>±0.357</sub> |    40.54<sub>±00.15</sub> + 1.47<sub>±00.68</sub> |   344.24<sub>±10.88</sub> |
|                      Zig |  16.566<sub>±0.264</sub> |     1.86<sub>±00.03</sub> + 0.52<sub>±00.00</sub> |   369.81<sub>±12.63</sub> |
|                       Go |  16.775<sub>±0.359</sub> |     3.27<sub>±00.04</sub> + 1.26<sub>±00.00</sub> |   362.23<sub>±18.24</sub> |
|  Racket (Syntax Objects) |  17.202<sub>±0.465</sub> |  111.70<sub>±00.16</sub> + 69.09<sub>±00.26</sub> |   321.72<sub>±05.79</sub> |
|                  Crystal |  18.466<sub>±0.693</sub> |     3.35<sub>±00.03</sub> + 0.43<sub>±00.04</sub> |   375.10<sub>±23.81</sub> |
|                  Nim/gcc |  19.017<sub>±0.672</sub> |     1.83<sub>±00.04</sub> + 0.51<sub>±00.00</sub> |   332.29<sub>±13.09</sub> |
|             C#/.NET Core |  19.969<sub>±0.442</sub> |    34.52<sub>±00.05</sub> + 1.06<sub>±00.06</sub> |   355.62<sub>±07.49</sub> |
|               Vala/clang |  20.178<sub>±0.180</sub> |     4.00<sub>±00.06</sub> + 1.96<sub>±00.06</sub> |   453.66<sub>±12.56</sub> |
|                 Vala/gcc |  20.321<sub>±0.647</sub> |     4.02<sub>±00.09</sub> + 1.99<sub>±00.06</sub> |   407.74<sub>±32.77</sub> |
|                Nim/clang |  21.227<sub>±0.693</sub> |     2.28<sub>±00.06</sub> + 0.54<sub>±00.03</sub> |   428.60<sub>±39.25</sub> |
|                     Java |  21.415<sub>±1.823</sub> |    36.94<sub>±00.43</sub> + 2.10<sub>±00.22</sub> |   432.08<sub>±13.84</sub> |
|                    Swift |  22.341<sub>±0.790</sub> |    13.84<sub>±00.16</sub> + 0.00<sub>±00.00</sub> |   456.48<sub>±39.37</sub> |
|                 Go/gccgo |  22.386<sub>±0.531</sub> |    22.33<sub>±00.10</sub> + 1.30<sub>±00.03</sub> |   376.18<sub>±06.41</sub> |
|                    Scala |  25.819<sub>±0.453</sub> |  75.74<sub>±00.48</sub> + 126.67<sub>±00.13</sub> |   528.38<sub>±24.50</sub> |
|                  V/clang |  27.096<sub>±0.151</sub> |     1.71<sub>±00.08</sub> + 1.03<sub>±00.13</sub> |   482.06<sub>±10.44</sub> |
|             F#/.NET Core |  34.720<sub>±1.446</sub> |    36.91<sub>±00.05</sub> + 2.10<sub>±00.02</sub> |   651.90<sub>±38.35</sub> |
|                    OCaml |  37.501<sub>±1.039</sub> |     3.92<sub>±00.03</sub> + 9.00<sub>±01.68</sub> |   757.51<sub>±56.05</sub> |
|              Chez Scheme |  39.244<sub>±0.525</sub> |    25.65<sub>±00.06</sub> + 3.68<sub>±00.03</sub> |   912.03<sub>±17.86</sub> |
|                   Racket |  39.329<sub>±1.385</sub> |    95.78<sub>±00.22</sub> + 0.00<sub>±00.00</sub> |   740.93<sub>±21.01</sub> |
|                    D/dmd |  42.036<sub>±0.418</sub> |     3.51<sub>±00.02</sub> + 0.77<sub>±00.00</sub> |   939.92<sub>±17.87</sub> |
|                  C#/Mono |  45.269<sub>±1.684</sub> |    20.00<sub>±00.03</sub> + 0.87<sub>±00.00</sub> |   954.08<sub>±51.91</sub> |
|               Lua/luajit |  45.359<sub>±0.918</sub> |     2.37<sub>±00.04</sub> + 0.44<sub>±00.00</sub> |   834.79<sub>±30.69</sub> |
|                  Node.js |  50.321<sub>±1.956</sub> |    33.56<sub>±00.02</sub> + 6.51<sub>±00.12</sub> |   913.61<sub>±34.03</sub> |
|                    MLton |  50.858<sub>±0.634</sub> |     1.48<sub>±00.05</sub> + 4.11<sub>±00.00</sub> |  1148.64<sub>±18.38</sub> |
|         Haskell (MArray) |  59.687<sub>±0.301</sub> |     3.70<sub>±00.02</sub> + 2.62<sub>±00.00</sub> |  1383.22<sub>±15.33</sub> |
|              Python/pypy |  65.668<sub>±2.354</sub> |   63.37<sub>±00.05</sub> + 47.76<sub>±00.05</sub> |  1333.51<sub>±81.90</sub> |
|                    Julia |  75.405<sub>±1.755</sub> |   200.84<sub>±00.22</sub> + 0.61<sub>±00.00</sub> | 1569.12<sub>±122.00</sub> |
| Ruby/truffleruby (--jvm) | 120.743<sub>±5.839</sub> | 581.66<sub>±04.97</sub> + 539.81<sub>±13.64</sub> | 2466.87<sub>±108.51</sub> |
|         Ruby/truffleruby | 131.313<sub>±4.944</sub> | 445.97<sub>±01.32</sub> + 574.91<sub>±13.29</sub> | 2693.24<sub>±211.06</sub> |
|                  Haskell | 220.958<sub>±4.270</sub> |    3.81<sub>±00.02</sub> + 26.13<sub>±00.00</sub> | 4785.77<sub>±295.26</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|          C/clang (aklomp) |  0.138<sub>±0.006</sub> |     1.99<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   3.22<sub>±00.06</sub> |
|            C/gcc (aklomp) |  0.143<sub>±0.004</sub> |     2.02<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.30<sub>±00.09</sub> |
|                      Rust |  1.214<sub>±0.035</sub> |     2.53<sub>±00.05</sub> + 0.01<sub>±00.01</sub> |  21.33<sub>±00.76</sub> |
|                     C/gcc |  1.223<sub>±0.039</sub> |     1.95<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  23.75<sub>±01.20</sub> |
|                   Nim/gcc |  1.325<sub>±0.045</sub> |     2.28<sub>±00.03</sub> + 4.12<sub>±00.00</sub> |  25.65<sub>±02.32</sub> |
|                   V/clang |  1.326<sub>±0.054</sub> |     1.82<sub>±00.03</sub> + 0.06<sub>±00.06</sub> |  25.64<sub>±01.98</sub> |
|                     V/gcc |  1.496<sub>±0.037</sub> |     2.19<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  25.95<sub>±00.76</sub> |
|                   C/clang |  1.498<sub>±0.063</sub> |     1.99<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  28.50<sub>±02.14</sub> |
|                 Nim/clang |  1.648<sub>±0.061</sub> |     2.74<sub>±00.07</sub> + 4.38<sub>±00.03</sub> |  32.85<sub>±03.03</sub> |
|                    D/ldc2 |  1.852<sub>±0.040</sub> |     3.51<sub>±00.04</sub> + 3.63<sub>±00.00</sub> |  33.83<sub>±00.82</sub> |
|                   Crystal |  2.066<sub>±0.008</sub> |     3.84<sub>±00.02</sub> + 1.24<sub>±00.06</sub> |  54.16<sub>±00.92</sub> |
|                      Ruby |  2.069<sub>±0.060</sub> |   14.47<sub>±00.04</sub> + 56.72<sub>±01.25</sub> |  40.96<sub>±03.11</sub> |
|              Ruby (--jit) |  2.090<sub>±0.071</sub> |   14.50<sub>±00.02</sub> + 56.97<sub>±00.90</sub> |  42.45<sub>±03.61</sub> |
|                Vala/clang |  2.158<sub>±0.042</sub> |     5.53<sub>±00.06</sub> + 0.50<sub>±00.01</sub> |  44.95<sub>±01.73</sub> |
|                  Vala/gcc |  2.162<sub>±0.057</sub> |     5.48<sub>±00.03</sub> + 0.57<sub>±00.04</sub> |  45.44<sub>±01.42</sub> |
|                      Java |  2.221<sub>±0.058</sub> |  39.00<sub>±00.15</sub> + 241.56<sub>±00.85</sub> |  43.91<sub>±02.02</sub> |
|                    Kotlin |  2.405<sub>±0.043</sub> |  41.57<sub>±00.32</sub> + 241.80<sub>±00.16</sub> |  47.72<sub>±01.82</sub> |
|                     Scala |  2.428<sub>±0.052</sub> |  75.08<sub>±00.34</sub> + 223.54<sub>±00.27</sub> |  46.71<sub>±01.10</sub> |
|                        Go |  2.595<sub>±0.009</sub> |     4.47<sub>±00.02</sub> + 5.39<sub>±00.14</sub> |  51.93<sub>±00.66</sub> |
|   C++/clang++ (libcrypto) |  2.736<sub>±0.063</sub> |     5.65<sub>±00.03</sub> + 0.08<sub>±00.00</sub> |  58.36<sub>±01.49</sub> |
|       C++/g++ (libcrypto) |  2.737<sub>±0.088</sub> |     5.62<sub>±00.03</sub> + 0.08<sub>±00.01</sub> |  58.77<sub>±03.27</sub> |
|                       PHP |  2.748<sub>±0.035</sub> |    15.80<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  58.78<sub>±00.63</sub> |
|                   Node.js |  3.072<sub>±0.041</sub> |   34.24<sub>±00.03</sub> + 36.38<sub>±00.05</sub> |  61.58<sub>±01.89</sub> |
|                  Go/gccgo |  3.077<sub>±0.007</sub> |    23.28<sub>±00.23</sub> + 8.87<sub>±00.35</sub> |  67.33<sub>±01.20</sub> |
|       Perl (MIME::Base64) |  3.151<sub>±0.037</sub> |    14.08<sub>±00.04</sub> + 0.04<sub>±00.03</sub> |  52.92<sub>±00.63</sub> |
|                     D/gdc |  3.421<sub>±0.132</sub> |     7.02<sub>±00.03</sub> + 3.41<sub>±00.02</sub> |  63.34<sub>±05.83</sub> |
|                     D/dmd |  3.836<sub>±0.135</sub> |     4.00<sub>±00.07</sub> + 3.62<sub>±00.00</sub> |  66.05<sub>±02.78</sub> |
|                    Python |  3.957<sub>±0.061</sub> |    10.11<sub>±00.02</sub> + 0.18<sub>±00.00</sub> |  85.37<sub>±03.11</sub> |
|                       Zig |  4.496<sub>±0.065</sub> |     1.89<sub>±00.02</sub> + 0.37<sub>±00.00</sub> |  78.19<sub>±02.97</sub> |
|                       Tcl |  5.014<sub>±0.136</sub> |     4.84<sub>±00.05</sub> + 0.19<sub>±00.00</sub> |  85.69<sub>±02.14</sub> |
|               Python/pypy |  5.181<sub>±0.106</sub> |   63.18<sub>±00.06</sub> + 47.58<sub>±00.07</sub> |  89.58<sub>±05.14</sub> |
|              F#/.NET Core |  5.851<sub>±0.057</sub> |   37.26<sub>±00.06</sub> + 33.61<sub>±06.18</sub> | 107.97<sub>±01.45</sub> |
|              C#/.NET Core |  5.929<sub>±0.018</sub> |   34.75<sub>±00.06</sub> + 33.53<sub>±06.20</sub> | 108.73<sub>±01.08</sub> |
|                     Julia |  5.959<sub>±0.097</sub> |  219.92<sub>±00.10</sub> + 63.12<sub>±00.07</sub> | 127.82<sub>±08.80</sub> |
|  Ruby/truffleruby (--jvm) |  6.169<sub>±0.108</sub> | 578.96<sub>±02.64</sub> + 263.18<sub>±30.25</sub> | 135.51<sub>±03.74</sub> |
|                   C#/Mono |  7.695<sub>±0.199</sub> |   20.70<sub>±00.05</sub> + 18.46<sub>±00.07</sub> | 151.76<sub>±10.94</sub> |
|                Ruby/jruby | 12.535<sub>±0.199</sub> | 185.46<sub>±01.81</sub> + 131.82<sub>±03.34</sub> | 259.51<sub>±13.05</sub> |
| Perl (MIME::Base64::Perl) | 18.103<sub>±0.519</sub> |    15.48<sub>±00.04</sub> + 0.20<sub>±00.05</sub> | 337.14<sub>±19.28</sub> |
|          Ruby/truffleruby | 20.236<sub>±0.501</sub> | 442.51<sub>±01.45</sub> + 320.98<sub>±00.80</sub> | 419.55<sub>±08.23</sub> |

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
| C++/clang++ (DAW JSON Link NoCh |  0.082<sub>±0.004</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   1.93<sub>±00.23</sub> |
|    C++/g++ (simdjson On-Demand) |  0.088<sub>±0.002</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   1.95<sub>±00.12</sub> |
| C++/clang++ (simdjson On-Demand |  0.094<sub>±0.001</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   1.87<sub>±00.06</sub> |
| C++/g++ (DAW JSON Link NoCheck) |  0.097<sub>±0.001</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   1.94<sub>±00.06</sub> |
|     C++/clang++ (DAW JSON Link) |  0.104<sub>±0.001</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   2.78<sub>±00.10</sub> |
|         C++/g++ (DAW JSON Link) |  0.106<sub>±0.001</sub> |    113.20<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   2.75<sub>±00.10</sub> |
|             Rust (Serde Custom) |  0.146<sub>±0.007</sub> |    111.85<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |   2.96<sub>±00.38</sub> |
|              Rust (Serde Typed) |  0.151<sub>±0.008</sub> |   111.85<sub>±00.04</sub> + 11.54<sub>±00.26</sub> |   3.06<sub>±00.36</sub> |
|          C++/g++ (simdjson DOM) |  0.158<sub>±0.004</sub> |  113.36<sub>±00.04</sub> + 176.60<sub>±00.00</sub> |   3.60<sub>±00.10</sub> |
|      C++/clang++ (simdjson DOM) |  0.160<sub>±0.004</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   3.54<sub>±00.22</sub> |
|                 C++/g++ (gason) |  0.164<sub>±0.002</sub> |   113.04<sub>±00.04</sub> + 96.80<sub>±00.06</sub> |   4.19<sub>±00.10</sub> |
|             C++/clang++ (gason) |  0.181<sub>±0.002</sub> |   113.11<sub>±00.05</sub> + 96.80<sub>±00.06</sub> |   4.82<sub>±00.10</sub> |
|             C++/g++ (RapidJSON) |  0.224<sub>±0.002</sub> |  113.14<sub>±00.05</sub> + 128.82<sub>±00.06</sub> |   4.26<sub>±00.06</sub> |
|           D/ldc2 (Mir Asdf DOM) |  0.226<sub>±0.006</sub> |   112.78<sub>±00.05</sub> + 61.36<sub>±00.01</sub> |   4.96<sub>±00.36</sub> |
|   D/ldc2 (Mir Amazon's Ion DOM) |  0.231<sub>±0.006</sub> |   112.74<sub>±00.04</sub> + 16.25<sub>±00.00</sub> |   5.20<sub>±00.38</sub> |
|         C++/clang++ (RapidJSON) |  0.269<sub>±0.004</sub> |  113.17<sub>±00.03</sub> + 128.82<sub>±00.06</sub> |   5.05<sub>±00.17</sub> |
|     C++/g++ (RapidJSON Precise) |  0.279<sub>±0.005</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   6.53<sub>±00.38</sub> |
| C++/clang++ (RapidJSON Precise) |  0.387<sub>±0.018</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   8.05<sub>±00.92</sub> |
|            C++/g++ (Boost.JSON) |  0.515<sub>±0.017</sub> |  113.21<sub>±00.05</sub> + 435.79<sub>±00.03</sub> |  10.49<sub>±00.64</sub> |
|        C++/clang++ (Boost.JSON) |  0.540<sub>±0.018</sub> |  113.21<sub>±00.04</sub> + 435.82<sub>±00.06</sub> |  12.07<sub>±01.36</sub> |
|     C++/clang++ (RapidJSON SAX) |  0.568<sub>±0.021</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  11.58<sub>±01.23</sub> |
|         C++/g++ (RapidJSON SAX) |  0.581<sub>±0.013</sub> |    112.94<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  10.30<sub>±00.30</sub> |
| C++/g++ (RapidJSON SAX Precise) |  0.606<sub>±0.006</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  14.00<sub>±00.54</sub> |
|                   Go (jsoniter) |  0.664<sub>±0.018</sub> |    231.16<sub>±00.07</sub> + 1.56<sub>±00.29</sub> |  13.45<sub>±00.69</sub> |
| C++/clang++ (RapidJSON SAX Prec |  0.772<sub>±0.015</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  13.58<sub>±00.48</sub> |
|                 Java (DSL-JSON) |  0.773<sub>±0.012</sub> |  263.56<sub>±00.19</sub> + 225.46<sub>±10.44</sub> |  20.15<sub>±01.22</sub> |
|            Rust (Serde Untyped) |  0.791<sub>±0.006</sub> |  111.90<sub>±00.11</sub> + 839.98<sub>±00.00</sub> |  18.49<sub>±00.42</sub> |
|                     Python/pypy |  0.793<sub>±0.006</sub> |  283.38<sub>±00.03</sub> + 123.29<sub>±00.00</sub> |  19.15<sub>±00.50</sub> |
|                         Node.js |  0.849<sub>±0.008</sub> |   253.44<sub>±00.02</sub> + 79.43<sub>±00.41</sub> |  22.33<sub>±00.56</sub> |
|                           V/gcc |  0.857<sub>±0.039</sub> |  111.18<sub>±00.02</sub> + 496.21<sub>±00.03</sub> |  18.45<sub>±01.93</sub> |
|                         V/clang |  0.857<sub>±0.037</sub> |  111.20<sub>±00.02</sub> + 496.21<sub>±00.00</sub> |  18.26<sub>±01.80</sub> |
|                   Julia (JSON3) |  0.899<sub>±0.009</sub> |  381.33<sub>±00.84</sub> + 369.86<sub>±00.60</sub> |  21.70<sub>±01.08</sub> |
|                  Crystal (Pull) |  0.947<sub>±0.025</sub> |   113.79<sub>±00.02</sub> + 18.24<sub>±00.02</sub> |  19.54<sub>±01.04</sub> |
|                             Zig |  0.953<sub>±0.009</sub> |   111.18<sub>±00.03</sub> + 12.18<sub>±00.00</sub> |  22.83<sub>±00.59</sub> |
| C#/.NET Core (System.Text.Json) |  1.001<sub>±0.011</sub> |  479.24<sub>±00.04</sub> + 139.04<sub>±00.00</sub> |  24.33<sub>±00.62</sub> |
|         Perl (Cpanel::JSON::XS) |  1.011<sub>±0.038</sub> |  124.66<sub>±00.06</sub> + 402.75<sub>±00.03</sub> |  23.72<sub>±00.45</sub> |
|                Crystal (Schema) |  1.034<sub>±0.007</sub> |   113.81<sub>±00.04</sub> + 47.37<sub>±00.02</sub> |  16.49<sub>±00.24</sub> |
|                              Go |  1.081<sub>±0.019</sub> |   117.16<sub>±00.06</sub> + 83.09<sub>±00.14</sub> |  19.55<sub>±00.61</sub> |
|          Nim/clang (Packedjson) |  1.109<sub>±0.027</sub> |  112.48<sub>±00.14</sub> + 294.04<sub>±00.13</sub> |  26.51<sub>±01.10</sub> |
|                             PHP |  1.238<sub>±0.012</sub> |  125.00<sub>±00.19</sub> + 682.01<sub>±00.00</sub> |  26.76<sub>±00.45</sub> |
|                         Crystal |  1.255<sub>±0.059</sub> |  113.80<sub>±00.01</sub> + 399.04<sub>±00.01</sub> |  25.29<sub>±02.32</sub> |
|            Nim/gcc (Packedjson) |  1.284<sub>±0.064</sub> |  112.12<sub>±00.05</sub> + 293.91<sub>±00.00</sub> |  27.48<sub>±02.49</sub> |
|                C++/g++ (json-c) |  1.500<sub>±0.010</sub> | 113.26<sub>±00.11</sub> + 1215.99<sub>±00.09</sub> |  37.11<sub>±00.18</sub> |
|            C++/clang++ (json-c) |  1.573<sub>±0.069</sub> | 113.20<sub>±00.10</sub> + 1215.96<sub>±00.06</sub> |  33.92<sub>±03.38</sub> |
|                         Clojure |  1.583<sub>±0.029</sub> |  453.73<sub>±04.32</sub> + 506.41<sub>±07.00</sub> |  44.71<sub>±00.52</sub> |
|                       Nim/clang |  1.656<sub>±0.072</sub> |  112.37<sub>±00.05</sub> + 925.03<sub>±00.06</sub> |  35.60<sub>±03.51</sub> |
|                        Go/gccgo |  1.674<sub>±0.031</sub> |   137.51<sub>±00.09</sub> + 83.45<sub>±00.14</sub> |  30.36<sub>±00.83</sub> |
|                    C#/.NET Core |  1.694<sub>±0.045</sub> |  488.52<sub>±00.18</sub> + 293.96<sub>±00.00</sub> |  37.22<sub>±04.32</sub> |
|                         Nim/gcc |  1.714<sub>±0.034</sub> |  111.90<sub>±00.08</sub> + 919.62<sub>±00.06</sub> |  38.51<sub>±01.83</sub> |
|             CPython (UltraJSON) |  1.770<sub>±0.036</sub> |  121.70<sub>±00.03</sub> + 549.18<sub>±01.21</sub> |  40.13<sub>±00.72</sub> |
|                          Python |  1.841<sub>±0.024</sub> |  120.09<sub>±00.02</sub> + 377.21<sub>±00.00</sub> |  38.50<sub>±01.67</sub> |
|              C++/g++ (Nlohmann) |  1.864<sub>±0.025</sub> |  113.19<sub>±00.03</sub> + 447.91<sub>±00.03</sub> |  34.90<sub>±01.41</sub> |
|          C++/clang++ (Nlohmann) |  2.049<sub>±0.060</sub> |  113.09<sub>±00.16</sub> + 447.94<sub>±00.06</sub> |  47.98<sub>±02.62</sub> |
|                         C#/Mono |  2.299<sub>±0.013</sub> |    476.38<sub>±00.10</sub> + 0.00<sub>±00.00</sub> |  41.26<sub>±00.38</sub> |
|                     Ruby (YAJL) |  2.304<sub>±0.047</sub> |  123.91<sub>±00.02</sub> + 283.34<sub>±00.00</sub> |  54.65<sub>±00.52</sub> |
|                            Ruby |  2.406<sub>±0.032</sub> |  123.99<sub>±00.03</sub> + 410.71<sub>±00.02</sub> |  56.49<sub>±00.67</sub> |
|                 Scala (uPickle) |  2.523<sub>±0.111</sub> |  304.10<sub>±00.25</sub> + 690.92<sub>±53.38</sub> |  55.24<sub>±02.68</sub> |
|                    Ruby (--jit) |  2.597<sub>±0.028</sub> |  124.00<sub>±00.03</sub> + 410.85<sub>±00.01</sub> |  50.76<sub>±01.05</sub> |
|                          D/ldc2 |  2.605<sub>±0.046</sub> |  113.02<sub>±00.08</sub> + 680.14<sub>±00.02</sub> |  51.33<sub>±02.05</sub> |
| F#/.NET Core (System.Text.Json) |  2.647<sub>±0.035</sub> |  485.48<sub>±00.06</sub> + 455.40<sub>±02.70</sub> |  47.38<sub>±00.44</sub> |
|                           D/gdc |  2.954<sub>±0.040</sub> |  116.83<sub>±00.02</sub> + 600.59<sub>±00.00</sub> |  68.72<sub>±00.70</sub> |
|                       Rust (jq) |  3.683<sub>±0.146</sub> |  113.80<sub>±00.03</sub> + 778.63<sub>±00.77</sub> |  72.59<sub>±07.14</sub> |
|                         Haskell |  3.741<sub>±0.030</sub> |  115.81<sub>±00.04</sub> + 715.16<sub>±00.12</sub> |  71.95<sub>±00.46</sub> |
|    C++/g++ (Boost.PropertyTree) |  3.925<sub>±0.087</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  95.42<sub>±01.65</sub> |
|                      Ruby/jruby |  4.031<sub>±0.122</sub> | 454.49<sub>±01.35</sub> + 1057.26<sub>±47.95</sub> |  98.09<sub>±04.16</sub> |
| C++/clang++ (Boost.PropertyTree |  4.129<sub>±0.036</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |  99.31<sub>±02.29</sub> |
|                      Vala/clang |  4.842<sub>±0.043</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 115.87<sub>±01.41</sub> |
|                           D/dmd |  5.149<sub>±0.109</sub> |  113.70<sub>±00.04</sub> + 680.12<sub>±00.03</sub> | 100.42<sub>±03.82</sub> |
|                        Vala/gcc |  5.622<sub>±0.125</sub> |      0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> | 114.93<sub>±11.38</sub> |
|               Perl (JSON::Tiny) | 11.569<sub>±0.109</sub> |  125.21<sub>±00.17</sub> + 528.75<sub>±00.12</sub> | 278.19<sub>±03.01</sub> |
|        Ruby/truffleruby (--jvm) | 16.371<sub>±0.077</sub> | 782.39<sub>±13.10</sub> + 1618.32<sub>±65.18</sub> | 519.73<sub>±02.85</sub> |
|                Ruby/truffleruby | 22.785<sub>±0.967</sub> | 782.26<sub>±02.71</sub> + 1935.10<sub>±55.69</sub> | 533.64<sub>±34.13</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                  Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |   0.077<sub>±0.001</sub> |    6.99<sub>±00.06</sub> + 56.20<sub>±00.16</sub> |      4.35<sub>±00.12</sub> |
|           Python (NumPy) |   0.102<sub>±0.003</sub> |   27.86<sub>±00.09</sub> + 57.56<sub>±00.02</sub> |      5.88<sub>±00.24</sub> |
|              Java (ND4J) |   0.102<sub>±0.004</sub> |  145.50<sub>±00.65</sub> + 90.75<sub>±00.04</sub> |      5.09<sub>±00.15</sub> |
|  Nim/clang (Arraymancer) |   0.107<sub>±0.013</sub> |    6.42<sub>±00.05</sub> + 55.80<sub>±00.23</sub> |      5.47<sub>±00.74</sub> |
|    Nim/gcc (Arraymancer) |   0.140<sub>±0.014</sub> |    5.58<sub>±00.06</sub> + 55.63<sub>±00.30</sub> |      7.48<sub>±00.76</sub> |
|      C++/clang++ (Eigen) |   0.187<sub>±0.001</sub> |    3.88<sub>±00.04</sub> + 85.25<sub>±00.00</sub> |      3.75<sub>±00.05</sub> |
|          C++/g++ (Eigen) |   0.192<sub>±0.002</sub> |    3.87<sub>±00.04</sub> + 85.25<sub>±00.00</sub> |      3.90<sub>±00.08</sub> |
|       Julia (threads: 8) |   0.227<sub>±0.002</sub> |  239.76<sub>±00.56</sub> + 37.61<sub>±00.24</sub> |     11.96<sub>±00.21</sub> |
|       Julia (threads: 1) |   0.572<sub>±0.028</sub> |  239.99<sub>±00.21</sub> + 37.76<sub>±00.09</sub> |     12.56<sub>±01.37</sub> |
|          Julia (no BLAS) |   1.253<sub>±0.009</sub> |  218.35<sub>±00.13</sub> + 51.61<sub>±00.02</sub> |     31.79<sub>±01.39</sub> |
|                   D/ldc2 |   1.959<sub>±0.009</sub> |    3.65<sub>±00.04</sub> + 70.13<sub>±00.00</sub> |     44.91<sub>±00.90</sub> |
|                    D/gdc |   2.100<sub>±0.034</sub> |    7.08<sub>±00.04</sub> + 70.02<sub>±00.01</sub> |     50.67<sub>±02.22</sub> |
|                    D/dmd |   2.138<sub>±0.032</sub> |    3.82<sub>±00.10</sub> + 70.15<sub>±00.01</sub> |     47.70<sub>±00.63</sub> |
|                    C/gcc |   3.257<sub>±0.018</sub> |    2.04<sub>±00.02</sub> + 68.06<sub>±00.00</sub> |     76.57<sub>±00.63</sub> |
|                 Vala/gcc |   3.353<sub>±0.025</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     78.61<sub>±00.54</sub> |
|                  Nim/gcc |   3.363<sub>±0.030</sub> |    2.62<sub>±00.04</sub> + 65.74<sub>±00.00</sub> |     78.76<sub>±01.61</sub> |
|               Vala/clang |   3.391<sub>±0.021</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     73.15<sub>±01.59</sub> |
|                  C/clang |   3.395<sub>±0.015</sub> |    2.03<sub>±00.02</sub> + 68.06<sub>±00.00</sub> |     71.73<sub>±01.44</sub> |
|                     Rust |   3.406<sub>±0.015</sub> |    2.63<sub>±00.11</sub> + 68.32<sub>±00.00</sub> |     72.84<sub>±01.02</sub> |
|                      Zig |   3.416<sub>±0.022</sub> |    2.14<sub>±00.01</sub> + 68.58<sub>±00.00</sub> |     72.97<sub>±01.17</sub> |
|                    Swift |   3.425<sub>±0.018</sub> |    7.74<sub>±00.04</sub> + 68.96<sub>±00.01</sub> |     72.56<sub>±00.36</sub> |
|                     Java |   3.430<sub>±0.024</sub> |   38.93<sub>±00.08</sub> + 77.12<sub>±00.17</sub> |     73.65<sub>±01.31</sub> |
|                Nim/clang |   3.462<sub>±0.029</sub> |    3.06<sub>±00.02</sub> + 66.26<sub>±00.00</sub> |     75.28<sub>±00.53</sub> |
|                 Go/gccgo |   3.545<sub>±0.036</sub> |   22.75<sub>±00.10</sub> + 73.53<sub>±00.16</sub> |     76.82<sub>±00.71</sub> |
|                       Go |   3.552<sub>±0.038</sub> |    3.60<sub>±00.04</sub> + 73.23<sub>±00.20</sub> |     82.81<sub>±01.32</sub> |
|                    Scala |   3.554<sub>±0.032</sub> |  75.55<sub>±00.46</sub> + 141.56<sub>±00.24</sub> |     83.25<sub>±00.61</sub> |
|                    V/gcc |   3.721<sub>±0.139</sub> |    1.97<sub>±00.06</sub> + 68.84<sub>±00.00</sub> |     83.13<sub>±07.86</sub> |
|                  V/clang |   3.838<sub>±0.106</sub> |    2.38<sub>±00.15</sub> + 68.71<sub>±00.13</sub> |     78.54<sub>±06.42</sub> |
|                  Crystal |   3.857<sub>±0.022</sub> |    4.21<sub>±00.02</sub> + 59.74<sub>±00.01</sub> |     78.29<sub>±01.78</sub> |
|                  Node.js |   4.013<sub>±0.062</sub> |   37.90<sub>±00.06</sub> + 71.76<sub>±00.02</sub> |     74.39<sub>±01.63</sub> |
|                   Kotlin |   4.069<sub>±0.058</sub> |   39.69<sub>±00.04</sub> + 80.42<sub>±00.31</sub> |     73.95<sub>±01.96</sub> |
|              Python/pypy |   6.519<sub>±0.056</sub> |   64.68<sub>±00.10</sub> + 69.15<sub>±00.04</sub> |    121.39<sub>±06.90</sub> |
|             C#/.NET Core |   6.869<sub>±0.111</sub> |   34.07<sub>±00.07</sub> + 69.18<sub>±00.00</sub> |    156.48<sub>±03.37</sub> |
|                  C#/Mono |  11.581<sub>±0.099</sub> |   20.21<sub>±00.04</sub> + 69.02<sub>±00.01</sub> |    199.28<sub>±02.02</sub> |
|         Ruby/truffleruby |  32.387<sub>±0.687</sub> | 688.24<sub>±01.27</sub> + 623.19<sub>±04.11</sub> |    686.65<sub>±15.77</sub> |
| Ruby/truffleruby (--jvm) |  42.831<sub>±0.879</sub> | 664.54<sub>±04.51</sub> + 594.68<sub>±09.62</sub> |   1072.60<sub>±08.74</sub> |
|                     Ruby | 220.443<sub>±9.912</sub> |   15.21<sub>±00.05</sub> + 68.64<sub>±00.00</sub> |  4474.96<sub>±226.36</sub> |
|             Ruby (--jit) | 226.957<sub>±2.259</sub> |   15.23<sub>±00.03</sub> + 68.89<sub>±00.00</sub> |  4979.78<sub>±158.80</sub> |
|                   Python | 245.013<sub>±6.027</sub> |   10.54<sub>±00.04</sub> + 68.58<sub>±00.00</sub> |  4514.15<sub>±193.21</sub> |
|                      Tcl | 344.324<sub>±5.976</sub> |   7.16<sub>±00.03</sub> + 400.44<sub>±00.03</sub> |  7117.51<sub>±516.35</sub> |
|                     Perl | 407.359<sub>±2.716</sub> |   8.97<sub>±00.07</sub> + 599.66<sub>±00.05</sub> |   8493.58<sub>±94.37</sub> |
|               Ruby/jruby | 510.916<sub>±7.856</sub> | 255.46<sub>±04.21</sub> + 743.22<sub>±90.55</sub> | 11145.56<sub>±191.17</sub> |

## Primes

Testing:

 - generating primes using the optimized [sieve of Atkin](https://www.geeksforgeeks.org/sieve-of-atkin/);
 - prefix search for their decimal numbers using Trie data structure.

[Primes](primes)

|                 Language |                Time, s |                                       Memory, MiB |               Energy, J |
| :----------------------- | ---------------------: | ------------------------------------------------: | ----------------------: |
|                  C++/g++ | 0.182<sub>±0.002</sub> |    3.46<sub>±00.04</sub> + 84.79<sub>±00.00</sub> |   3.57<sub>±00.09</sub> |
|              C++/clang++ | 0.195<sub>±0.002</sub> |    3.45<sub>±00.05</sub> + 84.80<sub>±00.00</sub> |   3.76<sub>±00.12</sub> |
|                     Java | 0.212<sub>±0.003</sub> |  38.16<sub>±00.23</sub> + 100.12<sub>±01.94</sub> |   4.06<sub>±00.09</sub> |
|                  Node.js | 0.318<sub>±0.002</sub> |  34.64<sub>±00.04</sub> + 176.54<sub>±01.14</sub> |   5.61<sub>±00.07</sub> |
|                    Scala | 0.456<sub>±0.022</sub> |  75.93<sub>±00.18</sub> + 195.62<sub>±51.29</sub> |   9.15<sub>±00.40</sub> |
|               Lua/luajit | 0.603<sub>±0.011</sub> |   2.46<sub>±00.04</sub> + 165.41<sub>±00.39</sub> |  12.42<sub>±00.32</sub> |
|              Python/pypy | 0.961<sub>±0.022</sub> |  63.14<sub>±00.02</sub> + 251.30<sub>±00.12</sub> |  21.02<sub>±00.17</sub> |
|         Ruby/truffleruby | 1.555<sub>±0.009</sub> | 444.96<sub>±01.80</sub> + 484.02<sub>±05.14</sub> |  32.94<sub>±00.67</sub> |
|                      Lua | 2.028<sub>±0.044</sub> |   2.21<sub>±00.04</sub> + 373.61<sub>±00.06</sub> |  38.61<sub>±02.56</sub> |
| Ruby/truffleruby (--jvm) | 2.372<sub>±0.061</sub> | 577.30<sub>±02.86</sub> + 496.29<sub>±46.33</sub> |  49.55<sub>±01.18</sub> |
|             Ruby (--jit) | 2.575<sub>±0.010</sub> |  14.11<sub>±00.04</sub> + 150.19<sub>±00.02</sub> |  54.68<sub>±00.98</sub> |
|                     Ruby | 2.696<sub>±0.074</sub> |  14.07<sub>±00.05</sub> + 149.89<sub>±00.04</sub> |  46.06<sub>±02.25</sub> |
|               Ruby/jruby | 3.111<sub>±0.210</sub> | 187.63<sub>±01.50</sub> + 459.96<sub>±27.29</sub> |  60.96<sub>±05.23</sub> |
|                   Python | 5.714<sub>±0.175</sub> |  10.08<sub>±00.02</sub> + 236.86<sub>±00.62</sub> | 107.01<sub>±08.77</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bookworm/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.402                         |
| C#/.NET Core     | 3.11.0-4.21451.6 (0b1a27fd)     |
| C#/Mono          | 6.12.0.122                      |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.3"                        |
| Crystal          | 1.2.1                           |
| D/dmd            | v2.098.0                        |
| D/gdc            | 11.2.0                          |
| D/ldc2           | 1.28.0                          |
| Elixir           | 1.12.2                          |
| F#/.NET Core     | 11.4.2.0 for F# 5.0             |
| Go               | go1.17.2                        |
| Go/gccgo         | 11.2.0                          |
| Haskell          | 9.0.1                           |
| Java             | 17.0.1                          |
| Julia            | v"1.6.3"                        |
| Kotlin           | 1.5.31                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20210117                        |
| Nim              | 1.6.0                           |
| Node.js          | v17.0.1                         |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.21                          |
| Perl             | v5.32.1                         |
| Python           | 3.9.7                           |
| Python/pypy      | 7.3.7-final0 for Python 3.8.12  |
| Racket           | "8.2"                           |
| Ruby             | 3.0.2p107                       |
| Ruby/jruby       | 9.3.1.0                         |
| Ruby/truffleruby | 21.3.0                          |
| Rust             | 1.56.0                          |
| Scala            | 3.1.0                           |
| Swift            | swift-5.5.1-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.2.4 b72a2de                   |
| Vala             | 0.54.2                          |
| Zig              | 0.8.1                           |
| clang/clang++    | 11.1.0                          |
| gcc/g++          | 11.2.0                          |

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
