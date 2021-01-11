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

UPDATE: 2021-01-11

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
|                  C++/g++ |    0.887<sub>±0.042</sub> |     1.49<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     17.29<sub>±01.73</sub> |
|                  Nim/gcc |    1.786<sub>±0.034</sub> |     1.86<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     39.74<sub>±01.19</sub> |
|                   D/ldc2 |    1.854<sub>±0.038</sub> |     3.01<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |     33.68<sub>±01.80</sub> |
|                   Kotlin |    1.865<sub>±0.034</sub> |    38.20<sub>±00.29</sub> + 1.98<sub>±00.26</sub> |     32.87<sub>±00.35</sub> |
|                    C/gcc |    1.907<sub>±0.065</sub> |     0.50<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     35.03<sub>±00.84</sub> |
|                Nim/clang |    1.945<sub>±0.045</sub> |     2.32<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     45.73<sub>±01.24</sub> |
|                    D/gdc |    1.995<sub>±0.061</sub> |     6.33<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     36.50<sub>±01.89</sub> |
|                     Rust |    2.119<sub>±0.047</sub> |     1.98<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     38.02<sub>±00.97</sub> |
|                    OCaml |    2.130<sub>±0.094</sub> |     2.57<sub>±00.03</sub> + 2.50<sub>±00.05</sub> |     49.06<sub>±04.11</sub> |
|                  C/clang |    2.260<sub>±0.051</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     43.96<sub>±02.89</sub> |
|             C#/.NET Core |    2.311<sub>±0.085</sub> |    34.37<sub>±00.06</sub> + 0.01<sub>±00.00</sub> |     48.42<sub>±04.26</sub> |
|                       Go |    2.329<sub>±0.092</sub> |     3.51<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     48.09<sub>±03.76</sub> |
|                 Vala/gcc |    2.345<sub>±0.095</sub> |     3.73<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     46.38<sub>±02.41</sub> |
|                  Crystal |    2.388<sub>±0.096</sub> |     3.35<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     53.85<sub>±00.83</sub> |
|               Vala/clang |    2.408<sub>±0.079</sub> |     3.75<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     54.26<sub>±01.53</sub> |
|                    V/gcc |    2.439<sub>±0.086</sub> |     0.52<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     52.32<sub>±02.56</sub> |
|                     Java |    2.502<sub>±0.144</sub> |    37.73<sub>±00.15</sub> + 0.91<sub>±00.18</sub> |     47.92<sub>±02.98</sub> |
|                 Go/gccgo |    2.586<sub>±0.238</sub> |    21.28<sub>±00.19</sub> + 0.00<sub>±00.00</sub> |     53.03<sub>±07.57</sub> |
|                  V/clang |    2.752<sub>±0.106</sub> |     0.87<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     58.64<sub>±04.35</sub> |
|                    Julia |    2.942<sub>±0.135</sub> |   169.08<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     55.03<sub>±06.16</sub> |
|              Chez Scheme |    2.950<sub>±0.121</sub> |    24.91<sub>±00.02</sub> + 4.20<sub>±00.03</sub> |     61.04<sub>±06.15</sub> |
|                    MLton |    2.980<sub>±0.131</sub> |     1.44<sub>±00.02</sub> + 0.25<sub>±00.00</sub> |     63.08<sub>±06.69</sub> |
|                    Scala |    3.503<sub>±0.029</sub> |   80.33<sub>±00.45</sub> + 58.71<sub>±03.87</sub> |     69.07<sub>±02.62</sub> |
|                    D/dmd |    3.636<sub>±0.065</sub> |     3.52<sub>±00.07</sub> + 0.00<sub>±00.00</sub> |     64.71<sub>±03.91</sub> |
|                  C#/Mono |    4.053<sub>±0.021</sub> |    20.24<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     91.30<sub>±03.42</sub> |
|                  Node.js |    4.200<sub>±0.194</sub> |    30.67<sub>±00.09</sub> + 3.62<sub>±00.01</sub> |     74.58<sub>±03.73</sub> |
|         Haskell (MArray) |    4.672<sub>±0.068</sub> |     3.69<sub>±00.07</sub> + 1.16<sub>±00.00</sub> |     84.51<sub>±01.79</sub> |
|             F#/.NET Core |    5.630<sub>±0.033</sub> |   36.51<sub>±00.05</sub> + 90.36<sub>±00.13</sub> |    130.47<sub>±01.40</sub> |
|               Lua/luajit |    6.762<sub>±0.158</sub> |     2.89<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |    114.01<sub>±03.57</sub> |
|                   Racket |    9.654<sub>±0.103</sub> |   101.03<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |    215.06<sub>±03.58</sub> |
| Ruby/truffleruby (--jvm) |   10.676<sub>±0.709</sub> | 553.73<sub>±07.23</sub> + 425.15<sub>±55.94</sub> |    337.56<sub>±09.05</sub> |
|              Python/pypy |   15.303<sub>±0.588</sub> |   63.97<sub>±00.09</sub> + 45.37<sub>±00.00</sub> |    339.70<sub>±27.42</sub> |
|                  Haskell |   16.627<sub>±0.594</sub> |     3.82<sub>±00.04</sub> + 0.88<sub>±00.00</sub> |    317.75<sub>±16.26</sub> |
|         Ruby/truffleruby |   21.569<sub>±1.239</sub> | 253.01<sub>±00.44</sub> + 646.27<sub>±09.10</sub> |   465.69<sub>±119.53</sub> |
|                      Lua |   59.453<sub>±1.091</sub> |     2.67<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1086.03<sub>±14.38</sub> |
|             Ruby (--jit) |   61.091<sub>±1.388</sub> |    14.05<sub>±00.03</sub> + 0.23<sub>±00.00</sub> |   1104.23<sub>±29.71</sub> |
|                     Ruby |   88.905<sub>±3.702</sub> |    14.03<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |  1874.14<sub>±104.91</sub> |
|               Ruby/jruby |  111.976<sub>±4.390</sub> | 199.00<sub>±04.69</sub> + 232.92<sub>±01.53</sub> |   2283.61<sub>±73.13</sub> |
|                   Elixir |  115.244<sub>±2.160</sub> |    56.56<sub>±00.51</sub> + 0.00<sub>±00.00</sub> |  2538.04<sub>±130.27</sub> |
|                   Python |  232.090<sub>±5.249</sub> |    10.41<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  5198.30<sub>±150.90</sub> |
|                 Tcl (FP) |  277.932<sub>±7.323</sub> |     4.28<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  6132.28<sub>±205.33</sub> |
|                     Perl | 367.572<sub>±10.403</sub> |     6.48<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  6994.56<sub>±109.33</sub> |
|                Tcl (OOP) |  562.710<sub>±8.313</sub> |     4.28<sub>±00.05</sub> + 0.00<sub>±00.00</sub> | 10516.03<sub>±111.12</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|                    C/gcc |  13.327<sub>±0.268</sub> |     0.55<sub>±00.01</sub> + 1.07<sub>±00.02</sub> |   226.88<sub>±06.84</sub> |
|                  C++/g++ |  13.353<sub>±0.246</sub> |     3.27<sub>±00.05</sub> + 0.47<sub>±00.00</sub> |   271.19<sub>±05.91</sub> |
|                    D/gdc |  13.611<sub>±0.159</sub> |     6.74<sub>±00.10</sub> + 0.52<sub>±00.00</sub> |   290.12<sub>±04.00</sub> |
|                   D/ldc2 |  14.124<sub>±0.330</sub> |     3.04<sub>±00.03</sub> + 0.77<sub>±00.00</sub> |   241.94<sub>±04.27</sub> |
|                  Nim/gcc |  15.135<sub>±0.699</sub> |     1.87<sub>±00.06</sub> + 0.51<sub>±00.00</sub> |   285.26<sub>±21.64</sub> |
|                    V/gcc |  15.619<sub>±0.648</sub> |     0.56<sub>±00.00</sub> + 1.95<sub>±00.06</sub> |   310.08<sub>±24.89</sub> |
|                   Kotlin |  16.558<sub>±0.253</sub> |    38.27<sub>±00.23</sub> + 2.63<sub>±00.31</sub> |   321.66<sub>±27.02</sub> |
|                  C/clang |  17.680<sub>±0.169</sub> |     0.52<sub>±00.02</sub> + 1.09<sub>±00.03</sub> |   404.60<sub>±02.14</sub> |
|             C#/.NET Core |  19.262<sub>±0.280</sub> |    34.42<sub>±00.04</sub> + 1.04<sub>±00.03</sub> |   332.74<sub>±13.85</sub> |
|                  V/clang |  20.233<sub>±0.836</sub> |     0.98<sub>±00.12</sub> + 1.95<sub>±00.09</sub> |   391.71<sub>±32.22</sub> |
|                     Rust |  20.594<sub>±0.390</sub> |     1.97<sub>±00.04</sub> + 0.25<sub>±00.00</sub> |   389.28<sub>±16.81</sub> |
|               Vala/clang |  21.297<sub>±0.454</sub> |     3.59<sub>±00.03</sub> + 2.04<sub>±00.05</sub> |   476.19<sub>±15.96</sub> |
|                 Vala/gcc |  21.454<sub>±0.172</sub> |     3.59<sub>±00.03</sub> + 2.03<sub>±00.03</sub> |   454.96<sub>±11.26</sub> |
|                Nim/clang |  21.827<sub>±0.352</sub> |     2.31<sub>±00.01</sub> + 0.51<sub>±00.00</sub> |   398.81<sub>±09.98</sub> |
|                     Java |  23.685<sub>±1.904</sub> |    37.70<sub>±00.22</sub> + 1.41<sub>±00.12</sub> |   458.21<sub>±31.50</sub> |
|                  Crystal |  23.903<sub>±0.971</sub> |     3.35<sub>±00.03</sub> + 0.46<sub>±00.02</sub> |   475.53<sub>±36.59</sub> |
|                    Scala |  24.425<sub>±0.438</sub> |   80.31<sub>±00.49</sub> + 41.52<sub>±03.52</sub> |   472.17<sub>±28.71</sub> |
|                 Go/gccgo |  26.150<sub>±0.487</sub> |    21.30<sub>±00.05</sub> + 1.28<sub>±00.01</sub> |   448.70<sub>±12.23</sub> |
|                       Go |  33.741<sub>±0.576</sub> |     3.53<sub>±00.05</sub> + 1.25<sub>±00.01</sub> |   709.36<sub>±28.06</sub> |
|                    OCaml |  37.524<sub>±1.155</sub> |     3.88<sub>±00.03</sub> + 9.40<sub>±01.92</sub> |   754.09<sub>±61.18</sub> |
|              Chez Scheme |  39.908<sub>±0.185</sub> |    25.44<sub>±00.02</sub> + 3.65<sub>±00.02</sub> |   900.93<sub>±06.60</sub> |
|                  C#/Mono |  44.691<sub>±0.148</sub> |    20.34<sub>±00.05</sub> + 0.89<sub>±00.00</sub> |   963.56<sub>±10.00</sub> |
|                    D/dmd |  46.028<sub>±0.501</sub> |     3.55<sub>±00.08</sub> + 0.77<sub>±00.00</sub> |   821.16<sub>±10.79</sub> |
|                  Node.js |  47.273<sub>±0.664</sub> |    30.62<sub>±00.10</sub> + 6.18<sub>±00.01</sub> |   862.48<sub>±25.11</sub> |
|                    MLton |  51.157<sub>±0.803</sub> |     1.44<sub>±00.05</sub> + 4.11<sub>±00.00</sub> |  1125.53<sub>±16.85</sub> |
|                    Julia |  57.933<sub>±0.971</sub> |   169.52<sub>±00.18</sub> + 0.00<sub>±00.00</sub> |  1217.32<sub>±17.16</sub> |
|         Haskell (MArray) |  61.182<sub>±0.580</sub> |     3.68<sub>±00.05</sub> + 2.68<sub>±00.00</sub> |  1366.02<sub>±31.06</sub> |
|              Python/pypy |  68.463<sub>±1.458</sub> |   64.10<sub>±00.19</sub> + 46.16<sub>±00.09</sub> |  1507.60<sub>±67.16</sub> |
| Ruby/truffleruby (--jvm) | 130.745<sub>±1.922</sub> | 556.91<sub>±06.97</sub> + 399.51<sub>±20.62</sub> |  2730.87<sub>±29.04</sub> |
|             F#/.NET Core | 131.618<sub>±0.400</sub> |   36.52<sub>±00.12</sub> + 91.49<sub>±00.04</sub> |  2979.33<sub>±18.09</sub> |
|                   Racket | 163.988<sub>±1.458</sub> |   101.01<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  3487.58<sub>±19.89</sub> |
|         Ruby/truffleruby | 173.815<sub>±2.968</sub> | 252.98<sub>±00.33</sub> + 652.14<sub>±18.65</sub> |  3763.90<sub>±85.58</sub> |
|                  Haskell | 217.835<sub>±1.549</sub> |    3.87<sub>±00.02</sub> + 26.16<sub>±00.00</sub> |  4856.01<sub>±69.73</sub> |
|               Lua/luajit | 252.222<sub>±3.794</sub> |     2.96<sub>±00.07</sub> + 0.86<sub>±00.00</sub> | 4624.78<sub>±140.51</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.155<sub>±0.004</sub> |     1.91<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   3.53<sub>±00.22</sub> |
|                     C/gcc |  1.286<sub>±0.022</sub> |     1.88<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |  21.62<sub>±00.47</sub> |
|                      Rust |  1.367<sub>±0.018</sub> |     2.43<sub>±00.08</sub> + 0.04<sub>±00.03</sub> |  23.76<sub>±00.76</sub> |
|                     V/gcc |  1.490<sub>±0.017</sub> |     1.72<sub>±00.04</sub> + 0.27<sub>±00.03</sub> |  31.92<sub>±00.56</sub> |
|                   V/clang |  1.547<sub>±0.044</sub> |     2.01<sub>±00.04</sub> + 0.48<sub>±00.01</sub> |  32.47<sub>±01.29</sub> |
|                 Nim/clang |  1.776<sub>±0.069</sub> |     2.75<sub>±00.05</sub> + 4.38<sub>±00.00</sub> |  35.06<sub>±03.11</sub> |
|                    D/ldc2 |  1.776<sub>±0.009</sub> |     3.46<sub>±00.08</sub> + 3.67<sub>±00.00</sub> |  30.96<sub>±00.26</sub> |
|                   Crystal |  1.970<sub>±0.004</sub> |     3.81<sub>±00.03</sub> + 1.81<sub>±00.01</sub> |  47.33<sub>±00.28</sub> |
|                     D/gdc |  1.991<sub>±0.013</sub> |     7.12<sub>±00.03</sub> + 3.46<sub>±00.00</sub> |  43.91<sub>±00.53</sub> |
|                   Nim/gcc |  2.127<sub>±0.066</sub> |     2.25<sub>±00.04</sub> + 4.47<sub>±00.03</sub> |  37.94<sub>±01.81</sub> |
|                      Java |  2.221<sub>±0.047</sub> |  38.51<sub>±00.11</sub> + 328.86<sub>±19.54</sub> |  48.33<sub>±02.29</sub> |
|                      Ruby |  2.320<sub>±0.080</sub> |   14.45<sub>±00.04</sub> + 57.18<sub>±01.10</sub> |  49.27<sub>±03.59</sub> |
|                    Kotlin |  2.401<sub>±0.042</sub> |  39.44<sub>±00.15</sub> + 314.60<sub>±02.85</sub> |  54.82<sub>±00.96</sub> |
|              Ruby (--jit) |  2.434<sub>±0.033</sub> |   14.44<sub>±00.05</sub> + 56.54<sub>±01.23</sub> |  46.07<sub>±01.63</sub> |
|                        Go |  2.526<sub>±0.007</sub> |     4.57<sub>±00.06</sub> + 4.86<sub>±00.27</sub> |  47.73<sub>±00.46</sub> |
|                     Scala |  2.590<sub>±0.055</sub> |   80.96<sub>±00.24</sub> + 70.38<sub>±06.02</sub> |  50.33<sub>±00.82</sub> |
|       C++/g++ (libcrypto) |  2.662<sub>±0.042</sub> |     5.46<sub>±00.04</sub> + 0.07<sub>±00.00</sub> |  47.36<sub>±01.47</sub> |
|                       PHP |  2.775<sub>±0.065</sub> |    15.77<sub>±00.13</sub> + 0.00<sub>±00.00</sub> |  59.73<sub>±02.12</sub> |
|                   Node.js |  3.039<sub>±0.035</sub> | 31.17<sub>±00.04</sub> + 1029.97<sub>±00.12</sub> |  65.58<sub>±01.74</sub> |
|       Perl (MIME::Base64) |  3.062<sub>±0.072</sub> |    14.14<sub>±00.08</sub> + 0.02<sub>±00.00</sub> |  53.75<sub>±02.24</sub> |
|                  Go/gccgo |  3.530<sub>±0.003</sub> |    22.04<sub>±00.10</sub> + 7.43<sub>±00.21</sub> |  74.98<sub>±01.12</sub> |
|                     D/dmd |  3.940<sub>±0.104</sub> |     3.77<sub>±00.03</sub> + 3.67<sub>±00.06</sub> |  78.69<sub>±03.66</sub> |
|                    Python |  3.989<sub>±0.034</sub> |    10.12<sub>±00.03</sub> + 0.18<sub>±00.00</sub> |  90.21<sub>±00.87</sub> |
|                       Tcl |  4.167<sub>±0.146</sub> |     5.17<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  83.77<sub>±06.55</sub> |
|               Python/pypy |  4.769<sub>±0.055</sub> |   63.84<sub>±00.14</sub> + 45.68<sub>±00.03</sub> | 104.57<sub>±01.58</sub> |
|              C#/.NET Core |  5.330<sub>±0.017</sub> |   34.62<sub>±00.04</sub> + 36.88<sub>±04.38</sub> |  97.45<sub>±01.70</sub> |
|                     Julia |  5.715<sub>±0.120</sub> |  201.46<sub>±00.06</sub> + 42.97<sub>±00.08</sub> | 131.81<sub>±01.76</sub> |
|  Ruby/truffleruby (--jvm) |  6.023<sub>±0.110</sub> | 555.55<sub>±06.98</sub> + 324.02<sub>±32.15</sub> | 120.70<sub>±05.32</sub> |
|                   C#/Mono |  7.229<sub>±0.042</sub> |   20.80<sub>±00.06</sub> + 18.48<sub>±00.07</sub> | 170.53<sub>±01.78</sub> |
|                Ruby/jruby | 10.802<sub>±0.487</sub> | 201.17<sub>±07.47</sub> + 164.91<sub>±12.64</sub> | 216.26<sub>±18.52</sub> |
| Perl (MIME::Base64::Perl) | 16.743<sub>±0.684</sub> |    15.41<sub>±00.08</sub> + 0.26<sub>±00.02</sub> | 344.13<sub>±23.56</sub> |
|          Ruby/truffleruby | 20.875<sub>±0.745</sub> | 253.25<sub>±01.02</sub> + 404.54<sub>±00.39</sub> | 405.75<sub>±37.59</sub> |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|                        Language |                  Time, s |                                        Memory, MiB |                  Energy, J |
| :------------------------------ | -----------------------: | -------------------------------------------------: | -------------------------: |
|         C++/g++ (DAW JSON Link) |   0.090<sub>±0.004</sub> |    109.26<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |      2.04<sub>±00.21</sub> |
|    C++/g++ (simdjson On-Demand) |   0.092<sub>±0.003</sub> |   109.83<sub>±00.02</sub> + 59.55<sub>±00.00</sub> |      1.97<sub>±00.12</sub> |
|                    D/gdc (fast) |   0.105<sub>±0.004</sub> |   220.09<sub>±00.01</sub> + 11.27<sub>±00.26</sub> |      2.49<sub>±00.14</sub> |
|             Rust (Serde Custom) |   0.138<sub>±0.006</sub> |    108.52<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |      2.85<sub>±00.28</sub> |
|              Rust (Serde Typed) |   0.150<sub>±0.006</sub> |   108.44<sub>±00.06</sub> + 11.89<sub>±00.13</sub> |      2.96<sub>±00.19</sub> |
|                 C++/g++ (gason) |   0.160<sub>±0.006</sub> |   109.25<sub>±00.02</sub> + 97.17<sub>±00.06</sub> |      3.61<sub>±00.30</sub> |
|          C++/g++ (simdjson DOM) |   0.165<sub>±0.003</sub> |  109.87<sub>±00.06</sub> + 176.60<sub>±00.00</sub> |      3.28<sub>±00.14</sub> |
|             C++/g++ (RapidJSON) |   0.232<sub>±0.008</sub> |  109.26<sub>±00.02</sub> + 128.85<sub>±00.03</sub> |      4.56<sub>±00.24</sub> |
|            C++/g++ (Boost.JSON) |   0.495<sub>±0.020</sub> |  109.85<sub>±00.05</sub> + 435.70<sub>±00.00</sub> |     11.05<sub>±01.15</sub> |
|                            Java |   0.533<sub>±0.020</sub> |   252.48<sub>±00.10</sub> + 74.44<sub>±01.27</sub> |     13.90<sub>±00.59</sub> |
|         C++/g++ (RapidJSON SAX) |   0.570<sub>±0.022</sub> |    109.50<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     10.43<sub>±00.53</sub> |
|                   Julia (JSON3) |   0.597<sub>±0.015</sub> |  348.38<sub>±00.08</sub> + 256.97<sub>±01.51</sub> |     13.68<sub>±00.48</sub> |
|                           Scala |   0.610<sub>±0.018</sub> |   320.42<sub>±06.88</sub> + 73.82<sub>±00.73</sub> |     16.67<sub>±00.71</sub> |
|                         Node.js |   0.663<sub>±0.011</sub> |  244.09<sub>±00.09</sub> + 184.99<sub>±00.20</sub> |     14.68<sub>±00.33</sub> |
|                   Go (jsoniter) |   0.691<sub>±0.017</sub> |   224.87<sub>±00.16</sub> + 13.79<sub>±00.21</sub> |     15.68<sub>±00.90</sub> |
|                     Python/pypy |   0.831<sub>±0.023</sub> |  277.41<sub>±00.23</sub> + 127.96<sub>±00.00</sub> |     16.15<sub>±00.48</sub> |
|                         V/clang |   0.841<sub>±0.024</sub> |  107.97<sub>±00.11</sub> + 484.45<sub>±00.09</sub> |     15.29<sub>±00.24</sub> |
|                  Crystal (Pull) |   0.843<sub>±0.004</sub> |   110.35<sub>±00.05</sub> + 18.19<sub>±00.05</sub> |     17.42<sub>±00.37</sub> |
|            Rust (Serde Untyped) |   0.850<sub>±0.013</sub> |  108.34<sub>±00.06</sub> + 840.04<sub>±00.03</sub> |     18.99<sub>±01.02</sub> |
|                           V/gcc |   0.895<sub>±0.026</sub> |  107.39<sub>±00.01</sub> + 484.56<sub>±00.03</sub> |     19.52<sub>±01.73</sub> |
|                Crystal (Schema) |   0.917<sub>±0.031</sub> |   110.27<sub>±00.03</sub> + 47.04<sub>±00.07</sub> |     14.97<sub>±00.54</sub> |
|         Perl (Cpanel::JSON::XS) |   0.993<sub>±0.030</sub> |  121.27<sub>±00.07</sub> + 402.72<sub>±00.00</sub> |     19.53<sub>±00.70</sub> |
| C#/.NET Core (System.Text.Json) |   1.065<sub>±0.016</sub> |  465.55<sub>±00.23</sub> + 135.54<sub>±00.11</sub> |     19.24<sub>±00.29</sub> |
|                         Crystal |   1.142<sub>±0.027</sub> |  110.36<sub>±00.02</sub> + 393.34<sub>±00.03</sub> |     25.01<sub>±00.65</sub> |
|                              Go |   1.167<sub>±0.037</sub> |   113.96<sub>±00.11</sub> + 95.70<sub>±00.03</sub> |     21.04<sub>±00.71</sub> |
|                             PHP |   1.191<sub>±0.010</sub> |  121.61<sub>±00.19</sub> + 682.01<sub>±00.00</sub> |     26.72<sub>±00.27</sub> |
|                        Go/gccgo |   1.409<sub>±0.008</sub> |   132.28<sub>±00.16</sub> + 95.87<sub>±00.21</sub> |     31.98<sub>±00.53</sub> |
|                C++/g++ (json-c) |   1.466<sub>±0.018</sub> | 109.45<sub>±00.01</sub> + 1216.08<sub>±00.00</sub> |     35.13<sub>±00.58</sub> |
|            Nim/gcc (Packedjson) |   1.522<sub>±0.037</sub> |  108.77<sub>±00.06</sub> + 290.55<sub>±00.00</sub> |     27.16<sub>±00.46</sub> |
|          Nim/clang (Packedjson) |   1.528<sub>±0.057</sub> |  109.02<sub>±00.09</sub> + 290.81<sub>±00.00</sub> |     31.61<sub>±02.83</sub> |
|                         Clojure |   1.610<sub>±0.061</sub> |  487.96<sub>±06.62</sub> + 520.30<sub>±04.81</sub> |     41.80<sub>±02.48</sub> |
|                          Python |   1.721<sub>±0.010</sub> |  116.67<sub>±00.03</sub> + 377.21<sub>±00.00</sub> |     40.79<sub>±00.48</sub> |
|                    C#/.NET Core |   1.729<sub>±0.060</sub> |  474.64<sub>±00.30</sub> + 288.63<sub>±00.08</sub> |     31.25<sub>±01.48</sub> |
|                         Haskell |   1.741<sub>±0.016</sub> |      4.73<sub>±00.07</sub> + 4.44<sub>±00.03</sub> |     41.18<sub>±01.13</sub> |
|             CPython (UltraJSON) |   1.774<sub>±0.016</sub> |  118.31<sub>±00.02</sub> + 545.93<sub>±01.85</sub> |     38.65<sub>±01.38</sub> |
|                       Nim/clang |   1.826<sub>±0.025</sub> |  109.05<sub>±00.07</sub> + 919.62<sub>±00.06</sub> |     42.11<sub>±01.44</sub> |
|                         Nim/gcc |   1.865<sub>±0.071</sub> |  108.61<sub>±00.16</sub> + 919.52<sub>±00.16</sub> |     38.84<sub>±03.13</sub> |
|                         C#/Mono |   2.152<sub>±0.105</sub> |    462.45<sub>±00.13</sub> + 0.17<sub>±00.01</sub> |     43.70<sub>±04.37</sub> |
|                            Ruby |   2.198<sub>±0.021</sub> |  120.60<sub>±00.02</sub> + 410.64<sub>±00.04</sub> |     50.74<sub>±00.63</sub> |
|                           D/gdc |   2.243<sub>±0.092</sub> |  113.30<sub>±00.02</sub> + 600.25<sub>±00.00</sub> |     50.49<sub>±02.53</sub> |
|                    Ruby (--jit) |   2.290<sub>±0.064</sub> |  120.62<sub>±00.06</sub> + 410.77<sub>±00.06</sub> |     49.99<sub>±04.26</sub> |
|                     Ruby (YAJL) |   2.307<sub>±0.079</sub> |  120.51<sub>±00.03</sub> + 281.59<sub>±00.00</sub> |     51.52<sub>±02.14</sub> |
|                          D/ldc2 |   2.532<sub>±0.040</sub> |  109.59<sub>±00.04</sub> + 680.19<sub>±00.02</sub> |     50.59<sub>±01.78</sub> |
|                       Rust (jq) |   3.496<sub>±0.125</sub> |  110.42<sub>±00.06</sub> + 775.88<sub>±00.78</sub> |     78.27<sub>±02.30</sub> |
|                      Ruby/jruby |   3.841<sub>±0.114</sub> | 475.22<sub>±07.28</sub> + 1481.21<sub>±39.95</sub> |    113.68<sub>±03.22</sub> |
|    C++/g++ (Boost.PropertyTree) |   4.302<sub>±0.080</sub> | 109.65<sub>±00.05</sub> + 1440.06<sub>±00.00</sub> |    102.62<sub>±01.70</sub> |
|                           D/dmd |   4.951<sub>±0.083</sub> |  110.10<sub>±00.06</sub> + 680.20<sub>±00.01</sub> |    102.50<sub>±03.04</sub> |
|               Perl (JSON::Tiny) |  12.346<sub>±0.529</sub> |  121.91<sub>±00.04</sub> + 525.10<sub>±00.01</sub> |    236.77<sub>±15.96</sub> |
|        Ruby/truffleruby (--jvm) | 103.399<sub>±0.732</sub> | 725.13<sub>±06.18</sub> + 1191.03<sub>±96.08</sub> |   2883.54<sub>±44.89</sub> |
|                Ruby/truffleruby | 585.759<sub>±4.354</sub> | 731.19<sub>±00.36</sub> + 2023.45<sub>±42.56</sub> | 12242.31<sub>±167.47</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.067<sub>±0.002</sub> |    6.74<sub>±00.06</sub> + 55.61<sub>±00.16</sub> |      3.78<sub>±00.08</sub> |
|           Python (NumPy) |    0.102<sub>±0.001</sub> |   27.59<sub>±00.10</sub> + 57.67<sub>±00.08</sub> |      5.40<sub>±00.13</sub> |
|    Nim/gcc (Arraymancer) |    0.157<sub>±0.016</sub> |    5.63<sub>±00.07</sub> + 57.68<sub>±00.01</sub> |      8.35<sub>±00.54</sub> |
|  Nim/clang (Arraymancer) |    0.166<sub>±0.022</sub> |    6.44<sub>±00.12</sub> + 57.42<sub>±00.04</sub> |      7.92<sub>±00.63</sub> |
|       Julia (threads: 8) |    0.200<sub>±0.005</sub> |  221.80<sub>±00.12</sub> + 52.58<sub>±00.17</sub> |     10.40<sub>±00.40</sub> |
|              Java (ND4J) |    0.206<sub>±0.028</sub> |  135.38<sub>±01.86</sub> + 87.35<sub>±00.00</sub> |      9.81<sub>±01.38</sub> |
|       Julia (threads: 1) |    0.537<sub>±0.012</sub> |  221.39<sub>±00.14</sub> + 53.25<sub>±00.03</sub> |     12.99<sub>±00.29</sub> |
|                   D/ldc2 |    1.984<sub>±0.013</sub> |    3.56<sub>±00.02</sub> + 70.11<sub>±00.00</sub> |     44.09<sub>±00.55</sub> |
|                    D/gdc |    2.118<sub>±0.049</sub> |    6.65<sub>±00.07</sub> + 70.71<sub>±00.01</sub> |     49.15<sub>±02.42</sub> |
|                    D/dmd |    2.146<sub>±0.035</sub> |    3.54<sub>±00.04</sub> + 70.11<sub>±00.00</sub> |     45.76<sub>±01.20</sub> |
|                    C/gcc |    3.307<sub>±0.016</sub> |    2.02<sub>±00.04</sub> + 68.06<sub>±00.00</sub> |     77.93<sub>±00.61</sub> |
|                     Java |    3.331<sub>±0.044</sub> |   37.95<sub>±00.10</sub> + 77.51<sub>±00.20</sub> |     76.41<sub>±01.37</sub> |
|                     Rust |    3.389<sub>±0.016</sub> |    2.63<sub>±00.06</sub> + 68.32<sub>±00.00</sub> |     71.84<sub>±00.43</sub> |
|                    Scala |    3.422<sub>±0.107</sub> |   84.64<sub>±00.68</sub> + 79.79<sub>±02.91</sub> |     77.24<sub>±01.05</sub> |
|                Nim/clang |    3.462<sub>±0.014</sub> |    3.11<sub>±00.02</sub> + 70.64<sub>±00.52</sub> |     72.49<sub>±00.79</sub> |
|                  Nim/gcc |    3.465<sub>±0.014</sub> |    2.61<sub>±00.03</sub> + 71.41<sub>±01.29</sub> |     71.65<sub>±01.21</sub> |
|                 Go/gccgo |    3.560<sub>±0.016</sub> |   21.43<sub>±00.20</sub> + 72.99<sub>±00.18</sub> |     77.74<sub>±01.42</sub> |
|          Julia (no BLAS) |    3.560<sub>±0.085</sub> |  177.86<sub>±00.10</sub> + 69.76<sub>±00.04</sub> |     72.02<sub>±02.93</sub> |
|                       Go |    3.568<sub>±0.018</sub> |    3.80<sub>±00.03</sub> + 73.18<sub>±00.27</sub> |     79.64<sub>±01.43</sub> |
|                  V/clang |    3.707<sub>±0.041</sub> |    2.44<sub>±00.02</sub> + 68.84<sub>±00.00</sub> |     89.45<sub>±02.24</sub> |
|                  Crystal |    3.815<sub>±0.145</sub> |    4.17<sub>±00.02</sub> + 59.63<sub>±00.03</sub> |     77.84<sub>±04.22</sub> |
|                    Swift |    3.831<sub>±0.107</sub> |  148.70<sub>±00.06</sub> + 59.66<sub>±00.11</sub> |     76.12<sub>±04.76</sub> |
|                  Node.js |    3.856<sub>±0.067</sub> |   34.16<sub>±00.12</sub> + 71.25<sub>±00.03</sub> |     86.36<sub>±01.23</sub> |
|                   Kotlin |    3.926<sub>±0.328</sub> |   38.00<sub>±00.26</sub> + 77.66<sub>±00.46</sub> |     77.64<sub>±07.02</sub> |
|                    V/gcc |    4.673<sub>±0.052</sub> |    1.97<sub>±00.04</sub> + 68.84<sub>±00.00</sub> |    103.90<sub>±02.28</sub> |
|              Python/pypy |    6.511<sub>±0.192</sub> |   64.35<sub>±00.14</sub> + 69.21<sub>±00.02</sub> |    114.96<sub>±05.01</sub> |
|             C#/.NET Core |    6.849<sub>±0.050</sub> |   34.01<sub>±00.08</sub> + 69.11<sub>±00.00</sub> |    157.79<sub>±03.48</sub> |
|                  C#/Mono |   11.242<sub>±0.494</sub> |   20.21<sub>±00.08</sub> + 69.07<sub>±00.01</sub> |    216.89<sub>±23.82</sub> |
|         Ruby/truffleruby |   37.614<sub>±1.111</sub> | 527.38<sub>±00.25</sub> + 480.37<sub>±03.33</sub> |    774.93<sub>±65.86</sub> |
| Ruby/truffleruby (--jvm) |   71.812<sub>±1.623</sub> | 605.31<sub>±08.13</sub> + 318.65<sub>±11.88</sub> |  1915.91<sub>±107.73</sub> |
|             Ruby (--jit) |  213.953<sub>±4.452</sub> |   15.11<sub>±00.02</sub> + 68.87<sub>±00.03</sub> |  4556.86<sub>±112.03</sub> |
|                     Ruby |  219.297<sub>±6.115</sub> |   15.11<sub>±00.01</sub> + 68.64<sub>±00.00</sub> |  4348.97<sub>±369.08</sub> |
|                   Python |  248.815<sub>±6.912</sub> |   10.54<sub>±00.03</sub> + 68.58<sub>±00.00</sub> |  5171.34<sub>±260.75</sub> |
|                      Tcl |  348.512<sub>±8.997</sub> |   7.16<sub>±00.06</sub> + 400.44<sub>±00.06</sub> |  7136.61<sub>±492.96</sub> |
|                     Perl |  396.918<sub>±3.594</sub> |   9.00<sub>±00.04</sub> + 599.61<sub>±00.05</sub> |  9398.78<sub>±528.90</sub> |
|               Ruby/jruby | 500.782<sub>±14.495</sub> | 270.32<sub>±06.16</sub> + 670.69<sub>±06.01</sub> | 10825.70<sub>±291.87</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 5.0.101                         |
| C#/.NET Core     | 3.8.0-5.20570.14 (6559f38c)     |
| C#/Mono          | 6.12.0.107                      |
| C/clang          | 11.0.0                          |
| C/gcc            | 10.2.1                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.1"                        |
| Crystal          | 0.35.1                          |
| D/dmd            | v2.095.0                        |
| D/gdc            | 10.2.1                          |
| D/ldc2           | 1.24.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 11.0.0.0 for F# 5.0             |
| Go               | go1.15.6                        |
| Go/gccgo         | 10.2.1                          |
| Haskell          | 8.10.3                          |
| Java             | 15.0.1                          |
| Julia            | v"1.5.3"                        |
| Kotlin           | 1.4.21                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20201002                        |
| Nim              | 1.4.2                           |
| Node.js          | v15.5.1                         |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.11                          |
| Perl             | v5.32.0                         |
| Python           | 3.9.1                           |
| Python/pypy      | 7.3.3-beta0 for Python 3.7.9    |
| Racket           | "7.9"                           |
| Ruby             | 3.0.0p0                         |
| Ruby/jruby       | 9.2.14.0                        |
| Ruby/truffleruby | 20.3.0                          |
| Rust             | 1.49.0                          |
| Scala            | 2.13.4                          |
| Swift            | swift-5.3.2-RELEASE             |
| Tcl              | 8.6                             |
| V                | 0.2                             |
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
