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
  * [Havlak](#havlak)
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

UPDATE: 2020-10-30

# Test Cases

## Brainfuck

Testing brainfuck implementations using two code samples (bench.b and mandel.b).

[Brainfuck](brainfuck)

### bench.b

|                 Language |                  Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | -----------------------: | ------------------------------------------------: | -------------------------: |
|                  C++/g++ |   0.876<sub>±0.010</sub> |     1.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     17.08<sub>±00.19</sub> |
|                  Nim/gcc |   1.826<sub>±0.010</sub> |     1.80<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     33.07<sub>±00.41</sub> |
|                 Vala/gcc |   1.830<sub>±0.009</sub> |     3.98<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     33.87<sub>±00.54</sub> |
|                    C/gcc |   1.854<sub>±0.036</sub> |     0.55<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     35.34<sub>±01.45</sub> |
|                   Kotlin |   1.867<sub>±0.026</sub> |    37.01<sub>±00.14</sub> + 2.88<sub>±00.26</sub> |     35.09<sub>±00.90</sub> |
|                    OCaml |   1.868<sub>±0.045</sub> |     2.57<sub>±00.02</sub> + 2.48<sub>±00.03</sub> |     38.04<sub>±01.74</sub> |
|                   D/ldc2 |   1.934<sub>±0.019</sub> |     2.98<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     36.46<sub>±00.54</sub> |
|                Nim/clang |   1.939<sub>±0.048</sub> |     2.29<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     38.99<sub>±01.12</sub> |
|               Vala/clang |   1.975<sub>±0.027</sub> |     3.94<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |     41.58<sub>±00.67</sub> |
|                    D/gdc |   2.027<sub>±0.037</sub> |     6.28<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |     39.34<sub>±01.18</sub> |
|                     Rust |   2.102<sub>±0.050</sub> |     2.03<sub>±00.09</sub> + 0.00<sub>±00.00</sub> |     41.01<sub>±01.32</sub> |
|                 Go/gccgo |   2.206<sub>±0.074</sub> |    20.78<sub>±00.21</sub> + 0.00<sub>±00.00</sub> |     45.18<sub>±02.77</sub> |
|                  C/clang |   2.245<sub>±0.024</sub> |     0.50<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     42.46<sub>±02.19</sub> |
|                     Java |   2.337<sub>±0.022</sub> |    35.96<sub>±00.18</sub> + 1.89<sub>±00.30</sub> |     42.72<sub>±00.65</sub> |
|             C#/.NET Core |   2.451<sub>±0.037</sub> |    33.26<sub>±00.12</sub> + 1.30<sub>±00.05</sub> |     46.20<sub>±01.47</sub> |
|                       Go |   2.475<sub>±0.035</sub> |     3.38<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |     48.20<sub>±01.90</sub> |
|                    MLton |   2.493<sub>±0.024</sub> |     0.55<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |     52.45<sub>±02.54</sub> |
|                    V/gcc |   2.530<sub>±0.063</sub> |     0.50<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |     51.52<sub>±02.51</sub> |
|                  Crystal |   2.670<sub>±0.014</sub> |     3.35<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     51.04<sub>±01.64</sub> |
|             F#/.NET Core |   2.794<sub>±0.062</sub> |   35.15<sub>±00.13</sub> + 90.27<sub>±00.91</sub> |     55.36<sub>±01.72</sub> |
|                  V/clang |   2.867<sub>±0.077</sub> |     0.87<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |     56.10<sub>±03.03</sub> |
|              Chez Scheme |   2.936<sub>±0.054</sub> |    22.88<sub>±00.06</sub> + 6.24<sub>±00.05</sub> |     58.09<sub>±03.11</sub> |
|                    Julia |   3.242<sub>±0.133</sub> |  155.09<sub>±00.47</sub> + 21.86<sub>±00.38</sub> |     64.32<sub>±04.93</sub> |
|                    Scala |   3.496<sub>±0.078</sub> |   80.21<sub>±00.68</sub> + 57.68<sub>±05.60</sub> |     71.55<sub>±02.78</sub> |
|                    D/dmd |   3.771<sub>±0.039</sub> |     3.49<sub>±00.05</sub> + 0.00<sub>±00.00</sub> |     66.90<sub>±02.18</sub> |
|         Haskell (MArray) |   4.380<sub>±0.073</sub> |     4.11<sub>±00.08</sub> + 1.25<sub>±00.00</sub> |    101.37<sub>±03.59</sub> |
|                  C#/Mono |   4.459<sub>±0.179</sub> |    20.03<sub>±00.05</sub> + 0.37<sub>±00.00</sub> |     90.94<sub>±07.00</sub> |
|                  Node.js |   5.063<sub>±0.030</sub> |    29.49<sub>±00.02</sub> + 1.97<sub>±00.19</sub> |     88.66<sub>±01.54</sub> |
|               Lua/luajit |   7.571<sub>±0.151</sub> |     2.82<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    128.54<sub>±04.21</sub> |
|                   Racket |   8.077<sub>±0.170</sub> |   106.98<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |    141.31<sub>±03.73</sub> |
|              Python/pypy |  13.003<sub>±0.274</sub> |   63.79<sub>±00.21</sub> + 45.38<sub>±00.00</sub> |    315.76<sub>±02.70</sub> |
|                  Haskell |  16.686<sub>±0.512</sub> |     4.11<sub>±00.04</sub> + 1.34<sub>±00.00</sub> |    310.02<sub>±13.55</sub> |
| Ruby/truffleruby (--jvm) |  18.445<sub>±0.329</sub> | 596.26<sub>±11.32</sub> + 324.76<sub>±14.58</sub> |    522.27<sub>±23.71</sub> |
|         Ruby/truffleruby |  51.534<sub>±2.159</sub> | 260.91<sub>±00.06</sub> + 277.20<sub>±10.48</sub> |   978.82<sub>±137.20</sub> |
|                      Lua |  56.908<sub>±1.507</sub> |     2.64<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |   1203.05<sub>±52.83</sub> |
|             Ruby (--jit) |  61.920<sub>±1.734</sub> |    14.12<sub>±00.04</sub> + 0.15<sub>±00.00</sub> |   1110.14<sub>±49.84</sub> |
|                     Ruby |  82.990<sub>±1.020</sub> |    14.08<sub>±00.01</sub> + 0.00<sub>±00.00</sub> |   1771.74<sub>±67.67</sub> |
|               Ruby/jruby | 111.106<sub>±3.402</sub> | 199.68<sub>±05.79</sub> + 242.16<sub>±03.23</sub> |  2182.73<sub>±161.51</sub> |
|                   Elixir | 113.521<sub>±0.406</sub> |    53.51<sub>±00.27</sub> + 0.47<sub>±00.15</sub> |   2493.22<sub>±31.14</sub> |
|                   Python | 220.743<sub>±3.369</sub> |     9.32<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   4797.72<sub>±88.75</sub> |
|                 Tcl (FP) | 274.283<sub>±3.413</sub> |     4.27<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |  5234.49<sub>±210.69</sub> |
|                     Perl | 339.112<sub>±7.722</sub> |     6.43<sub>±00.02</sub> + 0.00<sub>±00.00</sub> |  7050.02<sub>±301.70</sub> |
|                Tcl (OOP) | 529.515<sub>±6.545</sub> |     4.28<sub>±00.05</sub> + 0.00<sub>±00.00</sub> | 11591.55<sub>±176.57</sub> |

### mandel.b

[Mandel in Brainfuck](brainfuck/mandel.b)

|                 Language |                   Time, s |                                       Memory, MiB |                 Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | ------------------------: |
|                  C++/g++ |   11.750<sub>±0.200</sub> |     1.50<sub>±00.03</sub> + 2.20<sub>±00.03</sub> |   238.08<sub>±19.76</sub> |
|                    C/gcc |   13.152<sub>±0.230</sub> |     0.55<sub>±00.01</sub> + 1.09<sub>±00.03</sub> |   226.01<sub>±09.52</sub> |
|                 Vala/gcc |   13.553<sub>±0.141</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   232.63<sub>±09.26</sub> |
|                    D/gdc |   14.056<sub>±0.695</sub> |     6.60<sub>±00.06</sub> + 0.52<sub>±00.00</sub> |   256.56<sub>±26.63</sub> |
|                   D/ldc2 |   15.040<sub>±0.137</sub> |     3.09<sub>±00.02</sub> + 0.77<sub>±00.00</sub> |   319.73<sub>±08.24</sub> |
|               Vala/clang |   15.198<sub>±0.222</sub> |     0.00<sub>±00.00</sub> + 0.00<sub>±00.00</sub> |   328.05<sub>±10.27</sub> |
|                  Crystal |   15.442<sub>±0.156</sub> |     3.36<sub>±00.01</sub> + 0.41<sub>±00.02</sub> |   322.26<sub>±06.55</sub> |
|                    V/gcc |   16.213<sub>±0.211</sub> |     0.53<sub>±00.03</sub> + 1.92<sub>±00.03</sub> |   303.77<sub>±17.59</sub> |
|                  C/clang |   18.020<sub>±0.552</sub> |     0.55<sub>±00.01</sub> + 1.06<sub>±00.04</sub> |   329.77<sub>±13.16</sub> |
|                Nim/clang |   18.082<sub>±0.336</sub> |     2.32<sub>±00.04</sub> + 0.51<sub>±00.00</sub> |   382.29<sub>±19.97</sub> |
|             C#/.NET Core |   18.168<sub>±0.287</sub> |    33.42<sub>±00.03</sub> + 2.48<sub>±00.05</sub> |   377.64<sub>±08.61</sub> |
|                     Rust |   18.961<sub>±0.684</sub> |     2.02<sub>±00.07</sub> + 0.28<sub>±00.03</sub> |   356.32<sub>±27.58</sub> |
|                  V/clang |   19.085<sub>±0.786</sub> |     0.86<sub>±00.06</sub> + 1.98<sub>±00.12</sub> |   338.08<sub>±24.00</sub> |
|                  Nim/gcc |   20.081<sub>±0.098</sub> |     1.86<sub>±00.04</sub> + 0.51<sub>±00.00</sub> |   447.29<sub>±05.66</sub> |
|                     Java |   21.567<sub>±1.482</sub> |    35.91<sub>±00.21</sub> + 7.39<sub>±00.35</sub> |   422.11<sub>±40.28</sub> |
|                   Kotlin |   22.433<sub>±0.767</sub> |    36.90<sub>±00.03</sub> + 8.33<sub>±00.17</sub> |   461.84<sub>±23.20</sub> |
|                    Scala |   23.711<sub>±0.735</sub> |   72.71<sub>±00.27</sub> + 32.66<sub>±05.22</sub> |   493.04<sub>±40.26</sub> |
|                    MLton |   24.570<sub>±0.317</sub> |     1.24<sub>±00.04</sub> + 1.78<sub>±00.00</sub> |   420.67<sub>±12.22</sub> |
|                 Go/gccgo |   27.242<sub>±0.228</sub> |    21.23<sub>±00.51</sub> + 1.28<sub>±00.00</sub> |   455.59<sub>±17.41</sub> |
|                    OCaml |   29.912<sub>±1.068</sub> |     3.29<sub>±00.02</sub> + 7.26<sub>±01.97</sub> |   565.03<sub>±42.35</sub> |
|                       Go |   32.794<sub>±0.618</sub> |     2.78<sub>±00.09</sub> + 1.25<sub>±00.00</sub> |   686.73<sub>±17.76</sub> |
|                    D/dmd |   44.664<sub>±1.621</sub> |     3.63<sub>±00.04</sub> + 0.77<sub>±00.00</sub> |   838.73<sub>±38.99</sub> |
|                  C#/Mono |   46.634<sub>±0.426</sub> |    20.14<sub>±00.05</sub> + 1.14<sub>±00.00</sub> |   990.63<sub>±12.21</sub> |
|              Chez Scheme |   48.198<sub>±1.162</sub> |    23.48<sub>±00.03</sub> + 5.73<sub>±00.04</sub> |   936.23<sub>±64.37</sub> |
|                  Node.js |   58.719<sub>±1.972</sub> |    29.56<sub>±00.15</sub> + 6.96<sub>±00.13</sub> |  1164.88<sub>±81.55</sub> |
|                    Julia |   59.297<sub>±0.975</sub> |  154.91<sub>±00.49</sub> + 21.99<sub>±00.40</sub> |  1277.50<sub>±17.66</sub> |
|         Haskell (MArray) |   62.629<sub>±0.442</sub> |     4.09<sub>±00.05</sub> + 2.45<sub>±00.00</sub> |  1401.28<sub>±13.64</sub> |
|               Lua/luajit |   64.387<sub>±0.450</sub> |     2.88<sub>±00.06</sub> + 0.85<sub>±00.00</sub> |  1364.42<sub>±54.12</sub> |
|              Python/pypy |   70.615<sub>±0.932</sub> |   63.83<sub>±00.25</sub> + 46.61<sub>±00.10</sub> |  1326.94<sub>±73.41</sub> |
| Ruby/truffleruby (--jvm) |  113.544<sub>±1.747</sub> | 589.01<sub>±03.74</sub> + 341.00<sub>±21.87</sub> |  2375.13<sub>±83.06</sub> |
|             F#/.NET Core |  124.378<sub>±0.274</sub> |   35.29<sub>±00.07</sub> + 93.01<sub>±00.09</sub> |  2416.39<sub>±10.62</sub> |
|                   Racket |  136.034<sub>±4.331</sub> |   106.99<sub>±00.03</sub> + 0.00<sub>±00.00</sub> | 2542.90<sub>±190.09</sub> |
|         Ruby/truffleruby | 194.627<sub>±15.454</sub> | 261.21<sub>±00.27</sub> + 251.62<sub>±11.10</sub> | 3804.72<sub>±383.77</sub> |
|                  Haskell |  214.731<sub>±1.549</sub> |     4.01<sub>±00.08</sub> + 2.54<sub>±00.00</sub> | 4658.95<sub>±141.03</sub> |

## Base64

Testing base64 encoding/decoding of the large blob into the newly allocated buffers.

[Base64](base64)

|                  Language |                 Time, s |                                       Memory, MiB |               Energy, J |
| :------------------------ | ----------------------: | ------------------------------------------------: | ----------------------: |
|            C/gcc (aklomp) |  0.157<sub>±0.002</sub> |     1.92<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |   3.41<sub>±00.14</sub> |
|                   V/clang |  0.831<sub>±0.032</sub> |     1.97<sub>±00.01</sub> + 0.43<sub>±00.10</sub> |  14.57<sub>±01.00</sub> |
|                     C/gcc |  1.237<sub>±0.067</sub> |     1.88<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  22.43<sub>±02.28</sub> |
|                      Rust |  1.256<sub>±0.022</sub> |     2.55<sub>±00.05</sub> + 0.01<sub>±00.00</sub> |  27.66<sub>±01.25</sub> |
|                     V/gcc |  1.474<sub>±0.026</sub> |     1.72<sub>±00.04</sub> + 0.23<sub>±00.07</sub> |  31.07<sub>±01.58</sub> |
|                 Nim/clang |  1.578<sub>±0.036</sub> |     2.72<sub>±00.05</sub> + 4.41<sub>±00.03</sub> |  35.12<sub>±02.09</sub> |
|                    D/ldc2 |  1.897<sub>±0.039</sub> |     3.42<sub>±00.04</sub> + 3.61<sub>±00.00</sub> |  32.52<sub>±01.16</sub> |
|                   Crystal |  1.982<sub>±0.014</sub> |     3.83<sub>±00.04</sub> + 1.80<sub>±00.02</sub> |  46.74<sub>±00.43</sub> |
|                   Nim/gcc |  1.983<sub>±0.024</sub> |     2.27<sub>±00.04</sub> + 4.44<sub>±00.00</sub> |  43.66<sub>±01.35</sub> |
|                      Java |  2.271<sub>±0.078</sub> |  38.62<sub>±00.04</sub> + 327.05<sub>±22.71</sub> |  48.84<sub>±03.76</sub> |
|                     D/gdc |  2.306<sub>±0.098</sub> |     7.11<sub>±00.06</sub> + 3.47<sub>±00.05</sub> |  45.83<sub>±05.01</sub> |
|                    Kotlin |  2.427<sub>±0.038</sub> |  39.58<sub>±00.05</sub> + 345.29<sub>±04.26</sub> |  56.48<sub>±01.80</sub> |
|                        Go |  2.505<sub>±0.013</sub> |     4.61<sub>±00.03</sub> + 4.50<sub>±00.17</sub> |  47.49<sub>±00.47</sub> |
|              Ruby (--jit) |  2.549<sub>±0.096</sub> |   14.51<sub>±00.02</sub> + 43.25<sub>±00.57</sub> |  47.31<sub>±03.07</sub> |
|                     Scala |  2.554<sub>±0.031</sub> |   80.89<sub>±00.67</sub> + 71.32<sub>±05.09</sub> |  51.15<sub>±03.92</sub> |
|                      Ruby |  2.569<sub>±0.075</sub> |   14.53<sub>±00.02</sub> + 43.50<sub>±00.16</sub> |  44.80<sub>±00.80</sub> |
|       Perl (MIME::Base64) |  2.646<sub>±0.104</sub> |    14.45<sub>±00.08</sub> + 0.02<sub>±00.00</sub> |  48.60<sub>±04.70</sub> |
|       C++/g++ (libcrypto) |  2.719<sub>±0.084</sub> |     5.34<sub>±00.16</sub> + 0.07<sub>±00.00</sub> |  62.23<sub>±03.17</sub> |
|                       PHP |  2.942<sub>±0.101</sub> |    15.80<sub>±00.08</sub> + 0.00<sub>±00.00</sub> |  49.58<sub>±01.39</sub> |
|                   Node.js |  3.123<sub>±0.105</sub> | 31.01<sub>±00.06</sub> + 1029.64<sub>±00.08</sub> |  68.11<sub>±01.22</sub> |
|                  Go/gccgo |  3.506<sub>±0.010</sub> |    22.03<sub>±00.17</sub> + 7.14<sub>±00.14</sub> |  72.97<sub>±01.36</sub> |
|                     D/dmd |  3.815<sub>±0.128</sub> |     3.69<sub>±00.06</sub> + 3.67<sub>±00.06</sub> |  65.92<sub>±04.33</sub> |
|                       Tcl |  4.008<sub>±0.080</sub> |     5.23<sub>±00.03</sub> + 0.00<sub>±00.00</sub> |  87.92<sub>±02.24</sub> |
|                    Python |  5.166<sub>±0.061</sub> |     9.37<sub>±00.02</sub> + 0.18<sub>±00.00</sub> | 117.11<sub>±03.97</sub> |
|               Python/pypy |  5.475<sub>±0.300</sub> |   63.99<sub>±00.11</sub> + 45.65<sub>±00.07</sub> | 106.11<sub>±11.82</sub> |
|              C#/.NET Core |  5.513<sub>±0.093</sub> |   33.86<sub>±00.11</sub> + 40.39<sub>±00.61</sub> | 104.92<sub>±01.62</sub> |
|                     Julia |  5.623<sub>±0.132</sub> |  199.35<sub>±00.31</sub> + 43.17<sub>±00.15</sub> | 131.97<sub>±01.86</sub> |
|  Ruby/truffleruby (--jvm) |  6.183<sub>±0.233</sub> | 595.53<sub>±10.25</sub> + 132.10<sub>±12.97</sub> | 125.76<sub>±15.33</sub> |
|                   C#/Mono |  7.263<sub>±0.080</sub> |   20.73<sub>±00.07</sub> + 18.50<sub>±00.02</sub> | 173.29<sub>±05.14</sub> |
|                Ruby/jruby | 10.530<sub>±0.747</sub> | 206.29<sub>±07.04</sub> + 169.80<sub>±07.50</sub> | 215.73<sub>±13.87</sub> |
| Perl (MIME::Base64::Perl) | 16.329<sub>±0.482</sub> |    15.77<sub>±00.12</sub> + 0.25<sub>±00.04</sub> | 358.67<sub>±13.10</sub> |
|          Ruby/truffleruby | 24.242<sub>±0.227</sub> | 263.40<sub>±00.14</sub> + 354.47<sub>±00.19</sub> | 411.96<sub>±05.26</sub> |

## Json

Testing parsing and simple calculating of values from a big JSON file.

[Json](json)

|                        Language |                 Time, s |                                        Memory, MiB |                Energy, J |
| :------------------------------ | ----------------------: | -------------------------------------------------: | -----------------------: |
|         C++/g++ (DAW JSON Link) |  0.087<sub>±0.003</sub> |    109.28<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |    1.73<sub>±00.10</sub> |
|    C++/g++ (simdjson On-Demand) |  0.091<sub>±0.002</sub> |   109.62<sub>±00.24</sub> + 59.90<sub>±00.09</sub> |    1.72<sub>±00.11</sub> |
|                    D/gdc (fast) |  0.111<sub>±0.000</sub> |   219.99<sub>±00.10</sub> + 11.09<sub>±00.00</sub> |    2.15<sub>±00.08</sub> |
|             Rust (Serde Custom) |  0.152<sub>±0.003</sub> |    108.40<sub>±00.06</sub> + 0.00<sub>±00.00</sub> |    2.54<sub>±00.07</sub> |
|              Rust (Serde Typed) |  0.158<sub>±0.002</sub> |   108.50<sub>±00.09</sub> + 11.77<sub>±00.19</sub> |    2.60<sub>±00.10</sub> |
|                 C++/g++ (gason) |  0.163<sub>±0.003</sub> |   109.26<sub>±00.01</sub> + 97.17<sub>±00.00</sub> |    3.19<sub>±00.15</sub> |
|          C++/g++ (simdjson DOM) |  0.169<sub>±0.002</sub> |  109.85<sub>±00.05</sub> + 176.60<sub>±00.00</sub> |    3.22<sub>±00.06</sub> |
|             C++/g++ (RapidJSON) |  0.227<sub>±0.007</sub> |  109.26<sub>±00.01</sub> + 128.85<sub>±00.03</sub> |    5.00<sub>±00.44</sub> |
|            C++/g++ (Boost.JSON) |  0.502<sub>±0.010</sub> |  109.76<sub>±00.01</sub> + 435.70<sub>±00.00</sub> |   11.39<sub>±00.55</sub> |
|                            Java |  0.522<sub>±0.018</sub> |   252.52<sub>±00.10</sub> + 74.25<sub>±00.75</sub> |   14.47<sub>±00.36</sub> |
|         C++/g++ (RapidJSON SAX) |  0.567<sub>±0.014</sub> |    109.46<sub>±00.04</sub> + 0.00<sub>±00.00</sub> |   11.43<sub>±00.56</sub> |
|                           Scala |  0.588<sub>±0.016</sub> |   327.12<sub>±00.93</sub> + 74.86<sub>±00.92</sub> |   15.12<sub>±00.83</sub> |
|                         Node.js |  0.665<sub>±0.021</sub> |  243.12<sub>±00.07</sub> + 184.81<sub>±00.18</sub> |   15.24<sub>±01.17</sub> |
|                   Go (jsoniter) |  0.703<sub>±0.022</sub> |   224.85<sub>±00.09</sub> + 13.71<sub>±00.10</sub> |   14.27<sub>±00.99</sub> |
|                   Julia (JSON3) |  0.710<sub>±0.019</sub> |  408.21<sub>±00.90</sub> + 141.92<sub>±00.89</sub> |   12.78<sub>±00.14</sub> |
|                     Python/pypy |  0.827<sub>±0.020</sub> |  277.20<sub>±00.18</sub> + 127.96<sub>±00.00</sub> |   18.50<sub>±01.17</sub> |
|            Rust (Serde Untyped) |  0.875<sub>±0.012</sub> |  108.46<sub>±00.07</sub> + 839.98<sub>±00.00</sub> |   19.34<sub>±00.64</sub> |
|                Crystal (Schema) |  0.889<sub>±0.045</sub> |   110.32<sub>±00.05</sub> + 46.85<sub>±00.09</sub> |   16.80<sub>±00.73</sub> |
|                  Crystal (Pull) |  0.901<sub>±0.043</sub> |   110.34<sub>±00.04</sub> + 18.23<sub>±00.01</sub> |   15.21<sub>±01.31</sub> |
|         Perl (Cpanel::JSON::XS) |  1.025<sub>±0.020</sub> |  121.62<sub>±00.08</sub> + 402.76<sub>±00.03</sub> |   19.48<sub>±00.72</sub> |
|                              Go |  1.107<sub>±0.031</sub> |   113.88<sub>±00.13</sub> + 95.54<sub>±00.16</sub> |   24.14<sub>±01.13</sub> |
|                         Crystal |  1.159<sub>±0.017</sub> |  110.31<sub>±00.07</sub> + 393.35<sub>±00.02</sub> |   24.91<sub>±00.86</sub> |
|                         V/clang |  1.232<sub>±0.018</sub> |  107.89<sub>±00.02</sub> + 484.45<sub>±00.06</sub> |   27.71<sub>±00.73</sub> |
|                             PHP |  1.243<sub>±0.020</sub> |  121.59<sub>±00.04</sub> + 682.01<sub>±00.00</sub> |   25.89<sub>±00.38</sub> |
|                           V/gcc |  1.330<sub>±0.017</sub> |  107.43<sub>±00.05</sub> + 484.56<sub>±00.00</sub> |   23.84<sub>±00.92</sub> |
|                        Go/gccgo |  1.443<sub>±0.024</sub> |   132.33<sub>±00.23</sub> + 96.04<sub>±00.04</sub> |   31.07<sub>±00.79</sub> |
|          Nim/clang (Packedjson) |  1.477<sub>±0.024</sub> |  109.02<sub>±00.06</sub> + 290.81<sub>±00.00</sub> |   32.36<sub>±00.67</sub> |
|                C++/g++ (json-c) |  1.541<sub>±0.052</sub> | 109.48<sub>±00.03</sub> + 1216.08<sub>±00.00</sub> |   34.25<sub>±01.00</sub> |
|            Nim/gcc (Packedjson) |  1.566<sub>±0.015</sub> |  108.54<sub>±00.04</sub> + 290.81<sub>±00.00</sub> |   27.16<sub>±00.44</sub> |
|                         Clojure |  1.611<sub>±0.019</sub> |  483.89<sub>±04.47</sub> + 529.38<sub>±25.20</sub> |   41.64<sub>±01.98</sub> |
|                         Haskell |  1.740<sub>±0.036</sub> |      4.79<sub>±00.06</sub> + 5.76<sub>±00.08</sub> |   39.03<sub>±00.59</sub> |
|                          Python |  1.785<sub>±0.042</sub> |  115.90<sub>±00.03</sub> + 377.23<sub>±00.01</sub> |   37.85<sub>±02.11</sub> |
|             CPython (UltraJSON) |  1.813<sub>±0.026</sub> |  117.51<sub>±00.06</sub> + 544.41<sub>±01.03</sub> |   39.64<sub>±01.78</sub> |
|                         Nim/gcc |  1.858<sub>±0.058</sub> |  108.55<sub>±00.01</sub> + 915.81<sub>±00.00</sub> |   36.52<sub>±02.83</sub> |
|                       Nim/clang |  1.969<sub>±0.021</sub> |  109.01<sub>±00.03</sub> + 915.75<sub>±00.00</sub> |   35.52<sub>±00.93</sub> |
|                    C#/.NET Core |  2.047<sub>±0.042</sub> |  472.62<sub>±00.04</sub> + 288.50<sub>±00.03</sub> |   43.35<sub>±02.30</sub> |
|                            Ruby |  2.197<sub>±0.020</sub> |  120.73<sub>±00.03</sub> + 277.82<sub>±00.05</sub> |   50.09<sub>±00.62</sub> |
|                           D/gdc |  2.211<sub>±0.017</sub> |  113.23<sub>±00.10</sub> + 600.30<sub>±00.00</sub> |   46.68<sub>±01.28</sub> |
|                     Ruby (YAJL) |  2.223<sub>±0.014</sub> |  120.66<sub>±00.03</sub> + 287.03<sub>±00.01</sub> |   49.99<sub>±01.09</sub> |
|                         C#/Mono |  2.233<sub>±0.041</sub> |    462.10<sub>±00.08</sub> + 0.17<sub>±00.00</sub> |   39.21<sub>±01.32</sub> |
|                    Ruby (--jit) |  2.245<sub>±0.059</sub> |  120.77<sub>±00.05</sub> + 277.87<sub>±00.16</sub> |   50.96<sub>±00.94</sub> |
|                          D/ldc2 |  2.560<sub>±0.023</sub> |  109.60<sub>±00.04</sub> + 680.21<sub>±00.00</sub> |   51.78<sub>±00.54</sub> |
|                       Rust (jq) |  3.753<sub>±0.117</sub> |  110.43<sub>±00.04</sub> + 775.50<sub>±00.77</sub> |   77.53<sub>±04.71</sub> |
|                      Ruby/jruby |  3.834<sub>±0.087</sub> | 477.64<sub>±08.08</sub> + 1485.96<sub>±39.70</sub> |  112.73<sub>±03.38</sub> |
|    C++/g++ (Boost.PropertyTree) |  4.629<sub>±0.110</sub> | 109.59<sub>±00.02</sub> + 1440.03<sub>±00.03</sub> |   89.83<sub>±06.60</sub> |
|                           D/dmd |  5.052<sub>±0.027</sub> |  110.11<sub>±00.05</sub> + 680.11<sub>±00.01</sub> |   98.13<sub>±01.90</sub> |
| C#/.NET Core (System.Text.Json) |  7.049<sub>±0.162</sub> |  463.86<sub>±00.06</sub> + 183.08<sub>±00.02</sub> |  151.72<sub>±08.05</sub> |
|               Perl (JSON::Tiny) | 11.629<sub>±0.119</sub> |  122.21<sub>±00.07</sub> + 525.14<sub>±00.04</sub> |  256.57<sub>±04.73</sub> |
|        Ruby/truffleruby (--jvm) | 23.532<sub>±0.665</sub> | 842.35<sub>±06.97</sub> + 1280.99<sub>±74.13</sub> |  597.41<sub>±27.51</sub> |
|                Ruby/truffleruby | 66.911<sub>±1.141</sub> | 532.60<sub>±00.30</sub> + 2336.97<sub>±87.82</sub> | 1503.14<sub>±30.64</sub> |

## Matmul

Testing allocating and multiplying matrices.

[Matmul](matmul)

|                 Language |                   Time, s |                                       Memory, MiB |                  Energy, J |
| :----------------------- | ------------------------: | ------------------------------------------------: | -------------------------: |
|          D/ldc2 (lubeck) |    0.146<sub>±0.005</sub> |    6.77<sub>±00.08</sub> + 51.80<sub>±00.15</sub> |      8.26<sub>±00.16</sub> |
|  Nim/clang (Arraymancer) |    0.151<sub>±0.005</sub> |    5.56<sub>±00.03</sub> + 51.56<sub>±00.00</sub> |      8.53<sub>±00.24</sub> |
|    Nim/gcc (Arraymancer) |    0.159<sub>±0.008</sub> |    5.61<sub>±00.08</sub> + 51.56<sub>±00.00</sub> |      9.18<sub>±00.25</sub> |
|           Python (NumPy) |    0.181<sub>±0.003</sub> |   27.05<sub>±00.07</sub> + 51.75<sub>±00.02</sub> |     10.17<sub>±00.41</sub> |
|              Java (ND4J) |    0.216<sub>±0.041</sub> |  140.37<sub>±02.41</sub> + 87.35<sub>±00.00</sub> |      9.54<sub>±00.77</sub> |
|       Julia (threads: 8) |    0.410<sub>±0.006</sub> |  220.29<sub>±00.23</sub> + 54.06<sub>±00.13</sub> |     20.38<sub>±01.02</sub> |
|       Julia (threads: 1) |    0.671<sub>±0.013</sub> |  221.04<sub>±01.05</sub> + 54.56<sub>±00.09</sub> |     15.50<sub>±00.93</sub> |
|                   D/ldc2 |    1.940<sub>±0.002</sub> |    3.56<sub>±00.12</sub> + 70.11<sub>±00.00</sub> |     42.77<sub>±00.46</sub> |
|                    D/gdc |    2.058<sub>±0.006</sub> |    6.62<sub>±00.10</sub> + 70.71<sub>±00.01</sub> |     48.22<sub>±01.30</sub> |
|                    D/dmd |    2.080<sub>±0.009</sub> |    3.86<sub>±00.04</sub> + 70.43<sub>±00.00</sub> |     46.80<sub>±00.39</sub> |
|                     Java |    3.278<sub>±0.005</sub> |   38.00<sub>±00.05</sub> + 77.25<sub>±00.19</sub> |     74.92<sub>±01.45</sub> |
|                    C/gcc |    3.290<sub>±0.017</sub> |    2.03<sub>±00.03</sub> + 68.06<sub>±00.00</sub> |     76.05<sub>±00.70</sub> |
|                     Rust |    3.357<sub>±0.012</sub> |    2.55<sub>±00.07</sub> + 68.32<sub>±00.00</sub> |     68.73<sub>±00.38</sub> |
|                  Nim/gcc |    3.419<sub>±0.026</sub> |    2.58<sub>±00.10</sub> + 73.89<sub>±03.25</sub> |     70.50<sub>±00.83</sub> |
|                    Scala |    3.422<sub>±0.020</sub> |   79.20<sub>±05.16</sub> + 75.16<sub>±04.73</sub> |     80.10<sub>±01.21</sub> |
|                Nim/clang |    3.437<sub>±0.018</sub> |    3.03<sub>±00.05</sub> + 76.70<sub>±06.06</sub> |     70.87<sub>±01.75</sub> |
|                       Go |    3.550<sub>±0.011</sub> |    3.74<sub>±00.08</sub> + 73.47<sub>±00.11</sub> |     76.46<sub>±01.28</sub> |
|                 Go/gccgo |    3.551<sub>±0.029</sub> |   21.64<sub>±00.20</sub> + 72.62<sub>±00.25</sub> |     73.43<sub>±01.75</sub> |
|          Julia (no BLAS) |    3.596<sub>±0.036</sub> |  175.97<sub>±00.21</sub> + 70.58<sub>±00.66</sub> |     76.74<sub>±00.60</sub> |
|                    Swift |    3.641<sub>±0.069</sub> |  148.96<sub>±00.05</sub> + 59.85<sub>±00.09</sub> |     75.35<sub>±01.71</sub> |
|                  Crystal |    3.709<sub>±0.107</sub> |    4.22<sub>±00.15</sub> + 59.61<sub>±00.10</sub> |     81.25<sub>±05.47</sub> |
|                    V/gcc |    3.738<sub>±0.166</sub> |    1.96<sub>±00.04</sub> + 68.84<sub>±00.00</sub> |     81.40<sub>±08.60</sub> |
|                  Node.js |    3.850<sub>±0.165</sub> |   33.01<sub>±00.11</sub> + 70.91<sub>±00.06</sub> |     78.56<sub>±08.23</sub> |
|                  V/clang |    3.887<sub>±0.080</sub> |    2.38<sub>±00.04</sub> + 68.84<sub>±00.00</sub> |     72.67<sub>±03.83</sub> |
|                   Kotlin |    3.954<sub>±0.097</sub> |   37.82<sub>±00.18</sub> + 77.84<sub>±00.32</sub> |     72.87<sub>±04.67</sub> |
|              Python/pypy |    6.400<sub>±0.073</sub> |   64.16<sub>±00.09</sub> + 69.19<sub>±00.01</sub> |    114.49<sub>±04.52</sub> |
|             C#/.NET Core |    6.681<sub>±0.070</sub> |   33.05<sub>±00.02</sub> + 69.24<sub>±00.01</sub> |    153.27<sub>±01.94</sub> |
|                  C#/Mono |   11.415<sub>±0.250</sub> |   20.08<sub>±00.04</sub> + 69.07<sub>±00.05</sub> |    198.90<sub>±05.25</sub> |
|         Ruby/truffleruby |   51.640<sub>±1.525</sub> | 422.12<sub>±00.40</sub> + 322.40<sub>±01.61</sub> |   1125.56<sub>±59.73</sub> |
| Ruby/truffleruby (--jvm) |   71.474<sub>±0.498</sub> | 744.41<sub>±32.72</sub> + 300.52<sub>±21.73</sub> |   1702.07<sub>±78.24</sub> |
|                     Ruby |  217.074<sub>±6.499</sub> |   15.07<sub>±00.03</sub> + 68.84<sub>±00.00</sub> |  4461.27<sub>±192.83</sub> |
|             Ruby (--jit) |  218.813<sub>±7.056</sub> |   15.16<sub>±00.03</sub> + 69.14<sub>±00.00</sub> |  4118.08<sub>±250.93</sub> |
|                   Python |  241.538<sub>±6.917</sub> |    9.84<sub>±00.06</sub> + 68.58<sub>±00.00</sub> |  4648.52<sub>±396.62</sub> |
|                      Tcl |  357.500<sub>±3.871</sub> |   7.22<sub>±00.05</sub> + 400.41<sub>±00.03</sub> |  6710.92<sub>±194.07</sub> |
|                     Perl |  387.874<sub>±2.819</sub> |   8.89<sub>±00.04</sub> + 599.60<sub>±00.01</sub> |  9600.36<sub>±211.20</sub> |
|               Ruby/jruby | 493.308<sub>±11.743</sub> | 271.62<sub>±05.57</sub> + 664.67<sub>±10.39</sub> | 11016.14<sub>±247.29</sub> |

## Havlak

Testing Havlak's loop finder implementations.

[Havlak](havlak)

|     Language |                  Time, s |                                       Memory, MiB |                 Energy, J |
| :----------- | -----------------------: | ------------------------------------------------: | ------------------------: |
|      Crystal |   6.919<sub>±0.034</sub> |   3.31<sub>±00.03</sub> + 225.37<sub>±00.68</sub> |   170.28<sub>±02.29</sub> |
|      C++/g++ |  13.672<sub>±0.160</sub> |   1.49<sub>±00.00</sub> + 176.77<sub>±00.03</sub> |   294.67<sub>±03.91</sub> |
|      Nim/gcc |  13.863<sub>±0.319</sub> |   1.85<sub>±00.04</sub> + 498.51<sub>±03.35</sub> |   318.93<sub>±19.72</sub> |
|    Nim/clang |  13.891<sub>±0.151</sub> |   2.29<sub>±00.01</sub> + 504.69<sub>±00.16</sub> |   342.27<sub>±07.22</sub> |
| C#/.NET Core |  14.928<sub>±0.117</sub> | 32.35<sub>±00.05</sub> + 1357.19<sub>±39.35</sub> |   371.50<sub>±01.94</sub> |
|        Scala |  17.869<sub>±0.326</sub> |  75.99<sub>±03.94</sub> + 312.90<sub>±01.65</sub> |   559.85<sub>±12.85</sub> |
|           Go |  18.582<sub>±0.064</sub> |   3.58<sub>±00.03</sub> + 364.18<sub>±00.38</sub> |   427.98<sub>±04.67</sub> |
|       D/ldc2 |  18.731<sub>±0.256</sub> |   2.95<sub>±00.03</sub> + 475.72<sub>±04.96</sub> |   498.51<sub>±07.46</sub> |
|        D/gdc |  22.151<sub>±0.494</sub> |   6.25<sub>±00.07</sub> + 345.21<sub>±00.01</sub> |   480.81<sub>±24.95</sub> |
|        D/dmd |  23.633<sub>±0.093</sub> |   3.20<sub>±00.07</sub> + 438.16<sub>±00.34</sub> |   569.98<sub>±08.72</sub> |
|      C#/Mono |  25.318<sub>±0.340</sub> |  19.79<sub>±00.03</sub> + 314.47<sub>±01.46</sub> |   621.60<sub>±42.61</sub> |
|     Go/gccgo |  26.014<sub>±0.120</sub> |  21.01<sub>±00.32</sub> + 361.91<sub>±04.37</sub> |   663.53<sub>±10.03</sub> |
|  Python/pypy |  28.480<sub>±0.449</sub> |  65.90<sub>±00.25</sub> + 569.64<sub>±06.46</sub> |   685.08<sub>±14.01</sub> |
|       Python | 103.731<sub>±2.547</sub> |   9.02<sub>±00.03</sub> + 398.56<sub>±00.00</sub> | 2379.21<sub>±104.07</sub> |

# Tests Execution

## Environment

CPU: Intel(R) Core(TM) i7-10710U

Base Docker image: Debian GNU/Linux bullseye/sid

| Language         | Version                         |
| ---------------- | ------------------------------- |
| .NET Core        | 3.1.109                         |
| C#/.NET Core     | 3.4.1-beta4-20127-10 (d8180a5e) |
| C#/Mono          | 6.12.0.90                       |
| C/clang          | 10.0.1                          |
| C/gcc            | 10.2.0                          |
| Chez Scheme      | 9.5.4                           |
| Clojure          | "1.10.1"                        |
| Crystal          | 0.35.1                          |
| D/dmd            | v2.094.0                        |
| D/gdc            | 10.2.0                          |
| D/ldc2           | 1.23.0                          |
| Elixir           | 1.10.3                          |
| F#/.NET Core     | 10.7.0.0 for F# 4.7             |
| Go               | go1.15.2                        |
| Go/gccgo         | 10.2.0                          |
| Haskell          | 8.10.2                          |
| Java             | 15                              |
| Julia            | v"1.5.2"                        |
| Kotlin           | 1.4.10                          |
| Lua              | Lua 5.4                         |
| Lua/luajit       | LuaJIT 2.1.0-beta3              |
| MLton            | 20201002                        |
| Nim              | 1.2.6                           |
| Node.js          | v14.13.1                        |
| OCaml            | 4.11.1                          |
| PHP              | 7.4.10                          |
| Perl             | v5.30.3                         |
| Python           | 3.8.6                           |
| Python/pypy      | 7.3.2-alpha0 for Python 3.7.4   |
| Racket           | "7.8"                           |
| Ruby             | 2.7.2p137                       |
| Ruby/jruby       | 9.2.13.0                        |
| Ruby/truffleruby | 20.2.0                          |
| Rust             | 1.47.0                          |
| Scala            | 2.13.3                          |
| Swift            | swift-5.3-RELEASE               |
| Tcl              | 8.6                             |
| V                | 0.1.29                          |
| Vala             | 0.48.11                         |

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
 - `havlak` (build and run Havlak benchmarks).

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

 - [network-simple](http://hackage.haskell.org/package/network-simple) for
TCP connectivity between the tests and the test runner.

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
