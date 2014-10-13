Brainfuck interepter and benchmark
----------------------------------

In benchmark i using only standard language containers and compile keys, without any kind of hacks to fair compare.

To compile all: `sh build.sh`

To run all: `sh run.sh`

Intel(R) Core(TM) i5-2400 CPU @ 3.10GHz (Ubuntu 14.04.1 LTS x86_64)

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C++             | 5.18    | 1.1        |
| Nimrod          | 5.35    | 0.6        |
| Crystal         | 7.18    | 0.9        |
| Go              | 7.47    | 1.0        |
| D               | 9.40    | 0.9        |
| Javascript V8   | 9.41    | 8.1        |
| Rust            | 9.91    | 5.0        |
| Scala           | 11.79   | 1.6        |
| Javascript Node | 17.72   | 9.5        |
| Python Pypy     | 18.13   | 22.4       |
| Ruby Topaz      | 112.91  | 36.0       |
| Ruby            | 226.86  | 8.0        |
| Python          | 452.44  | 4.9        |


# Versions:

* gcc (Ubuntu 4.8.2-19ubuntu1) 4.8.2
* Nimrod Compiler Version 0.9.5 (2014-05-25) [Linux: amd64]
* Crystal 0.5.0 [fa9f28e] (Tue Sep 30 15:43:16 UTC 2014)
* go version go1.3.1 linux/amd64
* DMD64 D Compiler v2.066.0
* V8 version 3.29.62 (candidate)
* rustc 0.13.0-nightly (adb44f53d 2014-10-12 00:07:15 +0000)
* Scala code runner version 2.11.1
* Nodejs v0.10.25
* PyPy 2.1.0-alpha0 with GCC 4.6.1
* topaz (ruby-1.9.3p125) (git rev b95c858) [x86_64-linux]
* ruby 2.1.2p95 (2014-05-08 revision 45877) [x86_64-linux]
* Python 2.7.6
