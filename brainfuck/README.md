Brainfuck interepter and benchmark
----------------------------------

In benchmark i using only standard language containers and compile keys, without any kind of hacks to fair compare.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C++             | 5.18    | 1.1        |
| Nimrod          | 5.35    | 0.6        |
| Crystal         | 6.98    | 0.9        |
| Go              | 7.47    | 1.0        |
| D               | 9.40    | 0.9        |
| Javascript V8   | 9.41    | 8.1        |
| Rust            | 9.91    | 5.0        |
| Scala           | 11.79   | 1.6        |
| Javascript Node | 17.72   | 9.5        |
| Python Pypy     | 20.12   | 20.8       |
| Ruby JRuby      | 100.86  | 96.5       |
| Ruby Topaz      | 112.91  | 36.0       |
| Ruby            | 226.86  | 8.0        |
| Python          | 452.44  | 4.9        |
| Ruby Rbx        | 472.08  | 45.0       |
