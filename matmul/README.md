Matrix multiplication
---------------------

Most implementations copyied from https://github.com/attractivechaos/plb, added Scala, Crystal, Rust, Nimrod.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C               | 3.67    | 69.2       |
| Java            | 3.75    | 134.3      |
| Rust            | 3.79    | 100.9      |
| D               | 3.80    | 71.8       |
| Nimrod          | 3.83    | 135.4      |
| Crystal         | 3.88    | 72.1       |
| Go              | 4.83    | 74.5       |
| Javascript V8   | 6.77    | 81.5       |
| Python Pypy     | 6.91    | 89.2       |
| Scala           | 10.71   | 154.0      |
| Ruby Topaz      | 81.41   | 206.2      |
| Python          | 447.39  | 74.0       |
| Ruby JRuby      | 412.61  | 574.9      |
| Ruby Rbx        | 591.70  | 325.0      |
| Ruby            | 1039.25 | 500.6      |
