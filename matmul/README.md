Matrix multiplication
---------------------

Most implementations copyied from https://github.com/attractivechaos/plb, added Scala, Crystal, Rust, Nim, Julia.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Julia Native    | 0.69    | 119.6      |
| D               | 2.30    | 71.3       |
| D Gdc           | 2.32    | 73.3       |
| C               | 3.64    | 69.2       |
| Java            | 3.68    | 134.3      |
| Nim             | 3.73    | 131.8      |
| Rust            | 3.79    | 100.9      |
| Crystal         | 3.84    | 72.1       |
| Go              | 4.77    | 75.6       |
| Javascript V8   | 6.87    | 81.5       |
| Python Pypy     | 7.10    | 89.2       |
| Scala           | 10.26   | 154.0      |
| Julia           | 27.74   | 159.7      |
| Ruby Topaz      | 81.41   | 206.2      |
| Ruby            | 338.40  | 82.8       |
| Python          | 447.39  | 74.0       |
| Ruby JRuby      | 412.61  | 574.9      |
| Ruby JRuby9k    | 467.99  | 602.3      |
| Ruby Rbx        | 591.70  | 325.0      |
