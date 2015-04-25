Matrix multiplication
---------------------

Most implementations copyied from https://github.com/attractivechaos/plb, added Scala, Crystal, Rust, Nim, Julia.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Julia Native    | 0.12    | 111.7      |
| D Ldc           | 2.01    | 68.9       |
| D               | 2.30    | 71.3       |
| D Gdc           | 2.32    | 73.3       |
| C               | 3.64    | 69.2       |
| Java            | 3.68    | 134.3      |
| Rust            | 3.72    | 101.0      |
| Nim             | 3.73    | 131.8      |
| Crystal         | 3.84    | 72.1       |
| Go              | 4.77    | 75.6       |
| Javascript V8   | 6.87    | 81.5       |
| Python Pypy     | 7.10    | 89.2       |
| Scala           | 10.26   | 154.0      |
| C# Mono         | 15.14   | 80.6       |
| Julia           | 27.05   | 128.4      |
| Ruby Topaz      | 81.41   | 206.2      |
| Ruby            | 338.40  | 82.8       |
| Python          | 447.39  | 74.0       |
| Ruby JRuby      | 412.61  | 574.9      |
| Ruby JRuby9k    | 467.99  | 602.3      |
| Ruby Rbx        | 591.70  | 325.0      |
