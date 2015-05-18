Base64 encode and decode
------------------------

Base64 is a string transformation algorithm. For most languages it is implemented in the language itself (except Ruby, Python which implemented in C), so I think it's a good benchmark for the language.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| D Gdc           | 2.48    | 44.3       |
| C               | 2.70    | 32.3       |
| Ruby            | 2.73    | 125.3      |
| D Ldc           | 3.27    | 44.1       |
| Crystal         | 3.68    | 82.7       |
| Nim             | 4.13    | 52.4       |
| Ruby Rbx        | 4.29    | 30.7       |
| C++ Openssl     | 5.45    | 65.2       |
| D               | 6.18    | 89.1       |
| Python          | 7.62    | 52.6       |
| Rust            | 7.53    | 41.0       |
| Javascript Node | 7.93    | 777.1      |
| Python Pypy     | 8.22    | 114.6      |
| Ruby JRuby      | 16.76   | 496.6      |
| Julia           | 16.86   | 163.9      |
| Ruby JRuby9k    | 17.72   | 417.1      |
| C# Mono         | 18.11   | 119.7      |
| Go              | 21.24   | 94.2       |
| Scala           | 35.06   | 301.2      |
| Go Gcc          | 39.56   | 185.5      |
