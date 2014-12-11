Base64 encode and decode
------------------------

Base64 is a string transformation algorithm. For most languages it is implemented in the language itself (except Ruby, Python which implemented in C), so I think it's a good benchmark for the language.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Ruby            | 2.73    | 125.3      |
| Crystal         | 3.37    | 106.2      |
| Nimrod          | 4.20    | 52.4       |
| Ruby Rbx        | 4.29    | 30.7       |
| C++             | 5.69    | 65.5       |
| D               | 6.05    | 43.9       |
| Python          | 7.62    | 52.6       |
| Rust            | 7.84    | 42.9       |
| Javascript Node | 7.93    | 777.1      |
| Python Pypy     | 8.22    | 114.6      |
| Ruby JRuby      | 16.57   | 480.9      |
| Julia           | 19.08   | 155.7      |
| Go              | 21.24   | 94.2       |
| Scala           | 35.06   | 301.2      |
