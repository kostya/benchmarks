Base64 encode and decode
------------------------

Base64 is a string transformation algorithm. For most languages it is implemented in the language itself (except Ruby, Python which implemented in C), so I think it's a good benchmark for the language.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Ruby            | 2.73    | 125.3      |
| Crystal         | 3.37    | 82.4       |
| Nimrod          | 4.24    | 52.4       |
| Ruby Rbx        | 4.29    | 30.7       |
| C++             | 5.69    | 65.5       |
| D               | 6.23    | 45.0       |
| Python          | 7.62    | 52.6       |
| Javascript Node | 7.80    | 777.1      |
| Rust            | 8.02    | 42.9       |
| Python Pypy     | 8.22    | 114.6      |
| Ruby Jruby      | 16.57   | 480.9      |
| Scala           | 35.52   | 301.2      |
| Go              | 37.90   | 116.8      |
