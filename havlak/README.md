Havlak loop finder
------------------

Original benchmark https://code.google.com/p/multi-language-bench/, some code remastered, added Crystal, D, Nim.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Crystal         | 15.80   | 397.0      |
| Nim Gcc         | 17.26   | 913.0      |
| Nim Clang       | 17.82   | 874.4      |
| C++             | 17.72   | 174.5      |
| D               | 24.97   | 370.2      |
| D Ldc           | 25.15   | 214.9      |
| D Gdc           | 25.75   | 230.6      |
| Go Gcc          | 30.36   | 436.7      |
| Scala           | 33.38   | 341.0      |
| C# Mono         | 40.54   | 270.0      |
| Go              | 44.56   | 424.9      |
| Python Pypy     | 69.46   | 730.2      |
| Python          | 396.54  | 724.0      |

