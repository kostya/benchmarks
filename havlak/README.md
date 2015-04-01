Havlak loop finder
------------------

Original benchmark https://code.google.com/p/multi-language-bench/, some code remastered, added Crystal, D, Nim.

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Crystal         | 15.66   | 390.9      |
| Nim             | 16.74   | 887.7      |
| C++             | 17.72   | 174.5      |
| D               | 26.78   | 380.8      |
| D Gdc           | 27.88   | 205.9      |
| D Ldc           | 30.57   | 231.6      |
| Scala           | 33.38   | 341.0      |
| Go              | 44.56   | 424.9      |
| Python Pypy     | 69.46   | 730.2      |
| Python          | 396.54  | 724.0      |

