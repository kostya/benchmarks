Parse big json file
-------------------

To generate json file: `ruby generate_json.rb`

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| C++ Rapid       | 0.79    | 687.1      |
| Crystal Schema  | 1.39    | 292.3      |
| Crystal Pull    | 1.75    | 1.2        |
| Crystal         | 2.27    | 1085.8     |
| Nim             | 3.32    | 1344.3     |
| Python Pypy     | 4.99    | 1365.4     |
| C++ LibJson     | 5.49    | 2796.3     |
| Rust            | 5.63    | 2909.5     |
| Go              | 6.27    | 420.9      |
| Python          | 9.85    | 1409.1     |
| D               | 9.87    | 1316.6     |
| Julia           | 10.21   | 2335.9     |
| Ruby            | 10.54   | 2086.2     |
| Javascript Node | 11.61   | 926.4      |
| C++ Boost       | 16.44   | 2915.2     |
| D Gdc           | 17.68   | 1008.4     |
| Go Gcc          | 18.69   | 494.4      |
| Ruby JRuby9k    | 18.89   | 1996.8     |
| C# Mono         | 25.58   | 4853.7     |
| Ruby JRuby      | 25.78   | 2712.6     |
| D Ldc           | 27.23   | 919.6      |
| Ruby Rbx        | 67.13   | 4681.0     |
| Scala           | 343.13  | 2373.0     |
