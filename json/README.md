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
| Nimrod          | 3.53    | 1377.3     |
| Python Pypy     | 4.99    | 1365.4     |
| Rust            | 5.24    | 1519.1     |
| C++ LibJson     | 5.49    | 2796.3     |
| Go              | 6.27    | 420.9      |
| Python          | 9.85    | 1409.1     |
| Julia           | 10.48   | 2342.9     |
| Ruby            | 10.54   | 2086.2     |
| Javascript Node | 11.61   | 926.4      |
| C++ Boost       | 16.44   | 2915.2     |
| Ruby JRuby      | 25.78   | 2712.6     |
| D               | 35.39   | 926.6      |
| Ruby Rbx        | 67.13   | 4681.0     |
| Scala           | 343.13  | 2373.0     |
