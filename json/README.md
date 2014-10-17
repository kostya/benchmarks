Parse big json file
-------------------

To generate json file: `ruby generate_json.rb`

To compile all: `sh build.sh`

To run all: `sh run.sh`

# Benchmark

| Language        | Time,s  | Memory, Mb |
| --------------- | ------- | ---------- |
| Crystal Schema  | 1.40    | 290.9      |
| Crystal Pull    | 1.75    | 1.2        |
| Crystal         | 2.31    | 1078.8     |
| Nimrod          | 3.05    | 1328.3     |
| Python Pypy     | 4.56    | 1365.4     |
| Rust            | 5.27    | 1549.1     |
| Go              | 5.94    | 430.6      |
| Python          | 9.85    | 1409.1     |
| Ruby            | 10.35   | 2113.2     |
| Javascript Node | 11.01   | 923.4      |
| C++ Boost       | 16.44   | 2915.2     |
| Ruby JRuby      | 25.78   | 2712.6     |
| D               | 34.44   | 926.6      |
| Ruby Rbx        | 67.13   | 4681.0     |
| Scala           | 343.13  | 2373.0     |

