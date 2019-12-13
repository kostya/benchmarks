#!/usr/bin/env ruby

case ARGV[0]
when "versions"
  exec("./versions.rb")
when "shell"
  Dir.chdir("/src")
  exec("bash")
when "brainfuck2"
  case ARGV[1]
  when "bench"
    Dir.chdir("/src/brainfuck2")
    exec("./build.sh && ./run.sh")
  when "mandel"
    Dir.chdir("/src/brainfuck2")
    exec("./build.sh && ./run2.sh")
  end
when "base64"
  Dir.chdir("/src/base64")
  exec("./build.sh && ./run.sh")
when "havlak"
  Dir.chdir("/src/havlak")
  exec("./build.sh && ./run.sh")
when "json"
  Dir.chdir("/src/json")
  exec("./build.sh && ./run.sh")
when "matmul"
  Dir.chdir("/src/matmul")
  exec("./build.sh && ./run.sh")
end

puts <<-EOF
Usage: run.rb [command]

Commands:
  versions              Print installed language versions
  shell                 Start the shell
  brainfuck2 bench      Build and run Brainfuck2 bench.b benchmarks
  brainfuck2 mandel     Build and run Brainfuck2 mandel.b benchmarks
  base64                Build and run Base64 benchmarks
  json                  Build and run Json benchmarks
  matmul                Build and run Matmul benchmarks
  havlak                Build and run Havlak benchmarks
EOF
