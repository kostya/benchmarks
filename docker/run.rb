#!/usr/bin/env ruby

unknown_cmd = true

case ARGV[0]
when "versions"
  system("./versions.rb")
  unknown_cmd = false
when "shell"
  Dir.chdir("/src")
  system("bash")
  unknown_cmd = false
when "brainfuck2"
  case ARGV[1]
  when "bench"
    Dir.chdir("/src/brainfuck2")
    system("./build.sh && ./run.sh")
    unknown_cmd = false
  when "mandel"
    Dir.chdir("/src/brainfuck2")
    system("./build.sh && ./run2.sh")
    unknown_cmd = false
  end
when "base64"
  Dir.chdir("/src/base64")
  system("./build.sh && ./run.sh")
  unknown_cmd = false
when "havlak"
  Dir.chdir("/src/base64")
  system("./build.sh && ./run.sh")
  unknown_cmd = false
when "json"
  Dir.chdir("/src/base64")
  system("./build.sh && ./run.sh")
  unknown_cmd = false
when "matmul"
  Dir.chdir("/src/base64")
  system("./build.sh && ./run.sh")
  unknown_cmd = false
end

if unknown_cmd
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
end
