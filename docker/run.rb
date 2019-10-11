#!/usr/bin/env ruby

unknown_cmd = true

case ARGV[0]
when "versions"
  system("./versions.rb")
  unknown_cmd = false
when "brainfuck"
  case ARGV[1]
  when "bench"
    puts "bench"
    unknown_cmd = false
  when "mendel"
    puts "mender"
  end
end

if unknown_cmd
  puts <<-EOF
Usage: run.rb [command]

Commands:
  versions              Print installed language versions
  brainfuck2 bench      Build and run Brainfuck2 bench.b benchmarks
EOF
end
