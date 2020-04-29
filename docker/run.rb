#!/usr/bin/env ruby
# frozen_string_literal: true

CHANGE_CMD = <<~CMD

  Please change the CPU governor:

  $ for CPUFREQ in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; do [ -f $CPUFREQ ] || continue; echo -n performance > $CPUFREQ; done
CMD

def check_perf
  puts 'Checking CPU governor of all CPUs/cores:'
  Dir['/sys/devices/system/cpu/cpu*/cpufreq/scaling_governor'].each do |f|
    governon = File.read(f).strip
    puts "- #{f}: #{governon}"
    abort(CHANGE_CMD) if governon != 'performance'
  end
end

case ARGV[0]
when 'versions'
  exec('./versions.rb')
when 'shell'
  check_perf
  Dir.chdir('/src')
  exec('bash')
when 'brainfuck2'
  case ARGV[1]
  when 'bench'
    Dir.chdir('/src/brainfuck2')
    check_perf
    exec('./build.sh && ./run.sh')
  when 'mandel'
    Dir.chdir('/src/brainfuck2')
    check_perf
    exec('./build.sh && ./run2.sh')
  end
when 'base64'
  Dir.chdir('/src/base64')
  check_perf
  exec('./build.sh && ./run.sh')
when 'havlak'
  Dir.chdir('/src/havlak')
  check_perf
  exec('./build.sh && ./run.sh')
when 'json'
  Dir.chdir('/src/json')
  check_perf
  exec('./build.sh && ./run.sh')
when 'matmul'
  Dir.chdir('/src/matmul')
  check_perf
  exec('./build.sh && ./run.sh')
end

puts <<~USAGE
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
USAGE
