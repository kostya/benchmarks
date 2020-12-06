#!/usr/bin/env ruby
# frozen_string_literal: true

case ARGV[0]
when 'versions'
  exec('./versions.rb')
when 'shell'
  Dir.chdir('/src')
  exec('bash')
when 'brainfuck'
  case ARGV[1]
  when 'bench'
    Dir.chdir('/src/brainfuck')
    exec('make run')
  when 'mandel'
    Dir.chdir('/src/brainfuck')
    exec('make run2')
  end
when 'base64'
  Dir.chdir('/src/base64')
  exec('make run')
when 'json'
  Dir.chdir('/src/json')
  exec('make run')
when 'matmul'
  Dir.chdir('/src/matmul')
  exec('make run')
end

puts <<~USAGE
  Usage: run.rb [command]

  Commands:
    versions              Print installed language versions
    shell                 Start the shell
    brainfuck bench       Build and run Brainfuck bench.b benchmarks
    brainfuck mandel      Build and run Brainfuck mandel.b benchmarks
    base64                Build and run Base64 benchmarks
    json                  Build and run Json benchmarks
    matmul                Build and run Matmul benchmarks
USAGE
