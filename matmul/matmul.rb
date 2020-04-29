# Writen by Attractive Chaos; distributed under the MIT license
# frozen_string_literal: true

require 'socket'

def transposed(n_size, p_size, matrix_b)
  b2 = Array.new(n_size) { Array.new(p_size) { 0 } }
  n_size.times do |i|
    p_size.times do |j|
      b2[j][i] = matrix_b[i][j]
    end
  end
  b2
end

def multiplication(m_size, n_size, p_size, matrix_a, matrix_b2)
  c = Array.new(m_size) { Array.new(p_size) { 0 } }
  m_size.times do |i|
    p_size.times do |j|
      ai = matrix_a[i]
      b2j = matrix_b2[j]
      c[i][j] = n_size.times.reduce(0) { |s, k| s + ai[k] * b2j[k] }
    end
  end
  c
end

def matmul(matrix_a, matrix_b)
  m = matrix_a.length
  n = matrix_a[0].length
  p = matrix_b[0].length

  # transpose
  b2 = transposed(n, p, matrix_b)

  # multiplication
  multiplication(m, n, p, matrix_a, b2)
end

def matgen(num)
  tmp = 1.0 / num / num
  a = Array.new(num) { Array.new(num) { 0 } }
  num.times do |i|
    num.times do |j|
      a[i][j] = tmp * (i - j) * (i + j)
    end
  end
  a
end

def notify(msg)
  Socket.tcp('localhost', 9001) { |s| s.puts msg }
rescue SystemCallError
  # standalone usage
end

engine = RUBY_ENGINE
if engine == 'truffleruby'
  desc = RUBY_DESCRIPTION
  if desc.include?('Native')
    engine = 'TruffleRuby Native'
  elsif desc.include?('JVM')
    engine = 'TruffleRuby JVM'
  end
elsif engine == 'ruby' && RubyVM::MJIT.enabled?
  engine = 'Ruby JIT'
end
pid = Process.pid
notify("#{engine}\t#{pid}")

n = 100
n = ARGV[0].to_i if ARGV.length >= 1

n = n / 2 * 2
a = matgen(n)
b = matgen(n)
c = matmul(a, b)
puts c[n / 2][n / 2]

notify('stop')
