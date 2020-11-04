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

def matgen(num, seed)
  tmp = seed / num / num
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

def calc(n)
  n = n >> 1 << 1
  a = matgen(n, 1.0)
  b = matgen(n, 2.0)
  c = matmul(a, b)
  c[n >> 1][n >> 1]
end

if __FILE__ == $PROGRAM_NAME
  n = ARGV.length.positive? ? ARGV[0].to_i : 100

  left = calc(101)
  right = -18.67
  if (left - right).abs > 0.1
    warn "#{left} != #{right}"
    exit(1)
  end

  engine = RUBY_ENGINE
  if engine == 'truffleruby'
    desc = RUBY_DESCRIPTION
    if desc.include?('Native')
      engine = 'Ruby/truffleruby'
    elsif desc.include?('JVM')
      engine = 'Ruby/truffleruby (--jvm)'
    end
  elsif engine == 'ruby' && RubyVM::MJIT.enabled?
    engine = 'Ruby (--jit)'
  end

  notify("#{engine}\t#{Process.pid}")
  results = calc(n)
  notify('stop')

  puts results
end
