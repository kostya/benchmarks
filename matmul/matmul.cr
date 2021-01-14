require "socket"

def matmul(a, b)
  m = a.size
  n = a[0].size
  p = b[0].size
  # transpose
  b2 = Array.new(n) { Array.new(p, 0.0) }
  (0...n).each do |i|
    (0...p).each do |j|
      b2[j][i] = b[i][j]
    end
  end
  # multiplication
  c = Array.new(m) { Array.new(p, 0.0) }
  c.each_with_index do |ci, i|
    ai = a[i]
    b2.each_with_index do |b2j, j|
      s = 0.0
      b2j.each_with_index do |b2jv, k|
        s += ai[k] * b2jv
      end
      ci[j] = s
    end
  end
  c
end

def matgen(n, seed)
  tmp = seed / n / n
  a = Array.new(n) { Array.new(n, 0.0) }
  (0...n).each do |i|
    (0...n).each do |j|
      a[i][j] = tmp * (i - j) * (i + j)
    end
  end
  a
end

def notify(msg)
  TCPSocket.open("localhost", 9001) { |s|
    s.puts msg
  }
rescue
  # standalone usage
end

def calc(n)
  n = n >> 1 << 1
  a = matgen(n, 1.0)
  b = matgen(n, 2.0)
  c = matmul(a, b)
  c[n >> 1][n >> 1]
end

class EntryPoint
  n = (ARGV[0]? || 100).to_i

  left = calc(101)
  right = -18.67
  if (left - right).abs > 0.1
    STDERR.puts "#{left} != #{right}"
    exit(1)
  end

  notify("Crystal\t#{Process.pid}")
  results = calc(n)
  notify("stop")

  puts results
end
