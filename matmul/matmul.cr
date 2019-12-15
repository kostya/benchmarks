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

def matgen(n)
  tmp = 1.0 / n / n
  a = Array.new(n) { Array.new(n, 0.0) }
  (0...n).each do |i|
    (0...n).each do |j|
      a[i][j] = tmp * (i - j) * (i + j)
    end
  end
  a
end

begin
  TCPSocket.open("localhost", 9001) { |s|
    s.puts "Crystal"
  }
rescue
  # standalone usage
end

n = (ARGV[0]? || 100).to_i
n = n >> 1 << 1
a = matgen(n)
b = matgen(n)
c = matmul(a, b)
puts c[n >> 1][n >> 1]
