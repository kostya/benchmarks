# Writen by Attractive Chaos; distributed under the MIT license

def matmul(a, b)
  m = a.length
  n = a[0].length
  p = b[0].length

  # transpose
  b2 = Array.new(n) { Array.new(p) { 0 } }
  n.times do |i|
    p.times do |j|
      b2[j][i] = b[i][j]
    end
  end

  # multiplication
  c = Array.new(m) { Array.new(p) { 0 } }
  m.times do |i|
   p.times do |j|
      s = 0
      ai, b2j = a[i], b2[j]
      n.times do |k|
        s += ai[k] * b2j[k]
      end
      c[i][j] = s
    end
  end
  c
end

def matgen(n)
  tmp = 1.0 / n / n
  a = Array.new(n) { Array.new(n) { 0 } }
  n.times do |i|
    n.times do |j|
      a[i][j] = tmp * (i - j) * (i + j)
    end
  end
  a
end

n = 100
if ARGV.length >= 1
  n = ARGV[0].to_i
end
n = n / 2 * 2
a = matgen(n)
b = matgen(n)
c = matmul(a, b)
puts c[n/2][n/2]
