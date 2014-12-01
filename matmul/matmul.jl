function matgen(n)
  tmp = 1.0 / n / n
  [ float32(tmp * (i - j) * (i + j - 2)) for i=1:n, j=1:n ]
end

n = 100
if length(ARGS) >= 1
  n = int(ARGS[1])
end
n = int(n / 2 * 2)
a = matgen(n)
b = matgen(n)
c = a * b
v = int(n/2) + 1
println(c[v, v])
