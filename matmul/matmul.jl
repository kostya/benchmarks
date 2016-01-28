function matgen(n)
  tmp = 1.0 / n / n
  [ tmp * (i - j) * (i + j - 2) for i=1:n, j=1:n ]
end

function mul_line(n, linea, lineb)
  s = 0.0
  for i=1:n
    s += linea[i] * lineb[i]
  end
  s
end

function mul(a, b)
  m = size(a)[1]
  n = size(a)[2]
  p = size(b)[2]

  b2 = [ b[j, i] for i=1:p, j=1:n ]
  [ mul_line(n, a[i,:], b2[j,:]) for i=1:m, j=1:p ]
end

function main(n)
  t = time()
  n = round(Int, n / 2 * 2)
  a = matgen(n)
  b = matgen(n)
  c = mul(a, b)
  v = round(Int, n/2) + 1
  println(c[v, v])
  println(time() - t)
end

function test()
  n = 100
  if length(ARGS) >= 1
    n = parse(Int, ARGS[1])
  end

  println(STDERR, "warming")
  main(200)

  println(STDERR, "bench")
  x = @timed main(n)
  println(STDERR, "Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")
end

test()
