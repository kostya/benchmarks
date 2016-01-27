function matgen(n)
  tmp = 1.0 / n / n
  [ tmp * (i - j) * (i + j - 2) for i=1:n, j=1:n ]
end

function main()
  n = 100
  if length(ARGS) >= 1
    n = parse(Int, ARGS[1])
  end
  t = time()
  n = round(Int, n / 2 * 2)
  a = matgen(n)
  b = matgen(n)
  c = a * b
  v = round(Int, n/2) + 1
  println(c[v, v])
  println(time() - t)
end

@time main()
