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

function test()
  for i in 1:2    # First time it's also JIT compiling!
    x = @timed main()
    if i == 2
      println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")
    end
  end
end

test()
