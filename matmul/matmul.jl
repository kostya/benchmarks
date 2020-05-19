using Sockets

function matgen(n)
  tmp = 1.0 / n / n
  [ tmp * (i - j) * (i + j - 2) for i=1:n, j=1:n ]
end

function mul(a, b)  
  m,n = size(a)
  q,p = size(b)
  @assert n == q

  # transpose a for cache-friendliness
  aT = zeros(n,m)
  @simd for i = 1:m
    for j = 1:n      
      @inbounds aT[j,i] = a[i,j]
    end  
  end

  out = zeros(m,p)
  @simd for i = 1:m
    for j = 1:p
      z = 0.0
      for k = 1:n
        @inbounds z += aT[k,i]*b[k,j]
      end
      @inbounds out[i,j] = z
    end
  end
  out
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

function notify(msg)
  try
    socket = connect("localhost", 9001)
    write(socket, msg)
    close(socket)
  catch
    # standalone usage
  end
end

function test()
  n = 100
  if length(ARGS) >= 1
    n = parse(Int, ARGS[1])
  end

  t = mul(matgen(100), matgen(100))
  if abs(t[1][1] + 19.5) > 0.5
    exit(-1)
  end

  notify("Julia (no BLAS)\t$(getpid())")

  x = @timed main(n)
  println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")

  notify("stop")
end

test()
