using Base64

function main(tries)
  str_size = 10_000_000
  str = repeat("a", str_size)
  str2 = ""

  print("encode: ")
  t = time()
  s = 0
  for i in 1:tries
    str2 = base64encode(str)
    s += length(str2)
  end
  print(s, ", ", time() - t, "\n")

  print("decode: ")
  t = time()
  s = 0
  for i in 1:tries
    s += length(base64decode(str2))
  end
  print(s, ", ", time() - t, "\n")
end

println("JIT warming up")
main(5)

println("bench")
x = @timed main(100)
println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")
