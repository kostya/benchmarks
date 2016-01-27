using Codecs

function main()
  str_size = 10_000_000
  tries = 100

  str = repeat("a", str_size)
  str2 = ""

  print("encode: ")
  t = time()
  s = 0
  for i in range(0, tries)
    str2 = ASCIIString(encode(Base64, str))
    s += length(str2)
  end
  print(s, ", ", time() - t, "\n")

  print("decode: ")
  t = time()
  s = 0
  for i in range(0, tries)
    s += length(ASCIIString(decode(Base64, str2)))
  end
  print(s, ", ", time() - t, "\n")
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
