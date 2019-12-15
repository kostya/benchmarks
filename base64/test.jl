using Base64
using Sockets

function main(tries)
  str_size = 131072
  str = repeat("a", str_size)

  str2 = base64encode(str)
  print("encode $(str[1:4])... to $(str2[1:4]): ")

  t = time()
  s = 0
  for i in 1:tries
    str2 = base64encode(str)
    s += length(str2)
  end
  print("$s, $(time() - t)\n")

  str3 = base64decode(str2)
  print("decode $(str2[1:4])... to $(String(str3[1:4])): ")

  t = time()
  s = 0
  for i in 1:tries
    str3 = base64decode(str2)
    s += length(str3)
  end
  print("$s, $(time() - t)\n")
end

println("JIT warming up")
main(5)

println("bench")
try
  socket = connect("localhost", 9001)
  write(socket, "Julia")
  close(socket)
catch
  # standalone usage
end

x = @timed main(8192)
println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")
