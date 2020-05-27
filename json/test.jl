using Pkg; Pkg.add("JSON3")

import JSON3
using Sockets

function main(text)
  jobj = JSON3.read(text)
  coordinates = jobj["coordinates"]
  len = length(coordinates)
  x = y = z = 0

  for coord in coordinates
    x += coord["x"]
    y += coord["y"]
    z += coord["z"]
  end

  println(x / len)
  println(y / len)
  println(z / len)
end

function test(text)
  x = @timed main(text)
  println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")
end

text = open("/tmp/1.json") do file
  read(file, String)
end

for i in 1:4
  test(text)
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

notify("Julia JSON3\t$(getpid())")

test(text)

notify("stop")
