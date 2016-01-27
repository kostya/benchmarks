import JSON

function main()
  text = open(readall, "1.json")
  jobj = JSON.parse(text)
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

function test()
  for i in 1:2    # First time it's also JIT compiling!
    x = @timed main()
    if i == 2
      println("Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")
    end
  end
end

test()
