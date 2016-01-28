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
  x = @timed main()
  println(STDERR, "Elapsed: $(x[2]), Allocated: $(x[3]), GC Time: $(x[4])")
end

test()
