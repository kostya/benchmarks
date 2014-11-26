import JSON

text = open(readall, "1.json")
j = JSON.parse(text)
coordinates = j["coordinates"]

x = 0
y = 0
z = 0

for coord in coordinates
  x += coord["x"]
  y += coord["y"]
  z += coord["z"]
end

l = length(coordinates)
println(x / l)
println(y / l)
println(z / l)
