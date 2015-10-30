require "json"

struct Coordinate
  JSON.mapping({
    x: Float64,
    y: Float64,
    z: Float64,
  })
end

class Coordinates
  JSON.mapping({
    coordinates: {type: Array(Coordinate)},
  })
end

text = File.read("1.json")
coordinates = Coordinates.from_json(text).coordinates
len = coordinates.size
x = y = z = 0

coordinates.each do |e|
  x += e.x
  y += e.y
  z += e.z
end

p x / len
p y / len
p z / len
