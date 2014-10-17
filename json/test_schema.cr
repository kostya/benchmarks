require "json"

struct Coordinate
  json_mapping({
    x: {type: Float64},
    y: {type: Float64},
    z: {type: Float64},
  })
end

class Coordinates
  json_mapping({
    coordinates: {type: Array(Coordinate)}
  })
end

text = File.read("1.json")
coordinates = Coordinates.from_json(text).coordinates

x = y = z = 0

res = coordinates.each do |e|
  x += e.x
  y += e.y
  z += e.z
end

p x / coordinates.length
p y / coordinates.length
p z / coordinates.length
