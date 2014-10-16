require "json"

text = File.read("1.json")
json = Json.parse(text) as Hash
coordinates = json["coordinates"] as Array

x = y = z = 0

res = coordinates.each do |e|
  h = e as Hash
  x += h["x"] as Float64
  y += h["y"] as Float64
  z += h["z"] as Float64
end

p x / coordinates.length
p y / coordinates.length
p z / coordinates.length
