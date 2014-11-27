require "json"

text = File.read("1.json")
jobj = Json.parse(text) as Hash
coordinates = jobj["coordinates"] as Array
len = coordinates.length
x = y = z = 0

coordinates.each do |e|
  coord = e as Hash
  x += coord["x"] as Float64
  y += coord["y"] as Float64
  z += coord["z"] as Float64
end

p x / len
p y / len
p z / len
