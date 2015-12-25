require "json"

text = File.read("1.json")
jobj = JSON.parse(text)
coordinates = jobj["coordinates"]
len = coordinates.size
x = y = z = 0

coordinates.each do |coord|
  x += coord["x"].as_f
  y += coord["y"].as_f
  z += coord["z"].as_f
end

p x / len
p y / len
p z / len
