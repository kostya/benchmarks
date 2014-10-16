require 'json'

json = JSON.parse(File.read('1.json'))
coordinates = json['coordinates']

x = y = z = 0

res = coordinates.each do |e|
  x += e['x']
  y += e['y']
  z += e['z']
end

p x / coordinates.length
p y / coordinates.length
p z / coordinates.length
