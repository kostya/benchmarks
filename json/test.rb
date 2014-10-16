require 'json'

json = JSON.parse(File.read('1.json'))
coordinates = json['coordinates']
res = coordinates.inject({:x => 0, :y => 0, :z => 0}) do |r, e|
  r[:x] += e['x']  
  r[:y] += e['y']
  r[:z] += e['z']
  r
end

p res[:x] / coordinates.length
p res[:y] / coordinates.length
p res[:z] / coordinates.length
