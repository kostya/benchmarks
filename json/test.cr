require "json"

text = File.read("1.json")
json = Json.parse(text) as Hash
coordinates = json["coordinates"] as Array
res = coordinates.inject({:x => 0.0, :y => 0.0, :z => 0.0}) do |r, e|
  h = e as Hash
  r[:x] += h["x"] as Float64
  r[:y] += h["y"] as Float64
  r[:z] += h["z"] as Float64
  r
end

p coordinates.length
p res
p res[:x] / coordinates.length
p res[:y] / coordinates.length
p res[:z] / coordinates.length
