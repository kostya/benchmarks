import json

let jobj = parseFile("1.json")

let coordinates = jobj["coordinates"].elems
let len = float(coordinates.len)
var x = 0.0
var y = 0.0
var z = 0.0

for coord in coordinates:
  x += coord["x"].fnum
  y += coord["y"].fnum
  z += coord["z"].fnum

echo x / len
echo y / len
echo z / len
