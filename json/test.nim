import json

let text = readFile("./1.json")
let jobj = parseJson(text)

let coordinates = jobj["coordinates"].elems

var x = 0.0
var y = 0.0
var z = 0.0

for coord in coordinates:
  x += coord["x"].fnum
  y += coord["y"].fnum
  z += coord["z"].fnum

let len = float(len(coordinates))
echo x / len
echo y / len
echo z / len
