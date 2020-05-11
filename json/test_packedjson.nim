import packedjson
import net
import strformat
import posix

proc notify(msg: string) =
  try:
    var socket = newSocket()
    defer: socket.close()
    socket.connect("localhost", Port(9001))
    socket.send(msg)
  except:
    discard

var text = "/tmp/1.json".readFile()

var compiler = "Nim Packedjson Clang"
when defined(gcc):
  compiler = "Nim Packedjson GCC"
notify(&"{compiler}\t{getpid()}")

let jobj = parseJson(text)

let coordinates = jobj["coordinates"]
let len = float(coordinates.len)
var x = 0.0
var y = 0.0
var z = 0.0

for coord in coordinates:
  x += coord["x"].getFloat()
  y += coord["y"].getFloat()
  z += coord["z"].getFloat()

echo x / len
echo y / len
echo z / len

notify("stop")
