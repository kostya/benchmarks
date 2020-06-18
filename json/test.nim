import json
import net
import strformat
import posix

type Coordinate = tuple[x: float, y: float, z: float]

proc notify(msg: string) =
  try:
    let socket = newSocket()
    defer: socket.close()
    socket.connect("localhost", Port(9001))
    socket.send(msg)
  except:
    discard

proc calc(text: string): Coordinate =
  let jobj = parseJson(text)

  let coordinates = jobj["coordinates"].elems
  let len = float(coordinates.len)
  var x = 0.0
  var y = 0.0
  var z = 0.0

  for coord in coordinates:
    x += coord["x"].fnum
    y += coord["y"].fnum
    z += coord["z"].fnum

  result = (x: x / len, y: y / len, z: z / len)

when isMainModule:
  let left = calc("""{"coordinates":[{"x":1.1,"y":2.2,"z":3.3}]}""")
  let right = (x: 1.1, y: 2.2, z: 3.3)
  if left != right:
    stderr.writeLine(&"{left} != {right}")
    quit(1)

  let text = "/tmp/1.json".readFile()

  var compiler = "Nim Clang"
  when defined(gcc):
    compiler = "Nim GCC"
  notify(&"{compiler}\t{getpid()}")

  echo calc(text)

  notify("stop")
