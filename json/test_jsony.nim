import std/[net, strformat, posix]
import jsony

type
  Coordinate = tuple
    x, y, z: float

  CoordinateObject = object
    coordinates: seq[Coordinate]

proc notify(msg: string) =
  try:
    let socket = newSocket()
    defer: socket.close()
    socket.connect("localhost", Port(9001))
    socket.send(msg)
  except:
    discard

proc calc(text: string): Coordinate =
  let obj = text.fromJson(CoordinateObject)

  let coordinates = obj.coordinates
  let len = float(coordinates.len)
  var x, y, z: float

  for coord in coordinates:
    x += coord.x
    y += coord.y
    z += coord.z

  result = (x: x / len, y: y / len, z: z / len)

when isMainModule:
  let right = (x: 2.0, y: 0.5, z: 0.25)
  for v in ["""{"coordinates":[{"x":2.0,"y":0.5,"z":0.25}]}""",
            """{"coordinates":[{"y":0.5,"x":2.0,"z":0.25}]}"""]:
    let left = calc(v)
    if left != right:
      stderr.writeLine(&"{left} != {right}")
      quit(1)

  let text = "/tmp/1.json".readFile()

  let compiler = when defined(gcc):
    "Nim/gcc (jsony)"
  else:
    "Nim/clang (jsony)"

  notify(&"{compiler}\t{getpid()}")
  let results = calc(text)
  notify("stop")

  echo results
