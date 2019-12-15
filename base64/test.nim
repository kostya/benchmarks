import base64, times, strutils, strformat
import net

let STR_SIZE = 131072
let TRIES = 8192
let str = strutils.repeat('a', STR_SIZE)

try:
  var socket = newSocket()
  defer: socket.close()
  socket.connect("localhost", Port(9001))
  when defined(gcc):
    socket.send("Nim GCC")
  else:
    socket.send("Nim Clang")
except:
  discard

var str2 = base64.encode(str)
stdout.write(fmt"encode {str[..3]}... to {str2[..3]}...: ")

var t = times.epochTime()
var i = 0
var s:int64 = 0
while i < TRIES:
  str2 = base64.encode(str)
  s += len(str2)
  i += 1
echo(fmt"{s}, {formatFloat(times.epochTime() - t, ffDefault, 6)}")

var str3 = base64.decode(str2)
stdout.write(fmt"decode {str2[..3]}... to {str3[..3]}...: ")

t = times.epochTime()
i = 0
s = 0
while i < TRIES:
  str3 = base64.decode(str2)
  s += len(str3)
  i += 1
echo(fmt"{s}, {formatFloat(times.epochTime() - t, ffDefault, 6)}")
