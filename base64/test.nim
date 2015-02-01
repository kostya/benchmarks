import base64, times, strutils

let STR_SIZE = 10_000_000
let TRIES = 100
let str = strutils.repeatChar(STR_SIZE, 'a')
var str2 = ""

var t = times.epochTime()
var i = 0
var s:int64 = 0
while i < TRIES:
  str2 = base64.encode(str)
  s += len(str2)
  i += 1
echo("encode: ", s, ", ", formatFloat(times.epochTime() - t, ffDefault, 6))

t = times.epochTime()
i = 0
s = 0
while i < TRIES:
  s += len(base64.decode(str2))
  i += 1
echo("decode: ", s, ", ", formatFloat(times.epochTime() - t, ffDefault, 6))
