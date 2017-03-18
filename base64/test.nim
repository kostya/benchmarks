import base64, times, strutils

let STR_SIZE = 10_000_000
let TRIES = 100
let str = strutils.repeat('a', STR_SIZE)
var str2 = ""

var t = times.epochTime()
var i = 0
var s:int64 = 0
while i < TRIES:
  # set large line length to avoid adding line breaks,
  # outputing identical encoded strings as in other languages
  str2 = base64.encode(str, 2 * STR_SIZE)
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
