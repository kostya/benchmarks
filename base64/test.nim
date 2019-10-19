import base64, times, strutils

let STR_SIZE = 10_000_000
let TRIES = 100
let str = strutils.repeat('a', STR_SIZE)
var str2 = ""

block:
  var t = times.epochTime()
  var s: int64 = 0
  for i in 0 ..< TRIES:
    when NimMajor <= 1 and NimMinor <= 0 and NimPatch <= 0:
      # Old version of nim also did MIME encoding,
      # which needs to be turned off:
      str2 = base64.encode(str, lineLen = 2 * STR_SIZE, newLine = "")
    else:
      str2 = base64.encode(str)
    s += len(str2)
  echo("encode: ", s, ", ", formatFloat(times.epochTime() - t, ffDefault, 6))

block:
  var t = times.epochTime()
  var s: int64 = 0
  for i in 0 ..< TRIES:
    s += len(base64.decode(str2))
  echo("decode: ", s, ", ", formatFloat(times.epochTime() - t, ffDefault, 6))
