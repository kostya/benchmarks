import base64, times, strutils, strformat
import net
import posix

proc notify(msg: string) =
  try:
    var socket = newSocket()
    defer: socket.close()
    socket.connect("localhost", Port(9001))
    socket.send(msg)
  except:
    discard

when isMainModule:
  for (src, dst) in [("hello", "aGVsbG8="), ("world", "d29ybGQ=")]:
    let encoded = base64.encode(src)
    if encoded != dst:
      stderr.writeLine(&"{encoded} != {dst}")
      quit(1)
    let decoded = base64.decode(dst)
    if decoded != src:
      stderr.writeLine(&"{decoded} != {src}")
      quit(1)

  const STR_SIZE = 131072
  const TRIES = 8192

  let str = strutils.repeat('a', STR_SIZE)
  let str2 = base64.encode(str)
  let str3 = base64.decode(str2)

  var compiler = "Nim/clang"
  when defined(gcc):
    compiler = "Nim/gcc"
  notify(&"{compiler}\t{getpid()}")

  let t = times.epochTime()
  var s_encoded = 0
  for i in 1 .. TRIES:
    s_encoded += len(base64.encode(str))
  let t_encoded = formatFloat(times.epochTime() - t, ffDefault, 6)

  let t1 = times.epochTime()
  var s_decoded = 0
  for i in 1 .. TRIES:
    s_decoded += len(base64.decode(str2))
  let t_decoded = formatFloat(times.epochTime() - t1, ffDefault, 6)

  notify("stop")

  echo(fmt"encode {str[..3]}... to {str2[..3]}...: {s_encoded}, {t_encoded}")
  echo(fmt"decode {str2[..3]}... to {str3[..3]}...: {s_decoded}, {t_decoded}")
