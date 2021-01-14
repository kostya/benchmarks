import arraymancer, strutils, os
import net, times
import strformat
import posix

proc matgen*(n: int, seed: float): auto =
  result = newTensor[float](@[n,n])
  let tmp = seed / (n * n).float
  for i in 0 ..< n:
    for j in 0 ..< n:
      result[i,j] = tmp * (i - j).float * (i + j).float

proc matmul*[T](a: Tensor[T], b: Tensor[T]) : auto =
  a * b

proc notify(msg: string) =
  try:
    var socket = newSocket()
    defer: socket.close()
    socket.connect("localhost", Port(9001))
    socket.send(msg)
  except:
    discard

proc calc(n: int): auto =
  let size = (n div 2) * 2
  let a = matgen(size, 1.0)
  let b = matgen(size, 2.0)
  let c = matmul(a, b)
  c[size div 2, size div 2]

when isMainModule:
  let n = if paramCount() > 0:
            parseInt(paramStr(1))
          else:
            100

  let left = calc(101)
  let right = -18.67
  if abs(left - right) > 0.1:
    stderr.writeLine(&"{left} != {right}")
    quit(1)

  var compiler = "Nim/clang"
  when defined(gcc):
    compiler = "Nim/gcc"

  notify(&"{compiler} (Arraymancer)\t{getpid()}")
  let results = calc(n)
  notify("stop")

  echo formatFloat(results, ffDefault, 8)
