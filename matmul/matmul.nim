import os, strutils
import net
import strformat
import posix

type
  Matrix = seq[seq[float]]

proc newmat(x: int, y: int): Matrix =
  result.setLen x
  for i in 0 ..< x:
    result[i] = newSeq[float](y)

proc matgen(n: int, seed: float): Matrix =
  result = newmat(n, n)
  let tmp = seed / float(n) / float(n)
  for i in 0 ..< n:
    for j in 0 ..< n:
      result[i][j] = tmp * float(i - j) * float(i + j)

proc matmul(a: Matrix, b: Matrix): Matrix =
  let m = a.len
  let n = a[0].len
  let p = b[0].len

  # transpose
  var b2 = newmat(n, p)
  for i in 0 ..< n:
    for j in 0 ..< p:
      b2[j][i] = b[i][j]

  # multiplication
  var c = newmat(m, p)
  for i in 0 ..< m:
   for j in 0 ..< p:
      var s = 0.0
      let ai = a[i]
      let b2j = b2[j]
      for k in 0 ..< n:
        s += ai[k] * b2j[k]
      c[i][j] = s
  result = c

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
  c[size div 2][size div 2]

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
  notify(&"{compiler}\t{getpid()}")
  let results = calc(n)
  notify("stop")

  echo formatFloat(results, ffDefault, 8)
