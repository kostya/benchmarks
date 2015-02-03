import os, strutils

type
  Matrix = seq[seq[float64]]

proc newmat(x: int, y: int): Matrix =
  result = @[]
  for i in 0..x-1:
    result.add(@[])
    for j in 0..y-1: result[i].add(0.0)

proc matgen(n: int): Matrix =
  result = newmat(n, n)
  let tmp = 1.0'f32 / float64(n) / float64(n)
  for i in 0 .. <n:
    for j in 0 .. <n:
      result[i][j] = tmp * float64(i - j) * float64(i + j)

proc matmul(a: Matrix, b: Matrix): Matrix =
  let m = a.len
  let n = a[0].len
  let p = b[0].len

  # transpose
  var b2 = newmat(n, p)
  for i in 0 .. <n:
    for j in 0 .. <p:
      b2[j][i] = b[i][j]

  # multiplication
  var c = newmat(m, p)
  for i in 0 .. <m:
   for j in 0 .. <p:
      var s = 0.0
      let ai = a[i]
      let b2j = b2[j]
      for k in 0 .. <n:
        s += ai[k] * b2j[k]
      c[i][j] = s
  result = c

var n = 100
if paramCount() > 0:
  n = parseInt(paramStr(1))
n = n div 2 * 2

let a = matgen(n)
let b = matgen(n)
let c = matmul(a, b)
echo formatFloat(c[n div 2][n div 2], ffDefault, 8)
