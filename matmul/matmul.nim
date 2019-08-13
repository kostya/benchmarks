import os, strutils

type
  Matrix = seq[seq[float]]

proc newmat(x: int, y: int): Matrix =
  result.setLen x
  for i in 0 ..< x:
    result[i] = newSeq[float](y)

proc matgen(n: int): Matrix =
  result = newmat(n, n)
  let tmp = 1.0 / float(n) / float(n)
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
      for k in 0 ..< n:
        s += a[i][k] * b2[j][k]
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
