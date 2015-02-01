import os, strutils

type
  Matrix = object
    m, n: int
    s: seq[float64]

proc `[]`(a: Matrix, x, y: int): float64 =
  a.s[x*a.n+y]

proc `[]=`(a: var Matrix, x, y: int, v: float64) =
  a.s[x*a.n+y] = v

proc newmat(n, m: int): Matrix =
  result.n = n
  result.m = n
  result.s = newSeq[float64](n*m)

proc matgen(n: int): Matrix =
  result = newmat(n, n)
  let tmp = 1.0'f32 / float64(n) / float64(n)
  for i in 0 .. <n:
    for j in 0 .. <n:
      result[i,j] = tmp * float64(i - j) * float64(i + j)

proc matmul(a: Matrix, b: Matrix): Matrix =
  let m = a.n
  let n = a.m
  let p = b.m

  # transpose
  var b2 = newmat(n, p)
  for i in 0..n-1:
    for j in 0..p-1:
      b2[j,i] = b[i,j]

  # multiplication
  result = newmat(m, p)
  for i in 0..m-1:
    for j in 0..p-1:
      var s = 0.0
      for k in 0..n-1:
        s += a[i,k] * b2[j,k]
      result[i,j] = s

block:
  var n = 100
  if paramCount() > 0:
    n = parseInt(paramStr(1))
  n = n div 2 * 2

  let a = matgen(n)
  let b = matgen(n)
  let c = matmul(a, b)
  echo formatFloat(c[n div 2, n div 2], ffDefault, 8)
