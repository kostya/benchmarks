import arraymancer, strutils, os
import net, times
import strformat
import posix

proc matgen*(n: int): auto =
    result = newTensor[float](@[n,n])
    let tmp = 1.0 / (n*n).float
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

proc main() =
    var n = 100

    if paramCount() > 0:
        n = parseInt(paramStr(1))

    let t = matmul(matgen(100), matgen(100))
    if abs(t[1, 1] + 19.5) > 0.5:
        quit(-1)

    var compiler = "Nim Clang"
    when defined(gcc):
        compiler = "Nim GCC"
    notify(&"{compiler} Arraymancer\t{getpid()}")

    let a, b = matgen(n)
    let c = matmul(a, b)
    echo formatFloat(c[n div 2, n div 2], ffDefault, 16)

    notify("stop")

main()
