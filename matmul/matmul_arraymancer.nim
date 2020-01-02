import arraymancer, strutils, os
import net, times

proc matgen*(n: int): auto =
    result = newTensor[float](@[n,n])
    let tmp = 1.0 / (n*n).float
    for i in 0 ..< n:
        for j in 0 ..< n:
            result[i,j] = tmp * (i - j).float * (i + j).float

proc matmul*[T](a: Tensor[T], b: Tensor[T]) : auto =
    a * b

proc main() =
    try:
        var socket = newSocket()
        defer: socket.close()
        socket.connect("localhost", Port(9001))
        when defined(gcc):
            socket.send("Nim GCC Arraymancer")
        else:
            socket.send("Nim Clang Arraymancer")
    except:
        discard

    var n = 100

    if paramCount()>0:
        n = parseInt(paramStr(1))

    let a, b = matgen(n)
    let c = matmul(a, b)
    echo formatFloat(c[n div 2, n div 2], ffDefault, 16)

main()