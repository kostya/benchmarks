// Written by Kajal Sinha; distributed under the MIT license
import Glibc

func matgen(_ n: Int) -> [[Double]]{
    var a = Array(repeating: Array<Double>(repeating: 0, count: n), count: n)
    let divideBy = Double(n)
    let tmp = 1.0 / divideBy / divideBy
    for i in 0..<n {
        for j in 0..<n {
            a[i][j] = tmp * Double(i - j) * Double(i + j)
        }
    }
    return a
}

func matmul(_ a : [[Double]], b : [[Double]]) ->[[Double]] {
    let m = a.count
    let n = a[0].count
    let p = b[0].count
    var x = Array(repeating: Array<Double>(repeating: 0, count: p), count: m)
    var c = Array(repeating: Array<Double>(repeating: 0, count: n), count: p)
    for  i in 0..<n  {// transpose
        for j in 0..<p {
            c[j][i] = b[i][j];
        }
    }
    
    for i in 0..<m {
        for j in 0..<p {
            var s = 0.0
            for k in 0..<n{
                s += a[i][k] * c[j][k];
            }
            x[i][j] = s;
        }
    }
    return x
}

var n = 100;

if CommandLine.argc != 2 {
    print("Usage: ./matmulswift {arg}")
    exit(1)
}

let end = Int(CommandLine.arguments[1])!
n = end
n = n / 2 * 2


var a = matgen(n);
var b = matgen(n);
var x = matmul(a, b: b)
//print("done")
print(x[n/2][n/2])
