// Written by Kajal Sinha; distributed under the MIT license
import Glibc

func matgen(n: Int) -> [[Double]]{
    var a = [[Double]](count: n, repeatedValue: [Double](count: n, repeatedValue : 0))
    let divideBy = Double(n)
    let tmp = 1.0 / divideBy / divideBy
    for i in 0..<n {
        for j in 0..<n {
            a[i][j] = tmp * Double(i - j) * Double(i + j)
        }
    }
    return a
}

func  matmul(a : [[Double]], b : [[Double]]) ->[[Double]] {
    let m = a.count
    let n = a[0].count
    let p = b[0].count
    var x = [[Double]](count: m, repeatedValue : [Double](count: p, repeatedValue : 0))
    var c = [[Double]](count: p, repeatedValue : [Double](count : n, repeatedValue: 0))
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

if Process.argc != 2 {
    print("Usage: ./matmulswift {arg}")
    exit(1)
}

let end = Int(String.fromCString(Process.arguments[1])!)!
n = end
n = n / 2 * 2


var a = matgen(n);
var b = matgen(n);
var x = matmul(a, b: b)
//print("done")
print(x[n/2][n/2])
