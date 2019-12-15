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

let sock = socket(AF_INET, Int32(SOCK_STREAM.rawValue), 0)
var serv_addr = (
    sa_family_t(AF_INET),
    in_port_t(htons(9001)),
    in_addr(s_addr: 0),
    (0,0,0,0,0,0,0,0))
inet_pton(AF_INET, "127.0.0.1", &serv_addr.2)
let len = MemoryLayout.stride(ofValue: serv_addr)
let rc = withUnsafePointer(to: &serv_addr) { ptr -> Int32 in
    return ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) {
        bptr in return connect(sock, bptr, socklen_t(len))
    }
}
if rc == 0 {
    "Swift".withCString { (cstr: UnsafePointer<Int8>) -> Void in
        send(sock, cstr, Int(strlen(cstr)), 0)
        close(sock)
    }
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
print(x[n/2][n/2])
