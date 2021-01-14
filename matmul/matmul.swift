// Written by Kajal Sinha; distributed under the MIT license
import Glibc

func matgen(_ n: Int, _ seed: Double) -> [[Double]] {
    var a = Array(repeating: Array<Double>(repeating: 0, count: n), count: n)
    let divideBy = Double(n)
    let tmp = seed / divideBy / divideBy
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

func notify(_ msg: String) {
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
        msg.withCString { (cstr: UnsafePointer<Int8>) -> Void in
            send(sock, cstr, Int(strlen(cstr)), 0)
            close(sock)
        }
    }
}

func calc(_ n: Int) -> Double {
    let size = n / 2 * 2
    let a = matgen(size, 1.0)
    let b = matgen(size, 2.0)
    let x = matmul(a, b: b)
    return x[size / 2][size / 2]
}

let _ = { () -> () in
    let n = CommandLine.argc > 1 ? Int(CommandLine.arguments[1])! : 100

    let left = calc(101)
    let right = -18.67
    if abs(left - right) > 0.1 {
        fputs("\(left) != \(right)\n", stderr)
        exit(1)
    }

    notify("Swift\t\(getpid())")
    let results = calc(n)
    notify("stop")

    print(results)
} ()
