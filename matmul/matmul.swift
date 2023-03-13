// Written by Kajal Sinha; distributed under the MIT license

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

    // Transpose
    for i in 0..<n  {
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

func calc(_ n: Int) -> Double {
    let size = n / 2 * 2
    let a = matgen(size, 1.0)
    let b = matgen(size, 2.0)
    let x = matmul(a, b: b)
    return x[size / 2][size / 2]
}

@main
struct Matmul {
    static func main() {
        let n = CommandLine.argc > 1 ? Int(CommandLine.arguments[1])! : 100

        let left = calc(101)
        let right = -18.67
        if abs(left - right) > 0.1 {
            fatalError("\(left) != \(right)")
        }

        notify_with_pid("Swift")
        let results = calc(n)
        notify("stop")

        print(results)
    }
}
