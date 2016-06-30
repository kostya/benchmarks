fun matgen(n: Int): Array<DoubleArray> {
    val a = Array<DoubleArray>(n, {
        DoubleArray(n)
    })
    val tmp = 1.0 / n.toDouble() / n.toDouble()
    
    for (i in 0..n - 1) {
        for (j in 0..n - 1) {
            a[i][j] = tmp * (i - j).toDouble() * (i + j).toDouble()
        }
    }

    return a
}

fun matmul(a: Array<DoubleArray>, b: Array<DoubleArray>): Array<DoubleArray> {
    val m = a.size
    val n = a[0].size
    val p = b[0].size
    val x = Array<DoubleArray>(m, {
        DoubleArray(p)
    })
    val c = Array<DoubleArray>(p, {
        DoubleArray(n)
    })

    for (i in 0..n - 1) {
        for (j in 0..p - 1) {
            c[j][i] = b[i][j]
        }
    }

    for (i in 0..m - 1) {
        for (j in 0..p - 1) {
            var s = 0.0
            
            for (k in 0..n - 1) {
                s += a[i][k] * c[j][k]
            }
            x[i][j] = s
        }
    }
    
    return x
}

fun main(args: Array<String>) {
    val n =
        if (args.size >= 1) { Integer.parseInt(args[0]) / 2 * 2 }
        else { 25 }

    val t = matmul(matgen(500), matgen(500))
    println("warmup: ${t[1][1]}")

    val start_time = System.currentTimeMillis()
    val a = matgen(n)
    val b = matgen(n)
    val x = matmul(a, b)
    
    println(x[n / 2][n / 2])
    println("time: ${(System.currentTimeMillis() - start_time) / 1e3}s")
}
