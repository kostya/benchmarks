fun matgen(n: Int, seed: Double): Array<DoubleArray> {
    val a = Array<DoubleArray>(n, {
        DoubleArray(n)
    })
    val tmp = seed / n.toDouble() / n.toDouble()

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

fun notify(msg: String) {
    try {
        java.net.Socket("localhost", 9001).getOutputStream().use {
            it.write(msg.toByteArray())
        }
    } catch (e: java.io.IOException) {
        // standalone usage
    }
}

fun calc(n: Int): Double {
    val size = n / 2 * 2
    val a = matgen(size, 1.0)
    val b = matgen(size, 2.0)
    val x = matmul(a, b)
    return x[size / 2][size / 2]
}

fun main(args: Array<String>) {
    val n = if (args.size > 0) {
        Integer.parseInt(args[0])
    } else {
        100
    }

    val left = calc(101)
    val right = -18.67
    if (Math.abs(left - right) > 0.1) {
        System.err.println("${left} != ${right}")
        System.exit(1)
    }

    notify("Kotlin\t${ProcessHandle.current().pid()}")
    val start_time = System.currentTimeMillis()
    val results = calc(n)
    val elapsed = (System.currentTimeMillis() - start_time) / 1e3
    notify("stop")

    println(results)
    println("time: ${elapsed}s")
}
