object MatMul {
  type Matrix = Array[Array[Double]]

  def matgen(n: Int, seed: Double): Matrix = {
    var a = Array.ofDim[Double](n, n)
    val tmp = seed / n / n
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        a(i)(j) = tmp * (i - j) * (i + j)
      }
    }
    a
  }

  def matmul(a: Matrix, b: Matrix): Matrix = {
    val m = a.length
    val n = a(0).length
    val p = b(0).length

    // transpose
    var b2 = Array.ofDim[Double](n, p)
    for (i <- 0 until n)
      for (j <- 0 until p)
        b2(j)(i) = b(i)(j)

    // multiplication
    var c = Array.ofDim[Double](m, p)
    for (i <- 0 until m) {
      for (j <- 0 until p) {
        var s = 0.0
        val ai = a(i)
        val b2j = b2(j)
        for (k <- 0 until n) s += ai(k) * b2j(k)
        c(i)(j) = s
      }
    }

    c
  }

  def notify(msg: String): Unit = {
    scala.util.Using(
      (new java.net.Socket("localhost", 9001)).getOutputStream()
    ) {
      _.write(msg.getBytes())
    }
  }

  def calc(n: Int): Double = {
    val size = n / 2 * 2
    val a = matgen(size, 1.0)
    val b = matgen(size, 2.0)
    val x = matmul(a, b)
    x(size / 2)(size / 2)
  }

  def main(args: Array[String]): Unit = {
    val n = if (args.length > 0) args(0).toInt else 100

    val left = calc(101)
    val right = -18.67
    if (Math.abs(left - right) > 0.1) {
      System.err.println(s"${left} != ${right}")
      System.exit(1)
    }

    notify(s"Scala\t${ProcessHandle.current().pid()}")

    val start_time = System.nanoTime
    val results = calc(n)
    val elapsed = (System.nanoTime - start_time) / 1e9

    notify("stop")

    println(results)
    println("time: " + elapsed + "s")
  }
}
