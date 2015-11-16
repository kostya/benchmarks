object MatMul {
  type Matrix = Array[Array[Double]]

  def matgen(n: Int) : Matrix = {
    var a = Array.ofDim[Double](n, n);
    val tmp = 1.0 / n / n
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

  def main(args: Array[String]): Unit = {
    var n = 100

    if (args.length >= 1)
      n = args(0).toInt

    n = n / 2 * 2;

    val t = matmul(matgen(500), matgen(500))
    println("warnup: " + t(1)(1))

    val start_time = System.nanoTime

    val a = matgen(n)
    val b = matgen(n)
    val x = matmul(a, b)

    println(x(n/2)(n/2))
    println("time: "+(System.nanoTime-start_time)/1e9+"s")
  }
}
