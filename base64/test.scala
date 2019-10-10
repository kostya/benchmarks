object Base64 {
  val enc = java.util.Base64.getEncoder
  val dec = java.util.Base64.getDecoder

  def encode(str: Array[Byte]) = enc.encodeToString(str)
  def decode(str: String) = dec.decode(str)

  def main(args: Array[String]): Unit = {
    val STR_SIZE = 10000000
    val TRIES = 100
    val str = ("a" * STR_SIZE).getBytes()
    var str2 = ""

    println("JIT warming up")
    for (_ <- 1 to 5) {
      decode(encode(str))
    }

    println("run")
    val t = System.nanoTime

    var s = 0
    for (_ <- 1 to TRIES) {
      str2 = encode(str)
      s += str2.length
    }
    println("encode: " + s + ", " + (System.nanoTime - t)/1e9)

    s = 0
    val t1 = System.nanoTime
    for (_ <- 1 to TRIES) {
      val str3 = decode(str2)
      s += str3.length
    }
    val now = System.nanoTime
    println("decode: " + s + ", " + (now - t1) / 1e9)
    println("overall time: " + (now - t) / 1e9 + "s")
  }
}
