object Base64 {
  val enc = java.util.Base64.getEncoder
  val dec = java.util.Base64.getDecoder

  def encode(str: String) = enc.encodeToString(str.getBytes())
  def decode(str: String) = dec.decode(str)

  def main(args: Array[String]): Unit = {
    val STR_SIZE = 10000000
    val TRIES = 100
    val str = "a" * STR_SIZE
    var str2 = ""

    // warnup
    for (_ <- 1 to 100) {
      val str2 = encode("a" * 10000)
      val str3 = decode(str2)
    }
    //

    var t = System.nanoTime

    var s = 0
    for (_ <- 1 to TRIES) {
      str2 = encode(str)
      s += str2.length
    }
    println("encode: " + s + ", " + (System.nanoTime - t)/1e9)

    s = 0
    t = System.nanoTime
    for (_ <- 1 to TRIES) {
      val str3 = decode(str2)
      s += str3.length
    }
    println("decode: " + s + ", " + (System.nanoTime - t)/1e9)
  }
}
