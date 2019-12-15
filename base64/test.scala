object Base64 {
  val enc = java.util.Base64.getEncoder
  val dec = java.util.Base64.getDecoder

  def encode(str: Array[Byte]) = enc.encodeToString(str)
  def decode(str: String) = dec.decode(str)

  def main(args: Array[String]): Unit = {
    val STR_SIZE = 131072
    val TRIES = 8192
    val str = Array.fill[Byte](STR_SIZE)('a')

    println("JIT warming up")
    for (_ <- 1 to 5) {
      decode(encode(str))
    }

    scala.util.Using((new java.net.Socket("localhost", 9001)).getOutputStream()) {
        _.write("Scala".getBytes())
    }

    var str2 = encode(str)
    print(s"encode ${new String(str.take(4))}... to ${str2.substring(0, 4)}...: ")

    val t = System.nanoTime

    var s = 0
    for (_ <- 1 to TRIES) {
      str2 = encode(str)
      s += str2.length
    }
    println(s"$s, ${(System.nanoTime - t) / 1e9}")

    var str3 = decode(str2)
    print(s"decode ${str2.substring(0, 4)}... to ${new String(str3.take(4))}...: ")

    s = 0
    val t1 = System.nanoTime
    for (_ <- 1 to TRIES) {
      str3 = decode(str2)
      s += str3.length
    }
    val now = System.nanoTime
    println(s"$s, ${(now - t1) / 1e9}")
    println(s"overall time: ${(now - t) / 1e9}s")
  }
}
