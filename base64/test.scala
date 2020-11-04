object Base64 {
  val enc = java.util.Base64.getEncoder
  val dec = java.util.Base64.getDecoder

  def encode(str: Array[Byte]) = enc.encodeToString(str)
  def decode(str: String) = dec.decode(str)

  def notify(msg: String): Unit = {
    scala.util.Using((new java.net.Socket("localhost", 9001)).getOutputStream()) {
        _.write(msg.getBytes())
    }
  }

  def main(args: Array[String]): Unit = {
    for ((src, dst) <- Array(("hello", "aGVsbG8="), ("world", "d29ybGQ="))) {
      val encoded = encode(src.getBytes())
      if (encoded != dst) {
        System.err.println(s"${encoded} != ${dst}")
        System.exit(1)
      }
      val decoded = new String(decode(dst))
      if (decoded != src) {
        System.err.println(s"${decoded} != ${src}")
        System.exit(1)
      }
    }

    val STR_SIZE = 131072
    val TRIES = 8192

    val str = Array.fill[Byte](STR_SIZE)('a')
    val str2 = encode(str)
    val str3 = decode(str2)

    notify(s"Scala\t${ProcessHandle.current().pid()}")

    var s_encoded = 0
    val t = System.nanoTime
    for (_ <- 1 to TRIES) {
      s_encoded += encode(str).length
    }
    val t_encoded = (System.nanoTime - t) / 1e9

    var s_decoded = 0
    val t1 = System.nanoTime
    for (_ <- 1 to TRIES) {
      s_decoded += decode(str2).length
    }
    val t_decoded = (System.nanoTime - t1) / 1e9

    notify("stop")

    println(s"encode ${new String(str.take(4))}... " +
      s"to ${str2.substring(0, 4)}...: $s_encoded, $t_encoded")
    println(s"decode ${str2.substring(0, 4)}... " +
      s"to ${new String(str3.take(4))}...: $s_decoded, $t_decoded")
    println(s"overall time: ${t_encoded + t_decoded}s")
  }
}
