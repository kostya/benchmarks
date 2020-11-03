import java.util.Base64

val STR_SIZE = 131072
val TRIES = 8192

val enc = Base64.getEncoder();
val dec = Base64.getDecoder();

fun notify(msg: String) {
    try {
        java.net.Socket("localhost", 9001).getOutputStream().use {
            it.write(msg.toByteArray())
        }
    } catch (e: java.io.IOException) {
        // standalone usage
    }
}

fun main() {
    for ((src, dst) in arrayOf(
              arrayOf("hello", "aGVsbG8="), arrayOf("world", "d29ybGQ=")
    )) {
        val encoded = enc.encodeToString(src.toByteArray())
        if (encoded != dst) {
            System.err.println("${encoded} != ${dst}")
            System.exit(1)
        }
        val decoded = String(dec.decode(dst))
        if (decoded != src) {
            System.err.println("${decoded} != ${src}")
            System.exit(1)
        }
    }

    val str = "a".repeat(STR_SIZE).toByteArray()
    val encStr = enc.encodeToString(str)
    val decBytes = dec.decode(encStr)

    notify("Kotlin\t${ProcessHandle.current().pid()}")

    var s_encoded = 0
    val t1 = System.nanoTime()
    repeat(TRIES) {
        s_encoded += enc.encodeToString(str).length
    }
    val t_encoded = (System.nanoTime() - t1) / 1e9

    var s_decoded = 0
    val t2 = System.nanoTime()
    repeat(TRIES) {
        s_decoded += dec.decode(encStr).size
    }
    val t_decoded = (System.nanoTime() - t2) / 1e9

    notify("stop")

    println("encode ${String(str.sliceArray(1..4))}... " +
            "to ${encStr.substring(0, 4)}...: ${s_encoded}, ${t_encoded}")
    println("decode ${encStr.substring(0, 4)}... to " +
            "${String(decBytes.sliceArray(1..4))}...: " +
            "${s_decoded}, ${t_decoded}")
    println("overall time: ${t_encoded + t_decoded}s")
}
