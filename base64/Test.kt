import java.util.Base64

val STR_SIZE = 10000000
val TRIES = 100

val enc = Base64.getEncoder();
val dec = Base64.getDecoder();

fun main(args: Array<String>) {
    val str = "a".repeat(STR_SIZE).toByteArray()

    var count1 = 0
    var encStr = ""

    println("JIT warming up")
    repeat(5) {
        dec.decode(enc.encodeToString(str))
    }

    println("run")
    val t1 = System.nanoTime()
    repeat(TRIES) {
        encStr = enc.encodeToString(str)
        count1 += encStr.length
    }
    println("encode: ${count1}, ${(System.nanoTime() - t1) / 1e9}")

    var count2 = 0

    val t2 = System.nanoTime()
    repeat(TRIES) {
        val decBytes = dec.decode(encStr)
        count2 += decBytes.size
    }
    val now = System.nanoTime()
    println("decode: ${count2}, ${(now - t2) / 1e9}")
    println("overall time: ${(now - t1) / 1e9}s")
}
