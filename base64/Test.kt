import java.util.Base64

val STR_SIZE = 10000000
val TRIES = 100

val enc = Base64.getEncoder();
val dec = Base64.getDecoder();

fun main(args: Array<String>) {
    val str = "a".repeat(STR_SIZE)

    var count1 = 0
    var encStr = ""

    val t1 = System.nanoTime()
    repeat(TRIES) {
        encStr = enc.encodeToString(str.toByteArray())
        count1 += encStr.length
    }
    println("encode: ${count1}, ${(System.nanoTime() - t1) / 1e9}")

    var count2 = 0
    var decStr: String

    val t2 = System.nanoTime()
    repeat(TRIES) {
        val decBytes = dec.decode(encStr)
        decStr = String(decBytes)
        count2 += decStr.length
    }
    println("decode: ${count2}, ${(System.nanoTime() - t2) / 1e9}")
}