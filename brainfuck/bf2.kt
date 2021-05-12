import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import java.util.Arrays

sealed class Op() {
    class Inc(val v: Int): Op()
    class Move(val v: Int): Op()
    class Loop(val loop: Array<Op>): Op()
    object Print: Op()
}

class Tape {
    private var tape: IntArray = IntArray(1)
    private var pos: Int = 0

    fun get(): Int {
        return tape[pos]
    }

    fun inc(x: Int) {
        tape[pos] += x
    }

    fun move(x: Int) {
        pos += x
        while (pos >= tape.size) {
            this.tape = Arrays.copyOf(this.tape, this.tape.size * 2)
        }
    }
}

class Printer(val quiet: Boolean) {
    private var sum1: Int = 0
    private var sum2: Int = 0

    fun print(n: Int) {
        if (quiet) {
            sum1 = (sum1 + n) % 255
            sum2 = (sum2 + sum1) % 255
        } else {
            print(n.toChar())
        }
    }

    fun getChecksum() = (sum2 shl 8) or sum1
}

class Program(code: String, val p: Printer) {
    private val ops: Array<Op>

    init {
        val it = code.iterator()
        ops = parse(it)
    }

    private fun parse(it: CharIterator): Array<Op> {
        val res = arrayListOf<Op>()
        while (it.hasNext()) {
            when (it.next()) {
                '+' -> res.add(Op.Inc(1))
                '-' -> res.add(Op.Inc(-1))
                '>' -> res.add(Op.Move(1))
                '<' -> res.add(Op.Move(-1))
                '.' -> res.add(Op.Print)
                '[' -> res.add(Op.Loop(parse(it)))
                ']' -> return res.toTypedArray()
            }
        }
        return res.toTypedArray()
    }

    fun run() {
        _run(ops, Tape())
    }

    private fun _run(program: Array<Op>, tape: Tape) {
        for (op in program) {
            when (op) {
                is Op.Inc -> tape.inc(op.v)
                is Op.Move -> tape.move(op.v)
                is Op.Loop -> while (tape.get() > 0) {
                    _run(op.loop, tape)
                }
                is Op.Print -> p.print(tape.get())
            }
        }
    }
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

fun verify() {
    val code = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>" +
    "---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    val pLeft = Printer(true)
    Program(code, pLeft).run()
    val left = pLeft.getChecksum()

    val pRight = Printer(true)
    for (c in "Hello World!\n") {
        pRight.print(c.code)
    }
    val right = pRight.getChecksum()
    if (left != right) {
        System.err.println("${left} != ${right}")
        System.exit(1)
    }
}

@Throws(IOException::class)
fun main(args: Array<String>) {
    verify()
    val code = String(Files.readAllBytes(Paths.get(args[0])))
    val p = Printer(!System.getenv("QUIET").isNullOrEmpty())

    notify("Kotlin\t${ProcessHandle.current().pid()}")
    val startTime = System.currentTimeMillis()
    Program(code, p).run()
    val timeDiff = (System.currentTimeMillis() - startTime) / 1e3
    notify("stop")

    if (p.quiet) {
        println("Output checksum: ${p.getChecksum()}")
    }
    System.err.println("time: ${timeDiff}s")
}
