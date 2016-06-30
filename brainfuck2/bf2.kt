import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import java.util.ArrayList
import java.util.HashMap
import java.util.Stack

enum class OpT {
    INC, MOVE, PRINT, LOOP
}

data class Op(
    val op: OpT,
    val v: Int = 0,
    val loop: List<Op> = ArrayList<Op>()
)

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
            val tape = IntArray(this.tape.size * 2)
            System.arraycopy(this.tape, 0, tape, 0, this.tape.size)
            this.tape = tape
        }
    }
}
  
class Program(code: String) {
    private val ops: List<Op>

    init {
        val it = code.iterator()
        ops = parse(it)
    }

    private fun parse(it: CharIterator): List<Op> {
        val res = ArrayList<Op>()
        while (it.hasNext()) {
            when (it.next()) {
                '+' -> res.add(Op(OpT.INC, v = 1))
                '-' -> res.add(Op(OpT.INC, v = -1))
                '>' -> res.add(Op(OpT.MOVE, v = 1))
                '<' -> res.add(Op(OpT.MOVE, v = -1))
                '.' -> res.add(Op(OpT.PRINT, v = 0))
                '[' -> res.add(Op(OpT.LOOP, loop = parse(it)))
                ']' -> return res
            }
        }
        return res
    }

    fun run() {
        _run(ops, Tape())
    }
    
    private fun _run(program: List<Op>, tape: Tape) {
        for (op in program) {
            when (op.op) {
                OpT.INC -> tape.inc(op.v)
                OpT.MOVE -> tape.move(op.v)
                OpT.LOOP -> while (tape.get() != 0) {
                    _run(op.loop, tape)
                }
                OpT.PRINT -> print(tape.get().toChar())
            }
        }
    }
}

fun warming() {
    val WARM_PROGRAM = ">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++"
    
    System.err.println("warming")
    val start_time = System.currentTimeMillis()
    Program(WARM_PROGRAM).run()
    System.err.println("time: ${(System.currentTimeMillis() - start_time) / 1e3}s")
}

@Throws(IOException::class)
fun main(args: Array<String>) {
    val code = String(Files.readAllBytes(Paths.get(args[0])))
    
    warming()

    System.err.println("run")
    val start_time = System.currentTimeMillis()
    val program = Program(code)
    program.run()
    System.err.println("time: ${(System.currentTimeMillis() - start_time) / 1e3}s")
}