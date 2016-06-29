import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import java.util.ArrayList
import java.util.HashMap
import java.util.Stack

enum class OpT {
    INC, MOVE, PRINT, LOOP
}

class Op {
    var op:OpT
    var v:Int = 0
    var loop:List<Op>
    
    constructor(_op: OpT, _v: Int) {
        op = _op
        v = _v
        loop = ArrayList<Op>()
    }
    
    constructor(_op: OpT, _l: List<Op>) {
        op = _op
        loop = _l
        v = 0
    }
}

class Tape {
    private var tape: IntArray
    private var pos: Int = 0

    init{
        tape = IntArray(1)
    }
    
    fun get():Int {
        return tape[pos]
    }
    
    fun inc(x:Int) {
        tape[pos] += x
    }
    
    fun move(x:Int) {
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
                '+' -> res.add(Op(OpT.INC, 1))
                '-' -> res.add(Op(OpT.INC, -1))
                '>' -> res.add(Op(OpT.MOVE, 1))
                '<' -> res.add(Op(OpT.MOVE, -1))
                '.' -> res.add(Op(OpT.PRINT, 0))
                '[' -> res.add(Op(OpT.LOOP, parse(it)))
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
                OpT.LOOP -> while (tape.get() != 0) _run(op.loop, tape)
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
fun main(args:Array<String>) {
    val code = String(Files.readAllBytes(Paths.get(args[0])))
    
    warming()

    System.err.println("run")
    val start_time = System.currentTimeMillis()
    val program = Program(code)
    program.run()
    System.err.println("time: ${(System.currentTimeMillis() - start_time) / 1e3}s")
}