import scala.collection.mutable.ArrayBuffer

sealed abstract class Op
case class Inc(v: Int) extends Op
case class Move(v: Int) extends Op
case class Loop(loop: Array[Op]) extends Op
case object Print extends Op

class Tape() {
  private var tape: Array[Int] = Array(0)
  private var pos: Int = 0

  def get = tape(pos)
  def inc(x: Int) = tape(pos) += x
  def move(x: Int) = {
    pos += x
    while (pos >= tape.length) {
      tape = Array.copyOf(tape, tape.length * 2)
    }
  }
}

class Printer(val quiet: Boolean) {
  private var sum1: Int = 0
  private var sum2: Int = 0

  def write(n: Int) = {
    if (quiet) {
      sum1 = (sum1 + n) % 255
      sum2 = (sum2 + sum1) % 255
    } else {
      print(n.toChar)
    }
  }

  def checksum = (sum2 << 8) | sum1
}

class Program(text: String, p: Printer) {
  val ops: Array[Op] = parse(text.iterator)

  def parse(iterator: Iterator[Char]) : Array[Op] = {
    val res = ArrayBuffer[Op]()
    while (iterator.hasNext) {
      iterator.next() match {
        case '+' => res += Inc(1)
        case '-' => res += Inc(-1)
        case '>' => res += Move(1)
        case '<' => res += Move(-1)
        case '.' => res += Print
        case '[' => res += Loop(parse(iterator))
        case ']' => return res.toArray
	case _ =>
      }
    }

    res.toArray
  }

  def run = _run(ops, new Tape())

  def _run(program: Array[Op], tape: Tape): Unit = {
    for (op <- program) op match {
      case Inc(x) => tape.inc(x)
      case Move(x) => tape.move(x)
      case Loop(loop) => while (tape.get > 0) _run(loop, tape)
      case Print => p.write(tape.get)
    }
  }

}

object BrainFuck {
  def notify(msg: String): Unit = {
    scala.util.Using((new java.net.Socket("localhost", 9001)).getOutputStream()) {
        _.write(msg.getBytes())
    }
  }

  def verify = {
    val text = """++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
        ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."""
    val pLeft = new Printer(true)
    new Program(text, pLeft).run
    val left = pLeft.checksum

    val pRight = new Printer(true)
    for (c <- "Hello World!\n") {
      pRight.write(c)
    }
    val right = pRight.checksum
    if (left != right) {
        System.err.println(s"${left} != ${right}")
        System.exit(1)
    }
  }

  def main(args: Array[String]): Unit = {
    val text = scala.util.Using(scala.io.Source.fromFile(args(0))) { _.mkString }.get
    val p = new Printer(sys.env.get("QUIET").isDefined)

    notify(s"Scala\t${ProcessHandle.current().pid()}")
    val s = System.nanoTime
    new Program(text, p).run
    val elapsed = (System.nanoTime - s) / 1e9
    notify("stop")

    System.err.println(s"time: $elapsed s")
    if (p.quiet) {
      System.out.println(s"Output checksum: ${p.checksum}");
    }
  }
}
