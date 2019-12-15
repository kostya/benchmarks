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

class Program(text: String) {
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
      case Print => print(tape.get.toChar)
    }
  }

}

object BrainFuck {
  def time(f: => Any) = {
    val s = System.nanoTime
    val ret = f
    System.err.println("time: "+(System.nanoTime-s)/1e9+"s")
    ret
  }

  def main(args: Array[String]): Unit = {
    val text = scala.io.Source.fromFile(args(0)).mkString

    // JIT warming up
    System.err.print("JIT warming up\n")
    time {
      new Program(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++").run
    }
    //

    System.err.print("run\n")
    scala.util.Using((new java.net.Socket("localhost", 9001)).getOutputStream()) {
        _.write("Scala".getBytes())
    }

    time {
      new Program(text).run
    }

  }
}
