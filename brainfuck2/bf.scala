abstract class Op
case class Inc(v: Int) extends Op
case class Move(v: Int) extends Op
case class Print() extends Op
case class Loop(loop: Array[Op]) extends Op
case class Nop() extends Op

class Tape() {
  private var tape = Array(0)
  private var pos  = 0

  def get = tape(pos)
  def inc(x: Int) = tape(pos) += x
  def move(x: Int) = { 
    pos += x
    while (pos >= tape.length) { tape :+= 0 }
  }
}

class Program(text: String) {
  var ops = parse(text.iterator)

  def parse(iterator: Iterator[Char]) : Array[Op] = {
    var res = Array[Op]()
    while (iterator.hasNext) {
      val op = iterator.next() match {
        case '+' => new Inc(1)
        case '-' => new Inc(-1)
        case '>' => new Move(1)
        case '<' => new Move(-1)
        case '.' => new Print()
        case '[' => new Loop(parse(iterator))
        case ']' => return res
        case _ => new Nop()
      }

      op match {
        case Nop() => ()
        case _ => res :+= op
      }
    }

    res
  }

  def run = _run(ops, new Tape())

  def _run(program: Array[Op], tape: Tape) {
    for (op <- program) op match {
      case Inc(x) => tape.inc(x)
      case Move(x) => tape.move(x)
      case Loop(loop) => while (tape.get != 0) _run(loop, tape)
      case Print() => print(tape.get.toChar)
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

    //warmup
    System.err.print("warmup\n")
    time {
      new Program(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++").run
    }
    //

    System.err.print("run\n")
    time {
      new Program(text).run
    }

  }
}
