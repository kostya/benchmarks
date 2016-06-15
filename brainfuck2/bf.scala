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
    val new_pos = pos + x
    if (new_pos >= tape.length) 
      1 to (new_pos - tape.length + 1) foreach { _ => tape :+= 0 }
    if (new_pos >= 0) pos = new_pos
  }
}

class Program(text: String) {
  var ops = parse(text.iterator)

  run(ops, new Tape())

  def parse(iterator: Iterator[Char]) : Array[Op] = {
    var res = Array[Op]()
    while (iterator.hasNext) {
      val c = iterator.next()
      val op = c match {
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

  def run(program: Array[Op], tape: Tape) {
    for (op <- program) op match {
      case Inc(x) => tape.inc(x)
      case Move(x) => tape.move(x)
      case Loop(loop) => while (tape.get != 0) run(loop, tape)
      case Print() => print(tape.get.toChar)
    }
  }

}

object BrainFuck {
  def time(f: => Any) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e9+"s")
    ret
  }

  def main(args: Array[String]): Unit = {
    val text = scala.io.Source.fromFile(args(0)).mkString

    //warmup
    print("warmup\n")
    time {
      new Program(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++")
    }
    //

    print("run\n")
    time {
      new Program(text)
    }

  }
}
