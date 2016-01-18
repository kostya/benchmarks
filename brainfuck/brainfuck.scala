class Tape() {
  private var tape = Array(0)
  private var pos  = 0

  def get = tape(pos)
  def inc = tape(pos) += 1
  def dec = tape(pos) -= 1
  def advance = { pos += 1; if (tape.length <= pos) { tape :+= 0 } }
  def devance = { if (pos > 0) { pos -= 1 } }
}

class Program(text: String) {
  var code = Array.empty[Char]
  var bracket_map = collection.mutable.HashMap.empty[Int, Int]

  parse_code(text)

  def parse_code(text: String) {
    val leftstack = collection.mutable.ArrayStack.empty[Int]
    var pc = 0
    for (ch <- text)
      ch match {

        case '.' | ',' | '+' | '-' | '<' | '>' =>
          code :+= ch
          pc += 1

        case '[' =>
          leftstack push pc
          code :+= ch
          pc += 1

        case ']' =>
          if (leftstack.nonEmpty) {
            val left = leftstack.pop()
            val right = pc
            bracket_map.update(left, right)
            bracket_map.update(right, left)
          }
          code :+= ch
          pc += 1

        case _ =>
      }
  }

  def run = {
    var pc = 0
    var tape = new Tape()
    while ( pc < code.length ) {
      code(pc) match {
        case '>' => tape.advance
        case '<' => tape.devance
        case '+' => tape.inc
        case '-' => tape.dec
        case '[' => if (tape.get == 0) pc = bracket_map(pc)
        case ']' => if (tape.get != 0) pc = bracket_map(pc)
        case '.' => print(tape.get.toChar)
      }
      pc += 1
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
      val prog = new Program(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++")
      prog.run
    }
    //

    print("run\n")
    time {
      val prog = new Program(text)
      prog.run
    }

  }
}
