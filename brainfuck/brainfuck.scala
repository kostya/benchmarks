class Tape() {
  private var tape = Array(0)
  private var pos  = 0

  def get = tape(pos)
  def inc = tape(pos) += 1
  def dec = tape(pos) -= 1
  def advance = { pos += 1; if (tape.size <= pos) { tape :+= 0 } }
  def devance = { if (pos > 0) { pos -= 1 } }
}

class Program(text: String) {
  var code = ""
  var bracket_map = Map[Int, Int]()

  parse_code(text)

  def parse_code(text: String) {
    var leftstack = List[Int]()
    var pc = 0
    for (ch <- text if ("[].,+-<>" contains ch)) {
      code :+= ch
      if (ch == '[') { leftstack :+= pc }
      else if (ch == ']' && leftstack.size != 0) {
        val left = leftstack.last
        leftstack = leftstack.init
        val right = pc
        bracket_map += (left -> right)
        bracket_map += (right -> left)
      }
      pc += 1
    }
  }

  def run = {
    var pc = 0
    var tape = new Tape()
    while ( pc < code.length ) {
      code(pc) match {
        case '+' => tape.inc
        case '-' => tape.dec
        case '>' => tape.advance
        case '<' => tape.devance
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
