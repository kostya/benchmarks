import scala.quoted.*
import scala.collection.mutable.ArrayBuffer

class Tape:
  private var tape: Array[Int] = Array(0)
  private var pos: Int = 0

  def get = tape(pos)

  def inc(x: Int) = tape(pos) += x

  def move(x: Int) =
    pos += x
    if pos >= tape.length then
      tape = Array.copyOf(tape, tape.length * 2)

class Printer(val quiet: Boolean):
  private var sum1, sum2: Int = 0

  def write(n: Int) =
    if quiet then
      sum1 = (sum1 + n) % 255
      sum2 = (sum2 + sum1) % 255
    else print(n.toChar)

  def checksum = (sum2 << 8) | sum1

class Program(text: String, p: Printer):
  def parse(using Quotes)(iter : Iterator[Char], t : Expr[Tape], p : Expr[Printer]) : Expr[Unit] =
    val code = ArrayBuffer.empty[Expr[Unit]]
    while (iter.hasNext) do
      iter.next() match
        case '+' => code += '{ $t.inc(1) }
        case '-' => code += '{ $t.inc(-1) }
        case '>' => code += '{ $t.move(1) }
        case '<' => code += '{ $t.move(-1) }
        case '.' => code += '{ $p.write($t.get) }
        case '[' => code += '{ def body() = ${ parse(iter, t, p) }
                               while $t.get > 0 do body()
                             }
        case ']' => return Expr.block(code.toList, '{})
        case _ =>
    Expr.block(code.toList, '{})

  given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

  val runOn : (Tape, Printer) => Unit =
    staging.run('{ ((t : Tape, p : Printer) => ${ parse(text.iterator, 't, 'p) }) })

  def run = runOn(Tape(), p)

def notify(msg: String) =
  scala.util.Using((java.net.Socket("localhost", 9001)).getOutputStream()) {
      _.write(msg.getBytes())
  }

def verify =
  val text = """++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>
      ---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."""
  val pLeft = Printer(true)
  Program(text, pLeft).run
  val left = pLeft.checksum
  val pRight = Printer(true)
  for c <- "Hello World!\n" do
    pRight.write(c)
  val right = pRight.checksum
  if left != right then
      System.err.println(s"${left} != ${right}")
      System.exit(1)

@main def main(filename : String) : Unit =
  verify
  val text = scala.util.Using(scala.io.Source.fromFile(filename)) { _.mkString }.get
  val p = Printer(sys.env.get("QUIET").isDefined)

  notify(s"Scala (Staged)\t${ProcessHandle.current().pid()}")
  val s = System.nanoTime
  Program(text, p).run
  val elapsed = (System.nanoTime - s) / 1e9
  notify("stop")

  System.err.println(s"time: $elapsed s")
  if p.quiet then
    System.out.println(s"Output checksum: ${p.checksum}")
