import scala.collection.mutable.{BitSet, HashMap, Queue, ArrayBuffer}

object Primes {
  val UpperBound = 5000000
  val Prefix = 32338

  class Node(
    var children: HashMap[Char, Node] = HashMap(),
    var terminal: Boolean = false
  )

  class Sieve(val limit: Int) {
    private val prime = new BitSet(limit + 1)

    def toList(): Iterable[Int] = {
      var result = ArrayBuffer(2, 3)
      for (p <- 5 to limit) {
        if (prime(p)) {
          result += p
        }
      }
      result
    }

    def omitSquares(): Sieve = {
      var r = 5
      while (r * r < limit) {
        if (prime(r)) {
          var i = r * r
          while (i < limit) {
            prime(i) = false
            i += r * r
          }
        }
        r += 1
      }
      this
    }

    def step1(x: Int, y: Int): Unit = {
      val n = (4 * x * x) + (y * y)
      if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
        prime(n) = !prime(n)
      }
    }

    def step2(x: Int, y: Int): Unit = {
      val n = (3 * x * x) + (y * y)
      if (n <= limit && n % 12 == 7) {
        prime(n) = !prime(n)
      }
    }

    def step3(x: Int, y: Int): Unit = {
      val n = (3 * x * x) - (y * y)
      if (x > y && n <= limit && n % 12 == 11) {
        prime(n) = !prime(n)
      }
    }

    def loopY(x: Int): Unit = {
      var y = 1
      while (y * y < limit) {
        step1(x, y)
        step2(x, y)
        step3(x, y)
        y += 1
      }
    }

    def loopX(): Unit = {
      var x = 1
      while (x * x < limit) {
        loopY(x)
        x += 1
      }
    }

    def calc(): Sieve = {
      loopX()
      omitSquares()
    }
  }

  def generateTrie(l: Iterable[Int]): Node = {
    var root = new Node()
    for (el <- l) {
      var head = root
      for (ch <- el.toString) {
        head = head.children.getOrElseUpdate(ch, new Node())
      }
      head.terminal = true
    }
    root
  }

  def find(upperBound: Int, prefix: Int): Option[Iterable[Int]] = {
    val primes = new Sieve(upperBound).calc()
    val strPrefix = prefix.toString
    var head: Option[Node] = Some(generateTrie(primes.toList()))
    for (ch <- strPrefix) {
      head = head.flatMap(_.children.get(ch))
    }
    head match {
      case Some(head) => {
        var queue: Queue[(Node, String)] = Queue((head, strPrefix))
        var result: ArrayBuffer[Int] = ArrayBuffer()
        while (!queue.isEmpty) {
          val (top, prefix) = queue.dequeue()
          if (top.terminal) {
            result += prefix.toInt
          }
          for ((ch, v) <- top.children) {
            queue += ((v, prefix + ch))
          }
        }
        scala.util.Sorting.stableSort(result)
        Some(result)
      }
      case None => None
    }
  }

  def notify(msg: String): Unit = {
    scala.util.Using(
      (new java.net.Socket("localhost", 9001)).getOutputStream()
    ) {
      _.write(msg.getBytes())
    }
  }

  def verify(): Unit = {
    val left = Seq(2, 23, 29)
    find(100, 2) match {
      case Some(right) => {
        if (left != right) {
          System.err.println(s"${left} != ${right}")
          System.exit(1)
        }
      }
      case None => {
          System.err.println("find() didn't return anything")
          System.exit(1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    verify()

    notify(s"Scala\t${ProcessHandle.current().pid()}")
    val results = find(UpperBound, Prefix)
    notify("stop")

    println(results)
  }
}
