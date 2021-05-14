import scala.collection.mutable.{HashMap, Queue, ArrayBuffer}

object Primes {
  val UpperBound = 5000000
  val Prefix = 32338

  class Node(
    var children: HashMap[Char, Node] = HashMap(),
    var terminal: Boolean = false
  )

  def generatePrimes(limit: Int): Iterable[Int] = {
    var prime = ArrayBuffer.fill(limit + 1)(0)

    var x = 1
    while (x * x < limit) {
        var y = 1
        while (y * y < limit) {
            var n = (4 * x * x) + (y * y)
            if (n <= limit && (n % 12 == 1 || n % 12 == 5)) {
                prime(n) = 1 - prime(n)
            }

            n = (3 * x * x) + (y * y)
            if (n <= limit && n % 12 == 7) {
                prime(n) = 1 - prime(n)
            }

            n = (3 * x * x) - (y * y)
            if (x > y && n <= limit && n % 12 == 11) {
                prime(n) = 1 - prime(n)
            }
            y += 1
        }
        x += 1
    }

    var r = 5
    while (r * r < limit) {
        if (prime(r) > 0) {
            var i = r * r
            while (i < limit) {
                prime(i) = 0
                i += r * r
            }
        }
        r += 1
    }

    var result = ArrayBuffer(2, 3)
    for (p <- 5 to limit) {
        if (prime(p) > 0) {
            result += p
        }
    }
    result
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
    val primes = generatePrimes(upperBound)
    val root = generateTrie(primes)
    val strPrefix = prefix.toString
    var head: Option[Node] = Some(root)
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
