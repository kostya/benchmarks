import scala.collection.mutable.{Set, HashMap, Queue, ArrayBuffer}

object Primes {
  val UpperBound = 5000000
  val Prefix = 32338

  class Node(
    var children: HashMap[Char, Node] = HashMap(),
    var terminal: Boolean = false
  )

  def generatePrimes(m: Int): Iterable[Int] = {
    var result = Set(2)
    for (i <- 1 to (m - 1) / 2) {
      result += 2 * i + 1
    }
    var (k, j) = (1, 1)
    def sqr(i: Int): Int = i * i
    def maxN(i: Int): Int = (m - sqr(2 * i + 1)) / (4 * i + 2)
    while (k > 0) {
      k = maxN(j)
      j += 1
    }
    k = j
    for (i <- 1 to k) {
      for (n <- 0 until maxN(i - 1)) {
        result -= (2 * i + 1) * (2 * i + 2 * n + 1)
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
