import java.nio.file.{Files, Paths}
import com.dslplatform.json._

object JsonTest {

  case class Root(coordinates: Seq[Coordinate])

  case class Coordinate(
    x: Double,
    y: Double,
    z: Double)

  def notify(msg: String): Unit = {
    val socket = new java.net.Socket("localhost", 9001)
    scala.util.Using(socket.getOutputStream()) {
        _.write(msg.getBytes())
    }
  }

  private def calc(bytes: Array[Byte]): Coordinate = {
    val settings = new DslJson.Settings[Any]()
      .doublePrecision(JsonReader.DoublePrecision.LOW)
      .`with`(new ConfigureScala)
    implicit val dslJson = new DslJson[Any](settings)

    val root = dslJson.decode[Root](bytes)

    var (x, y, z) = (0.0, 0.0, 0.0)

    root.coordinates.foreach { c =>
      x += c.x
      y += c.y
      z += c.z
    }

    val len = root.coordinates.size
    Coordinate(x / len, y / len, z / len)
  }

  private def parseJson(bytes: Array[Byte]): Unit = {
    val start_time = System.nanoTime
    println(calc(bytes))
    println("time: " + (System.nanoTime - start_time) / 1e9 + "s")
  }

  def main(args: Array[String]): Unit = {
    val json = "{\"coordinates\":[{\"x\":1.1,\"y\":2.2,\"z\":3.3}]}"
            .getBytes()
    val left = calc(json)
    val right = Coordinate(1.1, 2.2, 3.3)
    if (left != right) {
      System.err.println(s"${left} != ${right}")
      System.exit(1)
    }

    val bytes = Files.readAllBytes(Paths.get("/tmp/1.json"))

    notify(s"Scala\t${ProcessHandle.current().pid()}")
    parseJson(bytes)
    notify("stop")
  }
}
