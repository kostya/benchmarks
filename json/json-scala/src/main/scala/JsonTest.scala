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

  def main(args: Array[String]): Unit = {
    val right = Coordinate(2.0, 0.5, 0.25)
    for (v <- Array(
      "{\"coordinates\":[{\"x\":2.0,\"y\":0.5,\"z\":0.25}]}",
      "{\"coordinates\":[{\"y\":0.5,\"x\":2.0,\"z\":0.25}]}")) {
      val json = v.getBytes()
      val left = calc(json)
      if (left != right) {
        System.err.println(s"${left} != ${right}")
        System.exit(1)
      }
    }

    val bytes = Files.readAllBytes(Paths.get("/tmp/1.json"))

    notify(s"Scala\t${ProcessHandle.current().pid()}")
    val results = calc(bytes)
    notify("stop")

    println(results)
  }
}
