import java.nio.file.{Files, Paths}
import com.dslplatform.json._

object JsonTest {

  case class Root(coordinates: Seq[Coordinates])

  case class Coordinates(
    x: Double,
    y: Double,
    z: Double)

  def notify(msg: String): Unit = {
    scala.util.Using((new java.net.Socket("localhost", 9001)).getOutputStream()) {
        _.write(msg.getBytes())
    }
  }

  def main(args: Array[String]): Unit = {
    val bytes = Files.readAllBytes(Paths.get("/tmp/1.json"))

    1 to 4 foreach (_ => parseJson(bytes))
    notify(s"Scala\t${ProcessHandle.current().pid()}")
    parseJson(bytes)
    notify("stop")
  }

  private def parseJson(bytes: Array[Byte]): Unit = {
    val start_time = System.nanoTime

    val settings = new DslJson.Settings[Any]().doublePrecision(JsonReader.DoublePrecision.LOW).`with`(new ConfigureScala)
    implicit val dslJson = new DslJson[Any](settings)

    val root = dslJson.decode[Root](bytes)

    var (x, y, z) = (0.0, 0.0, 0.0)

    root.coordinates.foreach { c =>
      x += c.x
      y += c.y
      z += c.z
    }

    val len = root.coordinates.size
    println(x / len)
    println(y / len)
    println(z / len)

    println("time: " + (System.nanoTime - start_time) / 1e9 + "s")
  }
}
