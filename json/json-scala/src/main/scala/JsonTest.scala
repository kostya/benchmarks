import java.io.FileInputStream

import com.dslplatform.json._

object JsonTest {

  case class Root(coordinates: Seq[Coordinates])

  case class Coordinates(
    x: Double,
    y: Double,
    z: Double)

  def main(args: Array[String]): Unit = {
    1 to 4 foreach (_ => parseJson())
    scala.util.Using((new java.net.Socket("localhost", 9001)).getOutputStream()) {
        _.write("Scala".getBytes())
    }
    parseJson()
  }

  private def parseJson(): Unit = {
    val start_time = System.nanoTime

    val settings = new DslJson.Settings[Any]().doublePrecision(JsonReader.DoublePrecision.LOW).`with`(new ConfigureScala)
    implicit val dslJson = new DslJson[Any](settings)

    val fs = new FileInputStream("1.json")
    val root = dslJson.decode[Root](fs)

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
