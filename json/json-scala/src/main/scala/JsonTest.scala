import java.io.FileInputStream

import org.json4s._
import org.json4s.jackson.JsonMethods._

object JsonTest {

  case class Coordinates(x:Double, y:Double, z:Double)

  implicit val formats = DefaultFormats

  def main(args: Array[String]) {
    val start_time = System.nanoTime

    val fileStream = new FileInputStream("./1.json")
    val json = parse(fileStream)
    val coordsJson: JValue = json \ "coordinates"
    val coords = coordsJson.extract[Seq[Coordinates]]

    val len = coords.length
    var (x,y,z) = (0.0,0.0,0.0)

    coords.foreach{c =>
      x += c.x
      y += c.y
      z += c.z
    }

    println(x / len)
    println(y / len)
    println(z / len)

    println("time: "+(System.nanoTime-start_time)/1e9+"s")
  }
}
