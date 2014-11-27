object TestJson {
  def main(args: Array[String]): Unit = {
    val text = scala.io.Source.fromFile("./1.json").mkString

    val jobj = scala.util.parsing.json.JSON.parseFull(text).get
    val map = jobj.asInstanceOf[Map[String, Any]]
    val coordinates = map.get("coordinates").get.asInstanceOf[List[Any]]

    val len = coordinates.length
    var x = 0.0
    var y = 0.0
    var z = 0.0

    for (coordinate <- coordinates) {
      val coord = coordinate.asInstanceOf[Map[String, Any]]
      x += coord.get("x").get.asInstanceOf[Double]
      y += coord.get("y").get.asInstanceOf[Double]
      z += coord.get("z").get.asInstanceOf[Double]
    }

    println(x / len)
    println(y / len)
    println(z / len)
  }
}
