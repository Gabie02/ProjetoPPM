import javafx.scene.shape.{Cylinder, Box, Shape3D}

case class GraphicModelConstructor() {

  def buildObject(obj: String): Shape3D = {GraphicModelConstructor.buildObject(obj, this)}

}

object GraphicModelConstructor {

  type Translate = (Int, Int, Int)
  type Scale = (Double, Double, Double)

  def buildObject(line: String, gmc: GraphicModelConstructor): Shape3D = {
    val obj = line.split(" ")
    obj.foreach(l => println(l))
    val translate = (obj(2), obj(3), obj(4))
    val scale = (obj(5), obj(6), obj(7))
    val shape = obj(0)
    val values = obj(1).trim.split("[,(,)]")
    //values.foreach(v => println(s"Value: $v"))
    createShape(shape, values)
  }

  private def createShape(shape:String, values: Array[String]): Shape3D = {
    shape match {
      case "Box" => {
        val a = new Box(1,1,1)
        println(s"Box: ${a.toString}")
        a.translateXProperty()

        a
      }
      case "Cylinder" => val a = new Cylinder(0.5, 1, 10); println(s"Cylinder: ${a.toString}");a
    }

  }

}