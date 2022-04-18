import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, Cylinder, Shape3D}

import java.io.FileNotFoundException
import scala.util.{Failure, Success, Try}

case class GraphicModelConstructor() {

  def buildObject(line: String): Shape3D = {GraphicModelConstructor.buildObject(line).get}

}

object GraphicModelConstructor {

  type Translate = (Int, Int, Int)
  type Scale = (Double, Double, Double)
  type Color = (Int, Int, Int)

  def buildObject(line: String): Try[Shape3D] = {
    try {
      val obj = line.split(" ")
      println(s"Length: ${obj.length}")
      val shape = obj(0)
      val values = obj(1).replaceAll("[()]", "").split(",")
      val color = (values(0).toInt, values(1).toInt, values(2).toInt)
      obj.length match {
        case 8 =>
          val translate = (obj(2).toInt, obj(3).toInt, obj(4).toInt)
          val scale = (obj(5).toDouble, obj(6).toDouble, obj(7).toDouble)
          Success(createShape(shape, color, translate, scale))
        case 2 =>
          val translate = (0,0,0)
          val scale = (1.0,1.0,1.0)
          Success(createShape(shape, color, translate, scale))
        case _ => Failure(new IllegalArgumentException("Ficheiro mal formatado"))
      }
    } catch {
      case e: FileNotFoundException => e.printStackTrace();Failure(new FileNotFoundException("Ficheiro não encontrado"));
    }
  }

  private def createShape(shape:String, color:Color, translate: Translate, scale: Scale): Shape3D = {
    def setShapes(s:Shape3D, color:Color, translate: Translate, scale: Scale):Shape3D = {
      println(s"Box: ${s.toString}")
      s.setTranslateX(translate._1)
      s.setTranslateY(translate._2)
      s.setTranslateZ(translate._3)
      s.setScaleX(scale._1)
      s.setScaleY(scale._2)
      s.setScaleZ(scale._3)
      val shapeColor= new PhongMaterial()
      shapeColor.setDiffuseColor(Color.rgb(color._1,color._2,color._3))
      s.setMaterial(shapeColor)
      s
    }
    shape match {
      case "Box" => setShapes(new Box(1,1,1),color, translate, scale)
      case "Cylinder" =>  setShapes(new Cylinder(0.5,1,10),color, translate, scale)
    }

  }

}