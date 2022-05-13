import OctreeUtils.Placement
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, Cylinder, DrawMode, Shape3D}

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source

object GraphicModelConstructor {

  type Translate = (Double, Double, Double)
  type Scale = (Double, Double, Double)
  type Color = (Int, Int, Int)

  //  --- T1 ---
  // Consergue ler ficheiros a partir da diretoria da main
  def readFromFile(file: String): List[Shape3D] = {
    val shapes = new ListBuffer[Shape3D]()
    val dir = System.getProperty("user.dir")
    val bufferedSource = Source.fromFile(s"$dir/$file")
    if (bufferedSource.isEmpty)
      println("Ficheiro vazio!")
    for (line <- bufferedSource.getLines) {
      shapes += convertShapes(line)
    }
    bufferedSource.close
    shapes.toList
  }

  private def convertShapes(line: String):Shape3D = {
    GraphicModelConstructor.buildObject(line)
  }

  def buildObject(line: String): Shape3D = {
      val obj = line.split(" ")
      val shape = obj(0)
      val values = obj(1).replaceAll("[()]", "").split(",")
      val color = (values(0).toInt, values(1).toInt, values(2).toInt)
      obj.length match {
        case 8 =>
          val translate = (obj(2).toDouble, obj(3).toDouble, obj(4).toDouble)
          val scale = (obj(5).toDouble, obj(6).toDouble, obj(7).toDouble)
          createShape(shape, color, translate, scale)
        case 2 =>
          val translate = (0.0,0.0,0.0)
          val scale = (1.0,1.0,1.0)
          createShape(shape, color, translate, scale)
        case _ => throw new IllegalArgumentException("Ficheiro mal formatado")
      }

  }

  def createShape(shape:String, color:Color, translate: Translate, scale: Scale): Shape3D = {
    def setShapes(s:Shape3D, color:Color, translate: Translate, scale: Scale):Shape3D = {
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
    shape.toUpperCase match {
      case "BOX" => setShapes(new Box(1,1,1),color, translate, scale)
      case "CYLINDER" =>  setShapes(new Cylinder(0.5,1,10),color, translate, scale)
      case _ => throw new IllegalArgumentException("Forma não suportada")
    }

  }

  def writeFile(oct:Octree[Placement]):Unit = {
    val leafsList = OctreeUtils.getAllShapes(oct)
    val shapesList = leafsList.filterNot(s => s.isInstanceOf[Box] && (s.asInstanceOf[Box].getDrawMode == DrawMode.LINE))
    val pw = new PrintWriter(new File("lastTree.txt"))
    (shapesList foldRight ()) ((h, t) => {
      h match {
        case _: Box =>
          val box = h.asInstanceOf[Box]
          pw.write("Box" + " " + "(" + (box.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getRed * 255).toInt + ","
            + (box.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getGreen * 255).toInt + ","
            + (box.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getBlue * 255).toInt + ")"
            + " " + box.getTranslateX.toInt + " " + box.getTranslateY.toInt + " " + box.getTranslateZ.toInt + " "
            + box.getScaleX + " " + box.getScaleY + " " + box.getScaleZ + "\n")

        case _: Cylinder =>
          val cylinder = h.asInstanceOf[Cylinder]
          pw.write("Cylinder" + " " + "(" + (cylinder.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getRed * 255).toInt
            + "," + (cylinder.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getGreen * 255).toInt + ","
            + (cylinder.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor.getBlue * 255).toInt + ")"
            + " " + cylinder.getTranslateX.toInt + " " + cylinder.getTranslateY.toInt + " " + cylinder.getTranslateZ.toInt + " "
            + cylinder.getScaleX + " " + cylinder.getScaleY + " " + cylinder.getScaleZ + "\n")

        case _ =>
      }
      t
    })
    pw.close()
  }


}