import javafx.scene.shape.Shape3D

import scala.io.Source
import scala.collection.mutable.ListBuffer

object IO_Utils {

  // Consergue ler ficheiros que estejam na mesma diretoria da main
  def readFromFile(file: String):Array[Shape3D] = {
    var shapes = new ListBuffer[Shape3D]()
    val dir = System.getProperty("user.dir")
    val bufferedSource = Source.fromFile(s"$dir/$file")
    for (line <- bufferedSource.getLines){
      //println(line.toUpperCase)
      shapes += convertShapes(line)

    }
    bufferedSource.close
    //println(shapes)
    shapes.toArray
  }



  def convertShapes(line: String):Shape3D = {
    val gmc = GraphicModelConstructor()
    gmc.buildObject(line)
  }

}
