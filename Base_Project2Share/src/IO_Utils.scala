import scala.io.Source

object IO_Utils {

  // Consergue ler ficheiros que estejam na mesma diretoria da main
  def readFromFile(file: String) = {
    val dir = System.getProperty("user.dir")
    val bufferedSource = Source.fromFile(s"$dir/$file")
    for (line <- bufferedSource.getLines){
      println(line.toUpperCase)
      convertShapes(line)
    }
    bufferedSource.close
  }

  def convertShapes(line: String)= {
    val gmc = GraphicModelConstructor()
    gmc.buildObject(line)
  }

}
