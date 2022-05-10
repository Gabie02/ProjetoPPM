import OctreeUtils.Placement
import scala.annotation.tailrec
import java.io.{FileNotFoundException, IOException}
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}


object IO_Utils {

  val oct = IO_Utils.getFile

  def showPrompt(): Unit = {
    println("-- Options --" +
      "\na) Escolher outro ficheiro de configuração " +
      "\nb) Mudar a cor da Tree " +
      "\nc) Mudar a escala da Tree" +
      "\nd) Lançar ambiente 3D" +
//      "\nq) Sair" +
      "\nOpção: ")
  }

  def getUserInput: String = readLine.trim

  @tailrec
  def getFile: Octree[Placement] = {
    println("Diga qual o nome do ficheiro de texto que pretende usar: ")
    val file = getUserInput
    val shapes = Try(GraphicModelConstructor.readFromFile(file))
    shapes match {
      case Success(s) =>
        println("Ficheiro lido com sucesso!")
        val oct1 = OctreeUtils.createTree(s)
        mainLoop(oct1)
      /* Guardar ficheiro em algum lado */
      case Failure(e) => e match {
        case _:FileNotFoundException => println("Ficheiro não encontrado. Selecione outro."); getFile
        case e:IllegalArgumentException => println(e + " Selecione outro ficheiro."); getFile
        case _:IOException => println("Ocorreu um erro a ler o ficheiro. Selecione outro."); getFile
      }
    }
  }


  @tailrec
  def mainLoop(oct:Octree[Placement]):Octree[Placement] = {
    showPrompt()
    val userInput = getUserInput

    userInput.toUpperCase match {
      case "A" =>
        getFile
      case "B" =>
        println("Que efeito pretende aplicar?" +
          "\na) Remover verdes" +
          "\nb) Efeito sépia")
        getUserInput.toUpperCase match {
          case "A" => mainLoop(OctreeUtils.mapColourEffect(c => OctreeUtils.greenRemove(c))(oct))
          case "B" => mainLoop(OctreeUtils.mapColourEffect(c => OctreeUtils.sepiaEffect(c))(oct))
        }
      case "C" =>
        //Perguntar para selecionar a escala
        println("Que escala pretende aplicar?" +
          "\na) Escala 2" +
          "\nb) Escala 0.5")
        getUserInput.toUpperCase match {
          case "A" => mainLoop(OctreeUtils.scaleOctree(2, oct))
          case "B" => mainLoop(OctreeUtils.scaleOctree(0.5, oct))
        }
      case "D" => oct
//      case "Q" =>
      case _  =>
        println("Input Inválido! Escolher uma das opções")
        mainLoop(oct)

    }
  }

  def main(args: Array[String]): Unit = {
    FxApp.main(args)
  }
}
