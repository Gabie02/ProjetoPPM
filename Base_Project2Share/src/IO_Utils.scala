import OctreeUtils.{Placement, createTree}

import scala.annotation.tailrec
import java.io.{FileNotFoundException, IOException}
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}


object IO_Utils extends App {

  var OcTree = mainLoop(loadFile)
  FxApp.main(args)

  //  --- T6 ---
  def showPrompt(): Unit = {
    println("-- Options --" +
      "\na) Escolher outro ficheiro de configuração " +
      "\nb) Carregar estado da última utilização" +
      "\nc) Mudar a cor da Tree " +
      "\nd) Mudar a escala da Tree" +
      "\ne) Lançar ambiente 3D" +
      "\nOpção: ")
  }

  def getUserInput: String = readLine.trim

  def handleIOExceptions(e: Throwable): Unit = e match {
    case _: FileNotFoundException => println("Ficheiro não encontrado. Selecione outro.");
    case e: IllegalArgumentException => println(e.getMessage + " Selecione outro ficheiro.");
    case _: IOException => println("Ocorreu um erro a ler o ficheiro. Selecione outro.");
    case _ => println("Ocorreu um erro inesperado. Selecione outro ficheiro.");
  }

  @tailrec
  def getFile: Octree[Placement] = {
    println("Diga qual o nome do ficheiro de texto que pretende usar: ")
    val file = getUserInput
    val shapes = Try(GraphicModelConstructor.readFromFile(file))
    shapes match {
      case Success(s) =>
        println("Ficheiro lido com sucesso!")
        OctreeUtils.createTree(s)
      case Failure(e) => handleIOExceptions(e);getFile
    }
  }

  @tailrec
  def loadFile: Octree[Placement] = {
    println("Pretende carregar um novo ficheiro ou o estado da última sessão?" +
      "\na) Novo ficheiro" +
      "\nb) Última sessão")
    val option = getUserInput
    option.toUpperCase match {
      case "A" =>
        getFile
      case "B" =>
        val shapes = Try( GraphicModelConstructor.readFromFile("lastTree.txt"))
        shapes match {
          case Success(s) =>
          println("Ficheiro lido com sucesso!")
          s match {
            case Nil => println("Não existe um última sessão, ou então não foi guardada."); loadFile
            case _ => println("Última sessão carregada com sucesso! "); createTree(s)
          }
        case Failure(e) => handleIOExceptions(e); loadFile
        }
      case _ => println("Input Inválido! Escolher uma das opções (a, b, ...)"); loadFile
    }
  }


  @tailrec
  def mainLoop(oct: Octree[Placement]): Octree[Placement] = {
    showPrompt()
    val userInput = getUserInput

    userInput.toUpperCase match {
      case "A" =>
        getFile
      case "B" =>
        val shapeList = GraphicModelConstructor.readFromFile("lastTree.txt")
        mainLoop(createTree(shapeList))
      case "C" =>
        println("Que efeito pretende aplicar?" +
          "\na) Remover verdes" +
          "\nb) Efeito sépia")
        getUserInput.toUpperCase match {
          case "A" => OctreeUtils.mapColourEffect(c => OctreeUtils.greenRemove(c))(oct); mainLoop(oct)
          case "B" => OctreeUtils.mapColourEffect(c => OctreeUtils.sepiaEffect(c))(oct); mainLoop(oct)
          case _ => println("Input Inválido! Escolher uma das opções (a, b, ...)"); mainLoop(oct)
        }
      case "D" =>
        println("Que escala pretende aplicar?" +
          "\na) Escala 2" +
          "\nb) Escala 0.5")
        getUserInput.toUpperCase match {
          case "A" => OctreeUtils.scaleOctree(2, oct); mainLoop(oct)
          case "B" => OctreeUtils.scaleOctree(0.5, oct); mainLoop(oct)
          case _ => println("Input Inválido! Escolher uma das opções (a, b, ...)"); mainLoop(oct)
        }
      case "E" => oct
      case _ =>
        println("Input Inválido! Escolher uma das opções (a, b, ...)")
        mainLoop(oct)
    }
  }

}


