import javafx.application.Application
import javafx.scene.shape.Shape3D

import java.io.{FileNotFoundException, IOException}
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.util.{Try, Success, Failure}

object IO_Utils {

  def showPrompt(): Unit = {
    println("-- Options --" +
      "\na) Escolher ficheiro de configuração " +
      "\nb) Mudar a cor da Tree " +
      "\nc) Mudar a escala da Tree" +
      "\nd) Mudar a visibilidade da Tree" +
      "\ne) Lançar ambiente 3D" +
      "\nq) Sair" +
      "\nOpção: ")
  }

  def getUserInput: String = readLine.trim

  //Incompleto!!
  @tailrec
  def mainLoop(oct ) {
    showPrompt()
    val userInput = getUserInput

    userInput.toUpperCase match {
      case "A" => oct1 = scal
        println("Diga qual o nome do ficheiro de texto que pretende usar: ")
        val file = getUserInput
        val shapes = Try(readFromFile(file))
        shapes match {
          case Success(s) =>
            println("Ficheiro lido com sucesso!")
            /* Guardar ficheiro em algum lado */
          case Failure(e) => e match {
            case _:FileNotFoundException => println("Ficheiro não encontrado. Selecione outro."); mainLoop
            case e:IllegalArgumentException => println(e + " Selecione outro ficheiro."); mainLoop
            case _:IOException => println("Ocorreu um erro a ler o ficheiro. Selecione outro."); mainLoop
          }
        }

      case "B" => mainLoop

      case "C" => mainLoop

      case "D" => mainLoop

      case "E" => Application.launch(classOf[Main])

      case "Q" =>

      case _  =>
        println("Input Inválido! Escolher uma das opções")
        mainLoop
    }
  }

  // Consergue ler ficheiros a partir da diretoria da main
  def readFromFile(file: String): Array[Shape3D] = {
    val shapes = new ListBuffer[Shape3D]()
    val dir = System.getProperty("user.dir")
    val bufferedSource = Source.fromFile(s"$dir/$file")
    if (bufferedSource.isEmpty)
      println("Ficheiro vazio!")
    for (line <- bufferedSource.getLines) {
      shapes += convertShapes(line)
    }
    bufferedSource.close
    shapes.toArray
  }

  private def convertShapes(line: String):Shape3D = {
    GraphicModelConstructor.buildObject(line)
  }

main
  val oct1 = IO_Utils.mainLoop()
  FxApp.main()
}
