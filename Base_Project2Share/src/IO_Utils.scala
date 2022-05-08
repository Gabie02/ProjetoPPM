import javafx.application.Application
import javafx.scene.shape.Shape3D

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object IO_Utils {

  def showPrompt(): Unit = {
    println("-- Options --")
    println("a) Escolher ficheiro de configuração")
    println("b) Mudar a cor da Tree")
    println("c) Mudar a escala da Tree")
    println("d) Mudar a visibilidade da Tree")
    println("e) Lançar ambiente 3D")
    println("q) Sair")
    print("Opção: ")
  }

  def getUserInput(): String = readLine.trim.toUpperCase

  //Incompleto!!
//  @tailrec
  def mainLoop() {
    showPrompt()
    val userInput = getUserInput()

    // handle the result
    userInput match {
      case "A" => mainLoop()

      case "B" => mainLoop()

      case "C" => mainLoop()

      case "D" => mainLoop()

      case "E" => Application.launch(classOf[Main])

      case "Q" =>

      case _  => println("Input Inválido! Escolher uma das opções");mainLoop()
    }
  }

  // Consergue ler ficheiros que estejam na mesma diretoria da main
  def readFromFile(file: String):Array[Shape3D] = {
    val shapes = new ListBuffer[Shape3D]()
    val dir = System.getProperty("user.dir")
    val bufferedSource = Source.fromFile(s"$dir/$file")
    for (line <- bufferedSource.getLines){
      shapes += convertShapes(line)

    }
    bufferedSource.close
    shapes.toArray
  }

  private def convertShapes(line: String):Shape3D = {
    GraphicModelConstructor.buildObject(line).get
  }

}
