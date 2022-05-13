import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage

class Main extends Application {

  override def start(stage: Stage): Unit = {

    //Get and print program arguments (args: Array[String])
//        val params = getParameters
//        println("Program arguments:" + params.getRaw)

//    Se o utilizador selecionou para escolher o ficheiro, usar a octree dada pelo utils, caso contrário fazer a arvore aqui (com o atributo da main?)
//    if (IO_Utils)
    stage.setTitle("Projeto PPM 21/22")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    stage.setScene(scene)
    stage.show()
  }

  override def init(): Unit = {
    println("init")
  }

  override def stop(): Unit = {
    println("stopped")
  }
}

object FxApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args: _*)
  }

}

