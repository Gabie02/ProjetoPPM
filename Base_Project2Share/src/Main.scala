import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class Main extends Application {

  override def start(stage: Stage): Unit = {
    stage.setTitle("Projeto PPM 21/22")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Controller.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    stage.setScene(scene)
    stage.show()
  }

  override def init(): Unit = {

  }

  override def stop(): Unit = {
    GraphicModelConstructor.writeFile(IO_Utils.OcTree)
  }
}

object FxApp {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args: _*)
  }

}

