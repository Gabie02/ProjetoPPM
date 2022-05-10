import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.scene.{Group, SubScene}
import javafx.scene.layout.{AnchorPane, GridPane}

class Controller {

  @FXML
  private var subScene1:SubScene = _
  @FXML
  private var gridPane1:GridPane = _
  @FXML
  private var anchorPane1:AnchorPane = _

  //method automatically invoked after the @FXML fields have been injected
  @FXML
  def initialize(): Unit = {
    InitSubScene.subScene.widthProperty.bind(subScene1.widthProperty)
    InitSubScene.subScene.heightProperty.bind(subScene1.heightProperty)
    subScene1.widthProperty.bind(anchorPane1.widthProperty)
    subScene1.heightProperty.bind(anchorPane1.heightProperty)
    subScene1.setRoot(InitSubScene.root)

//    InitSubScene.worldRoot.getChildren.add(OctreeUtils.SPACE_LIMIT)
    OctreeUtils.addOctreeToWorldRoot(IO_Utils.oct, InitSubScene.worldRoot)

  }
}
