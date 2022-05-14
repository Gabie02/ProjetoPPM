//import IO_Utils.oct
import OctreeUtils.mapColourEffect
import javafx.fxml.FXML
import javafx.scene.control.{Button, RadioButton, ToggleButton, ToggleGroup}
import javafx.scene.{Group, SubScene}
import javafx.scene.layout.{AnchorPane, GridPane}

class Controller {

  @FXML
  private var subScene1:SubScene = _
  @FXML
  private var anchorPane1:AnchorPane = _
  @FXML
  private var button_sepia : RadioButton = _
  @FXML
  private var button_greenRemove : RadioButton = _
  @FXML
  private var button_fator2 : RadioButton = _
  @FXML
  private var button_fator05 : RadioButton = _


  //  --- T7 ---
  @FXML
  def initialize(): Unit = {
    InitSubScene.subScene.widthProperty.bind(subScene1.widthProperty)
    InitSubScene.subScene.heightProperty.bind(subScene1.heightProperty)
    subScene1.widthProperty.bind(anchorPane1.widthProperty)
    subScene1.heightProperty.bind(anchorPane1.heightProperty)
    subScene1.setRoot(InitSubScene.root)

    OctreeUtils.addOctreeToWorldRoot(IO_Utils.OcTree, InitSubScene.worldRoot)
  }
    def onButtonClicked_color():Unit = {

      if (button_sepia.isSelected) OctreeUtils.mapColourEffect(c => OctreeUtils.sepiaEffect(c))(IO_Utils.OcTree)
      else if (button_greenRemove.isSelected) OctreeUtils.mapColourEffect(c => OctreeUtils.greenRemove(c))(IO_Utils.OcTree)
    }

  def onButtonClicked_scale():Unit = {

    if (button_fator2.isSelected) OctreeUtils.scaleOctree(2, IO_Utils.OcTree)
    else if(button_fator05.isSelected) OctreeUtils.scaleOctree(0.5, IO_Utils.OcTree)
  }

//
//  def onButtenClicked_chooseFile():Unit = {
//    import javafx.stage.FileChooser
//    val fileChooser = new FileChooser
//    fileChooser.setTitle("Open Resource File")
//    fileChooser.getExtensionFilters.addAll(new Nothing("Text Files", "*.txt"), new Nothing("Image Files", "*.png", "*.jpg", "*.gif"), new Nothing("Audio Files", "*.wav", "*.mp3", "*.aac"), new Nothing("All Files", "*.*"))
//    val selectedFile = fileChooser.showOpenDialog()
//    if (selectedFile != null) mainStage.display(selectedFile)
//  }

}
