import IO_Utils.oct
import OctreeUtils.mapColourEffect
import javafx.fxml.FXML
import javafx.scene.control.{Button, RadioButton, ToggleButton, ToggleGroup}
import javafx.scene.{Group, SubScene}
import javafx.scene.layout.{AnchorPane, GridPane}
import javafx.fxml.FXML
import javafx.scene.control.ToggleGroup

import java.awt.event.MouseEvent

class Controller {

  @FXML
  private var subScene1:SubScene = _
  @FXML
  private var gridPane1:GridPane = _
  @FXML
  private var anchorPane1:AnchorPane = _

  @FXML
  private var button_color : Button = _
  @FXML
  private var button_scale : Button = _
  @FXML
  private var button_visible : ToggleButton = _
  @FXML
  private var button_sepia : RadioButton = _
  @FXML
  private var button_greenRemove : RadioButton = _
  @FXML
  private var button_fator2 : RadioButton = _
  @FXML
  private var button_fator05 : RadioButton = _

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
    def onButtonClicked_color():Unit = {

      if (button_sepia.isSelected) OctreeUtils.mapColourEffect(c => OctreeUtils.sepiaEffect(c))(IO_Utils.oct)
      else if (button_greenRemove.isSelected) OctreeUtils.mapColourEffect(c => OctreeUtils.greenRemove(c))(IO_Utils.oct)
    }

  //def onButtonClicked_scale(): Octree[((Double, Double, Double), OctreeUtils.Size)] = {
    //case button_fator2.isSelected => button_scale.setOnMouseClicked(e=>OctreeUtils.scaleOctree(2, IO_Utils.oct))
    //case button_fator05.isSelected  => button_scale.setOnMouseClicked(e=>OctreeUtils.scaleOctree(0.5, IO_Utils.oct))
  //}


  def onButtonClicked_scale():Unit = {

    //println("Botao 0.5: " + button_fator05.isSelected.toString)
    //println("Botao 2: " + button_fator2.isSelected.toString)

    if (button_fator2.isSelected) OctreeUtils.scaleOctree(2, IO_Utils.oct)
    else if(button_fator05.isSelected) OctreeUtils.scaleOctree(0.5, IO_Utils.oct)

  }

    //def onButtonClicked_visible():Unit = {
      //button_visible
    //}

}
