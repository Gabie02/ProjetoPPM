
import OctreeUtils.{Placement, createTree}
import javafx.fxml.FXML
import javafx.scene.control.{Button, Label, RadioButton, TextField, ToggleButton, ToggleGroup}
import javafx.scene.{Group, SubScene}
import javafx.scene.layout.{AnchorPane, GridPane}
import javafx.scene.paint.Color
import java.io.{FileNotFoundException, IOException}
import scala.util.{Failure, Success, Try}

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
  @FXML
  private var text_field: TextField = _
  @FXML
  private var msg: Label = _

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

      if (button_sepia.isSelected) IO_Utils.OcTree = OctreeUtils.mapColourEffect(c => OctreeUtils.sepiaEffect(c))(IO_Utils.OcTree)
      else if (button_greenRemove.isSelected) IO_Utils.OcTree = OctreeUtils.mapColourEffect(c => OctreeUtils.greenRemove(c))(IO_Utils.OcTree)
    }

  def onButtonClicked_scale():Unit = {

    if (button_fator2.isSelected)  OctreeUtils.scaleOctree(2, IO_Utils.OcTree)
    else if(button_fator05.isSelected)  OctreeUtils.scaleOctree(0.5, IO_Utils.OcTree)
  }

  def onButtenClicked_chooseFile():Unit = {
    val file = text_field.getText
    val shapeList = Try(GraphicModelConstructor.readFromFile(file))
    shapeList match {
      case Success(s) =>
        msg.setTextFill(Color.GREEN)
        msg.setText("Ficheiro carregado com sucesso!");
        msg.setVisible(true)
        //Remover tree anterior
        val previousShapeList = OctreeUtils.getAllShapes(IO_Utils.OcTree)
        (previousShapeList foldRight()) ((h,_) => InitSubScene.worldRoot.getChildren.remove(h))
        //Adicionar a nova tree
        IO_Utils.OcTree = createTree(s)
        OctreeUtils.addOctreeToWorldRoot(IO_Utils.OcTree, InitSubScene.worldRoot)

      case Failure(e) => e match {
        case _: FileNotFoundException => msg.setTextFill(Color.RED); msg.setText("Ficheiro não encontrado. Selecione outro.");
        case e: IllegalArgumentException => msg.setTextFill(Color.RED); msg.setText(e.getMessage + " Selecione outro ficheiro.");
        case _: IOException => msg.setTextFill(Color.RED); msg.setText("Ocorreu um erro a ler o ficheiro. Selecione outro.");
        case _ => msg.setTextFill(Color.RED); msg.setText("Ocorreu um erro inesperado. Selecione outro ficheiro.");
      }
      msg.setVisible(true)
    }
  }

  def onButtenClicked_loadPreviousState(): Unit = {
    //Remover Tree
    val previousShapeList = OctreeUtils.getAllShapes(IO_Utils.OcTree)
    (previousShapeList foldRight()) ((h,_) => InitSubScene.worldRoot.getChildren.remove(h))
    //Adicionar nova tree
    val shapeList = Try(GraphicModelConstructor.readFromFile("lastTree.txt")).get
    IO_Utils.OcTree = createTree(shapeList)
    OctreeUtils.addOctreeToWorldRoot(IO_Utils.OcTree, InitSubScene.worldRoot)
  }

}
