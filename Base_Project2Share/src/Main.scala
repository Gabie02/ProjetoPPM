import OcNode.{auxScale, mapColourEffect, scaleOctree}
import javafx.application.Application
import javafx.geometry.Insets
import javafx.scene.paint.PhongMaterial
import javafx.scene.shape._
import javafx.scene.transform.Rotate
import javafx.scene.{Group, Node}
import javafx.stage.Stage
import javafx.geometry.Pos
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.{PerspectiveCamera, Scene, SceneAntialiasing, SubScene}
import javafx.scene.input.KeyCode


class Main extends Application {

  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size

  //Shape3D is an abstract class that extends javafx.scene.Node
  //Box and Cylinder are subclasses of Shape3D
  type Section = (Placement, List[Node])  //example: ( ((0.0,0.0,0.0), 2.0), List(new Cylinder(0.5, 1, 10)))


  /*
    Additional information about JavaFX basic concepts (e.g. Stage, Scene) will be provided in week7
   */
  override def start(stage: Stage): Unit = {

    //Get and print program arguments (args: Array[String])
    val params = getParameters
    println("Program arguments:" + params.getRaw)

    //Materials to be applied to the 3D objects
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150,0,0))

    val greenMaterial = new PhongMaterial()
    greenMaterial.setDiffuseColor(Color.rgb(0,255,0))

    val blueMaterial = new PhongMaterial()
    blueMaterial.setDiffuseColor(Color.rgb(0,0,150))

    //3D objects
    val lineX = new Line(0, 0, 200, 0)
    lineX.setStroke(Color.GREEN)

    val lineY = new Line(0, 0, 0, 200)
    lineY.setStroke(Color.YELLOW)

    val lineZ = new Line(0, 0, 200, 0)
    lineZ.setStroke(Color.LIGHTSALMON)
    lineZ.getTransforms().add(new Rotate(-90, 0, 0, 0, Rotate.Y_AXIS))

    val camVolume = new Cylinder(10, 50, 10)
    camVolume.setTranslateX(1)
    camVolume.getTransforms().add(new Rotate(45, 0, 0, 0, Rotate.X_AXIS))
    camVolume.setMaterial(blueMaterial)
    camVolume.setDrawMode(DrawMode.LINE)

    val wiredBox = new Box(32, 32, 32)
    wiredBox.setTranslateX(16)
    wiredBox.setTranslateY(16)
    wiredBox.setTranslateZ(16)
    wiredBox.setMaterial(redMaterial)
    wiredBox.setDrawMode(DrawMode.LINE)

    val cylinder1 = new Cylinder(0.5, 1, 10)
    cylinder1.setTranslateX(2)
    cylinder1.setTranslateY(2)
    cylinder1.setTranslateZ(2)
    cylinder1.setScaleX(2)
    cylinder1.setScaleY(2)
    cylinder1.setScaleZ(2)
    cylinder1.setMaterial(greenMaterial)

    //Leitura do ficheiro de configuração
    val listaShapes = IO_Utils.readFromFile(params.getRaw.get(0))

    // 3D objects (group of nodes - javafx.scene.Node) that will be provide to the subScene
    val worldRoot:Group = new Group(wiredBox, camVolume, lineX, lineY, lineZ, cylinder1)

    //Adicionar shapes à scene
    (listaShapes foldRight ()) ((h, t) => {
      worldRoot.getChildren.add(h); t
    })

    // Camera
    val camera = new PerspectiveCamera(true)

    val cameraTransform = new CameraTransformer
    cameraTransform.setTranslate(0, 0, 0)
    cameraTransform.getChildren.add(camera)
    camera.setNearClip(0.1)
    camera.setFarClip(10000.0)

    camera.setTranslateZ(-500)
    camera.setFieldOfView(20)
    cameraTransform.ry.setAngle(-45.0)
    cameraTransform.rx.setAngle(-45.0)
    worldRoot.getChildren.add(cameraTransform)

    // SubScene - composed by the nodes present in the worldRoot
    val subScene = new SubScene(worldRoot, 800, 600, true, SceneAntialiasing.BALANCED)
    subScene.setFill(Color.DARKSLATEGRAY)
    subScene.setCamera(camera)

    // CameraView - an additional perspective of the environment
    val cameraView = new CameraView(subScene)
    cameraView.setFirstPersonNavigationEabled(true)
    //cameraView.setFitWidth(350)
    cameraView.setFitWidth(400)
    //cameraView.setFitHeight(225)
    cameraView.setFitHeight(300)
    cameraView.getRx.setAngle(-45)
    cameraView.getT.setZ(-100)
    cameraView.getT.setY(-500)
    cameraView.getCamera.setTranslateZ(-50)
    cameraView.startViewing

      // Position of the CameraView: Right-bottom corner
      StackPane.setAlignment(cameraView, Pos.BOTTOM_RIGHT)
      StackPane.setMargin(cameraView, new Insets(5))

    // Scene - defines what is rendered (in this case the subScene and the cameraView)
    val root = new StackPane(subScene, cameraView)
    subScene.widthProperty.bind(root.widthProperty)
    subScene.heightProperty.bind(root.heightProperty)

    val scene = new Scene(root, 810, 610, true, SceneAntialiasing.BALANCED)

    scene.setOnScroll(event => {
      var zoomfactor = 1.05
      if(event.getDeltaY() < 0)
        zoomfactor = 2 - zoomfactor

      cameraView.setScaleX(cameraView.getScaleX * zoomfactor)
      cameraView.setScaleY(cameraView.getScaleY * zoomfactor)
    })

    //setup and start the Stage
    stage.setTitle("PPM Project 21/22")
    stage.setScene(scene)
    stage.show

val oct2 = OcNode.createTree(worldRoot, listaShapes.toList,((16.0,16.0,16.0), 32))

    //Permite mover a camera com as arrow keys
    scene.setOnKeyPressed(event => { event.getCode() match {
      case KeyCode.UP =>
        camVolume.setTranslateY(camVolume.getTranslateY - 2)
        intersectsCamera(oct2,camVolume)
      //        worldRoot.getChildren.removeAll()
      case KeyCode.DOWN =>
        camVolume.setTranslateY(camVolume.getTranslateY + 2)
        intersectsCamera(oct2,camVolume)
      //        worldRoot.getChildren.removeAll()
      case KeyCode.LEFT =>
        camVolume.setTranslateX(camVolume.getTranslateX - 2)
        intersectsCamera(oct2,camVolume)
      //        worldRoot.getChildren.removeAll()
      case KeyCode.RIGHT =>
        camVolume.setTranslateX(camVolume.getTranslateX + 2)
        intersectsCamera(oct2,camVolume)
      //        worldRoot.getChildren.removeAll()
      case _ =>
    }
    })
    intersectsCamera(oct2, camVolume)
    scaleOctree(2, oct2)
//    mapColourEffect(x => OcNode.greenRemove(x))(oct2)
  }

  def intersectsCamera(oct:Octree[Placement], camVolume: Cylinder):Unit = {
    oct match {
      // Se for um ocNode
      case _: OcNode[Placement] =>
        //println("ISTO É UM NODE")
        val ocnode = oct.asInstanceOf[OcNode[Placement]]
        intersectsCamera(ocnode.up_00, camVolume)
        intersectsCamera(ocnode.up_01, camVolume)
        intersectsCamera(ocnode.up_10, camVolume)
        intersectsCamera(ocnode.up_11, camVolume)
        intersectsCamera(ocnode.down_00, camVolume)
        intersectsCamera(ocnode.down_01, camVolume)
        intersectsCamera(ocnode.down_10, camVolume)
        intersectsCamera(ocnode.down_11, camVolume)


        // Se for uma ocLeaf
      case _: OcLeaf[Placement, Section] =>
        //println("ISTO É UMA LEAF")
          val ocleaf = oct.asInstanceOf[OcLeaf[Placement, Section]]
          val listaSection = ocleaf.section._2
          // Se a lista de nodes da section nao estiver vazia
          if (listaSection.nonEmpty) {
            listaSection.foldRight(0)((h, t) => {
              h.asInstanceOf[Shape3D]
              if (h.isInstanceOf[Box]) {
                val box = h.asInstanceOf[Box]
                if (box.getDrawMode.equals(DrawMode.LINE)) {
                  // Se intersetar
                  if (camVolume.asInstanceOf[Shape3D].getBoundsInParent.intersects(box.getBoundsInParent)) {
                    val greenMaterial = new PhongMaterial()
                    greenMaterial.setDiffuseColor(Color.rgb(0, 255, 0))
                    box.setMaterial(greenMaterial)
                  } else {
                    val redMaterial = new PhongMaterial()
                    redMaterial.setDiffuseColor(Color.rgb(150, 0, 0))
                    box.setMaterial(redMaterial)
                  }
                }
              }
              t
            })
          }

      case _ =>
        //println("ISTO É EMPTY")
    }
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

