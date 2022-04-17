import OcNode.scaleOctree
import Octree.{Placement, Section}
import Octree1.Octree1.partitionId
import Octree1.root
import com.sun.javafx.scene.shape.ShapeHelper.ShapeAccessor
import javafx.application.Application
import javafx.geometry.Insets
import javafx.scene.paint.PhongMaterial
import javafx.scene.shape._
import javafx.scene.transform.{Rotate, Translate}
import javafx.scene.{Group, Node}
import javafx.stage.Stage
import javafx.geometry.Pos
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javafx.scene.{PerspectiveCamera, Scene, SceneAntialiasing, SubScene}
import javafx.scene.input.{KeyCode, ScrollEvent}

import scala.runtime.Nothing$

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

    val box1 = new Box(1, 1, 1)
    box1.setTranslateX(5)
    box1.setTranslateY(5)
    box1.setTranslateZ(5)
    box1.setMaterial(greenMaterial)
    box1.getBoundsInParent

    //Leitura do ficheiro de configuração
    println("Introduza o nome do ficheiro: ")
    val file = scala.io.StdIn.readLine()
    val listaShapes = IO_Utils.readFromFile(file)
    //listaShapes.foreach(e => println(e))
    println(listaShapes)

    // 3D objects (group of nodes - javafx.scene.Node) that will be provide to the subScene
    val worldRoot:Group = new Group(wiredBox, camVolume, lineX, lineY, lineZ, cylinder1, box1)

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

    //Mouse left click interaction
//    scene.setOnMouseClicked((event) => {
//      camVolume.setTranslateX(camVolume.getTranslateX + 2)
//      worldRoot.getChildren.removeAll()
//    })

    //Tentar fazer com que o scroll amplie a imagem
    //scene.setOnScroll(() =>{})

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



    //example of bounding boxes (corresponding to the octree oct1) added manually to the world
    val b2 = new Box(8,8,8)
    //translate because it is added by default to the coords (0,0,0)
    b2.setTranslateX(8/2)
    b2.setTranslateY(8/2)
    b2.setTranslateZ(8/2)
    b2.setMaterial(redMaterial)
    if(camVolume.asInstanceOf[Shape3D].getBoundsInParent.intersects(b2.getBoundsInParent))
      b2.setMaterial(greenMaterial)
    b2.setDrawMode(DrawMode.LINE)

    val b3 = new Box(4,4,4)
    //translate because it is added by default to the coords (0,0,0)
    b3.setTranslateX(4/2)
    b3.setTranslateY(4/2)
    b3.setTranslateZ(4/2)
    b3.setMaterial(redMaterial)
    if(camVolume.asInstanceOf[Shape3D].getBoundsInParent.intersects(b3.getBoundsInParent))
      b3.setMaterial(greenMaterial)
    b3.setDrawMode(DrawMode.LINE)

    //oct1 - example of an Octree[Placement] that contains only one Node (i.e. cylinder1)
    //In case of difficulties to implement task T2 this octree can be used as input for tasks T3, T4 and T5

    val placement1: Placement = ((0, 0, 0), 8.0)
    val sec1: Section = (((0.0,0.0,0.0), 4.0), List(cylinder1.asInstanceOf[Node], b2.asInstanceOf[Node], b3.asInstanceOf[Node]))
    val ocLeaf1 = OcLeaf(sec1)
    var oct1:Octree[Placement] = OcNode[Placement](placement1, ocLeaf1, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)

    oct1 = scaleOctree(0.5, oct1)

    //example of bounding boxes (corresponding to the octree oct1) added manually to the world
    //val b2 = new Box(8,8,8)
    //translate because it is added by default to the coords (0,0,0)
    //b2.setTranslateX(8/2)
    //b2.setTranslateY(8/2)
    //b2.setTranslateZ(8/2)
    //b2.setMaterial(redMaterial)
    //if(camVolume.asInstanceOf[Shape3D].getBoundsInParent.intersects(b2.getBoundsInParent))
      //b2.setMaterial(greenMaterial)
    //b2.setDrawMode(DrawMode.LINE)

    //val b3 = new Box(4,4,4)
    //translate because it is added by default to the coords (0,0,0)
    //b3.setTranslateX(4/2)
    //b3.setTranslateY(4/2)
    //b3.setTranslateZ(4/2)
    //b3.setMaterial(redMaterial)
    //if(camVolume.asInstanceOf[Shape3D].getBoundsInParent.intersects(b3.getBoundsInParent))
      //b3.setMaterial(greenMaterial)
    //b3.setDrawMode(DrawMode.LINE)


    val teste1 = new Box(8.0,8.0,8.0)
    teste1.setTranslateX(8)
    teste1.setTranslateY(8)
    teste1.setTranslateZ(24)
    teste1.setMaterial(redMaterial)
    teste1.setDrawMode(DrawMode.LINE)
    val teste2 = new Box(8.0,8.0,8.0)
    teste2.setTranslateX(8)
    teste2.setTranslateY(0)
    teste2.setTranslateZ(24)
    teste2.setMaterial(redMaterial)
    teste2.setDrawMode(DrawMode.LINE)
    val teste3 = new Box(8.0,8.0,8.0)
    teste3.setTranslateX(0)
    teste3.setTranslateY(8)
    teste3.setTranslateZ(24)
    teste3.setMaterial(redMaterial)
    teste3.setDrawMode(DrawMode.LINE)
    val teste4 = new Box(8.0,8.0,8.0)
    teste4.setTranslateX(0)
    teste4.setTranslateY(0)
    teste4.setTranslateZ(24)
    teste4.setMaterial(redMaterial)
    teste4.setDrawMode(DrawMode.LINE)

    val teste11 = new Box(8.0,8.0,8.0)
    teste11.setTranslateX(8)
    teste11.setTranslateY(8)
    teste11.setTranslateZ(16)
    teste11.setMaterial(redMaterial)
    teste11.setDrawMode(DrawMode.LINE)
    val teste22 = new Box(8.0,8.0,8.0)
    teste22.setTranslateX(8)
    teste22.setTranslateY(0)
    teste22.setTranslateZ(16)
    teste22.setMaterial(redMaterial)
    teste22.setDrawMode(DrawMode.LINE)
    val teste33 = new Box(8.0,8.0,8.0)
    teste33.setTranslateX(0)
    teste33.setTranslateY(8)
    teste33.setTranslateZ(16)
    teste33.setMaterial(redMaterial)
    teste33.setDrawMode(DrawMode.LINE)
    val teste44 = new Box(8.0,8.0,8.0)
    teste44.setTranslateX(0)
    teste44.setTranslateY(0)
    teste44.setTranslateZ(16)
    teste44.setMaterial(redMaterial)
    teste44.setDrawMode(DrawMode.LINE)

    worldRoot.getChildren.add(teste1)
    worldRoot.getChildren.add(teste2)
    worldRoot.getChildren.add(teste3)
    worldRoot.getChildren.add(teste4)
    worldRoot.getChildren.add(teste11)
    worldRoot.getChildren.add(teste22)
    worldRoot.getChildren.add(teste33)
    worldRoot.getChildren.add(teste44)

    //adding boxes b2 and b3 to the world
//    worldRoot.getChildren.add(b2)
//    worldRoot.getChildren.add(b3)

    //Permite mover a camera com as arrow keys
    scene.setOnKeyPressed(event => { event.getCode() match {
      case KeyCode.UP =>
        camVolume.setTranslateY(camVolume.getTranslateY - 2)
        intersectsCamera(oct1,camVolume)
      //        worldRoot.getChildren.removeAll()
      case KeyCode.DOWN =>
        camVolume.setTranslateY(camVolume.getTranslateY + 2)
        intersectsCamera(oct1,camVolume)
      //        worldRoot.getChildren.removeAll()
      case KeyCode.LEFT =>
        camVolume.setTranslateX(camVolume.getTranslateX - 2)
        intersectsCamera(oct1,camVolume)
      //        worldRoot.getChildren.removeAll()
      case KeyCode.RIGHT =>
        camVolume.setTranslateX(camVolume.getTranslateX + 2)
        intersectsCamera(oct1,camVolume)
      //        worldRoot.getChildren.removeAll()
      case _ =>
    }
    })
    OcNode.createTree2(worldRoot, listaShapes.toList,((0.0,0.0,0.0), 32))
    intersectsCamera(oct1, camVolume)

  }

//  def createTree():Octree[Placement] = {
//    val placement1: Placement = ((0, 0, 0), 8.0)
//    val sec1: Section = (((0.0,0.0,0.0), 4.0), List(cylinder1.asInstanceOf[Node]))
//    val ocLeaf1 = OcLeaf(sec1)
//    val oct1:Octree[Placement] = OcNode[Placement](placement1, ocLeaf1, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)


//  }

  def intersectsCamera(oct:Octree[Placement], camVolume: Cylinder):Unit = {
    oct match {
      // Se for um ocNode
      case _: OcNode[Placement] =>
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
    //------- Area de Testes -------
    //IO_Utils.readFromFile(s"$args")
    //println("Partition: " + partitionId(root).toString())

    //------------------------------
    Application.launch(classOf[Main], args: _*)

  }


}

