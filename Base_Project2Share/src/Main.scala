import OctreeUtils.Placement
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.paint.PhongMaterial
import javafx.scene.shape._
import javafx.scene.{Node, Parent, Scene}
import javafx.stage.Stage
import javafx.scene.paint.Color

class Main extends Application {

  //Wiredbox que limita o espaço 3D
  val SPACE_LIMIT:Shape3D = OctreeUtils.createWiredBox((0,0,0), 32)

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
  //  --- T3 ---
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
  def intersectsCamera2(oct:Octree[Placement], camVolume: Cylinder):Unit = {
    oct match {
      // Se for um ocNode
      case _: OcNode[Placement] =>
        println("SOU UM NODE")
        val ocnode = oct.asInstanceOf[OcNode[Placement]]
        val atribList = OctreeUtils.createAttributesList(ocnode)
        (atribList foldRight List[Octree[Placement]]()) ((h,t) => {
          intersectsCamera2(h, camVolume); t
        })

        // Se for uma ocLeaf
      case _: OcLeaf[Placement, Section] =>
          println("SOU UMA LEAF")
          val ocleaf = oct.asInstanceOf[OcLeaf[Placement, Section]]
          val listaSection = ocleaf.section._2
          // Se a lista de nodes da section nao estiver vazia
            listaSection.foldRight(0)((h, t) => {
              h.asInstanceOf[Shape3D]
              if (h.isInstanceOf[Box]) {
                val box = h.asInstanceOf[Box]
                //Verifica se é uma wiredBox?
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
    Application.launch(classOf[Main], args: _*)
  }

}

