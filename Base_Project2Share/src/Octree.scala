
import javafx.scene.Node


sealed trait Octree[+A]

case class OcNode[A](placement: A,
                    up_00: Octree[A], up_01: Octree[A],
                     up_10: Octree[A], up_11: Octree[A],
                     down_00: Octree[A], down_01: Octree[A],
                     down_10: Octree[A], down_11: Octree[A]
                    ) extends Octree[A]

case class OcLeaf[A, B](section: B) extends Octree[A]

case object OcEmpty extends Octree[Nothing]


case object OctreeUtils extends Octree[Nothing]

  object Octree {

    type Point = (Double, Double, Double)
    type Size = Double
    type Placement = (Point, Size) //1st point: origin, 2nd point: size
    type Section = (Placement, List[Node])

    def auxScale(fact: Double, oct: Octree[Placement]): (Octree[Placement]) = {
      val octSize = oct.asInstanceOf[OcNode[Placement]].placement._2
      val octScale = octSize * fact

      val list_ocNodes = List() :+ oct.asInstanceOf[OcNode[Placement]].up_00 :+ oct.asInstanceOf[OcNode[Placement]].up_01 :+
        oct.asInstanceOf[OcNode[Placement]].up_10 :+ oct.asInstanceOf[OcNode[Placement]].up_11 :+ oct.asInstanceOf[OcNode[Placement]].down_00 :+
        oct.asInstanceOf[OcNode[Placement]].down_01 :+ oct.asInstanceOf[OcNode[Placement]].down_10 :+ oct.asInstanceOf[OcNode[Placement]].down_11

      // Da lista de nodes, tirar aqueles que sao ocLeafs
      val list_ocLeaf = list_ocNodes.filter(x => x.isInstanceOf[OcLeaf[Octree[Section], Section]])
      //println("lista_ocLeaf_auxScale ------>" + list_ocLeaf)
      val shapes_list : List[Node] = List()
      //Se a lista de ocLeafs não estiver vazia, vamos percorrê-la
      if (list_ocLeaf.nonEmpty) {

        list_ocLeaf.foldRight(0)((h, t) => {
          //Criar lista de modelos graficos da ocleaf
          shapes_list :: h.asInstanceOf[OcLeaf[Octree[Section], Section]].section._2
          //println("Shapes_list1: " + shapes_list)

          //Percorrer lista de modelos graficos da ocLeaf
          shapes_list.foldRight(0)((h, t) => {
            println("shape before: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
            h.setScaleX(h.getScaleX*fact)
            h.setScaleY(h.getScaleY * fact)
            h.setScaleY(h.getScaleZ * fact)
            println("shape after: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
            ;t
          });t
        })

        val placement1: Placement = (oct.asInstanceOf[OcNode[Placement]].placement._1, octScale)
        val sec1: Section = (placement1, shapes_list)
        val ocLeaf1 = OcLeaf(sec1)
        val scaleOctree:Octree[Placement] = OcNode[Placement](placement1, ocLeaf1,list_ocNodes(1), list_ocNodes(2), list_ocNodes(3), list_ocNodes(4),
          list_ocNodes(5), list_ocNodes(6), list_ocNodes(7))

        scaleOctree
      }
      else {
        val placement1: Placement = (oct.asInstanceOf[OcNode[Placement]].placement._1,octScale)
        val scaleOctree:Octree[Placement] = OcNode[Placement](placement1, list_ocNodes(0),list_ocNodes(1), list_ocNodes(2), list_ocNodes(3), list_ocNodes(4),
          list_ocNodes(5), list_ocNodes(6), list_ocNodes(7))
        scaleOctree
      }
    }

}

