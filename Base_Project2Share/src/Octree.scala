


import scala.collection.StepperShape.Shape

sealed trait Octree[+A]

case class OcNode[A](coords: A,
                    up_00: Octree[A], up_01: Octree[A],
                     up_10: Octree[A], up_11: Octree[A],
                     down_00: Octree[A], down_01: Octree[A],
                     down_10: Octree[A], down_11: Octree[A]
                    ) extends Octree[A]


case class OcLeaf[A, B](section: B) extends Octree[A]

case object OcEmpty extends Octree[Nothing]

//case class OcTreeShapes[Placement] extends Octree[Placement] {
//    def createTree():OcTreeShapes[Placement] = {
//      val placement1: Placement = ((0, 0, 0), 8.0)
//      val sec1: Section = (((0.0,0.0,0.0), 4.0), List(cylinder1.asInstanceOf[Node]))
//      val ocLeaf1 = OcLeaf(sec1)
//      val oct1:Octree[Placement] = OcNode[Placement](placement1, ocLeaf1, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
//
//
//    }
//}

//object OcTreeShapes{
//  def createTree():OcTreeShapes[Placement] = {

//  }
//}