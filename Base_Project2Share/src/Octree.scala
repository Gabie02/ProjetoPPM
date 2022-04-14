import javafx.scene.{Group, Node}
import javafx.scene.shape.{Box, Cylinder, Shape3D}

sealed trait Octree[+A]

case class OcNode[A](coords: A,
                    up_00: Octree[A], up_01: Octree[A],
                     up_10: Octree[A], up_11: Octree[A],
                     down_00: Octree[A], down_01: Octree[A],
                     down_10: Octree[A], down_11: Octree[A]
                    ) extends Octree[A] {

  def createTree[A](worldRoot: Group) =
    OcNode.createTree(worldRoot, this[A])

}


case class OcLeaf[A, B](section: B) extends Octree[A]

case object OcEmpty extends Octree[Nothing]


object OcNode {

  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size
  def addTwoPoints(p1:Point, p2:Point) = {
    (p1._1 + p2._1, p1._2 + p2._2, p1._3 + p2._3)
  }

  //Shape3D is an abstract class that extends javafx.scene.Node
  //Box and Cylinder are subclasses of Shape3D
  type Section = (Placement, List[Node])  //example: ( ((0.0,0.0,0.0), 2.0), List(new Cylinder(0.5, 1, 10)))


  def createTree(worldRoot: Group, root: Placement):Octree[Placement] = {
    //Criar os nodes children
    val point = root._1
    val size = root._2
    val corners = List[Point]()

    val up_00 = addTwoPoints(point, (0.0,0.0,0.0))
    corners:+up_00
    val b1 = new Box(up_00._1, up_00._2, up_00._3)
    worldRoot.getChildren.forEach(c =>{
      if(b1.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
      new OcNode[Placement]((up_00,size/2),
          createTree(worldRoot,(up_00,size/2)), OcEmpty,
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty)
    })

    val up_01 = addTwoPoints(point, (0.0,size / 2,0.0))
    corners:+up_01
    val b2 = new Box(up_01._1, up_01._2, up_01._3)
    worldRoot.getChildren.forEach(c =>{
      if(b2.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
        new OcNode[Placement]((up_01,size/2),
              OcEmpty, createTree(worldRoot,(up_01,size/2)),
              OcEmpty, OcEmpty,
              OcEmpty, OcEmpty,
              OcEmpty, OcEmpty)
      else if(c.asInstanceOf[Shape3D].getBoundsInParent.intersects(b2.getBoundsInParent))
        new OcNode[Placement]((up_01,size/2),
          OcEmpty, ,
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty))
    })

    val up_10 = addTwoPoints(point, (size / 2,0.0,0.0))
    corners:+up_10
    val b3 = new Box(up_10._1, up_10._2, up_10._3)
    worldRoot.getChildren.forEach(c =>{
      if(b3.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
      new OcNode[Placement]((up_10,size/2),
            OcEmpty, OcEmpty,
            createTree(worldRoot,(up_10,size/2)), OcEmpty,
            OcEmpty, OcEmpty,
            OcEmpty, OcEmpty)
    })

    val up_11 = addTwoPoints(point, (size / 2,size / 2,0.0))
    corners:+up_11
    val b4 = new Box(up_11._1, up_11._2, up_11._3)
    worldRoot.getChildren.forEach(c => {
      if(b4.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
      new OcNode[Placement]((up_11,size/2),
            OcEmpty, OcEmpty,
            OcEmpty, createTree(worldRoot,(up_11,size/2)),
            OcEmpty, OcEmpty,
            OcEmpty, OcEmpty)
    })

    val down_00 = addTwoPoints(point, (0.0,0.0,size / 2))
    corners:+down_00
    val b5 = new Box(down_00._1, down_00._2, down_00._3)
    worldRoot.getChildren.forEach(c =>{
      if(b5.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
        new OcNode[Placement]((down_00,size/2),
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty,
          createTree(worldRoot,(down_00,size/2)), OcEmpty,
          OcEmpty, OcEmpty)
    })


    val down_01 = addTwoPoints(point, (0.0,size / 2,size / 2))
    corners:+down_01
    val b6 = new Box(down_01._1, down_01._2, down_01._3)
    worldRoot.getChildren.forEach(c =>{
      if(b6.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
        new OcNode[Placement]((down_01,size/2),
          OcEmpty, OcEmpty,
          OcEmpty, createTree(worldRoot,(down_01,size/2)),
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty)
    })


    val down_10 = addTwoPoints(point, (size / 2,0.0,size / 2))
    corners:+down_10
    val b7 = new Box(down_10._1, down_10._2, down_10._3)
    worldRoot.getChildren.forEach(c =>{
      if(b7.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
        new OcNode[Placement]((down_10,size/2),
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty,
          createTree(worldRoot,(down_10,size/2)), OcEmpty)
    })

    val down_11 = addTwoPoints(point, (size / 2,size / 2,size / 2))
    corners:+down_11
    val b8 = new Box(down_11._1, down_11._2, down_11._3)
    worldRoot.getChildren.forEach(c =>{
      if(b8.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))
        new OcNode[Placement]((down_11,size/2),
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty,
          OcEmpty, OcEmpty,
          OcEmpty, createTree(worldRoot,(down_11,size/2)))
    })
    corners:+down_11

//    corners.foreach(co => {
//      val b = new Box(co._1, co._2, co._3)
//      worldRoot.getChildren.forEach(ch => {
//       if(b.getBoundsInParent.contains(ch.asInstanceOf[Shape3D].getBoundsInParent))
//         createTree(worldRoot, (co,size/2))
//        new OcNode[Placement]()
//      })
//    })

    //    val wiredBox1 = new Box(root._1._1 + root._2 root._1._2+ root._2, root._1._3+ root._2)
//    val wiredBox2 = new Box(root._1._1 + root._2, root._1._2 + root._2, root._1._3 + root._2)
//    val wiredBox3 = new Box(root._1._1 + root._2, root._1._2 + root._2, root._1._3 + root._2)
//    val wiredBox4 = new Box(root._1._1 + root._2, root._1._2 + root._2, root._1._3 + root._2)

//    worldRoot.getChildren.forEach(c =>
//      if(wiredBox1.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent))

  }


}

//case class OcTreeShapes[Placement] extends Octree[Placement] {
//    def createTree():OcTreeShapes[Placement] = {
//
//    }
//}
//
//object OcTreeShapes{
//  def createTree():OcTreeShapes[Placement] = {
//
//  }
//}

