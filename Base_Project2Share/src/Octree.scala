import OcNode.Placement
import javafx.scene.{Group, Node}
import javafx.scene.shape.{Box, Cylinder, DrawMode, Shape3D}
import javafx.scene.paint.{Color, PhongMaterial}

sealed trait Octree[+A]

case class OcNode[A](coords: A,
                     up_00: Octree[A], up_01: Octree[A],
                     up_10: Octree[A], up_11: Octree[A],
                     down_00: Octree[A], down_01: Octree[A],
                     down_10: Octree[A], down_11: Octree[A]
                    ) extends Octree[A] {
  def scaleOctree(fact:Double, oct:Octree[Placement]):Octree[Placement] = {
    OcNode.scaleOctree(fact, oct)
  }

  def createAttributesList(e:OcNode[Placement]):List[Octree[Placement]] = OcNode.createAttributesList(e)

  def createTree(worldRoot: Group, shapeRoot: Group,root: Placement):Octree[Placement] =
    OcNode.createTree(worldRoot, shapeRoot,root)

}


case class OcLeaf[A, B](section: B) extends Octree[A]

case object OcEmpty extends Octree[Nothing]

case object OctreeUtils extends Octree[Nothing]

object Octree {

  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size
  type Section = (Placement, List[Node])

  def auxScale(fact: Double, oct: Octree[Placement]): (Octree[Placement]) = {
    val octSize = oct.asInstanceOf[OcNode[Placement]].coords._2
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

      val placement1: Placement = (oct.asInstanceOf[OcNode[Placement]].coords._1, octScale)
      val sec1: Section = (placement1, shapes_list)
      val ocLeaf1 = OcLeaf(sec1)
      val scaleOctree:Octree[Placement] = OcNode[Placement](placement1, ocLeaf1,list_ocNodes(1), list_ocNodes(2), list_ocNodes(3), list_ocNodes(4),
        list_ocNodes(5), list_ocNodes(6), list_ocNodes(7))

      scaleOctree
    }
    else {
      val placement1: Placement = (oct.asInstanceOf[OcNode[Placement]].coords._1,octScale)
      val scaleOctree:Octree[Placement] = OcNode[Placement](placement1, list_ocNodes(0),list_ocNodes(1), list_ocNodes(2), list_ocNodes(3), list_ocNodes(4),
        list_ocNodes(5), list_ocNodes(6), list_ocNodes(7))
      scaleOctree
    }
  }

}
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

  def scaleOctree(fact:Double, oct:Octree[Placement]):Octree[Placement] = {
    val root = oct.asInstanceOf[OcNode[Placement]]
    val inputSize = root.productArity
    for(a <- 0 to inputSize - 1) {
      val partition = root.productElement(a)
      if(partition.isInstanceOf[OcNode[Placement]]) {
        scaleOctree(fact, oct)
      } else if(partition.isInstanceOf[OcLeaf[Placement,Section]]) {
        val shapelist = partition.asInstanceOf[OcLeaf[Placement, Section]].section._2
        (shapelist foldRight List[Node]()) ((h,t) => {
          h.setScaleX(h.getScaleX * fact)
          h.setScaleY(h.getScaleY * fact)
          h.setScaleZ(h.getScaleZ * fact)
          h::t
        })
      }
    }
    root
  }
  def createAttributesList(e:OcNode[Placement]):List[Octree[Placement]] = {
    def iterate(e:OcNode[Placement], l:List[Octree[Placement]], s:Int):List[Octree[Placement]] = {
      s match {
        case x => if (x == e.productArity) l else {
          iterate(e, l :+ e.productElement(s).asInstanceOf[Octree[Placement]], s + 1)}
      }
    }
    iterate(e,List[Octree[Placement]](), 1)

  }

  //
  //  def map[B](func: B => B)(oct:Octree[Placement]): Octree[Placement] = {
  ////    def iterateThroughNode(): Unit = {
  ////
  ////    }
  //    val root = oct.asInstanceOf[OcNode[Placement]]
  //    val inputSize = root.productArity
  //    for( a <- 1 to inputSize) {
  //      val partition = root.productElement(a)
  //      if(partition.isInstanceOf[OcNode[Placement]]){
  //        map(func)(partition.asInstanceOf[Octree[Placement]])
  //      } else if(partition.isInstanceOf[OcLeaf[Placement,Section]]){
  //       ( partition.asInstanceOf[OcLeaf[Placement,Section]].section._2 foldRight List[Node]()) ((h,t) => func(h))
  //      }
  //    }
  //
  //  }

  def mapping[A](lst:List[A], f:A => A):List[A] = {
    lst match {
      case Nil => Nil
      case x :: xs => f(x) :: mapping(xs,f)
    }
  }

  def sepiaEffect(c:Color):Color = {
    val r = c.getRed * 255
    val g = c.getGreen * 255
    val b = c.getBlue * 255
    println(s"Cor: $c RGB: $r, $g, $b")
    val newR = if(0.4*r + 0.77*g + 0.2*b > 255.0 ) 255  else  (0.4*r + 0.77*g + 0.2*b)
    val newG = if(0.35*r + 0.69*g + 0.17*b > 255.0) 255 else (0.35*r + 0.69*g + 0.17*b)
    val newB = if(0.27*r + 0.53*g + 0.13*b > 255.0) 255 else (0.27*r + 0.53*g + 0.13*b)
    println(s"New RGB: $newR, $newG, $newB")
    Color.rgb(newR.toInt, newG.toInt, newB.toInt)
  }

  def greenRemove(c:Color):Color = {
    Color.rgb((c.getRed * 255 ).toInt, 0, (c.getBlue * 255).toInt)
  }

  def mapColourEffect(func: Color => Color)(oct:Octree[Placement]): Octree[Placement] = {
    val root = oct.asInstanceOf[OcNode[Placement]]
    val inputSize = root.productArity
    for( a <- 0 to inputSize - 1) {
      val partition = root.productElement(a)
      if(partition.isInstanceOf[OcNode[Placement]]){
        mapColourEffect(func)(partition.asInstanceOf[Octree[Placement]])
      } else if(partition.isInstanceOf[OcLeaf[Placement,Section]]) {
        val shapelist:List[Node] = partition.asInstanceOf[OcLeaf[Placement,Section]].section._2
        (shapelist foldRight List[Node]())((h,t) => {
          val material = h.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial]
          val color:Color = material.getDiffuseColor
          material.setDiffuseColor(func(color));t
        })
      }
    }
    root
  }

  def smallestPartition() = {

  }

  def createTree(worldRoot: Group, shapeRoot: Group, root: Placement):Octree[Placement] = {
    val point = root._1 // A root n vai ser sempre o quadrado vermelho grande?
    val size = root._2
    val emptyOcNode = new OcNode[Placement](((0.0,0.0,0.0), size/2), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
    var finalTree = emptyOcNode
    val corners = List[Point]()
    val up_00 = addTwoPoints(point, (0.0,0.0,0.0))
    corners:+up_00
    val up_01 = addTwoPoints(point, (0.0,size / 2,0.0))
    corners:+up_01
    val up_10 = addTwoPoints(point, (size / 2,0.0,0.0))
    corners:+up_10
    val up_11 = addTwoPoints(point, (size / 2,size / 2,0.0))
    corners:+up_11
    val down_00 = addTwoPoints(point, (0.0,0.0,size / 2))
    corners:+down_00
    val down_01 = addTwoPoints(point, (0.0,size / 2,size / 2))
    corners:+down_01
    val down_10 = addTwoPoints(point, (size / 2,0.0,size / 2))
    corners:+down_10
    val down_11 = addTwoPoints(point, (size / 2,size / 2,size / 2))
    corners:+down_11
    shapeRoot.getChildren.forEach(c =>{
      for(a <- 0 to 7) {
        val partition = createWiredBox(corners(a), size/2)
        val parent = createWiredBox(point,size)
        //Criar mais niveis na arvore
        if(partition.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent)) {
          //tentar fazer a verificação se há interseção do mesmo objeto com várias partições, se assim for, deve-se mover para a partição "maior"
          //Fazer break do for loop e fazer a translação para esse canto?
          a match {
            case 0 => finalTree = finalTree.copy(up_00 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 1 => finalTree = finalTree.copy(up_01 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 2 => finalTree = finalTree.copy(up_10 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 3 => finalTree = finalTree.copy(up_11 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 4 => finalTree = finalTree.copy(down_00 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 5 => finalTree = finalTree.copy(down_01 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 6 => finalTree = finalTree.copy(down_10 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 7 => finalTree = finalTree.copy(down_11 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
          }
          //Caso a arvore não possa ser mais subdividida
        }else if(parent.getBoundsInParent.contains(c.asInstanceOf[Shape3D].getBoundsInParent)) {
          a match {
            //             case 0 => finalTree = finalTree.copy(up_00 = new OcLeaf[Placement,Section]((point, size),(corners(a), size/2), List()))
            case 1 => finalTree = finalTree.copy(up_01 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 2 => finalTree = finalTree.copy(up_10 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 3 => finalTree = finalTree.copy(up_11 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 4 => finalTree = finalTree.copy(down_00 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 5 => finalTree = finalTree.copy(down_01 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 6 => finalTree = finalTree.copy(down_10 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
            case 7 => finalTree = finalTree.copy(down_11 = createTree(worldRoot, shapeRoot,(corners(a), size/2)))
          }
        }
      }
    })
    finalTree
  }

  //  def canBeDivided(node:Placement, ) = {
  //
  //  }

  def createWiredBox(origin: Point, size:Size):Shape3D = {
    val box = new Box(size,size,size)
    box.setTranslateX(origin._1)
    box.setTranslateY(origin._2)
    box.setTranslateZ(origin._3)
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150,0,0))
    box.setMaterial(redMaterial)
    box.setDrawMode(DrawMode.LINE)
    box
  }


}

