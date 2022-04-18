import OcNode.Placement
import Octree.Section
import javafx.scene.{Group, Node}
import javafx.scene.shape.{Box, Cylinder, DrawMode, Shape3D}
import javafx.scene.paint.{Color, PhongMaterial}

sealed trait Octree[+A]

case class OcNode[A](placement: A,
                     up_00: Octree[A], up_01: Octree[A],
                     up_10: Octree[A], up_11: Octree[A],
                     down_00: Octree[A], down_01: Octree[A],
                     down_10: Octree[A], down_11: Octree[A]
                    ) extends Octree[A] {

  def scaleOctree(fact:Double, oct:Octree[Placement]):Octree[Placement] = {
    OcNode.scaleOctree(fact, oct)
  }

  def createAttributesList(e:OcNode[Placement]):List[Octree[Placement]] = OcNode.createAttributesList(e)

  def createTree2(worldRoot:Group, shapeList: List[Shape3D], root: Placement):Octree[Placement] =
    OcNode.createTree(worldRoot, shapeList, root)

}

case class OcLeaf[A, B](section: B) extends Octree[A]

case object OcEmpty extends Octree[Nothing]

case object OctreeUtils extends Octree[Nothing]

object Octree {

  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size
  type Section = (Placement, List[Node])

}

object OcNode {

  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size
  def addTwoPoints(p1:Point, p2:Point) = {
    (p1._1 + p2._1, p1._2 + p2._2, p1._3 + p2._3)
  }

  def scaleOctree(fact: Double, oct: Octree[Placement]): Octree[Placement]  = fact match {
    case 0.5| 2  => auxScale(fact,oct: Octree[Placement])
    case _ => println("--> Fator inválido!!!"); throw new IllegalArgumentException ("Argumento inválido: Não foi possível efetuar scale, factor inválido ")
  }

  def auxScale (fact: Double, oct: Octree[Placement]): Octree[Placement] = {

    val root = oct.asInstanceOf[OcNode[Placement]]
//    println("Root before: " + root)

    val list_Ocnodes = createAttributesList(root)
    println("Lista list_Ocnodes: " + list_Ocnodes)

    val wiredBox = createWiredBox(root.placement._1, 32)

    list_Ocnodes.foldRight()((h, t) => {

      if (h.isInstanceOf[OcNode[Placement]]) {
        scaleOctree(fact, oct)
      }
      if (h.isInstanceOf[OcLeaf[Placement, Section]]) {
        val shapeList = h.asInstanceOf[OcLeaf[Placement, Section]].section._2
        (shapeList foldRight List[Node]()) ((h, t) => {

//          println("Shape before: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
          val originalX = h.getScaleX
          val originalY = h.getScaleY
          val originalZ = h.getScaleZ
          h.setScaleX(originalX * fact)
          h.setScaleY(originalY * fact)
          h.setScaleZ(originalZ * fact)

          if (wiredBox.getBoundsInParent.contains(h.getBoundsInParent)){
//            println("Shape after: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
            h :: t
          }
          else {
            println("Shape fora dos limites")
            h.setScaleX(originalX)
            h.setScaleY(originalY)
            h.setScaleZ(originalZ)
            t
          }
        })
      }
    })
//    println("Root after: " + root)
    root
  }

  def createAttributesList(e:OcNode[Placement]):List[Octree[Placement]] = {
    def iterate(e:OcNode[Placement], l:List[Octree[Placement]], s:Int):List[Octree[Placement]] = {
       if (s == e.productArity) l else iterate(e, l :+ e.productElement(s).asInstanceOf[Octree[Placement]], s + 1)
    }
    iterate(e,List[Octree[Placement]](), 1)
  }

  def putElementAt(node:Octree[Placement], element:Octree[Placement], index:Int):Octree[Placement] = {
    index match {
      case 0 => node.asInstanceOf[OcNode[Placement]].copy(up_00 = element)
      case 1 => node.asInstanceOf[OcNode[Placement]].copy(up_01 = element)
      case 2 => node.asInstanceOf[OcNode[Placement]].copy(up_10 = element)
      case 3 => node.asInstanceOf[OcNode[Placement]].copy(up_11 = element)
      case 4 => node.asInstanceOf[OcNode[Placement]].copy(down_00 = element)
      case 5 => node.asInstanceOf[OcNode[Placement]].copy(down_01 = element)
      case 6 => node.asInstanceOf[OcNode[Placement]].copy(down_10 = element)
      case 7 => node.asInstanceOf[OcNode[Placement]].copy(down_11 = element)
    }
  }

  def createCorners(placement: Placement):List[Point] = {
    val point = placement._1
    val size = placement._2
    val newPoint = new Point(point._1 - size/2,point._2 - size/2,point._3 - size/2)
    List():+addTwoPoints(newPoint, (size/4,size/4,size/4)):+addTwoPoints(newPoint, (size/4,size/4,size/4 + size/2)):+
      addTwoPoints(newPoint, (size/4 + size/2,size/4,size/4)):+addTwoPoints(newPoint, (size/4 + size/2,size/4,size/4 + size/2)):+
      addTwoPoints(newPoint, (size/4,size/4 + size/2,size/4)):+addTwoPoints(newPoint, (size/4,size/4 + size/2 ,size/4 + size/2)):+
      addTwoPoints(newPoint, (size/4 + size/2,size/4 + size/2,size/4)):+addTwoPoints(newPoint, (size/4 + size/2,size/4 + size/2,size/4 + size/2))

  }

  def canBeDivided(node:Placement, s:Shape3D):Boolean = {
    println("Pode ser dividida?")
    val corners = createCorners(node)
    (corners foldRight false) ((h,t) => {
      val partition = createWiredBox(h,node._2/2)
      println(s"Partiçao para o canto $h com tamanho ${node._2/2} contem o shape $s?: ${partition.getBoundsInParent.contains(s.getBoundsInParent)}")
      if(partition.getBoundsInParent.contains(s.getBoundsInParent)) true else t
    })
  }

  def createWiredBox(origin: Point, size:Size):Shape3D = {
    val box = new Box(size,size,size)
    box.setTranslateY(origin._2)
    box.setTranslateZ(origin._3)
    box.setTranslateX(origin._1)
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150,0,0))
    box.setMaterial(redMaterial)
    box.setDrawMode(DrawMode.LINE)
    box
  }

//  def createTree(worldRoot:Group,shapeList: List[Shape3D],root: Placement):Octree[Placement] = {
//    val size = root._2
//    val emptyOcNode = new OcNode[Placement](((0.0,0.0,0.0),size/2),OcEmpty,OcEmpty,OcEmpty,OcEmpty,OcEmpty,OcEmpty,OcEmpty,OcEmpty)
//    val corners = createCorners(root)
//
//    //Para cada partição ver se se existe alguma figura que esteja contida na mesma
//    def iterateThroughCorners(finalTree: Octree[Placement],corners:List[Point],i:Int,stop:Int):Octree[Placement] = {
//      println(s"ITERAÇÃO : $i \n TREE ---")
//      for(a <- 0 to finalTree.asInstanceOf[OcNode[Placement]].productArity - 1)
//        println(s"${finalTree.asInstanceOf[OcNode[Placement]].productElementName(a)} ------> ${finalTree.asInstanceOf[OcNode[Placement]].productElement(a)}")
//      if(i >= stop) {
//        println("--- STOP ---")
//        return finalTree
//      }
//      val partition =createWiredBox(corners(i),size/2)
//            println(s"Partiçao para o canto ${corners(i)} com tamanho ${size/2}")
//      (shapeList foldRight List[Shape3D]()) ((h,t) =>{
//
//        if(partition.getBoundsInParent.contains(h.getBoundsInParent)) {
//          println(s"A partição contem o shape $h")
//          //Se puder ser dividida em ainda mais partições, criar um node, que representa um novo ramo
//          if (canBeDivided((corners(i),size / 2),h)) {
//                        println(s"A partição pode ser dividida para o shape $h. Fazer uma nova tree")
////            val node = createTree(worldRoot,shapeList,(corners(i),size / 2)).asInstanceOf[OcNode[Placement]]
//            val newTree = createTree(worldRoot,shapeList,(corners(i),size / 2)).asInstanceOf[OcNode[Placement]]
//            val node = putElementAt(finalTree, newTree, i)
//            println(s"NODE -> $node")
//            iterateThroughCorners(node, corners,i+1,stop)
//            //Se não puder ser dividida
//            //Se nessa partição já existir uma leaf, adicionar o novo objeto à leaf
//          } else if(finalTree.asInstanceOf[OcNode[Placement]].productElement(i).isInstanceOf[OcLeaf[Placement,Section]]) {
//                        println(s"A partição NÃO PODE ser dividida para o shape $h. Ver se já existe uma leaf")
//            val list = finalTree.asInstanceOf[OcNode[Placement]].productElement(i).asInstanceOf[OcLeaf[Placement,Section]].section._2
//            worldRoot.getChildren.add(partition)
//            //retorna uma nova árvore com a lista atualizada e o elemento, mais a partição, no seu sitio
//            val leaf = putElementAt(finalTree,new OcLeaf[Placement,Section]((corners(i),size/2),list:+h:+partition),i)
//            println(s"LEAF -> $leaf")
//            iterateThroughCorners(leaf, corners,i+1,stop)
//
//            //Se não existir uma leaf ainda, fazer uma nova
//          } else {
//                        println(s"A partição NÃO PODE ser dividida para o shape $h. Fazer uma nova leaf")
//            worldRoot.getChildren.add(partition)
//            //retorna uma nova árvore com a lista com o elemento e a partição no seu sitio
//            val leaf = putElementAt(finalTree,new OcLeaf[Placement,Section]((corners(i),size/2),List(h,partition)),i)
//            println(s"LEAF -> $leaf")
//            iterateThroughCorners(leaf, corners,i+1,stop)
//          }
//
//        }
//                println("Ver o próximo Shape")
//        //Repetir o processo com o seguinte shape
//        t
//
//      })
//
//      iterateThroughCorners(finalTree,corners,i+1,stop)
//
//    }
//
//    iterateThroughCorners(emptyOcNode,corners,0,corners.size - 1)
//  }

  def createTree(worldRoot:Group, shapeList: List[Shape3D], root: Placement):Octree[Placement] = {
    val point = root._1 // A root n vai ser sempre o quadrado vermelho grande?
    val size = root._2
    val emptyOcNode = new OcNode[Placement](((0.0,0.0,0.0), size/2), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
    val corners = createCorners(root)
    var finalTree = emptyOcNode
    //Para cada partição ver se se existe alguma figura que esteja contida na mesma
    def iterateThroughCorners(corners:List[Point],i:Int,stop:Int):Unit= {
      val partition = createWiredBox(corners(i),size/2)
      //      println(s"Partiçao para o canto ${corners(i)} com tamanho ${size/2}")
      (shapeList foldRight List[Shape3D]()) ((h,t) =>{
        if(partition.getBoundsInParent.contains(h.getBoundsInParent)) {
          //Se puder ser dividida em ainda mais partições, criar um node, que representa um novo ramo
          if (canBeDivided((corners(i),size / 2),h)) {
            //            println(s"A partição pode ser dividida para o shape $h. Fazer uma nova tree")
            val node = createTree(worldRoot,shapeList,(corners(i),size / 2))
            finalTree = putElementAt(finalTree, node,i).asInstanceOf[OcNode[Placement]]
            //Se não puder ser dividida
            //Se nessa partição já existir uma leaf, adicionar o novo objeto à leaf
          } else if(finalTree.productElement(i).isInstanceOf[OcLeaf[Placement,Section]]) {
            //            println(s"A partição NÃO PODE ser dividida para o shape $h. Ver se já existe uma leaf")
            val list = finalTree.productElement(i).asInstanceOf[OcLeaf[Placement,Section]].section._2
            worldRoot.getChildren.add(partition)
            //retorna uma nova árvore com a lista atualizada e o elemento, mais a partição, no seu sitio
            finalTree = putElementAt(finalTree,new OcLeaf[Placement,Section]((corners(i),size/2),list:+h:+partition),i).asInstanceOf[OcNode[Placement]]

            //Se não existir uma leaf ainda, fazer uma nova
          } else {
            //            println(s"A partição NÃO PODE ser dividida para o shape $h. Fazer uma nova leaf")
            worldRoot.getChildren.add(partition)
            //retorna uma nova árvore com a lista com o elemento e a partição no seu sitio
            finalTree = putElementAt(finalTree,new OcLeaf[Placement,Section]((corners(i),size/2),List(h,partition)),i).asInstanceOf[OcNode[Placement]]
          }

        }
        //        println("Ver o próximo Shape")
        //Repetir o processo com o seguinte shape
        t

      })
      if(i < stop)
        iterateThroughCorners(corners,i+1,stop)
    }
    iterateThroughCorners(corners, 0, corners.size - 1)
    finalTree
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

    def iterate(root:OcNode[Placement], i:Int, stop:Int):Octree[Placement] = {

      val partition = root.productElement(i)

      if(partition.isInstanceOf[OcNode[Placement]]){

        mapColourEffect(func)(partition.asInstanceOf[Octree[Placement]])

      } else if(partition.isInstanceOf[OcLeaf[Placement,Section]]) {

        val shapeList:List[Node] = partition.asInstanceOf[OcLeaf[Placement,Section]].section._2
        (shapeList foldRight List[Node]())((h,t) => {

          val material = h.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial]
          val color:Color = material.getDiffuseColor
          material.setDiffuseColor(func(color));t

        })

      }
      if(i < stop) iterate(root, i+1, stop) else root
    }
    iterate(root, 0, inputSize-1)
  }

}

