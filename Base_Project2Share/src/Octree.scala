import OcNode.Placement
import Octree.Section
import javafx.scene.{Group, Node}
import javafx.scene.shape.{Box, DrawMode, Shape3D}
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
  def addTwoPoints(p1:Point, p2:Point):Point = {
    (p1._1 + p2._1, p1._2 + p2._2, p1._3 + p2._3)
  }

  def scaleOctree(fact: Double, oct: Octree[Placement]): Octree[Placement]  = fact match {
    case 0.5| 2  =>
      if(checkInBounds(fact,oct)) {
        auxScale(fact, oct, oct)
      } else {
        oct
      }
    case _ => println("--> Fator inválido!!!"); throw new IllegalArgumentException ("Argumento inválido: Não foi possível efetuar scale, factor inválido ")
  }


  def checkInBounds (fact: Double, oct: Octree[Placement]): Boolean = {

    val root = oct.asInstanceOf[OcNode[Placement]]
    //    println("Root before: " + root)

    val list_Ocnodes = createAttributesList(root)
    //println("Lista list_Ocnodes: " + list_Ocnodes)

    val wiredBox = createWiredBox((16,16,16), 32)

    list_Ocnodes.foldRight()((h, t) => {

      if (h.isInstanceOf[OcNode[Placement]]) {
        checkInBounds(fact, h)
      }
      if (h.isInstanceOf[OcLeaf[Placement, Section]]) {
        val shapeList = h.asInstanceOf[OcLeaf[Placement, Section]].section._2
        (shapeList foldRight List[Node]()) ((h, t) => {
          var copia: Shape3D = null
          val originalX = h.getScaleX
          val originalY = h.getScaleY
          val originalZ = h.getScaleZ

          //          println("Shape before: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
          if (h.isInstanceOf[Box]) {
            val box = h.asInstanceOf[Box]
            copia = new Box(box.getWidth, box.getHeight, box.getDepth)

            copia.setScaleX(originalX * fact)
            copia.setScaleY(originalY * fact)
            copia.setScaleZ(originalZ * fact)

          }

          if (h.isInstanceOf[Cylinder]) {
            val cylinder = h.asInstanceOf[Cylinder]
            copia = new Cylinder(cylinder.getRadius, cylinder.getHeight, cylinder.getDivisions)

            copia.setScaleX(originalX * fact)
            copia.setScaleY(originalY * fact)
            copia.setScaleZ(originalZ * fact)


          }

          if (wiredBox.getBoundsInParent.contains(copia.getBoundsInParent)){
            println("DENTRO")
            t
          }
          else {
            println("Shape fora dos limites, operação de scale cancelada")
            return false
          }
        })
      }
    })
    //    println("Root after: " + root)
    true
  }

  def auxScale (fact: Double, oct: Octree[Placement], originalOct: Octree[Placement]): Octree[Placement] = {

    val root = oct.asInstanceOf[OcNode[Placement]]
    //    println("Root before: " + root)

    val list_Ocnodes = createAttributesList(root)
    //println("Lista list_Ocnodes: " + list_Ocnodes)

    val wiredBox = createWiredBox((16,16,16), 45)

    list_Ocnodes.foldRight()((h, t) => {

      if (h.isInstanceOf[OcNode[Placement]]) {
        auxScale(fact, h, originalOct)
      }
      if (h.isInstanceOf[OcLeaf[Placement, Section]]) {
        val shapeList = h.asInstanceOf[OcLeaf[Placement, Section]].section._2
        (shapeList foldRight List[Node]()) ((h, t) => {
//          var copia: Shape3D = null
          val originalX = h.getScaleX
          val originalY = h.getScaleY
          val originalZ = h.getScaleZ

          //          println("Shape before: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
//          if (h.isInstanceOf[Box]) {
//            val box = h.asInstanceOf[Box]
//            copia = new Box(box.getWidth, box.getHeight, box.getDepth)

//            copia.setScaleX(originalX * fact)
//            copia.setScaleY(originalY * fact)
//            copia.setScaleZ(originalZ * fact)

//          }

//          if (h.isInstanceOf[Cylinder]) {
//            val cylinder = h.asInstanceOf[Cylinder]
//            copia = new Cylinder(cylinder.getRadius, cylinder.getHeight, cylinder.getDivisions)

//            copia.setScaleX(originalX * fact)
//            copia.setScaleY(originalY * fact)
//            copia.setScaleZ(originalZ * fact)


//          }

          //if (wiredBox.getBoundsInParent.contains(copia.getBoundsInParent)){
          println("Shape after: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
          h.setScaleX(originalX * fact)
          h.setScaleY(originalY * fact)
          h.setScaleZ(originalZ * fact)
          t
          //}
          //else {
          //  println("Shape fora dos limites, operação de scale cancelada")
          //  return originalOct
          //}
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
    val corners = createCorners(node)
    (corners foldRight false) ((h,t) => {
      val partition = createWiredBox(h,node._2/2)
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


  /*
  * Está em falta a implementação do algoritmo de otimização que trata dos conflitos entre
  * diferentes partições para a mesma shape, em que o mesmo devia ia para a partição "ascendente".
  * */
  def createTree(worldRoot:Group, shapeList: List[Shape3D], root: Placement):Octree[Placement] = {
    val size = root._2
    val emptyOcNode = new OcNode[Placement](((0.0,0.0,0.0), size/2), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
    val corners = createCorners(root)
    //Para cada partição ver se se existe alguma figura que esteja contida na mesma
    def iterateThroughCorners(tree:Octree[Placement], corners:List[Point],i:Int,stop:Int):Octree[Placement]= {
      if(i > stop)
        return tree

      val partition = createWiredBox(corners(i),size/2)

      (shapeList foldRight List[Shape3D]()) ((h,t) => {

        if(partition.getBoundsInParent.contains(h.getBoundsInParent)) {

          //Se puder ser dividida em ainda mais partições, criar um node, que representa um novo ramo
          if (canBeDivided((corners(i),size / 2),h)) {
            val node = createTree(worldRoot,shapeList,(corners(i),size / 2))
            val finalTree = putElementAt(tree, node,i).asInstanceOf[OcNode[Placement]]
            return iterateThroughCorners(finalTree, corners, i+1, stop)

            //Se não puder ser dividida
            //Se nessa partição já existir uma leaf, adicionar o novo objeto à leaf
          } else if(tree.asInstanceOf[OcNode[Placement]].productElement(i).isInstanceOf[OcLeaf[Placement,Section]]) {

            val list = tree.asInstanceOf[OcNode[Placement]].productElement(i).asInstanceOf[OcLeaf[Placement,Section]].section._2
            worldRoot.getChildren.add(partition)
            //retorna uma nova árvore com a lista atualizada e o elemento, mais a partição, no seu sitio
            val finalTree = putElementAt(tree, new OcLeaf[Placement,Section]((corners(i),size/2),list:+h:+partition),i).asInstanceOf[OcNode[Placement]]
            return iterateThroughCorners(finalTree, corners, i+1, stop)

            //Se não existir uma leaf ainda, fazer uma nova
          } else {

            worldRoot.getChildren.add(partition)
            //retorna uma nova árvore com a lista com o elemento e a partição no seu sitio
            val finalTree = putElementAt(tree, new OcLeaf[Placement,Section]((corners(i),size/2),List(h,partition)),i).asInstanceOf[OcNode[Placement]]
            return iterateThroughCorners(finalTree, corners, i+1, stop)

          }

        }
        //Repetir o processo com o seguinte shape
        t
      })

      iterateThroughCorners(tree, corners,i+1,stop)
    }
    //Começar a iterar sobre as partições com uma nova árvore vazia
    iterateThroughCorners(emptyOcNode, corners, 0, corners.size - 1)
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

