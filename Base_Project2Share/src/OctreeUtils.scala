import javafx.scene.{Group, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, Cylinder, DrawMode, Shape3D}

import scala.annotation.tailrec

object OctreeUtils {

  //Wiredbox que limita o espaço 3D
//  val SPACE_LIMIT: Shape3D = OctreeUtils.createWiredBox((0, 0, 0), 32)

  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size
  type Section = (Placement, List[Node])

  //Auxiliary functions
  def addTwoPoints(p1: Point, p2: Point): Point = {
    (p1._1 + p2._1, p1._2 + p2._2, p1._3 + p2._3)
  }

  def getAllShapes(oct: Octree[Placement]):List[Node] = oct match {
    case n: OcNode[Placement] =>
      val atributos = createAttributesList(n)
      (atributos foldRight List[Node]()) ((h, t) => {
        getAllShapes(h)++t
      })
    case l: OcLeaf[Placement, Section] =>
      l.section._2
    case _ => List[Node]()
  }

  /*  Devolve os todos os atributos de um node, exceto o primeiro, que é o placement */
  def createAttributesList(e: OcNode[Placement]): List[Octree[Placement]] = {
    val numAtributos = e.productArity

    //Função auxiliar que itera sobre todos os atributos do node e devolve uma lista com os mesmos (exceto o placement)
    @tailrec
    def iterate(e: OcNode[Placement], l: List[Octree[Placement]], iter: Int): List[Octree[Placement]] = {
      if (iter == numAtributos) l else iterate(e, l :+ e.productElement(iter).asInstanceOf[Octree[Placement]], iter + 1)
    }

    iterate(e, List[Octree[Placement]](), 1)
  }

  //Cria uma wiredBox com o tamanho desejado
  def createWiredBox(origin: Point, size: Size): Shape3D = {
    val box = new Box(size, size, size)
    box.setTranslateX(origin._1)
    box.setTranslateY(origin._2)
    box.setTranslateZ(origin._3)
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150,0,0))
    box.setMaterial(redMaterial)
    box.setDrawMode(DrawMode.LINE)
    box
  }

  def addOctreeToWorldRoot(oct: Octree[Placement], worldRoot: Group): Unit = oct match {

    case n: OcNode[Placement] =>
      val atributos = createAttributesList(n)
      (atributos foldRight()) ((h, t) => {
        addOctreeToWorldRoot(h, worldRoot)
        t
      })

    case l: OcLeaf[Placement, Section] =>
      val listShapes = l.section._2

      (listShapes foldRight()) ((h, t) => {
        worldRoot.getChildren.add(h)
        t
      })

    case _ =>
  }

  //T2 Auxiliary functions

  /*  Devolve uma octree igual, mas com a tree (element) dada na posição indicada pelo index */
  def putElementAt(tree: Octree[Placement], element: Octree[Placement], index: Int): Octree[Placement] = {
    val node = tree.asInstanceOf[OcNode[Placement]]
    index match {
      case 0 => node.copy(up_00 = element)
      case 1 => node.copy(up_01 = element)
      case 2 => node.copy(up_10 = element)
      case 3 => node.copy(up_11 = element)
      case 4 => node.copy(down_00 = element)
      case 5 => node.copy(down_01 = element)
      case 6 => node.copy(down_10 = element)
      case 7 => node.copy(down_11 = element)
      case _ =>
        println(" (putElementAt) ERRO -> indice inválido")
        tree
    }
  }

  //Cria todas as origens que fazem as partições de um espaço
  def createOrigins(placement: Placement): List[Point] = {
    val point = placement._1
    val size = placement._2
    val newPoint = new Point(point._1 - size / 2, point._2 - size / 2, point._3 - size / 2)
    List() :+ addTwoPoints(newPoint, (size / 4, size / 4, size / 4)) :+ addTwoPoints(newPoint, (size / 4, size / 4, size / 4 + size / 2)) :+
      addTwoPoints(newPoint, (size / 4 + size / 2, size / 4, size / 4)) :+ addTwoPoints(newPoint, (size / 4 + size / 2, size / 4, size / 4 + size / 2)) :+
      addTwoPoints(newPoint, (size / 4, size / 4 + size / 2, size / 4)) :+ addTwoPoints(newPoint, (size / 4, size / 4 + size / 2, size / 4 + size / 2)) :+
      addTwoPoints(newPoint, (size / 4 + size / 2, size / 4 + size / 2, size / 4)) :+ addTwoPoints(newPoint, (size / 4 + size / 2, size / 4 + size / 2, size / 4 + size / 2))
  }

  //É meio estúpido porque vai devolver o mesmo shape
  def shapeInTheRightPosition(shape: Shape3D, partition: Shape3D):Shape3D = {
    if(!partition.getBoundsInParent.contains(shape.getBoundsInParent)) {
      shape.setTranslateX(partition.getTranslateX)
      shape.setTranslateY(partition.getTranslateY)
      shape.setTranslateZ(partition.getTranslateZ)
    }
    shape
  }

  //Determina se um shape contido num node está contido numa partição do mesmo
  // (Se a partição pode ser dividida para esse node)
  def canBeDivided(node: Placement, s: Shape3D): Boolean = {
    val partitionsOrigins = createOrigins(node)
    (partitionsOrigins foldRight false) ((h, t) => {
      val partition = createWiredBox(h, node._2 / 2)
      if (partition.getBoundsInParent.contains(s.getBoundsInParent)) true else t
    })
  }

  def canBeDivided2(node: Placement, s: Shape3D): Boolean = {
    val partitionsOrigins = createOrigins(node)
    //Cópia do shape, mas com uma origem diferente
    //    val shapeCopy = s.clone().asInstanceOf[Shape3D]
    val origin = (s.getTranslateX, s.getTranslateY, s.getTranslateZ)
    (partitionsOrigins foldRight false) ((h, t) => {
      s.setTranslateX(h._1)
      s.setTranslateY(h._2)
      s.setTranslateZ(h._3)
      //      val b = Bounds()
      val partition = createWiredBox(h, node._2 / 2)
      if (partition.getBoundsInParent.contains(s.getBoundsInParent)) {
        s.setTranslateX(origin._1); s.setTranslateY(origin._2); s.setTranslateZ(origin._3)
        true
      } else{
        s.setTranslateX(origin._1); s.setTranslateY(origin._2); s.setTranslateZ(origin._3)
        t
      }
    })
  }

  // --- T2 ---

  //Cria um octree sem especificar o tamanho da root (Tem como root o cubo que limita o espaço)
  def createTree(shapeList: List[Shape3D]): Octree[Placement] = createTree(shapeList, ((16.0, 16.0, 16.0), 32))

  def createTree(shapeList: List[Shape3D], root: Placement): Octree[Placement] = {
    val size = root._2
    val emptyOcNode = new OcNode[Placement](((0.0, 0.0, 0.0), size / 2), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
    val partitionsOrigins = createOrigins(root)

    //Para cada partição ver se se existe alguma figura que esteja contida na mesma
    def iterateThroughPartitions(tree: Octree[Placement], partitions: List[Point], i: Int, stop: Int): Octree[Placement] = {
      if (i > stop)
        return tree

      val partition = createWiredBox(partitions(i), size / 2)

      (shapeList foldRight List[Shape3D]()) ((h, t) => {

        //Este if pode estragar o algoritmo
        if (partition.getBoundsInParent.contains(h.getBoundsInParent)) {

          //Se puder ser dividida em ainda mais partições, criar um node, que representa um novo ramo
          if (canBeDivided((partitions(i), size / 2), h)) {

            val node = createTree(shapeList, (partitions(i), size / 2))
            val finalTree = putElementAt(tree, node, i).asInstanceOf[OcNode[Placement]]
            //Iterar para a próxima partição
            return iterateThroughPartitions(finalTree, partitions, i + 1, stop)

            //Se não puder ser dividida
            //Se nessa partição já existir uma leaf, adicionar o novo objeto à leaf
          } else tree.asInstanceOf[OcNode[Placement]].productElement(i) match {
            case value: OcLeaf[Placement, Section] =>
              val list = value.section._2

              val shape = shapeInTheRightPosition(h, partition)

              //retorna uma nova árvore com a lista atualizada e o elemento, mais a partição, no seu sitio
              //              val finalTree = putElementAt(tree, new OcLeaf[Placement, Section]((partitions(i), size / 2), list :+ h :+ partition), i).asInstanceOf[OcNode[Placement]]
              val finalTree = putElementAt(tree, new OcLeaf[Placement, Section]((partitions(i), size / 2), list :+ shape :+ partition), i).asInstanceOf[OcNode[Placement]]
              //Iterar para a próxima partição
              return iterateThroughPartitions(finalTree, partitions, i + 1, stop)

            //Se não existir uma leaf ainda, fazer uma nova
            case _ =>

              //retorna uma nova árvore com a lista com o elemento e a partição no seu sitio
              val finalTree = putElementAt(tree,
                new OcLeaf[Placement, Section]((partitions(i), size / 2), List(h, partition)), i).asInstanceOf[OcNode[Placement]]
              //Iterar para a próxima partição
              return iterateThroughPartitions(finalTree, partitions, i + 1, stop)
          }
        }
        //Repetir o processo com o seguinte shape
        t
      })

      iterateThroughPartitions(tree, partitions, i + 1, stop)
    }
    //Começar a iterar sobre as partições com uma nova árvore vazia
    iterateThroughPartitions(emptyOcNode, partitionsOrigins, 0, partitionsOrigins.size - 1)
  }

  //  --- T3 ---
  def intersectsCamera(oct:Octree[Placement], camVolume: Cylinder):Unit = {

    oct match {
      // Se for um ocNode
      case _: OcNode[Placement] =>
        //println("SOU UM NODE")
        val ocnode = oct.asInstanceOf[OcNode[Placement]]
        val atribList = OctreeUtils.createAttributesList(ocnode)
        (atribList foldRight List[Octree[Placement]]()) ((h,t) => {
          intersectsCamera(h, camVolume); t
        })

      // Se for uma ocLeaf
      case _: OcLeaf[Placement, Section] =>
        //println("SOU UMA LEAF")
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

  //  --- T4 ---
  def scaleOctree(fact: Double, oct: Octree[Placement]): Octree[Placement] = fact match {
    case 0.5 | 2 => auxScale(fact, oct)
    case _ => println("--> Fator inválido!!!"); throw new IllegalArgumentException("Argumento inválido: Não foi possível efetuar scale, factor inválido ")
  }

  def auxScale(fact: Double, oct: Octree[Placement]): Octree[Placement] = {

    val listaDeShapes = getAllShapes(oct)

    (listaDeShapes foldRight ())((h,_) => {

      h.setTranslateZ(h.getTranslateZ * fact)
      h.setTranslateX(h.getTranslateX * fact)
      h.setTranslateY(h.getTranslateY * fact)

      h.setScaleX(h.getScaleX * fact)
      h.setScaleY(h.getScaleY * fact)
      h.setScaleZ(h.getScaleZ * fact)
    })
    oct
  }

  //  --- T5 ---

  def sepiaEffect(c: Color): Color = {
    val r = c.getRed * 255
    val g = c.getGreen * 255
    val b = c.getBlue * 255
    println(s"Cor: $c RGB: $r, $g, $b")
    val newR = if (0.4 * r + 0.77 * g + 0.2 * b > 255.0) 255 else 0.4 * r + 0.77 * g + 0.2 * b
    val newG = if (0.35 * r + 0.69 * g + 0.17 * b > 255.0) 255 else 0.35 * r + 0.69 * g + 0.17 * b
    val newB = if (0.27 * r + 0.53 * g + 0.13 * b > 255.0) 255 else 0.27 * r + 0.53 * g + 0.13 * b
    println(s"New RGB: $newR, $newG, $newB")
    Color.rgb(newR.toInt, newG.toInt, newB.toInt)
  }

  def greenRemove(c: Color): Color = {
    Color.rgb((c.getRed * 255).toInt, 0, (c.getBlue * 255).toInt)
  }

  def mapColourEffect(func: Color => Color)(oct: Octree[Placement]): Octree[Placement] = {
    val root = oct.asInstanceOf[OcNode[Placement]]
    val inputSize = root.productArity

    @tailrec
    def iterate(root: OcNode[Placement], i: Int, stop: Int): Octree[Placement] = {

      val partition = root.productElement(i)

      partition match {
        case _: OcNode[Placement] =>

          mapColourEffect(func)(partition.asInstanceOf[Octree[Placement]])

        case _: OcLeaf[Placement, Section] =>

          val shapeList: List[Node] = partition.asInstanceOf[OcLeaf[Placement, Section]].section._2
          (shapeList foldRight List[Node]()) ((h, t) => {
            val material = h.asInstanceOf[Shape3D].getMaterial.asInstanceOf[PhongMaterial]
            val color: Color = material.getDiffuseColor
            h match {
              case _: Box =>
                val box = h.asInstanceOf[Box]
                if (box.getDrawMode.equals(DrawMode.LINE)) {
                  t
                } else {
                  material.setDiffuseColor(func(color))
                  t
                }
              case _ =>
                material.setDiffuseColor(func(color))
                t
            }
          })

        case _ =>
      }
      if (i < stop) iterate(root, i + 1, stop) else root
    }

    iterate(root, 0, inputSize - 1)
  }

}

