import javafx.scene.{Group, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, Cylinder, DrawMode, Shape3D}

import scala.annotation.tailrec

object OctreeUtils {

  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size
  type Section = (Placement, List[Node])

  //Auxiliary functions
  def addTwoPoints(p1: Point, p2: Point): Point = {
    (p1._1 + p2._1, p1._2 + p2._2, p1._3 + p2._3)
  }

  def getAllShapes(oct: Octree[Placement]): List[Shape3D] = {
    oct match {
      case n: OcNode[Placement] =>
        val atributos = createAttributesList(n)
        (atributos foldRight List[Shape3D]()) ((h, t) => {
          getAllShapes(h) ++ t
        })
      case l: OcLeaf[Placement, Section] =>
        l.section._2.asInstanceOf[List[Shape3D]]
      case _ => List[Shape3D]()
    }
  }

  //Devolve os todos os atributos de um node, exceto o primeiro, que é o placement
  def createAttributesList(e: OcNode[Placement]): List[Octree[Placement]] = {
    val numAtributos = e.productArity

    //Função auxiliar que itera sobre todos os atributos do node e devolve uma lista com os mesmos (exceto o placement)
    @tailrec
    def iterate(e: OcNode[Placement], l: List[Octree[Placement]], iter: Int): List[Octree[Placement]] = {
      if (iter == numAtributos) l else iterate(e, l :+ e.productElement(iter).asInstanceOf[Octree[Placement]], iter + 1)
    }

    iterate(e, List[Octree[Placement]](), 1)
  }

  //Adiciona a tree à WorldRoot
  def addOctreeToWorldRoot(oct: Octree[Placement], worldRoot: Group): Unit = {
    val shapes = getAllShapes(oct)
    (shapes foldRight())((h,_) => worldRoot.getChildren.add(h))
  }
  //T2 Auxiliary functions

  //Cria uma wiredBox com o tamanho desejado
  def createWiredBox(origin: Point, size: Size): Shape3D = {
    val box = new Box(size, size, size)
    box.setTranslateX(origin._1)
    box.setTranslateY(origin._2)
    box.setTranslateZ(origin._3)
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150, 0, 0))
    box.setMaterial(redMaterial)
    box.setDrawMode(DrawMode.LINE)
    box
  }


  //Devolve uma octree igual, mas com a tree (element) dada na posição indicada pelo index
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

  //Determina se um shape contido num node está contido numa partição do mesmo
  // (Se a partição pode ser dividida para esse node)
  def canBeDivided(node: Placement, s: Shape3D): Boolean = {
    val partitionsOrigins = createOrigins(node)
    (partitionsOrigins foldRight false) ((h, t) => {
      val partition = createWiredBox(h, node._2 / 2)
      if (partition.getBoundsInParent.contains(s.getBoundsInParent)) true else t
    })
  }

  // --- T2 ---
  //Cria um octree (Tem como root default o cubo que limita o espaço, profundidade de 16 e tamanho minimo para cada partição de 1)
  def createTree(shapeList: List[Shape3D], root: Placement = ((16.0, 16.0, 16.0), 32), depthLimit: Int = 16, smallestPartitionSize: Size = 1): Octree[Placement] = {
    val size = root._2
    val emptyOcNode = new OcNode[Placement](((0.0, 0.0, 0.0), size / 2), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
    val partitionsOrigins = createOrigins(root)

    //Para cada partição ver se se existe alguma figura que esteja contida na mesma
    def iterateThroughPartitions(tree: Octree[Placement], partitions: List[Point], i: Int, stop: Int, depth: Int): Octree[Placement] = {
      if (i > stop || root._2 == smallestPartitionSize || depth <= 0)
        return tree

      val partition = createWiredBox(partitions(i), size / 2)

      (shapeList foldRight List[Shape3D]()) ((h, t) => {

        if (partition.getBoundsInParent.contains(h.getBoundsInParent)) {

          //Se puder ser dividida em ainda mais partições, criar um node, que representa um novo ramo
          if (canBeDivided((partitions(i), size / 2), h)) {

            val node = createTree(shapeList, (partitions(i), size / 2))
            val finalTree = putElementAt(tree, node, i).asInstanceOf[OcNode[Placement]]

            //Iterar para a próxima partição
            return iterateThroughPartitions(finalTree, partitions, i + 1, stop, depth - 1)

            //Se não puder ser dividida
            //Se nessa partição já existir uma leaf, adicionar o novo objeto à leaf
          } else tree.asInstanceOf[OcNode[Placement]].productElement(i + 1) match {
            case value: OcLeaf[Placement, Section] =>
              val list = value.section._2

              //retorna uma nova árvore com a lista atualizada e o elemento, mais a partição, no seu sitio
              val finalTree = putElementAt(tree, new OcLeaf[Placement, Section]((partitions(i), size / 2), list :+ h :+ partition), i).asInstanceOf[OcNode[Placement]]
              //Iterar para a próxima partição
              return iterateThroughPartitions(finalTree, partitions, i + 1, stop, depth)

            //Se não existir uma leaf ainda, fazer uma nova
            case _ =>

              //retorna uma nova árvore com a lista com o elemento e a partição no seu sitio
              val finalTree = putElementAt(tree,
                new OcLeaf[Placement, Section]((partitions(i), size / 2), List(h, partition)), i).asInstanceOf[OcNode[Placement]]
              //Iterar para a próxima partição
              return iterateThroughPartitions(finalTree, partitions, i + 1, stop, depth)
          }
        }
        //Repetir o processo com o seguinte shape
        t
      })

      iterateThroughPartitions(tree, partitions, i + 1, stop, depth)
    }
    //Começar a iterar sobre as partições com uma nova árvore vazia
    iterateThroughPartitions(emptyOcNode, partitionsOrigins, 0, partitionsOrigins.size - 1, depthLimit)
  }

  //  --- T3 ---
  def intersectsCamera(oct: Octree[Placement], camVolume: Cylinder): Unit = {
    oct match {
      // Se for um ocNode
      case _: OcNode[Placement] =>
        val ocnode = oct.asInstanceOf[OcNode[Placement]]
        val atribList = OctreeUtils.createAttributesList(ocnode)
        (atribList foldRight List[Octree[Placement]]()) ((h, t) => {
          intersectsCamera(h, camVolume)
          t
        })

      // Se for uma ocLeaf
      case _: OcLeaf[Placement, Section] =>
        val ocleaf = oct.asInstanceOf[OcLeaf[Placement, Section]]
        val listaSection = ocleaf.section._2
        // Se a lista de nodes da section nao estiver vazia
        listaSection.foldRight(0)((h, t) => {
          h.asInstanceOf[Shape3D]
          h match {
            case box: Box =>
              // Verifica se é uma wiredBox
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
            case _ =>
          }
          t
        })

      case _ =>
    }
  }

  //  --- T4 ---
  def scaleOctree(fact: Double, oct: Octree[Placement]): Octree[Placement] = fact match {
    case 0.5 | 2 =>
      val listaDeShapes = getAllShapes(oct)

      (listaDeShapes foldRight()) ((h, _) => {

        h.setTranslateZ(h.getTranslateZ * fact)
        h.setTranslateX(h.getTranslateX * fact)
        h.setTranslateY(h.getTranslateY * fact)

        h.setScaleX(h.getScaleX * fact)
        h.setScaleY(h.getScaleY * fact)
        h.setScaleZ(h.getScaleZ * fact)
      })
      val shapeListWithoutPartitions = listaDeShapes.filterNot(s => s.isInstanceOf[Box] && (s.asInstanceOf[Box].getDrawMode == DrawMode.LINE))
      createTree(shapeListWithoutPartitions)

    case _ => println("--> Fator inválido!!!"); throw new IllegalArgumentException("Argumento inválido: Não foi possível efetuar scale, factor inválido ")
  }

  //  --- T5 ---
  def sepiaEffect(c: Color): Color = {
    val r = c.getRed * 255
    val g = c.getGreen * 255
    val b = c.getBlue * 255
    val newR = if (0.4 * r + 0.77 * g + 0.2 * b > 255.0) 255 else 0.4 * r + 0.77 * g + 0.2 * b
    val newG = if (0.35 * r + 0.69 * g + 0.17 * b > 255.0) 255 else 0.35 * r + 0.69 * g + 0.17 * b
    val newB = if (0.27 * r + 0.53 * g + 0.13 * b > 255.0) 255 else 0.27 * r + 0.53 * g + 0.13 * b
    Color.rgb(newR.toInt, newG.toInt, newB.toInt)
  }

  def greenRemove(c: Color): Color = {
    Color.rgb((c.getRed * 255).toInt, 0, (c.getBlue * 255).toInt)
  }

  def mapColourEffect(func: Color => Color)(oct: Octree[Placement]): Octree[Placement] = {
    val leafsList = getAllShapes(oct)
    val shapeList = leafsList.filterNot(s => s.isInstanceOf[Box] && (s.asInstanceOf[Box].getDrawMode == DrawMode.LINE))
    (shapeList foldRight()) ((h, _) => {
      val material = h.getMaterial.asInstanceOf[PhongMaterial]
      val color: Color = material.getDiffuseColor
      material.setDiffuseColor(func(color))
    })
    createTree(shapeList)
  }
}

