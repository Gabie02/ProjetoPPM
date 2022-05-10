import javafx.scene.{Group, Node}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{Box, DrawMode, Shape3D}

import scala.annotation.tailrec

object OctreeUtils {

  //Auxiliary types
  type Point = (Double, Double, Double)
  type Size = Double
  type Placement = (Point, Size) //1st point: origin, 2nd point: size
  type Section = (Placement, List[Node])

  def addTwoPoints(p1: Point, p2: Point): Point = {
    (p1._1 + p2._1, p1._2 + p2._2, p1._3 + p2._3)
  }

  //  --- T4 ---
  def scaleOctree(fact: Double, oct: Octree[Placement]): Octree[Placement] = fact match {
    case 0.5 | 2 => auxScale(fact, oct)
    case _ => println("--> Fator inválido!!!"); throw new IllegalArgumentException("Argumento inválido: Não foi possível efetuar scale, factor inválido ")
  }

  def auxScale(fact: Double, oct: Octree[Placement]): Octree[Placement] = {

    val root = oct.asInstanceOf[OcNode[Placement]]

    val list_Ocnodes = createAttributesList(root)

    list_Ocnodes.foldRight()((h, _) => {
      h match {
        case _: OcNode[Placement] =>
          auxScale(fact, h)

        case _: OcLeaf[Placement, Section] =>
          val shapeList = h.asInstanceOf[OcLeaf[Placement, Section]].section._2
          (shapeList foldRight ()) ((h, t) => {
            //println("Shape before: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
            h.setScaleX(h.getScaleX * fact)
            h.setScaleY(h.getScaleY * fact)
            h.setScaleZ(h.getScaleZ * fact)
            t
            //println("Shape after: " + h.getScaleX + " " + h.getScaleY + " " + h.getScaleZ)
          })
        case _ =>
      }
    })

    root
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
        println(" (putElementAt) ERRO -> indice inválido");
        tree
    }
  }

  def createCorners(placement: Placement): List[Point] = {
    val point = placement._1
    val size = placement._2
    val newPoint = new Point(point._1 - size / 2, point._2 - size / 2, point._3 - size / 2)
    List() :+ addTwoPoints(newPoint, (size / 4, size / 4, size / 4)) :+ addTwoPoints(newPoint, (size / 4, size / 4, size / 4 + size / 2)) :+
      addTwoPoints(newPoint, (size / 4 + size / 2, size / 4, size / 4)) :+ addTwoPoints(newPoint, (size / 4 + size / 2, size / 4, size / 4 + size / 2)) :+
      addTwoPoints(newPoint, (size / 4, size / 4 + size / 2, size / 4)) :+ addTwoPoints(newPoint, (size / 4, size / 4 + size / 2, size / 4 + size / 2)) :+
      addTwoPoints(newPoint, (size / 4 + size / 2, size / 4 + size / 2, size / 4)) :+ addTwoPoints(newPoint, (size / 4 + size / 2, size / 4 + size / 2, size / 4 + size / 2))

  }

  def canBeDivided(node: Placement, s: Shape3D): Boolean = {
    val corners = createCorners(node)
    (corners foldRight false) ((h, t) => {
      val partition = createWiredBox(h, node._2 / 2)
      if (partition.getBoundsInParent.contains(s.getBoundsInParent)) true else t
    })
  }

  def createWiredBox(origin: Point, size: Size): Shape3D = {
    val box = new Box(size, size, size)
    box.setTranslateY(origin._2)
    box.setTranslateZ(origin._3)
    box.setTranslateX(origin._1)
    val redMaterial = new PhongMaterial()
    redMaterial.setDiffuseColor(Color.rgb(150, 0, 0))
    box.setMaterial(redMaterial)
    box.setDrawMode(DrawMode.LINE)
    box
  }

  def addOctreeToWorldRoot(oct:Octree[Placement], worldRoot: Group):Unit = oct match {
    case n:OcNode[Placement] => addOctreeToWorldRoot(n, worldRoot)
    case l:OcLeaf[Placement, Section] =>
      val listShapes = l.section._2
      (listShapes foldRight()) ((h, t) => {
        worldRoot.getChildren.add(h); t
      })
    case _ =>
  }

  //Fazer com que esta função apenas crie as trees sem adicionar já na worldRoot
  /* --- T2 ---
  * Está em falta a implementação do algoritmo de otimização que trata dos conflitos entre
  *   diferentes partições para um shape, em que o mesmo devia ir para a partição "ascendente".
  *   Sendo assim, quando um shape é intersetado por várias partições, nenhuma das partições irá ser criada,
  *   por isso levando a com que possívelmente apareçam shapes que não se encontrem dentro de partições, ou
  *   em partições maiores do que deviam.
  * */

  //Cria um octree sem especificar o tamanho da root
  def createTree(shapeList: List[Shape3D]): Octree[Placement] = createTree(shapeList, ((16.0,16.0,16.0), 32))

  //  def createTree(worldRoot: Group, shapeList: List[Shape3D], root: Placement): Octree[Placement] = {
  def createTree(shapeList: List[Shape3D], root: Placement): Octree[Placement] = {
    val size = root._2
    val emptyOcNode = new OcNode[Placement](((0.0, 0.0, 0.0), size / 2), OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty, OcEmpty)
    val corners = createCorners(root)

    //Para cada partição ver se se existe alguma figura que esteja contida na mesma
    def iterateThroughCorners(tree: Octree[Placement], corners: List[Point], i: Int, stop: Int): Octree[Placement] = {
      if (i > stop)
        return tree

      val partition = createWiredBox(corners(i), size / 2)

      (shapeList foldRight List[Shape3D]()) ((h, t) => {

        if (partition.getBoundsInParent.contains(h.getBoundsInParent)) {

          //Se puder ser dividida em ainda mais partições, criar um node, que representa um novo ramo
          if (canBeDivided((corners(i), size / 2), h)) {
//            val node = createTree(worldRoot, shapeList, (corners(i), size / 2))
            val node = createTree(shapeList, (corners(i), size / 2))
            val finalTree = putElementAt(tree, node, i).asInstanceOf[OcNode[Placement]]
            return iterateThroughCorners(finalTree, corners, i + 1, stop)

            //Se não puder ser dividida
            //Se nessa partição já existir uma leaf, adicionar o novo objeto à leaf
          } else tree.asInstanceOf[OcNode[Placement]].productElement(i) match {
            case value: OcLeaf[Placement, Section] =>

              val list = value.section._2
//              worldRoot.getChildren.add(partition)
              //retorna uma nova árvore com a lista atualizada e o elemento, mais a partição, no seu sitio
              val finalTree = putElementAt(tree, new OcLeaf[Placement, Section]((corners(i), size / 2), list :+ h :+ partition), i).asInstanceOf[OcNode[Placement]]
              return iterateThroughCorners(finalTree, corners, i + 1, stop)

            //Se não existir uma leaf ainda, fazer uma nova
            case _ =>

//              worldRoot.getChildren.add(partition)
              //retorna uma nova árvore com a lista com o elemento e a partição no seu sitio
              val finalTree = putElementAt(tree, new OcLeaf[Placement, Section]((corners(i), size / 2), List(h, partition)), i).asInstanceOf[OcNode[Placement]]
              return iterateThroughCorners(finalTree, corners, i + 1, stop)

          }

        }
        //Repetir o processo com o seguinte shape
        t
      })

      iterateThroughCorners(tree, corners, i + 1, stop)
    }
    //Começar a iterar sobre as partições com uma nova árvore vazia
    iterateThroughCorners(emptyOcNode, corners, 0, corners.size - 1)
  }


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
  //  --- T5 ---
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
            material.setDiffuseColor(func(color))
            t

          })

        case _ =>
      }
      if (i < stop) iterate(root, i + 1, stop) else root
    }

    iterate(root, 0, inputSize - 1)
  }

  // ------- // --- Funções sem utilidade --- // ------- //

  // Tinham como objetivo auxiliar a implementação do algoritmo apresentado no Anexo I

  /*
* Se o shape intersetar essa partição, criar um objeto que fica com a mesma origem
*   que a partição e de seguida verificar se na mesma o shape não está contido. Caso sim,
*   retornar esse shape, caso contrário devolver o original
* */
  def transformShape(partition: Shape3D, s: Shape3D): Shape3D = {
    if (s.getBoundsInParent.intersects(partition.getBoundsInParent)) {
      val x = partition.getTranslateX.toInt
      val y = partition.getTranslateY.toInt
      val z = partition.getTranslateZ.toInt
      val difColor = s.getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor
      val color = ((difColor.getRed * 255.0).toInt, (difColor.getGreen * 255.0).toInt, (difColor.getBlue * 255.0).toInt)
      if (s.toString.contains("Cylinder")) {
        val newShape = GraphicModelConstructor.createShape("Cylinder", color, (x, y, z), (s.getScaleX, s.getScaleY, s.getScaleZ))
        if (partition.getBoundsInParent.contains(newShape.getBoundsInParent)) return newShape
      }
      if (s.toString.contains("Box")) {
        val newShape = GraphicModelConstructor.createShape("Box", color, (x, y, z), (s.getScaleX, s.getScaleY, s.getScaleZ))
        if (partition.getBoundsInParent.contains(newShape.getBoundsInParent)) return newShape
      }
    }
    s
  }

  /*
* Se o shape intersetar essa partição, criar um objeto que fica com a mesma origem
*   que a partição e de seguida verificar se na mesma o shape não está contido. Caso sim,
*   retornar true, caso contrário false
* */
  def canBeTransformed(node: Placement, s: Shape3D, worldRoot: Group): Boolean = {
    val corners = createCorners(node)
    (corners foldRight false) ((h, t) => {
      val partition = createWiredBox(h, node._2 / 2)
      if (s.getBoundsInParent.intersects(partition.getBoundsInParent)) {
        val x = partition.getTranslateX.toInt
        val y = partition.getTranslateY.toInt
        val z = partition.getTranslateZ.toInt
        val difColor = s.getMaterial.asInstanceOf[PhongMaterial].getDiffuseColor
        val color = ((difColor.getRed * 255.0).toInt, (difColor.getGreen * 255.0).toInt, (difColor.getBlue * 255.0).toInt)
        if (s.toString.contains("Cylinder")) {
          val newShape = GraphicModelConstructor.createShape("Cylinder", color, (x, y, z), (s.getScaleX, s.getScaleY, s.getScaleZ))
          if (partition.getBoundsInParent.contains(newShape.getBoundsInParent)) worldRoot.getChildren.add(newShape)
          return true
        }
        if (s.toString.contains("Box")) {
          val newShape = GraphicModelConstructor.createShape("Box", color, (x, y, z), (s.getScaleX, s.getScaleY, s.getScaleZ))
          if (partition.getBoundsInParent.contains(newShape.getBoundsInParent)) worldRoot.getChildren.add(newShape)
          return true
        }
      }
      t
    })
  }

  // ------- // ------ // ------- // ------ // ------- //
}

