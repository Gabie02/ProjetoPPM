import javafx.scene.shape.{Box, Shape3D}

object Octree1 {

  def partitionId(box: Box): (List[Shape3D]) = {Octree1.partitionId(box)}

  val root = new Box(32,32,32)
  println("Property: " + root.widthProperty().get()) //double

  object Octree1 {

    def partitionId(box: Box): (List[Shape3D]) =  {

      val size = box.widthProperty().get()
      val new_size = size/8
      List.fill(8)(new Box(new_size, new_size, new_size))
    }

    def () = {

    }



  }


}
