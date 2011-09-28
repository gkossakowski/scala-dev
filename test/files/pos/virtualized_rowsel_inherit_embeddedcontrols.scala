object Test extends App with EmbeddedControls { // test whether it works when we inherit from EmbeddedControls -- the reference to Row changes
  trait Rep[T]
  class MyRow extends Row[Rep]
  class ApplyDynamicOps {
    def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
  }
  implicit def applyDynamicOps[T <: MyRow](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps
  
  val qual = new MyRow{ val xxx: Rep[Int] = null }
  val x: Rep[Int] = qual.xxx // becomes `applyDynamicOps[MyRow{val xxx: Int}](qual).applyDynamic[Int]("xxx")()`
}