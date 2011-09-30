object Test extends App {
  trait Rep[T]
  class MyRow extends Row[Rep] // Predef.Row
  class ApplyDynamicOps {
    def selectDynamic[T](n: String): Rep[T] = error(n)
  }
  implicit def applyDynamicOps[T <: MyRow](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps
  
  val qual = new MyRow{ val xxx: Rep[Int] = null }
  val x: Rep[Int] = qual.xxx // becomes `applyDynamicOps[MyRow{val xxx: Int}](qual).applyDynamic[Int]("xxx")()`
}