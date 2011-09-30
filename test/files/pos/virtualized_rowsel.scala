object Test extends App {
  trait Rep[T]
  class MyRow extends Row[Rep]
  class MyQual extends Rep[MyRow{val xxx: Int}] {
    def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
  }
  val qual = new MyQual
  val x: Rep[Int] = qual.xxx // becomes `qual.applyDynamic[Int]("xxx")()`
}