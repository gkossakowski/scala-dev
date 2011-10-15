object Test extends App {
  trait Rep[T]
  class MyRow extends Row[Rep]
  class MyQual extends Rep[MyRow{val xxx: Int}] {
    def selectDynamic[T](n: String): Rep[T] = error(n)
  }
  val qual = new MyQual
  val x: Rep[Int] = qual.xxx // becomes `qual.applyDynamic[Int]("xxx")()`
}