object Test extends EmbeddedControls {
  case class Rep[T](x: T) {
    def selectDynamic[T](field: String): Rep[T] = null.asInstanceOf[Rep[T]]
  }
  val foo = new Row[Rep] {
    val a = Rep[Int](1)
    val b: Rep[Int] = a // it should be possible to refer a, b should have type Rep[Int]
  }
}
