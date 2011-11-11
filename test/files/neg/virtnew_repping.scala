object Test extends EmbeddedControls {
  trait Rep[T]
  val foo = new Row[Rep] {
    val a = 1 // a should be forced to have type Rep[Int]
  }
}
