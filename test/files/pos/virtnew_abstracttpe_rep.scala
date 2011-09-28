object Test extends EmbeddedControls {
  type JSLiteral <: Row[Rep]
  case class Rep[T](x: T)
  val foo = new JSLiteral {
    val a = Rep[Int](1)
  }
}
