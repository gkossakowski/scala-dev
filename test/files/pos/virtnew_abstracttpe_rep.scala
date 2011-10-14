object Test extends EmbeddedControls {
  class Foo {
    type JSLiteral <: Row[Rep]
    case class Rep[T](x: T)
    def __new[T](args: (String, Rep[T] => Rep[_])*): Rep[T] = error("")
    val foo = new JSLiteral {
      val a = Rep[Int](1)
    }
  }
}
