object Test extends EmbeddedControls {
  case class Rep[T](x: T)
  def __new[T](args: (String, Rep[T] => Rep[_])*): Rep[T] = error("")
  val foo = new Row[Rep] {
    val a = Rep(1) //type inference should infer T=Int
  }
}
