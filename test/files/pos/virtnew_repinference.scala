object Test extends EmbeddedControls {
  case class Rep[T](x: T)
  val foo = new Row[Rep] {
    val a = Rep(1) //type inference should infer T=Int
  }
}
