// issue #14
object Test {
  type Rep[x] = x
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
  val foo = new Row[Rep] { val bar = new Row[Rep] { val a = 1 } }
}