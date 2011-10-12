object Test extends App {
  trait Rep[x] {
    def __newVar[T](x: T): Rep[T] = error("")
    def selectDynamic[T](n: String): Rep[T] = error("")
  }

  // representation of a statically-known constant
  case class Const[T](x: T) extends Rep[T]

  // automatically lift strings into their representations
  implicit def liftString(x: String): Rep[String] = Const(x) 

  case class Var[T, U](self: Rep[T], x: U) extends Rep[U]

  // to represent the self/this reference in a reified object creation
  case class Self[T] extends Rep[T] {
    override def __newVar[T](x: T): Rep[T] = Var(this, x)
  }

  // this method is called by the virtualizing compiler
  def __new[T](args: (String, Rep[T] => Rep[_])*): Rep[T] = {
    val me = new Self[T]
    new Obj(me, args map {case (n, rhs) => (n, rhs(me))} toMap)
  }

  class Obj[T](self: Rep[T], fields: Map[String, Rep[_]]) extends Rep[T] {
    override def selectDynamic[T](n: String): Rep[T] = {
      val res = fields(n)
      println(self +" DOT "+ n + " = "+ res)
      res.asInstanceOf[Rep[T]]
    }
  }
  val foo: Rep[Row[Rep] { var x: Int; val y: String }] = new Row[Rep] { var x = 23; val y = "y" }
  println(foo.x)
}