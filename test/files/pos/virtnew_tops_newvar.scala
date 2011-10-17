object Test extends App {
  trait Rep[x] {
    def __newVar[T](x: T): Rep[T] = error("")
    def selectDynamic[T](n: String): Rep[T] = error("")
  }

  def __newVar[T:Manifest](init: T) = var_new(Const(init))
  //def __newVar[T](init: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = var_new(init)
  //def __newVar[T](init: Var[T])(implicit o: Overloaded2, mT: Manifest[T]) = var_new(init)

  // as in virtualization-lms-core, this is not a rep
  case class Variable[T](e: Rep[T])
  def var_new[T:Manifest](init: Rep[T]) = Variable(init)

  // representation of a statically-known constant
  case class Const[T](x: T) extends Rep[T]

  case class Var[T, U](self: Rep[T], x: U) extends Rep[U]
  // to represent the self/this reference in a reified object creation
  case class Self[T]() extends Rep[T] {
    override def __newVar[T](x: T): Rep[T] = Var(this, x)
  }

  // this method is called by the virtualizing compiler
  def __new[T](args: (String, Rep[T] => Rep[_])*): Rep[T] = {
    val me = new Self[T]()
    new Obj(me, args map {case (n, rhs) => (n, rhs(me))} toMap)
  }

  class Obj[T](self: Rep[T], fields: Map[String, Rep[_]]) extends Rep[T] {
    override def selectDynamic[T](n: String): Rep[T] = {
      val res = fields(n)
      println(self +" DOT "+ n + " = "+ res)
      res.asInstanceOf[Rep[T]]
    }
  }

  // the var xx should not be reified by the top-level __newVar call (which generates a Variable, not a Rep)
  // but its value should simply be wrapped in the __newVar call of Self (which generates a Var which is a Rep)
  val foo: Rep[Row[Rep] { var xx: Int }] = new Row[Rep] { var xx = 23 }
  println(foo.xx)
}
