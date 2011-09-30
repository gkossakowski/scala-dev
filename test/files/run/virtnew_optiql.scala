object Test extends EmbeddedControls with App {
  type Date = String
  trait Rep[x] {
    def selectDynamic[T](n: String): Rep[T] = error("")
  }
  
  case class Const[T](x: T) extends Rep[T]
  implicit def liftConst[T](x: T) = Const(x)

  implicit def dateOps(x: Rep[Date]) = new {
    def <=(x: Rep[Date]): Rep[Boolean] = Const(false)
  }

  case class Self[T] extends Rep[T]
  case class Null[T] extends Rep[T]

  class Obj[T](val self: Rep[T], val fields: Map[String, Rep[t] forSome {type t}]) extends Rep[T] {
    override def selectDynamic[T](n: String): Rep[T] = {
      val res = fields.get(n) getOrElse Null[T]
      println(self +" DOT "+ n + " = "+ res)
      res.asInstanceOf[Rep[T]]
    }
  }

  class Result extends Row[Rep]

  class ApplyDynamicOps {
    def applyDynamic[T](n: String)(as: AnyRef*): Rep[T] = error(n + as.mkString("(", ",", ")"))
  }
  implicit def applyDynamicOps[T <: Result](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps 

  // we can't statically guarantee that R == Rep, since Rep is not globally defined (as is, e.g., Row)
  // I'm trying to get a lifting of =:= to type constructors to work, so we could write R[x] : Eq[Rep], but I haven't gotten that to work yet
  override def __new[T, R[x]](args: (String, R[T] => (R[t] forSome {type t}))*): R[T] = {
    val me = new Self[T]
    new Obj[T](me, args map {case (n, rhs) => (n, rhs(me.asInstanceOf[R[T]]).asInstanceOf[Rep[_]])} toMap).asInstanceOf[R[T]]
  }
  
  case class LineItem(l_shipdate: Date)

  val lineItems = List(LineItem("yesterday"), LineItem("the other day"))

  trait FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]]
  }
  implicit def filterOps[T](x: List[Rep[T]]) = new FilterOps[T] {
    def Where(f: Rep[T] => Rep[Boolean]): List[Rep[T]] = {x map f; x} // just run the function so we get some output
  }
  
  lineItems map (e => new Result { val l_shipdate = e.l_shipdate }) Where (_.l_shipdate <= "today")
}