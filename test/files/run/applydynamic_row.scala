object Test extends App {
  class Rep[+x] {
    // TODO: support implicit args, the rewrite for functions does not work as it supplies the explicit argument where the implicit one is expected
    def selectDynamic[T](n: String): T = {
      println("select "+ n)// +" : "+ manifest[T])
      (n match {
        case "field" => 1
        case "fun" => (x: Int) => x+1
      }).asInstanceOf[T]
    }
    def applyDynamic[T](n: String): T = selectDynamic(n)
    def updateDynamic[T: Manifest](n: String)(rhs: T): T = {
      println("update "+n+" to "+rhs+" : "+ manifest[T]); rhs
    }    
  }
  case class Const[T](x: T) extends Rep[T]
  implicit def liftInt(x: Int) = new Const(x)
  implicit def liftFun(x: Int => Int) = new Const(x)

  val foo : Rep[Row[Rep] { val field: Int; val fun: Int => Int; var varia: Int }] = new Rep
  println(foo.field)    // println(foo.selectDynamic[Int]("field"));  
  println(foo.fun(1))   // println(foo.applyDynamic[Int => Int]("fun").apply(1));
  foo.varia = 10        // foo.updateDynamic[Int]("varia")(10)(Manifest.Int)
}