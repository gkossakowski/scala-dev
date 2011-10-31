class Dyna extends Dynamic {
  def selectDynamic(n: String): Int = {println("select "+n); 1}
  def applyDynamic(n: String) = new {
    def apply(as: Any*): String = {println("apply "+n+as.toList.mkString("(",",",")")); "a"}
    def update(as: Any*): String = {println("apply.update "+n+as.toList.mkString("(",",",")")); "a"}
  }
  def updateDynamic(n: String)(rhs: Any): Any = {println("update "+n+" to "+rhs); rhs}
}

object Test extends App {
  val foo = new Dyna
  foo.field          // foo.selectDynamic("field")                  -- BYVALmode
  foo.method("blah") // foo.applyDynamic("method")("blah")          -- FUNmode
  foo.arr(10) = 13   // foo.applyDynamic("method").update(10, 13)   -- QUALmode
  foo.varia = 10     // foo.updateDynamic("varia")(10)              -- LHSmode
}