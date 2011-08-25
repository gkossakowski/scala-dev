object Test { // none of this should be affected by embedding rewrites (== to _equal)
  class B {
    def ==(x1: String, xs: List[_]) = true
  }
  
  class C extends B {
    override def ==(x1: String, xs: List[_]) = super.==(x1, xs)
  }
  
  (new C).==("a", List("a"))
}

object EmbeddedTest {
  def __equal(expr1: Any, expr2: Any*): Boolean = { println("equal "+ (expr1, expr2)); true }
  
  "a" == "b" // should become __equal("a", "b")
  "a" == ("b", "c") // should become __equal("a", "b", "c")
}