object Reify {
  
  trait Base extends EmbeddedControls {
  
    trait Rep[T] {
      def applyDynamic[T](n: String)(as: Any*): T = {
        println("applyDynamic: "+ n + as.mkString("(", ",", ")"))
        null.asInstanceOf[T]
      }
    }
  
    type JSLiteral <: Row[Rep]
    
    implicit def magic[T](x: T): Rep[T] = null.asInstanceOf[Rep[T]]
  }
  
  object Specific extends Base {
  
    val foo = new JSLiteral {  //it should be possible to call new on abstract type T as long as T <: Row[Rep]
      val a = 1 //a should have type Rep[Int]
      val b = a + 1  //it should be possible to refer a, b should have type Rep[Int]
      val c = "str" //c should have type Rep[String]
    }
  
  }
  
}
