object Test {

  trait Base extends EmbeddedControls {
    type Rep[T]
    type JSLiteral <: Row[Rep]
  }

  object Specific extends Base {
    val foo = new JSLiteral { // it should be possible to call new on abstract type T as long as T <: Row[Rep]
    }
  }
}
