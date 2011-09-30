class Test extends EmbeddedControls {
  type Rep[T]
  //tests if it's possible to create two anynomous classes for Row[Rep]
  //and if they names do not clash
  val foo = new Row[Rep] {}
  val foo2 = new Row[Rep] {}
}
