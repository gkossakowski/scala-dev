object Test {
  type Rep[x] = x
  val foo: Rep[Row[Rep] { val x: Int; val y: String }] = new Row[Rep] { val x: Rep[Int] = 23; val y = "y" }
}