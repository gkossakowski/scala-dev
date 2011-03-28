class C {

  def foreach[T: Manifest](x: T) {
  }

  def foreach2[T: Manifest, S: Manifest](x: T, y: S) {
  }

  def foreach3[T: Manifest, S: Manifest](x: T, y: S)(implicit p: C) {
  }

}
