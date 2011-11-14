/* Tests implicit parameters in the presence of context bounds.
 * See Section 7.4 of the Scala Language Specification.
 */
class C {
  def foreach[T: Manifest](x: T) {
  }

  def foreach2[T: Manifest, S: Manifest](x: T, y: S) {
  }

  def foreach3[T: Manifest, S: Manifest](x: T, y: S)(implicit p: C) {
  }
}
