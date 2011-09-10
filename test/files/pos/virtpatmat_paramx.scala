object X {
  def unapply[T, U](i: T)(f: T => U): Option[U] = Some(f(i))
}

class Param[T, U](f: T => U) {
  def unapply(i: T): Option[U] = Some(f(i))
}

object Test {
  object Even extends Param[Int, Int](2 * _)
  object Uneven extends Param[Int, Int](2 *  _ + 1)

  def fp(m: Int, b: Int, i: Int): Int = i match {
    case 0 => m
    // case X(j)(2 * _)     if j > 0 => fp(m,   b*b, j)
    case Even(j)     if j > 0 => fp(m,   b*b, j)
    // case X(j)(2 *  _ + 1) if j < 0 => fp(m*b, b*b, j)
    case Uneven(j) if j < 0 => fp(m*b, b*b, j)
  }
}