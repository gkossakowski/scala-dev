package scala

abstract class MatchingStrategy[M[+x]] {
  def zero: M[Nothing]
  def one[T](x: T): M[T] // TODO: switch to CBN so isSuccess doesn't actually have to run the case bodies
  def guard[T](cond: Boolean, then: => T): M[T] // = if(cond) one(then) else zero
  def or[T, U](f: T => M[U], alts: M[T]*) = (alts foldLeft (zero: M[U]))(altFlatMap(f)) // find the first alternative to successfully flatMap f
  def altFlatMap[T, U](f: T => M[U])(a: M[U], b: M[T]): M[U] // = a orElse b.flatMap(f) -- can't easily&efficiently express M[T] should have flatMap and orElse
  def runOrElse[T, U](x: T)(f: T => M[U]): U
  def isSuccess[T, U](x: T)(f: T => M[U]): Boolean
}

object MatchingStrategy {
  implicit object OptionMatchingStrategy extends MatchingStrategy[Option] {
    type M[+x] = Option[x]
    @inline def guard[T](cond: Boolean, then: => T): M[T] = if(cond) Some(then) else None
    @inline def zero: M[Nothing] = None
    @inline def one[T](x: T): M[T] = Some(x)
    @inline def altFlatMap[T, U](f: T => M[U])(a: M[U], b: M[T]): M[U] = a orElse b.flatMap(f)
    @inline def runOrElse[T, U](x: T)(f: T => M[U]): U = f(x) getOrElse (throw new MatchError(x))
    @inline def isSuccess[T, U](x: T)(f: T => M[U]): Boolean = !f(x).isEmpty 
  }
}