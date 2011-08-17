/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Predef.scala 21249 2010-03-24 15:37:50Z extempore $


package scala

/** The <code>StandardEmbeddings</code> object provides method definitions
 *  where calls to the methods are treated by the compiler in a special way.
 *  The reason to express these calls as methods is to give embedded DSLs a chance
 *  to provide their own definitions and thereby override the standard
 *  interpretation of the compiler.
 *
 *  Example: When faces with an `if` construct, the parser will generate
 * 
 *  a method call: __ifThenElse(cond, thenp, elsep)
 * 
 *  It then depends on the type assignment which version of `ifThenElse` is called.
 *  If it is still the standard one in this trait, the type checker will
 *  replace it by an `If` tree node. If not, the call will be left as it is
 *  and a staging or interpreting DSL can take over.
 */
trait EmbeddedControls {
  /** Note why types are by-value
   */
  def __whileDo(cond: Boolean, body: Unit): Unit = 
    throw new UnsupportedOperationException("__whileDo")

  def __doWhile(body: Unit, cond: Boolean): Unit = 
    throw new UnsupportedOperationException("__doWhile")

  def __ifThenElse[T](cond: => Boolean, thenp: => T, elsep: => T): T = 
    throw new UnsupportedOperationException("__ifThenElse")

  def __newVar[T](init: T): T = 
    throw new UnsupportedOperationException("__newVar")

  def __assign[T](lhs: T, rhs: T): Unit = 
    throw new UnsupportedOperationException("__assign")

  def __return(expr: Any): Nothing = 
    throw new UnsupportedOperationException("__return")

  def __equal(expr1: Any, expr2: Any): Boolean = 
    throw new UnsupportedOperationException("__equal")

  trait Row[+Rep[x]]

  /** 
    * Rep is determined by looking at
    * for args(i) = (l_i, v_i) : (String, T_i)
    * type T'_i = T_i match { case Rep[T] => T case _ => T_i }
    * T is the Rep of the structural type that consist of a def for each i:
    * def <l_i> : <T'_i>
    */
  def __new[T](args: (String, Any)*): T =
    throw new UnsupportedOperationException("__new")

}

trait ProxyControlsBase extends EmbeddedControls {
  type TransparentProxy[+T]

  def __forward[A,B,C](self: TransparentProxy[A], method: String, x: TransparentProxy[B]*): TransparentProxy[C] =
    throw new UnsupportedOperationException("__forward")
}

trait ProxyControls extends ProxyControlsBase
