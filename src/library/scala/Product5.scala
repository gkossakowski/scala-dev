
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: genprod.scala 9543 2007-01-02 16:51:29Z emir $

// generated by genprod on Wed Jan 03 13:46:54 CET 2007

package scala

import Predef._

object Product5 {
  def unapply[T1, T2, T3, T4, T5](x: Product5[T1, T2, T3, T4, T5]): Option[Product5[T1, T2, T3, T4, T5]] = 
    Some(x)
}

/** Product5 is a cartesian product of 5 components 
 */
trait Product5[+T1, +T2, +T3, +T4, +T5] extends Product {

  /**
   *  The arity of this product.
   *  @return 5
   */
  override def arity = 5

  /**
   *  Returns the n-th projection of this product if 0&lt;=n&lt;arity,
   *  otherwise null.
   *
   *  @param n number of the projection to be returned 
   *  @return  same as _(n+1)
   *  @throws  IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case _ => throw new IndexOutOfBoundsException(n.toString())
  }

  /** projection of this product */
  def _1: T1

/** projection of this product */
  def _2: T2

/** projection of this product */
  def _3: T3

/** projection of this product */
  def _4: T4

/** projection of this product */
  def _5: T5


}
