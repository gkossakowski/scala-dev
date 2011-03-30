import scala.reflect.SourceContext

class ParamInfo[T]

abstract class ParList[+T] {
  def head: T
  def tail: ParList[T]

// Current behavior:
//sourcecontext-closures.scala:8: error: illegal dependent method type
//  def foreach(f: T => Unit)(implicit info: ParamInfo[f.type]): Unit
//              ^
//  def foreach(f: T => Unit)(implicit info: ParamInfo[f.type]): Unit

  def foreach(f: T => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
}

class ParCons[T](val head: T, val tail: ParList[T]) extends ParList[T]

object ParNil extends ParList[Nothing] {
  def head = sys.error("ParNil.head")
  def tail = sys.error("ParNil.tail")
}

object Test {
  
  def main(args: Array[String]) {
    val l = new ParCons(1, new ParCons(2, new ParCons(3, ParNil)))
    val fun: Int => Unit = { x: Int => println(x) }
    implicit val info = new ParamInfo[fun.type]
    l.foreach(fun)
  }
  
}
