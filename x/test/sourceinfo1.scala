
import reflect.SourceInfo

object Test {

/* The following doesn't work, since the newly created manifest would always
 * have the same source position, namely, the position of the invocation
 * of the withNewManifest method
  implicit def sourceInfoManifest[T]: SourceInfoManifest[T] = {
    def withNewManifest()(implicit m: Manifest[T]) {
      // here we know that m has been generated at the current source location
    }
    withNewManifest()
  }
*/

  def printInfo(m: SourceInfo) {
    println("line: "+m.line)
    println("binding: "+m.bindings(0)._1)
    println("method name: "+m.methodName)
  }
  
  def inspect[T](x: T)(implicit m: SourceInfo): Int = {
    def withManifest()(implicit mm: SourceInfo) {
      printInfo(mm)
    }
    printInfo(m)
    withManifest()
    0
  }

  /* - by-name passing of manifests?
   */
  def main(args: Array[String]) {
    val l = List(1, 2, 3)
    val x = inspect(l)
    val y = {
      val z = 4*7
      inspect(l)
    }
  }

}
