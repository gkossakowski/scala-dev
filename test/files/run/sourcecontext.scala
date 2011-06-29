
import reflect.SourceContext

object Test {

  def relative(name: String) = {
    val lastSlash = name.lastIndexOf('/')
    if (lastSlash == -1)
      name.substring(name.lastIndexOf('\\') + 1)
    else
      name.substring(lastSlash + 1)
  }

  def printInfo(m: SourceContext) {
    println("line: "+m.line)
    println("binding: "+m.bindings(0)._1)
    println("method name: "+m.methodName)
    println("contexts: "+m.allContexts)
    println("file name: "+relative(m.fileName))
  }
  
  def inspect[T](x: T)(implicit m: SourceContext): Int = {
    def withManifest()(implicit mm: SourceContext) {
      printInfo(mm)
    }
    printInfo(m)
    withManifest()
    0
  }
  
  def main(args: Array[String]) {
    val l = List(1, 2, 3)
    val x = inspect(l)
    val y = {
      val z = 4*7
      inspect(l)
    }
  }

}
