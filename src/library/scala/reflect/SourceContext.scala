package scala.reflect

trait SourceContext extends SourceLocation {
  def bindings: List[(String, Int)]

  def assignedVariable: Option[String] =
    if (bindings(0)._1 == null) None
    else Some(bindings(0)._1)

  def methodName: String

  def update(context: SourceContext): SourceContext

  def allContexts: List[List[(String, Int)]]

  var parent: Option[SourceContext] =
    None
}

object SourceContext {

  def apply(name: String, sourceInfo: List[(String, Int)]): SourceContext =
    apply("<unknown file>", name, sourceInfo)

  def apply(fileName: String, name: String, sourceInfo: List[(String, Int)]): SourceContext =
    new ConcreteSourceContext(fileName, name, sourceInfo)

  private class ConcreteSourceContext(override val fileName: String,
                                      override val methodName: String,
                                      override val bindings: List[(String, Int)])
  extends SourceContext {
    def line = bindings(0)._2

    def update(context: SourceContext): SourceContext = {
      context.parent = Some(this)
      context
    }

    def allContexts = {
      var contexts: List[List[(String, Int)]] = List()
      var curr: SourceContext = this
      contexts = contexts ::: List(curr.bindings)
      while (!curr.parent.isEmpty) {
        curr = curr.parent.get
        contexts = contexts ::: List(curr.bindings)
      }
      contexts
    }
  }
  
}
