package scala.reflect

trait SourceContext extends SourceLocation {
  def bindings: List[(String, Int)]

  def assignedVariable: Option[String] =
    if (bindings(0)._1 == null) None
    else Some(bindings(0)._1)

  def methodName: String

  def update(name: String, sourceInfo: List[(String, Int)]): SourceContext

  def firstContext: List[(String, Int)]

  def allContexts: List[List[(String, Int)]]
}

object SourceContext {
  def apply(name: String, sourceInfo: List[(String, Int)]): SourceContext =
    new ConcreteSourceContext(name, sourceInfo)

  private class ConcreteSourceContext(override val methodName: String,
                                      currBindings: List[(String, Int)])
  extends SourceContext {
    var contexts: List[List[(String, Int)]] =
      List(currBindings)

    def line = bindings(0)._2

    def bindings = contexts.head

    def firstContext: List[(String, Int)] =
      contexts.last

    def update(name: String, sourceInfo: List[(String, Int)]): SourceContext = {
      contexts ::= sourceInfo
      this
    }

    def allContexts = contexts
  }
  
}
