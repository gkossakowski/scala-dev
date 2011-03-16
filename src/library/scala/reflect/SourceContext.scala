package scala.reflect

trait SourceContext extends SourceLocation {
  def bindings: List[(String, Int)]

  def assignedVariable: Option[String] =
    if (bindings(0)._1 == null) None
    else Some(bindings(0)._1)

  def methodName: String
}

object SourceContext {
  def apply(name: String, sourceInfo: List[(String, Int)]): SourceContext =
    new ConcreteSourceContext(name, sourceInfo)

  private class ConcreteSourceContext(override val methodName: String,
                                      override val bindings: List[(String, Int)])
    extends SourceContext {
    def line = bindings(0)._2
  }
  
}
