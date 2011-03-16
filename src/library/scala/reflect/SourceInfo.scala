package scala.reflect

trait SourceInfo { self =>
  def line: Int

  def methodName: String

  def bindings: List[(String, Int)]

  def assignedVariable: Option[String] =
    if (bindings(0)._1 == null) None
    else Some(bindings(0)._1)
  
  def update(lineInfo: Int) = new SourceInfo {
    override def line = lineInfo // todo: update bindings instead
    def bindings = self.bindings
    def methodName = self.methodName
  }
}

object SourceInfo {
  def apply(name: String, sourceInfo: List[(String, Int)]): SourceInfo =
    new ConcreteSourceInfo(name, sourceInfo)

  private class ConcreteSourceInfo(override val methodName: String,
                                   override val bindings: List[(String, Int)])
    extends SourceInfo {
    override def line = bindings(0)._2
  }
  
}
