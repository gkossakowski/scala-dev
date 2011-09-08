class Bippy {
  def f = 1
}
trait Bippo

trait Foo {
  object Bar extends Bippy with Bippo {
    override def f = 2
  }

  def f(x: Bippo)
  def g = f(Bar)
}

trait Foo2 extends Foo {
  override object Bar extends Bippy {
    override def f = 3
  }
}
