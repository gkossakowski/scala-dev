class Bippy {
  def f = 1
 }

trait Foo {
  object Bar extends Bippy {
    override def f = 2
  }
}

trait Foo2 extends Foo {
  override object Bar extends Bippy {
    override def f = 3
  }
}
