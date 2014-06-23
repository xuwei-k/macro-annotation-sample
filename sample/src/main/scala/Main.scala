
object Bar {
  def aaa = 1
}

@mkCompanion class Bar

@mkCompanion class Baz


object Main {

  println(Baz.hasFoo)
  println(Bar.hasFoo)
  println(Bar.aaa)

  implicitly[Foo[Bar]]
  implicitly[Foo[Baz]]

}
