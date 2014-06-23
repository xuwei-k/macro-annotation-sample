import scalaz.liftFC

object Bar {
  def aaa = 1
}

@liftFC case class Bar[A](aaa: Int, bbb: A){
  def ccc = 9
}

sealed trait Baz[A]
@liftFC object Foo extends Baz[Int] {
  def bbb = 9
}


object Main {

  println(Bar.lift(1,"a"): scalaz.Free.FreeC[Bar, String])
  println(Bar.aaa)
  println(Foo.lift: scalaz.Free.FreeC[Baz, Int])
  println(Foo.bbb)

}
