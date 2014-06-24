import actormacro.create

import akka.actor.{ActorSystem, Actor}

@create class A(a: Int, b: String) extends Actor {
  override def receive = {
    case message =>
      println(message)
  }
}

object Main {
  implicit val system = ActorSystem.apply("hoge")
  A.create(1, "a")
}
