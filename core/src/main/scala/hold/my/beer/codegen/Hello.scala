package hold.my.beer.codegen

import hold.my.beer.codegen.generated._Person__V1

import scala.meta._

object Hello extends Greeting with App {
  println(greeting)

  _Person__V1("foo", 42)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
