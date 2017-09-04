package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.{Internal, Todo}

@Version(2)
case class TestIt(name: String)

object TestIt {
  val _version: Internal.Generated = Todo.generated[TestIt]("testit")

  val wat = 42
}
