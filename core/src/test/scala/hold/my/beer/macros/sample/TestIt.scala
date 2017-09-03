package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.Todo

@Version(1)
case class TestIt(name: String, age: Int)

object TestIt {
  val _version: Int = Todo.generated[TestIt]("testit")
}
