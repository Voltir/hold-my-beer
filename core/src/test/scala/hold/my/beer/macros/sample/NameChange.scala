package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.Todo

@Version(1)
case class NameChange(a: Int)

object NameChange {
  implicit val _test = Todo.test[NameChange]("namechange")
}
