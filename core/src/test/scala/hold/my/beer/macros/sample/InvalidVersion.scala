package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.Todo

@Version(42)
case class InvalidVersion(a: Int)

object InvalidVersion {
  val _test = Todo.test[InvalidVersion]("invalid.version")
}