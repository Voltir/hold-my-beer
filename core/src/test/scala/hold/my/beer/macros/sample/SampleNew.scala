package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.Todo

@Version(1)
case class SampleNew(a: Int, b: String)

object SampleNew {
  implicit val _test = Todo.test[SampleNew]("samplenew")
}