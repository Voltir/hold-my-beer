package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.{Internal, Todo}

@Version(2)
case class SampleV2(a: Int, b: String)

object SampleV2 {
  implicit val _test: Internal.Action = Todo.test[SampleV2]("samplev2")
}
