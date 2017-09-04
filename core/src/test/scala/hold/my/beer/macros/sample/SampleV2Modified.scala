package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.{Internal, Todo}

@Version(2)
case class SampleV2Modified(a: Int, b: String, c: Boolean)

object SampleV2Modified {
  implicit val _test: Internal.Action = Todo.test[SampleV2Modified]("samplev2modified")
}
