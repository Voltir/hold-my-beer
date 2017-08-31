package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.{Internal, Todo}

@Version(1)
case class SampleV1(a: Int, b: String)

object SampleV1 {
  implicit val _test: Internal.Action = Todo.test[SampleV1]("samplev1")
}
