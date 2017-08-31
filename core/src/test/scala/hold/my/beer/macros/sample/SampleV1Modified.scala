package hold.my.beer.macros.sample

import hold.my.beer.Version
import hold.my.beer.macros.{Internal, Todo}

@Version(1)
case class SampleV1Modified(x: Int, y: String, z: Boolean)

object SampleV1Modified {
  implicit val _test: Internal.Action = Todo.test[SampleV1Modified]("samplev1modified")
}
