package hold.my.beer.codegen.generated

import hold.my.beer.Version
@Version(1) case class _Person__V1(name: String, age: Int)
object _Person__V1 {
  val _ord: Ordering[_Person__V1] = new Ordering[_Person__V1] {
    override def compare(x: _Person__V1, y: _Person__V1): Int =
      x.age.compare(y.age)
  }
}
