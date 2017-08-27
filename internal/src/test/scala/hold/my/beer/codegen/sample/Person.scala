package hold.my.beer.codegen.sample

import hold.my.beer.Version
import scala.math.{Pi => PI, _}

/* This is a person.
 */
@Version(1)
case class Person(name: String, age: Int, height: Double) {
  def hello: String = s"Hello, $name"
}

object Person {
  val _ord: Ordering[Person] = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int = x.age.compare(y.age)
  }

  //For CodegenSpec only - macro will take care of this in the usual case
  val location: sourcecode.File = sourcecode.File.generate
  val pkg: sourcecode.Pkg = sourcecode.Pkg.generate
}

