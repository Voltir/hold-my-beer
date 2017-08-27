package hold.my.beer.codegen

import hold.my.beer.codegen.generated._Person__V1
import hold.my.beer.Version
import hold.my.beer.macros.Todo

/* This is a person.
 */
@Version(2)
case class Person(name: String, age: Int, height: Double)

object Person {
  val _todo = Todo.generate("example")
  val _ord: Ordering[Person] = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int = x.age.compare(y.age)
  }

  //Should only have to write the migrate for previous version to next version
  def migrate(v1: _Person__V1): Person = Person(v1.name, v1.age, height = 42)
}
