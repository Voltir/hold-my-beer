package hold.my.beer.codegen

import hold.my.beer.testing.Tags._
import org.scalatest._
import better.files._
import better.files.Cmds._

class CodegenSpec extends FlatSpec with Matchers with BeforeAndAfter {

  val sampleVersionDir
    : File = sample.Person.location.value.toFile.parent / "versions"

  val namespace = List("person")

  before {
    rm(sampleVersionDir)
  }

  "Codegen" should "be able to initialize a new version package" in {
    val errs = Codegen.initialize(sample.Person.location.value,
                                  sample.Person.pkg.value,
                                  namespace)
    val initialized = sampleVersionDir / "person"
    initialized.exists shouldBe true
    errs shouldBe None
  }

  "Codegen" should "be able to initialize a first version" taggedAs Nick in {
    val person = sample.Person.location.value.toFile

    val errInit =
      Codegen.initialize(sample.Person.location.value, sample.Person.pkg.value, namespace)

    val generated = sampleVersionDir / "person" / "Generated.scala"

    val errGen = Codegen.codegen(person, generated, "person", 1)

    errInit shouldBe None
    errGen shouldBe None
    generated.exists shouldBe true
  }

}
