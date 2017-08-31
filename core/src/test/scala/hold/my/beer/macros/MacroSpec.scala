package hold.my.beer.macros

import hold.my.beer.macros.sample.{SampleNew, SampleV1, SampleV1Modified}
import hold.my.beer.testing.Tags.Nick
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class MacroSpec extends FlatSpec with Matchers with BeforeAndAfter {

  "macros" should "detect when a new version package needs to be created" in {
    SampleNew._test shouldBe Internal.InitVersion1
  }

  "macros" should "detect no-op if version 1 exists and its the latest version" in {
    SampleV1._test shouldBe Internal.NoOp
  }

  "macros" should "detect UpdateVersion(1) if version 1 exists and has been modified" taggedAs Nick in {
    SampleV1Modified._test shouldBe Internal.UpdateVersion(1)

//    hold.my.beer.codegen.Codegen.initialize(SampleV1Modified.tmpLoc,SampleV1Modified.tmpPkg,List("samplev1modified"))
//    val a = sample.SampleV1Modified.tmpLoc.toFile
//    val b = sample.SampleV1Modified.tmpLoc.toFile.parent / "versions" / "samplev1modified" / "Generated.scala"
//    hold.my.beer.codegen.Codegen.codegen(a,b,"SampleV1Modified",1)
  }
}
