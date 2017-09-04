package hold.my.beer.macros

import hold.my.beer.macros.sample._
import hold.my.beer.testing.Tags.Nick
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class MacroSpec extends FlatSpec with Matchers with BeforeAndAfter {

  "macros" should "detect when a new version package needs to be created" in {
    SampleNew._test shouldBe Internal.InitVersion1
  }

  "macros" should "detect no-op if version 1 exists and its the latest version" in {
    SampleV1._test shouldBe Internal.NoOp
  }

  "macros" should "detect UpdateVersion(1) if version 1 exists and has been modified" in {
    SampleV1Modified._test shouldBe Internal.UpdateVersion(1)
  }

  "macros" should "detect UpdateVersion(2) if v1 exists and has been updated to v2" in {
    SampleV2._test shouldBe Internal.UpdateVersion(2)
  }

  "macros" should "detect UpdateVersion(2) if v2 exists and has been modified" in {
    SampleV2Modified._test shouldBe Internal.UpdateVersion(2)
  }
}
