package hold.my.beer.macros.sample.versions.samplev2
object V1 {
  import hold.my.beer.Version
  import hold.my.beer.macros.{ Internal, Todo }
  @Version(1) case class SampleV2_V1(a: Int, b: String) {}
  object SampleV2_V1 { implicit val _test: Internal.Action = Internal.NoOp }
}