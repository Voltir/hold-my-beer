package hold.my.beer.macros.sample.versions.samplev2modified
object V1 {
  import hold.my.beer.Version
  import hold.my.beer.macros.{ Internal, Todo }
  @Version(1) case class SampleV2Modified_V1(a: Int, b: String) {}
  object SampleV2Modified_V1 {}
}
object V2 {
  import hold.my.beer.Version
  import hold.my.beer.macros.{ Internal, Todo }
  @Version(2) case class SampleV2Modified_V2(a: Int, b: String) {}
  object SampleV2Modified_V2 {}
}