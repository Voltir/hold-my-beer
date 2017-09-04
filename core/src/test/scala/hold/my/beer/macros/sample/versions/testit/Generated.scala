package hold.my.beer.macros.sample.versions.testit
object V1 {
  import hold.my.beer.Version
  import hold.my.beer.macros.{ Internal, Todo }
  @Version(1) case class TestIt_V1(name: String) {}
  object TestIt_V1 { val wat = 42 }
}
object V2 {
  import hold.my.beer.Version
  import hold.my.beer.macros.{ Internal, Todo }
  @Version(2) case class TestIt_V2(name: String, c: Boolean) {}
  object TestIt_V2 { val wat = 42 }
}