package hold.my.beer.macros.sample.versions.invalid.version
object V1 {
  import hold.my.beer.Version
  import hold.my.beer.macros.Todo
  @Version(1) case class InvalidVersion_V1(a: Int) {}
  object InvalidVersion_V1 {}
}