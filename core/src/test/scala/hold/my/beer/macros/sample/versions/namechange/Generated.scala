package hold.my.beer.macros.sample.versions.namechange
object V1 {
  import hold.my.beer.Version
  import hold.my.beer.macros.Todo
  @Version(1) case class NameChangeOriginal_V1(a: Int) {}
  object NameChangeOriginal_V1 {}
}