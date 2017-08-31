package hold.my.beer.codegen

import better.files._
import better.files.Cmds._

import scala.meta._

object Codegen {

  val VERSION_PKG = "versions"

  def versionName(term: String, version: Int): String = s"_${term}__V$version"

  val version = """_(\w+)__V(\d+)""".r

  def extract(term: String): (String, Int) = {
    term match {
      case version(n, v) => (n, v.toInt)
    }
  }

  //Returns an error string if failed, None if success
  //def initialize(src: String, prefix: sourcecode.Pkg, namespace: List[String]): Option[String] = {
  def initialize(src: String, prefixPkg: String, namespace: List[String]): Option[String] = {
    //Create a versions directory in parent folder
    val dir = src.toFile.parent

    val suffix = VERSION_PKG :: namespace

    val init = s"package $prefixPkg.${suffix.mkString(".")}"

    //Init version package dir structure
    val initDirs = dir / suffix.mkString("/")
    if (initDirs.exists) {
      return Some("versions directory already initialized!")
    }

    mkdirs(initDirs)

    //Write out .scala file
    init.parse[Source] match {
      case Parsed.Success(parsed) =>
        (initDirs / s"Generated.scala").overwrite(parsed.syntax)
        None
      case Parsed.Error(_, _, details) =>
        Some(details.getMessage)
    }
  }

  //Returns an error string if failed, None if success
  def codegen(src: File, versions: File, target: String, version: Int): Option[String] = {
    (src.toJava.parse[Source], versions.toJava.parse[Source]) match {
      case (Parsed.Success(s), Parsed.Success(v)) =>
        process(target, s, v, version) match {
          case Right(neat) =>
            versions.overwrite(neat.syntax)
            None
          case Left(err) => Some(err)
        }
      case _ => Some("Failed to load inputs!")
    }
  }

  private def removeCurrentInstance(version: Int, versions: Source): Pkg = {
    val removed = Type.Name(s"V$version")

    val pkg = versions.collect {
      case x @ q"package $_ { ..$_ }" => x.asInstanceOf[Pkg]
    }

    val neat = pkg.head.stats.filter {
      case q"object $obj { ..$_ }" => obj.value != removed.value
      case _                       => true
    }

    pkg.head.copy(stats = neat)
  }

  private def generateNextVersion(src: Source, target: String, version: Int): Defn.Object = {
    val renamed = s"${target}_V$version"

    def renameTargetType(s: Stat): Stat = {
      s.transform {
          case Type.Name(`target`) => Type.Name(renamed)
        }
        .asInstanceOf[Stat]
    }

    val imports = src.collect {
      case x @ q"import ..$imp" => x.asInstanceOf[Stat]
    }

    val adt = src.collect {
      case q"..$mods case class $adt(..$fields) { ..$defs }" if adt.value == target =>
        val versionName = Type.Name(s"${adt.value}_V$version")
        val rewrite     = defs.map(renameTargetType)
        q"..$mods case class $versionName(..$fields) { ..$rewrite }"
    }

    val companion = src.collect {
      case q"..$mods object $adt { ..$defs }" if adt.value == target =>
        val versionName = Term.Name(renamed)
        val rewrite     = defs.map(renameTargetType)
        q"..$mods object $versionName { ..$rewrite }"
    }

    val result =
      q"""object ${Term.Name(s"V$version")} {
            ..$imports
            ..$adt
            ..$companion
           }"""
    result
  }

  private def process(target: String,
                      src: Source,
                      versions: Source,
                      version: Int): Either[String, Pkg] = {
    val next    = generateNextVersion(src, target, version)
    val current = removeCurrentInstance(version, versions)
    Right(current.copy(stats = current.stats ++ List(next)))
  }
}
