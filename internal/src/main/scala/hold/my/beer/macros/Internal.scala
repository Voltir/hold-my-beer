package hold.my.beer.macros

import hold.my.beer.codegen.Codegen

import scala.language.experimental.macros
import scala.reflect.macros._

object Internal {

  //Rules:
  //@Version(N) and _VN exists, and all fields and types line up, and definitions for all previous versions exist => OK!
  //@Version(1) and no versions => Create _V1
  //@Version(N+1) and all versions 1 to N exist => Create _V(N+1)
  //@Version(N) and @editable and fields do not match: Overwrite _V(N+1)!
  def isBeer(c: blackbox.Context)(sym: c.Symbol): Boolean = {
    sym.isClass && !sym.isJava && sym.asType.info.baseClasses.contains(c.typeOf[scala.Product].typeSymbol)
  }

  def versions(c: blackbox.Context)(generatedPackage: c.universe.ModuleSymbol, srcSym: c.universe.ClassSymbol): List[(Int, String, c.universe.ClassSymbol)] = {
    generatedPackage.moduleClass.info.decls.filter(isBeer(c)).map { x =>
      val (src, version) = Codegen.extract(x.name.toString)
      (version, src, x.asClass)
    }.filter(_._2 == srcSym.name.toString).toList
  }

  def generate(c: blackbox.Context)(pkg: c.Expr[String]): c.Expr[String] = {
    import c.universe._

    val targetPkg = pkg match {
      case Expr(Literal(Constant(x : String))) => x
      case _ =>
        c.abort(c.enclosingPosition, "Pkg must be a constant string literal")
        ""
    }

    val versioned = c.mirror.staticPackage(targetPkg)
    val beers = versioned.moduleClass.info.decls.filter(isBeer(c)).map(b => (Codegen.versionName(b.name.toString, 1), b.asClass)).toList.sortBy(_._1).map(_._2)

    val generated: c.universe.ModuleSymbol = versioned.moduleClass.info.decls.filter(x => x.isPackage && x.fullName.endsWith("generated")).toList match {
      case gen :: Nil => gen.asModule
      case _ => c.abort(c.enclosingPosition, s"package $targetPkg.generated not found! please create first!")
    }

    beers.foreach { b =>
      println(s"==== ${b.fullName} ====")
      println(s"Annotations: ${b.annotations}")
      println(s"Found: ${versions(c)(generated, b)}")
      println("\n")
    }
//    beers.zip(omgneat).foreach { case (b,g) =>
//      println(s"Woo: ${b.fullName} vs ${g.fullName}")
//
//      println(b.annotations)
//
//      b.info.decls.sorted.zip(g.info.decls.sorted).foreach { case (srcField, genField) =>
//        println(srcField.fullName)
//        if(srcField.name.toString != genField.name.toString) {
//          c.abort(c.enclosingPosition, s"Field names don't match: $srcField != $genField")
//        }
//      }
    val zzzzz = "OMG!"
    c.Expr[String](q"$zzzzz")
  }
}

object Todo {
  def generate(pkg: String): String = macro Internal.generate
}
