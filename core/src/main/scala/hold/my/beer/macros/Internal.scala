package hold.my.beer.macros

import hold.my.beer.codegen.Codegen

import better.files._

import scala.language.experimental.macros
import scala.reflect.macros._

object Internal {

  sealed trait Action
  case object NoOp                 extends Action
  case object InitVersion1         extends Action
  case class UpdateVersion(v: Int) extends Action

  def versionPkg(c: blackbox.Context): String = {
    import c.universe._
    var current = c.internal.enclosingOwner
    var pkg     = List(Codegen.VERSION_PKG)
    while (current != NoSymbol && current.toString != "package <root>") {
      if (current.isPackage) {
        pkg = current.name.decodedName.toString :: pkg
      }
      current = current.owner
    }
    pkg.mkString(".")
  }

  def logic[T: c.WeakTypeTag](c: blackbox.Context)(namespace: c.Expr[String]): Action = {
    import c.universe._
    //println(s"? ${c.enclosingPosition.source.path.toFile.parent}")
    //c.enclosingPosition.source.file

    println("ARGH?")
    def fields(x: c.universe.Symbol): Boolean = {
      x.isPublic && !x.isSynthetic && !x.isConstructor
    }

    //val targetTpe = c.weakTypeTag[T].tpe
    val targetTpe = c.weakTypeOf[T]

    println(targetTpe.decls)
    val ns = namespace match {
      case Expr(Literal(Constant(x: String))) => x
      case _ =>
        c.abort(c.enclosingPosition, "Namespace must be a constant string literal")
        ""
    }

    val namespacedPkg = s"${versionPkg(c)}.$ns"
    val bleh = scala.util.Try {
      c.mirror.staticPackage(namespacedPkg)
    }
    if (bleh.isFailure) {
      return InitVersion1
    }
    println("[[[[[[[[[[[[[[[[[[[[[[[[[[[")
    val name = targetTpe.typeSymbol.asType.name.decodedName.toString
    val vCurrent =
      bleh.get.asModule.info.members.filter(_.isModule).filter(_.name == TermName(s"V1"))
    val vCurrentClass = vCurrent.head.info.members.filter(_.isClass).map(_.asClass.toType).head

    println("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    println(vCurrentClass.dealias)
    println("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB")
    println(vCurrentClass.etaExpand.typeSymbol.info.decls.sorted.filter(fields))
    val srcFields = targetTpe.typeSymbol.info.decls.sorted.filter(fields)
    val omg       = vCurrentClass.etaExpand.typeSymbol.info.decls.sorted.filter(fields)

    println(vCurrentClass)
    //println(vCurrentClass.asClass.toType)
    println("...............................")
    println(srcFields)
    println(omg)
    println(srcFields.map(_.typeSignature.finalResultType))
    println(omg.map(_.typeSignature.finalResultType))

    case class VersionCompare(name: c.universe.Name, typ: c.universe.Type)

    println(srcFields == omg)
    println(srcFields == omg)
    println(srcFields == omg)

    println(srcFields.map(_.name.decodedName))
    println(omg.map(_.name.decodedName))

    val srcCmp = srcFields.map(x =>  VersionCompare(x.name.decodedName, x.typeSignature.finalResultType))
    val verCmp = omg.map(x =>  VersionCompare(x.name.decodedName, x.typeSignature.finalResultType))
    println(srcCmp)
    println(verCmp)
    println(srcCmp == verCmp)
    println(srcCmp == verCmp)
    println(srcCmp == verCmp)
    println(srcCmp == verCmp)
    if(srcCmp != verCmp) UpdateVersion(1)
    else NoOp
  }

  def testLogic[T: c.WeakTypeTag](c: blackbox.Context)(
      namespace: c.Expr[String]): c.Expr[Action] = {
    import c.universe._
    val result = logic[T](c)(namespace) match {
      case NoOp             => q"hold.my.beer.macros.Internal.NoOp"
      case InitVersion1     => q"hold.my.beer.macros.Internal.InitVersion1"
      case UpdateVersion(x) => q"hold.my.beer.macros.Internal.UpdateVersion($x)"
    }
    c.Expr[Action](result)
  }

  //Rules:
  //@Version(N) and _VN exists, and all fields and types line up, and definitions for all previous versions exist => OK!
  //@Version(1) and no versions => Create _V1
  //@Version(N+1) and all versions 1 to N exist => Create _V(N+1)
  //@Version(N) and @editable and fields do not match: Overwrite _V(N+1)!
  def isBeer(c: blackbox.Context)(sym: c.Symbol): Boolean = {
    sym.isClass && !sym.isJava && sym.asType.info.baseClasses
      .contains(c.typeOf[scala.Product].typeSymbol)
  }

  def versions(c: blackbox.Context)(
      generatedPackage: c.universe.ModuleSymbol,
      srcSym: c.universe.ClassSymbol): List[(Int, String, c.universe.ClassSymbol)] = {
    generatedPackage.moduleClass.info.decls
      .filter(isBeer(c))
      .map { x =>
        val (src, version) = Codegen.extract(x.name.toString)
        (version, src, x.asClass)
      }
      .filter(_._2 == srcSym.name.toString)
      .toList
  }

  def generate(c: blackbox.Context)(pkg: c.Expr[String]): c.Expr[String] = {
    import c.universe._

    val targetPkg = pkg match {
      case Expr(Literal(Constant(x: String))) => x
      case _ =>
        c.abort(c.enclosingPosition, "Pkg must be a constant string literal")
        ""
    }

    val versioned = c.mirror.staticPackage(targetPkg)
    val beers = versioned.moduleClass.info.decls
      .filter(isBeer(c))
      .map(b => (Codegen.versionName(b.name.toString, 1), b.asClass))
      .toList
      .sortBy(_._1)
      .map(_._2)

    val generated: c.universe.ModuleSymbol = versioned.moduleClass.info.decls
      .filter(x => x.isPackage && x.fullName.endsWith("generated"))
      .toList match {
      case gen :: Nil => gen.asModule
      case _ =>
        c.abort(c.enclosingPosition,
                s"package $targetPkg.generated not found! please create first!")
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
  def test[T](namespace: String): Internal.Action = macro Internal.testLogic[T]
  def generate(pkg: String): String = macro Internal.generate
}
