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

  private case class VersionInfo(src: File,
                                 target: String,
                                 srcPkg: List[String],
                                 namespace: List[String],
                                 currentVersion: Int) {
    def generatedPkg: String = s"${srcPkg.mkString(".")}.${Codegen.VERSION_PKG}.${namespace.mkString(".")}"
    def generatedFile: File = src.parent / Codegen.VERSION_PKG / namespace.mkString("/") / "Generated.scala"
  }

  def getPkg(c: blackbox.Context): List[String] = {
    import c.universe._
    var current = c.internal.enclosingOwner
    var pkg     = List.empty[String]
    while (current != NoSymbol && current.toString != "package <root>") {
      if (current.isPackage) {
        pkg = current.name.decodedName.toString :: pkg
      }
      current = current.owner
    }
    pkg
  }

  private def info[T: c.WeakTypeTag](c: blackbox.Context)(namespace: c.Expr[String]): VersionInfo = {
    import c.universe._
    println(c.enclosingPosition.source.path)
    println(c.enclosingPosition.source.path)
    println(c.enclosingPosition.source.path)

    val target = c.weakTypeOf[T]
    val name = target.typeSymbol.name.decodedName.toString
    val src = c.enclosingPosition.source.path.toFile
    val ns = namespace match {
      case Expr(Literal(Constant(x: String))) => x
      case _ =>
        c.abort(c.enclosingPosition, "Namespace must be a constant string literal")
        ""
    }
    VersionInfo(src, name, getPkg(c), ns.split('.').toList, 1)
  }

  def logic[T: c.WeakTypeTag](c: blackbox.Context)(info: VersionInfo): Action = {
    import c.universe._

    def fields(x: c.universe.Symbol): Boolean = {
      x.isPublic && !x.isSynthetic && !x.isConstructor
    }

    val targetTpe = c.weakTypeOf[T]

    val bleh = scala.util.Try {
      c.mirror.staticPackage(info.generatedPkg)
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

    val srcCmp =
      srcFields.map(x => VersionCompare(x.name.decodedName, x.typeSignature.finalResultType))
    val verCmp = omg.map(x => VersionCompare(x.name.decodedName, x.typeSignature.finalResultType))
    println(srcCmp)
    println(verCmp)
    println(srcCmp == verCmp)
    println(srcCmp == verCmp)
    println(srcCmp == verCmp)
    println(srcCmp == verCmp)
    if (srcCmp != verCmp) UpdateVersion(1)
    else NoOp
  }

  def testLogic[T: c.WeakTypeTag](c: blackbox.Context)(
      namespace: c.Expr[String]): c.Expr[Action] = {
    import c.universe._
    val targetInfo = info[T](c)(namespace)
    val result = logic[T](c)(targetInfo) match {
      case NoOp             => q"_root_.hold.my.beer.macros.Internal.NoOp"
      case InitVersion1     => q"_root_.hold.my.beer.macros.Internal.InitVersion1"
      case UpdateVersion(x) => q"_root_.hold.my.beer.macros.Internal.UpdateVersion($x)"
    }
    c.Expr[Action](result)
  }

  def generate2[T: c.WeakTypeTag](c: blackbox.Context)(namespace: c.Expr[String]): c.Expr[Int] = {
    import c.universe._
    val i = info[T](c)(namespace)
    logic[T](c)(i) match {
      case NoOp         => ()
      case InitVersion1 =>
        //    hold.my.beer.codegen.Codegen.initialize(SampleV1Modified.tmpLoc,SampleV1Modified.tmpPkg,List("samplev1modified"))
        //    val a = sample.SampleV1Modified.tmpLoc.toFile
        //    val b = sample.SampleV1Modified.tmpLoc.toFile.parent / "versions" / "samplev1modified" / "Generated.scala"
        //    hold.my.beer.codegen.Codegen.codegen(a,b,"SampleV1Modified",1)
        Codegen.initialize(i.src.pathAsString, s"${i.srcPkg.mkString(".")}", i.namespace).foreach { err =>
          c.abort(c.enclosingPosition, s"Codegen initalized failed!: $err")
        }
        Codegen.codegen(i.src, i.generatedFile, i.target, 1).foreach { err =>
          c.abort(c.enclosingPosition, s"Codegen init version 1 failed!: $err")
        }
        ()
      case UpdateVersion(v) =>
        Codegen.codegen(i.src, i.generatedFile, i.target, v).foreach { err =>
          c.abort(c.enclosingPosition, s"Codegen failed!: $err")
        }
        ()
    }
    c.Expr[Int](q"42")
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
  def generated[T](namespace: String): Int = macro Internal.generate2[T]
}
