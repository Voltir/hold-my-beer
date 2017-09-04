package hold.my.beer.macros

import hold.my.beer.codegen.Codegen
import better.files._

import scala.language.experimental.macros
import scala.reflect.macros._
import scala.reflect.internal.Symbols
object Internal {

  sealed trait Action
  case object NoOp                 extends Action
  case object InitVersion1         extends Action
  case class UpdateVersion(v: Int) extends Action

  sealed class Generated {}

  private case class VersionInfo(src: File,
                                 target: String,
                                 srcPkg: List[String],
                                 namespace: List[String],
                                 currentVersion: Int) {
    def generatedPkg: String =
      s"${srcPkg.mkString(".")}.${Codegen.VERSION_PKG}.${namespace.mkString(".")}"
    def generatedFile: File =
      src.parent / Codegen.VERSION_PKG / namespace.mkString("/") / "Generated.scala"
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

  private def info[T: c.WeakTypeTag](c: blackbox.Context)(
      namespace: c.Expr[String]): VersionInfo = {
    import c.universe._
    val target = c.weakTypeOf[T]
    val name   = target.typeSymbol.name.decodedName.toString
    val src    = c.enclosingPosition.source.path.toFile
    val ns = namespace match {
      case Expr(Literal(Constant(x: String))) => x
      case _ =>
        c.abort(c.enclosingPosition, "Namespace must be a constant string literal")
        ""
    }
    VersionInfo(src, name, getPkg(c), ns.split('.').toList, 1)
  }

  def companionTermFilter[T: c.WeakTypeTag](c: blackbox.Context): List[String] = {
    import c.universe._
    val target = c.weakTypeOf[T]
    val companionFields = target.companion.typeSymbol.asClass.info.decls.filter(x =>
      x.isPublic && !x.isSynthetic && !x.isConstructor)

    val filtered = companionFields.map { f =>
      (f, scala.util.Try(f.typeSignature.finalResultType))
    }.collect {
      //Remove cyclic reference terms (this macro without a type annotation)
      case (f, scala.util.Failure(_ : scala.reflect.internal.Symbols#CyclicReference)) =>
        f.asTerm.name.decodedName.toString
      case (f, scala.util.Success(t)) if t =:= typeOf[Generated] =>
        f.asTerm.name.decodedName.toString
    }.toList

    filtered
  }

  def logic[T: c.WeakTypeTag](c: blackbox.Context)(info: VersionInfo): Action = {
    import c.universe._

    def fields(x: c.universe.Symbol): Boolean = {
      x.isPublic && !x.isSynthetic && !x.isConstructor
    }

    val targetTpe = c.weakTypeOf[T]

    val versionsPkg = scala.util.Try {
      c.mirror.staticPackage(info.generatedPkg)
    }

    if (versionsPkg.isFailure) {
      return InitVersion1
    }

    //todo check name change
    val name = targetTpe.typeSymbol.asType.name.decodedName.toString

    val vCurrent =
      versionsPkg.get.asModule.info.members.filter(_.isModule).filter(_.name == TermName(s"V1"))
    val vCurrentClass = vCurrent.head.info.members.filter(_.isClass).map(_.asClass.toType).head
    val srcFields     = targetTpe.typeSymbol.info.decls.sorted.filter(fields)
    val versionFields = vCurrentClass.etaExpand.typeSymbol.info.decls.sorted.filter(fields)

    case class FieldCompare(name: c.universe.Name, typ: c.universe.Type)

    val srcCmp =
      srcFields.map(x => FieldCompare(x.name.decodedName, x.typeSignature.finalResultType))
    val verCmp =
      versionFields.map(x => FieldCompare(x.name.decodedName, x.typeSignature.finalResultType))

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

  def generate[T: c.WeakTypeTag](c: blackbox.Context)(namespace: c.Expr[String]): c.Expr[Generated] = {
    import c.universe._
    val i = info[T](c)(namespace)
    logic[T](c)(i) match {
      case NoOp         => ()
      case InitVersion1 =>
        Codegen.initialize(i.src.pathAsString, s"${i.srcPkg.mkString(".")}", i.namespace).foreach {
          err =>
            c.abort(c.enclosingPosition, s"Codegen initalized failed!: $err")
        }
        val filter = companionTermFilter[T](c)
        Codegen.codegen(i.src, i.generatedFile, i.target, 1, filter).foreach { err =>
          c.abort(c.enclosingPosition, s"Codegen init version 1 failed!: $err")
        }
        ()
      case UpdateVersion(v) =>
        val filter = companionTermFilter[T](c)
        Codegen.codegen(i.src, i.generatedFile, i.target, v, filter).foreach { err =>
          c.abort(c.enclosingPosition, s"Codegen failed!: $err")
        }
        ()
    }
    c.Expr[Generated](q"new _root_.hold.my.beer.macros.Internal.Generated")
  }
}

object Todo {
  def test[T](namespace: String): Internal.Action = macro Internal.testLogic[T]
  def generated[T](namespace: String): Internal.Generated = macro Internal.generate[T]
}
