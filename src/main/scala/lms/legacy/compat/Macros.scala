package scala.lms

import scala.reflect.ClassTag
import scala.quoted.*

def structFieldsImpl[T: Type](using quotes: Quotes): Expr[List[(String, Manifest[?])]] = {
  import quotes.reflect._

  val tpe = TypeRepr.of[T]
  val sym = tpe.typeSymbol

  if (!sym.isClassDef) {
    return '{ Nil }
  }

  val fieldExprs: List[Expr[(String, Manifest[?])]] = sym.fieldMembers.map { sym =>
    val name = Expr(sym.name)
    val fieldType = tpe.memberType(sym)
    fieldType.asType match
      case '[ft] =>
        val mft = '{ Manifest.of[ft] }
        '{ ($name, $mft) }
  }

  Expr.ofList(fieldExprs)
}

def derivedImpl[T: Type](using Quotes): Expr[Manifest[T]] = {
  import quotes.reflect._

  val tpe = TypeRepr.of[T]

  if (tpe.show.contains("Var")) {
    report.error("tpe = " + tpe + "\n" + tpe.show)
  }

  val ctExpr = Expr.summon[ClassTag[T]] match {
    case Some(exp) => exp
    case None => {
      report.error(s"Cannot summon ClassTag for ${Type.show[T]}")
      return '{ ??? }
    }
  }

  val typeArgsExprs: List[Expr[Manifest[?]]] = tpe match
    case AppliedType(_, args) =>
      args.map {
        case arg =>
          arg.asType match
            case '[ta] => '{ Manifest.of[ta] }
      }
    case _ => Nil

  val listExpr = Expr.ofList(typeArgsExprs)

  '{ ObjManifest[T]($ctExpr, $listExpr) }
}
