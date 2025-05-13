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
        val mft = '{ Manifest.derived[ft] }
        '{ ($name, $mft) }
  }

  Expr.ofList(fieldExprs)
}

def derivedImpl[T: Type](using Quotes): Expr[Manifest[T]] = {
  import quotes.reflect._

  val tpe = TypeRepr.of[T]

  val ctExpr = Expr.summon[ClassTag[T]].getOrElse {
    report.error(s"Cannot summon ClassTag for ${Type.show[T]}")
    return '{ ??? }
  }

  val typeArgsExprs: List[Expr[Manifest[?]]] = tpe match
    case AppliedType(_, args) =>
      args.map {
        case arg =>
          arg.asType match
            case '[ta] => '{ Manifest.derived[ta] }
      }
    case _ => Nil

  val listExpr = Expr.ofList(typeArgsExprs)

  '{
    new Manifest[T]:
      def runtimeClass: Class[?] = $ctExpr.runtimeClass
      def typeArguments: List[Manifest[?]] = $listExpr
      def arrayManifest: Manifest[Array[T]] = Manifest.derived[Array[T]]
  }
}
