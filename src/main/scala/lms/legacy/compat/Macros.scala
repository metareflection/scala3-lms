package scala.lms

import scala.reflect.ClassTag
import scala.quoted.*

inline def structFields[T]: List[(String, Manifest[?])] = ${ structFieldsImpl[T] }

def structFieldsImpl[T: Type](using Quotes): Expr[List[(String, ClassTag[?])]] = {
  import quotes.reflect.*

  val tpe = TypeRepr.of[T]
  val sym = tpe.typeSymbol

  if (!sym.isClassDef) {
    return '{ Nil }
  }

  val fieldExprs: List[Expr[(String, ClassTag[?])]] = sym.caseFields.map { fieldSym =>
    val nameExpr = Expr(fieldSym.name)

    val fieldTypeRepr = tpe.memberType(fieldSym)
    val fieldType = fieldTypeRepr.asType match {
      case '[ft] =>
        '{ ClassTag[ft] }.asExprOf[ClassTag[?]]
    }

    '{ ($nameExpr, $fieldType) }
  }

  Expr.ofList(fieldExprs)
}
