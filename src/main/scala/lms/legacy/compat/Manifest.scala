package scala.lms

import scala.reflect.ClassTag
import scala.quoted.*

type Manifest[T] = ClassTag[T]
case class RefinedManifest[T](ct: ClassTag[T], fields: List[(String, Manifest[?])])

inline def structFields[T]: List[(String, Manifest[?])] = ${ structFieldsImpl[T] }

def structFieldsImpl[T: Type](using Quotes): Expr[List[(String, ClassTag[?])]] = {
  import quotes.reflect.*

  val tpe = TypeRepr.of[T]
  val sym = tpe.typeSymbol

  if (!sym.isClassDef) {
    quotes.reflect.report.error(s"Type ${sym.name} is not a class/struct")
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
object RefinedManifest {
  implicit def materialize[T](implicit ct: ClassTag[T]): RefinedManifest[T] =
    RefinedManifest(ct, structFields[T])
}

inline def printTypeInfo[T]: Unit = ${ printTypeInfoImpl[T] }

def printTypeInfoImpl[T: Type](using Quotes): Expr[Unit] = {
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  println(s"Staged type: ${tpe.show}")
  '{ () }
}

implicit val nullManifest: Manifest[Null] = ClassTag(classOf[Null])
