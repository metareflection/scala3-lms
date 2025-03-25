package scala.lms

import scala.reflect.ClassTag
import scala.quoted.*

type RefinedManifest[T] = ClassTag[T]

object RefinedManifest {
  implicit def materialize[T](implicit ct: ClassTag[T]): RefinedManifest[T] = ct
}

inline def printTypeInfo[T]: Unit = ${ printTypeInfoImpl[T] }

def printTypeInfoImpl[T: Type](using Quotes): Expr[Unit] = {
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  println(s"Staged type: ${tpe.show}")
  '{ () }
}
