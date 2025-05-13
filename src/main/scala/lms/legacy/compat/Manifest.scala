package scala.lms

import scala.reflect.ClassTag
import scala.quoted.*

trait Manifest[T] {
  def classTag: ClassTag[T]
  def runtimeClass: Class[?]
  def typeArguments: List[Manifest[?]]
  def <:<[U](that: Manifest[U]): Boolean =
    that.runtimeClass.isAssignableFrom(this.runtimeClass)

  override def toString: String = {
    def simplify(name: String): String =
      name match {
        case "int" => "Int"
        case "long" => "Long"
        case "double" => "Double"
        case "float" => "Float"
        case "char" => "Char"
        case "byte" => "Byte"
        case "short" => "Short"
        case "boolean" => "Boolean"
        case "void" => "Unit"
        case fq if fq.startsWith("java.lang.") => fq.stripPrefix("java.lang.")
        case fq if fq.startsWith("scala.") => fq.stripPrefix("scala.")
        case fq if fq.endsWith("[]") => "Array[" + simplify(fq.stripSuffix("[]")) + "]"
        case other => other
      }

    val base = simplify(runtimeClass.getTypeName)
    if typeArguments.isEmpty then base
    else s"$base[${typeArguments.map(_.toString).mkString(", ")}]"
  }
}

private def arrayClassTag[T](using ct: ClassTag[T]): ClassTag[Array[T]] =
  summon[ClassTag[Array[T]]]

object Manifest {
  given manifestFromClassTag[T](using ct: ClassTag[T]): Manifest[T] with
    def classTag = ct
    def runtimeClass: Class[?] = ct.runtimeClass
    def typeArguments: List[Manifest[?]] = Nil

  def arrayManifest[T](m: Manifest[T]): Manifest[Array[T]] = {
    val ct: ClassTag[T] = m.classTag
    val arrayCt = arrayClassTag(using ct)
    manifestFromClassTag(using arrayCt)
  }

  inline def derived[T]: Manifest[T] = ${ derivedImpl[T] }

  def of[T](using ct: ClassTag[T]) = manifestFromClassTag[T]
}

case class RefinedManifest[T](ct: Manifest[T], fields: List[(String, Manifest[?])])

object RefinedManifest {
  implicit def materialize[T](implicit ct: ClassTag[T]): RefinedManifest[T] =
    RefinedManifest(Manifest.manifestFromClassTag(using ct), structFields[T])

  inline def structFields[T]: List[(String, Manifest[?])] = ${ structFieldsImpl[T] }
}

implicit val nullManifest: Manifest[Null] = Manifest.of(using ClassTag(classOf[Null]))
implicit val anyManifest: Manifest[Any] = Manifest.of(using ClassTag(classOf[Any]))
