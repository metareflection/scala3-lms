package scala.lms

import scala.reflect.ClassTag
import scala.quoted.*

trait Manifest[T] {
  def typeArguments: List[Manifest[?]]
  def runtimeClass: Class[?]
  def arrayManifest: Manifest[Array[T]]

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

case class ObjManifest[T](classTag: ClassTag[T], typeArguments: List[Manifest[?]])
  extends Manifest[T]
{
  override def runtimeClass = classTag.runtimeClass

  override def arrayManifest: Manifest[Array[T]] = {
    val ct = classTag
    val arrayCt = arrayClassTag(using ct)
    ObjManifest(arrayCt, List(this))
  }
}

case class NullManifest() extends Manifest[Null] {
  override def runtimeClass = classOf[Null]

  override def typeArguments = List()

  override def arrayManifest: Manifest[Array[Null]] =
    ObjManifest(ClassTag(classOf[Array[Any]]), List(this))
}

private def arrayClassTag[T](using ct: ClassTag[T]): ClassTag[Array[T]] =
  summon[ClassTag[Array[T]]]

object Manifest {
  inline def of[T]: Manifest[T] = ${ derivedImpl[T] }
}

implicit def manifestFromClassTag[T: ClassTag]: Manifest[T] = Manifest.of[T]

// TODO: This should extend `Manifest[T]` instead of including one.
case class RefinedManifest[T](ct: Manifest[T], fields: List[(String, Manifest[?])])

object RefinedManifest {
  implicit def materialize[T](implicit ct: ClassTag[T]): RefinedManifest[T] =
    RefinedManifest(null, structFields[T])

  inline def structFields[T]: List[(String, Manifest[?])] = ${ structFieldsImpl[T] }
}

implicit val nullManifest: Manifest[Null] = NullManifest()
implicit val anyManifest: Manifest[Any] = Manifest.of[Any]
