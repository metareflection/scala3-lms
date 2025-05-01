package scala.lms

import scala.reflect.ClassTag

case class RefinedManifest[T](ct: ClassTag[T], fields: List[(String, Manifest[?])])
object RefinedManifest {
  implicit def materialize[T](implicit ct: ClassTag[T]): RefinedManifest[T] =
    RefinedManifest(ct, structFields[T])
}

implicit val nullManifest: Manifest[Null] = ClassTag(classOf[Null])
implicit val anyManifest: Manifest[Any] = ClassTag(classOf[Any])

def manifestOfOptManifest(m: OptManifest[?]): Manifest[?] = m match {
  case m: scala.reflect.Manifest[?] => ClassTag(m.runtimeClass)
  case NoManifest => anyManifest
}
