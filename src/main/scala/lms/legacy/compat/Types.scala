package scala.lms

import scala.reflect.ClassTag

type Manifest[T] = ClassTag[T]
