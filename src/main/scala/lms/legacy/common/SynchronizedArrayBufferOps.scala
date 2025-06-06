package scala.lms
package common

import java.io.PrintWriter
import scala.lms.internal.GenericNestedCodegen
import collection.mutable.ArrayBuffer

trait SynchronizedArrayBufferOps extends ArrayBufferOps {

/*
  object SynchronizedArrayBuffer {
    def apply[A:Typ](xs: Rep[A]*)(implicit pos: SourceContext) = arraybuffer_new(xs)
  }
*/

}

trait SynchronizedArrayBufferOpsExp extends SynchronizedArrayBufferOps with ArrayBufferOpsExp {
  case class SyncArrayBufferNew[A:Typ](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    def mA = typ[A]
  }

  // all array buffers are synchronized (nackward compat). TODO: separate constructor

  override def arraybuffer_new[A:Typ](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(SyncArrayBufferNew(xs))
}

trait BaseGenSynchronizedArrayBufferOps extends BaseGenArrayBufferOps {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._
}

trait ScalaGenSynchronizedArrayBufferOps extends BaseGenSynchronizedArrayBufferOps with ScalaGenArrayBufferOps {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@SyncArrayBufferNew(xs) => emitValDef(sym, src"(new scala.collection.mutable.ArrayBuffer[${a.mA}] with scala.collection.mutable.SynchronizedBuffer[${a.mA}]) ++= List(${(xs map {quote}).mkString(",")})")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSynchronizedArrayBufferOps extends BaseGenSynchronizedArrayBufferOps with CLikeGenArrayBufferOps {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenSynchronizedArrayBufferOps extends CudaGenEffect with CLikeGenSynchronizedArrayBufferOps
trait OpenCLGenSynchronizedArrayBufferOps extends OpenCLGenEffect with CLikeGenSynchronizedArrayBufferOps
trait CGenSynchronizedArrayBufferOps extends CGenEffect with CLikeGenSynchronizedArrayBufferOps

