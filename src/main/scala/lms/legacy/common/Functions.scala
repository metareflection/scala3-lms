package scala.lms
package common

import java.io.PrintWriter

import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.lms.util.ClosureCompare

trait Functions extends Base {

  implicit def funTyp[A:Typ,B:Typ]: Typ[A => B]

  def doLambda[A:Typ,B:Typ](fun: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[A => B]
  implicit def fun[A:Typ,B:Typ](f: Rep[A] => Rep[B]): Rep[A=>B] = doLambda(f)

  implicit def toLambdaOps[A:Typ,B:Typ](fun: Rep[A => B]): LambdaOps[A, B] = new LambdaOps(fun)

  class LambdaOps[A:Typ,B:Typ](f: Rep[A => B]) {
    def apply(x: Rep[A])(implicit pos: SourceContext): Rep[B] = doApply(f,x)
  }

  def doApply[A:Typ,B:Typ](fun: Rep[A => B], arg: Rep[A])(implicit pos: SourceContext): Rep[B]
}

trait TupledFunctions extends Functions with TupleOps {
  implicit def fun[B:Typ](f: () => Rep[B]): Rep[Unit=>B] =
    fun((t: Rep[Unit]) => f())
  implicit def fun[A1:Typ,A2:Typ,B:Typ](f: (Rep[A1], Rep[A2]) => Rep[B]): Rep[((A1,A2))=>B] =
    fun((t: Rep[(A1,A2)]) => f(tuple2_get1(t), tuple2_get2(t)))
  implicit def fun[A1:Typ,A2:Typ,A3:Typ,B:Typ](f: (Rep[A1], Rep[A2], Rep[A3]) => Rep[B]): Rep[((A1,A2,A3))=>B] =
    fun((t: Rep[(A1,A2,A3)]) => f(tuple3_get1(t), tuple3_get2(t), tuple3_get3(t)))
  implicit def fun[A1:Typ,A2:Typ,A3:Typ,A4:Typ,B:Typ](f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4]) => Rep[B]): Rep[((A1,A2,A3,A4))=>B] =
    fun((t: Rep[(A1,A2,A3,A4)]) => f(tuple4_get1(t), tuple4_get2(t), tuple4_get3(t), tuple4_get4(t)))
  implicit def fun[A1:Typ,A2:Typ,A3:Typ,A4:Typ,A5:Typ,B:Typ](f: (Rep[A1], Rep[A2], Rep[A3], Rep[A4], Rep[A5]) => Rep[B]): Rep[((A1,A2,A3,A4,A5))=>B] =
    fun((t: Rep[(A1,A2,A3,A4,A5)]) => f(tuple5_get1(t), tuple5_get2(t), tuple5_get3(t), tuple5_get4(t), tuple5_get5(t)))

  class LambdaOps2[A1:Typ,A2:Typ,B:Typ](f: Rep[((A1,A2)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2]) = doApply(f,(x1, x2))
    def apply(x: Rep[(A1,A2)]): Rep[B] = doApply(f,x)
  }
  class LambdaOps3[A1:Typ,A2:Typ,A3:Typ,B:Typ](f: Rep[((A1,A2,A3)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2], x3: Rep[A3]) = doApply(f,(x1, x2, x3))
    def apply(x: Rep[(A1,A2,A3)]): Rep[B] = doApply(f,x)
  }
  class LambdaOps4[A1:Typ,A2:Typ,A3:Typ,A4:Typ,B:Typ](f: Rep[((A1,A2,A3,A4)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4]) = doApply(f,(x1, x2, x3, x4))
    def apply(x: Rep[(A1,A2,A3,A4)]): Rep[B] = doApply(f,x)
  }
  class LambdaOps5[A1:Typ,A2:Typ,A3:Typ,A4:Typ,A5:Typ,B:Typ](f: Rep[((A1,A2,A3,A4,A5)) => B]) {
    def apply(x1: Rep[A1], x2: Rep[A2], x3: Rep[A3], x4: Rep[A4], x5: Rep[A5]) = doApply(f,(x1, x2, x3, x4, x5))
    def apply(x: Rep[(A1,A2,A3,A4,A5)]): Rep[B] = doApply(f,x)
  }
  //implicit def toLambdaOpsAny[B:Typ](fun: Rep[Any => B]) =
  //  toLambdaOps(fun)
  implicit def toLambdaOps2[A1:Typ,A2:Typ,B:Typ](fun: Rep[((A1,A2)) => B]): LambdaOps2[A1,A2,B] =
    new LambdaOps2(fun)
  implicit def toLambdaOps3[A1:Typ,A2:Typ,A3:Typ,B:Typ](fun: Rep[((A1,A2,A3)) => B]): LambdaOps3[A1,A2,A3,B] =
    new LambdaOps3(fun)
  implicit def toLambdaOps4[A1:Typ,A2:Typ,A3:Typ,A4:Typ,B:Typ](fun: Rep[((A1,A2,A3,A4)) => B]): LambdaOps4[A1,A2,A3,A4,B] =
    new LambdaOps4(fun)
  implicit def toLambdaOps5[A1:Typ,A2:Typ,A3:Typ,A4:Typ,A5:Typ,B:Typ](fun: Rep[((A1,A2,A3,A4,A5)) => B]): LambdaOps5[A1,A2,A3,A4,A5,B] =
    new LambdaOps5(fun)
}

trait FunctionsExp extends Functions with EffectExp {
  implicit def funTyp[A:Typ,B:Typ]: Typ[A => B] = {
    implicit val ManifestTyp(mA: Manifest[A]) = typ[A]
    implicit val ManifestTyp(mB: Manifest[B]) = typ[B]
    manifestTyp
  }

  case class Lambda[A:Typ,B:Typ](f: Exp[A] => Exp[B], x: Exp[A], y: Block[B]) extends Def[A => B] { def mA = typ[A]; def mB = typ[B] }
  case class Apply[A:Typ,B:Typ](f: Exp[A => B], arg: Exp[A]) extends Def[B] { def mA = typ[A]; def mB = typ[B] }

  // unboxedFresh and unbox are hooks that can be overridden to
  // implement multiple-arity functions with tuples. These two methods
  // should be overridden consistently. unboxedFresh is used when
  // creating an abstraction, and unbox when applying it. See
  // TupledFunctionsExp for an example.

  def unboxedFresh[A:Typ] : Exp[A] = fresh[A]
  def unbox[A:Typ](x : Exp[A])(implicit pos: SourceContext) : Exp[A] = x

  def doLambdaDef[A:Typ,B:Typ](f: Exp[A] => Exp[B]) : Def[A => B] = {
    val x = unboxedFresh[A]
    val y = reifyEffects(f(x)) // unfold completely at the definition site.

    Lambda(f, x, y)
  }

  override def doLambda[A:Typ,B:Typ](f: Exp[A] => Exp[B])(implicit pos: SourceContext): Exp[A => B] =
    doLambdaDef(f)

  override def doApply[A:Typ,B:Typ](f: Exp[A => B], x: Exp[A])(implicit pos: SourceContext): Exp[B] = {
    val x1 = unbox(x)
    f match {
      case Def(Lambda(_,_,y)) =>
        // if function result is known to be pure, so is application
        // TODO: what about
        val ye = summarizeEffects(y)
        reflectEffect(Apply(f, x1), ye)
      case _ => // unknown function, assume it is effectful TODO: global vs simple?
        reflectEffect(Apply(f, x1))
    }
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Lambda(g,x:Exp[Any],y:Block[b]) => toAtom(Lambda(f(g),f(x),f(y))(using e.mA,e.mB))(using mtyp1[A],pos)
    case e@Apply(g,arg) => doApply(f(g), f(arg))(using e.mA,mtype(e.mB),pos)
    case Reflect(e@Apply(g,arg), u, es) => reflectMirrored(Reflect(Apply(f(g),f(arg))(using e.mA,mtype(e.mB)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case e@Lambda(g, x, y) => Lambda(f(g), f(x), f(y))(using e.mA,e.mB)
    case e@Apply(g, arg) => Apply(f(g), f(arg))(using e.mA, mtype(e.mB))
    case _ => super.mirrorDef(e, f)
  }).asInstanceOf[Def[A]]

  override def syms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(x) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }

// TODO: right now were trying to hoist as much as we can out of functions.
// That might not always be appropriate. A promising strategy would be to have
// explicit 'hot' and 'cold' functions.

/*
  override def hotSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, x, y) => syms(y)
    case _ => super.hotSyms(e)
  }
*/

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Lambda(f, x, y) => freqHot(y)
    case _ => super.symsFreq(e)
  }

}

trait TupledFunctionsExp extends TupledFunctions with FunctionsExp with TupleOpsExp {
  // used to represent unboxed tuples by a list of vars
  // T will be a tuple of a specified arity
  case class UnboxedTuple[T: Typ](val vars: List[Exp[Any]]) extends Exp[T]

  private def tupledTyp[T](m: Typ[T]): Boolean = m.runtimeClass.getName `startsWith` "scala.Tuple"
  private def tupledTypOf[T](m: Typ[T], arity: Int): Boolean = m.runtimeClass.getName == "scala.Tuple" + arity

  override def unboxedFresh[A:Typ] : Exp[A] = {
    val mA = typ[A]
    if (mA == typ[Unit] || tupledTyp(mA))
      UnboxedTuple[A](mA.typeArguments.map(fresh(using _)))
    else fresh[A]
  }

  override def unbox[A:Typ](x : Exp[A])(implicit pos: SourceContext) : Exp[A] = {
    val mA = typ[A]
    x match {
      case _ : UnboxedTuple[A] => x
      case _ if mA == typ[Unit] =>
        UnboxedTuple[A](List())
      case _ if tupledTypOf(mA, 2) =>
        x match { case t : Rep[(a1,a2)] =>
          UnboxedTuple[A](List(
            tuple2_get1(t)(using mA.typeArguments(0).asInstanceOf[Typ[a1]], pos),
            tuple2_get2(t)(using mA.typeArguments(1).asInstanceOf[Typ[a2]], pos)))
        }
      case _ if tupledTypOf(mA, 3) =>
        x match { case t : Rep[(a1,a2,a3)] =>
          UnboxedTuple[A](List(
            tuple3_get1(t)(using mA.typeArguments(0).asInstanceOf[Typ[a1]], pos),
            tuple3_get2(t)(using mA.typeArguments(1).asInstanceOf[Typ[a2]], pos),
            tuple3_get3(t)(using mA.typeArguments(2).asInstanceOf[Typ[a3]], pos)))
        }
      case _ if tupledTypOf(mA, 4) =>
        x match { case t : Rep[(a1,a2,a3,a4)] =>
          UnboxedTuple[A](List(
            tuple4_get1(t)(using mA.typeArguments(0).asInstanceOf[Typ[a1]], pos),
            tuple4_get2(t)(using mA.typeArguments(1).asInstanceOf[Typ[a2]], pos),
            tuple4_get3(t)(using mA.typeArguments(2).asInstanceOf[Typ[a3]], pos),
            tuple4_get4(t)(using mA.typeArguments(3).asInstanceOf[Typ[a4]], pos)))
        }
      case _ if tupledTypOf(mA, 5) =>
        x match { case t : Rep[(a1,a2,a3,a4,a5)] =>
          UnboxedTuple[A](List(
            tuple5_get1(t)(using mA.typeArguments(0).asInstanceOf[Typ[a1]], pos),
            tuple5_get2(t)(using mA.typeArguments(1).asInstanceOf[Typ[a2]], pos),
            tuple5_get3(t)(using mA.typeArguments(2).asInstanceOf[Typ[a3]], pos),
            tuple5_get4(t)(using mA.typeArguments(3).asInstanceOf[Typ[a4]], pos),
            tuple5_get5(t)(using mA.typeArguments(4).asInstanceOf[Typ[a5]], pos)))
        }
      case _ => x
    }
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Lambda(f, UnboxedTuple(xs), y) => xs.flatMap(syms) ::: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Lambda(g,UnboxedTuple(xs),y:Block[b]) => toAtom(Lambda(f(g),UnboxedTuple(f(xs))(using e.mA),f(y))(using e.mA,e.mB))(using mtyp1[A],implicitly[SourceContext])
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait FunctionsRecursiveExp extends FunctionsExp with ClosureCompare {
  var funTable: List[(Sym[_], Any)] = List()
  override def doLambda[A:Typ,B:Typ](f: Exp[A] => Exp[B])(implicit pos: SourceContext): Exp[A => B] = {
    val can = canonicalize(f)
    funTable.find(_._2 == can) match {
      case Some((funSym, _)) =>
        funSym.asInstanceOf[Exp[A=>B]]
      case _ =>
        val funSym = fresh[A=>B]
        funTable = (funSym,can)::funTable
        createDefinition(funSym, doLambdaDef(f))
        funSym
    }
  }

}

trait TupledFunctionsRecursiveExp extends FunctionsRecursiveExp with TupledFunctionsExp

trait GenericGenUnboxedTupleAccess extends GenericNestedCodegen {
  val IR: TupledFunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FieldApply(UnboxedTuple(vars), "_1") => emitValDef(sym, quote(vars(0)))
    case FieldApply(UnboxedTuple(vars), "_2") => emitValDef(sym, quote(vars(1)))
    case FieldApply(UnboxedTuple(vars), "_3") => emitValDef(sym, quote(vars(2)))
    case FieldApply(UnboxedTuple(vars), "_4") => emitValDef(sym, quote(vars(3)))
    case FieldApply(UnboxedTuple(vars), "_5") => emitValDef(sym, quote(vars(4)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait BaseGenFunctions extends GenericNestedCodegen {
  val IR: FunctionsExp
  import IR._

}

trait ScalaGenFunctions extends ScalaGenEffect with BaseGenFunctions {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      emitValDef(sym, "{" + quote(x) + ": (" + remap(x.tp) + ") => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + remap(y.tp))
      stream.println("}")

    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenTupledFunctions extends ScalaGenFunctions with GenericGenUnboxedTupleAccess {
  val IR: TupledFunctionsExp
  import IR._

  override def quote(x: Exp[Any]) : String = x match {
    case UnboxedTuple(t) => t.map(quote).mkString("((", ",", "))")
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, UnboxedTuple(xs), y) =>
      emitValDef(sym, "{" + xs.map(s=>quote(s)+":"+remap(s.tp)).mkString("(",",",")") + " => ")
      emitBlock(y)
      stream.println(quote(getBlockResult(y)) + ": " + remap(y.tp))
      stream.println("}")

    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, quote(fun) + args.map(quote).mkString("(", ",", ")"))

    case _ => super.emitNode(sym,rhs)
  }
  
  def unwrapTupleStr[A](m: Typ[A]): String = {
    val s = m.toString
    if (s.startsWith("scala.Tuple")) s.slice(s.indexOf("[")+1, s.length-1).filter(c => c != ' ')
    else remap(m)
  } 

  def topLevelTupleSize[A](m: Typ[A]): Int = {
    val s = m.toString
    val tupleStr = "scala.Tuple"
    if (s.startsWith(tupleStr)) {
      s.slice(s.indexOf(tupleStr)+tupleStr.size, s.indexOf("[")).toInt
    }
    else 1
  }
  
  override def remap[A](m: Typ[A]): String = m.toString match {    
    case f if f.startsWith("scala.Function") =>
      val targs = m.typeArguments.dropRight(1)
      val res = remap(m.typeArguments.last)
      val targsUnboxed = targs.map(t => unwrapTupleStr(t))
      val size = targs.map(s => topLevelTupleSize(s)).foldLeft(0)(_ + _)
      val sep = if (targsUnboxed.length > 0) "," else ""
      "scala.Function" + size + "[" + targsUnboxed.mkString(",") + sep + res + "]"

    case _ => super.remap(m)
  }
}

trait CudaGenFunctions extends CudaGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        throw new GenerationFailedException("CudaGenFunctions: Lambda is not supported yet")
        // The version for inlined device function
        //stream.println(addTab() + "%s %s = %s;".format(remap(x.tp), quote(x), quote(sym)+"_1"))
        //emitBlock(y)
        //stream.println(addTab() + "%s %s = %s;".format(remap(y.tp), quote(sym), quote(getBlockResult(y))))

        // The version for separate device function
        /*
        //TODO: If function parameter was originally tuple, then each element should be renamed?
        val freeVars = buildScheduleForResult(y).filter(scope.contains(_)).map(_.sym)
        stream.println("__device__ %s %s(%s %s) {".format(e.mB, quote(sym), e.mA, quote(x)))
        emitBlock(y)
        stream.println("%s %s = %s;".format(e.mB, quote(sym), quote(getBlockResult(y))))
        stream.println("return %s;".format(quote(getBlockResult(y))))
        stream.println("}")
        */
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait OpenCLGenFunctions extends OpenCLGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        throw new GenerationFailedException("OpenCLGenFunctions: Lambda is not supported yet")
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CGenFunctions extends CGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  // Case for functions with a single argument (therefore, not tupled)
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      val retType = remap(getBlockResult(y).tp)
      val retTp = if (cppExplicitFunRet == "true") "function<"+retType+"("+remap(x.tp)+")>" else "auto"
      stream.println(retTp+" "+quote(sym)+
        " = [&]("+remap(x.tp)+" "+quote(x)+") {")
      emitBlock(y)
      val z = getBlockResult(y)
      if (retType != "void")
        stream.println("return " + quote(z) + ";")
      stream.println("};")
    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case _ => super.emitNode(sym, rhs)
  }
  
}

trait CGenTupledFunctions extends CGenFunctions with GenericGenUnboxedTupleAccess {
  val IR: TupledFunctionsExp
  import IR._
  
  /*override def quote(x: Exp[Any]) : String = x match {
    case UnboxedTuple(t) => t.map(quote).mkString("((", ",", "))")
    case _ => super.quote(x)
  }*/


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, UnboxedTuple(xs), y) =>
      val retType = remap(getBlockResult(y).tp)
      val retTp = if (cppExplicitFunRet == "true") "function<"+retType+"("+xs.map(s=>remap(s.tp)).mkString(",")+")>" else "auto"
      stream.println(retTp+" "+quote(sym)+
        " = [&]("+xs.map(s=>remap(s.tp)+" "+quote(s)).mkString(",")+") {")
      emitBlock(y)
      val z = getBlockResult(y)
      if (retType != "void")
        stream.println("return " + quote(z) + ";")
      stream.println("};")
    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, quote(fun) + args.map(quote).mkString("(", ",", ")"))
    case _ => super.emitNode(sym,rhs)
  }

  /*def unwrapTupleStr(s: String): Array[String] = {
    if (s.startsWith("scala.Tuple")) s.slice(s.indexOf("[")+1,s.length-1).filter(c => c != ' ').split(",")
    else Array(s)
  }*/

  /*override def remap[A](m: Typ[A]): String = m.toString match {
    case f if f.startsWith("scala.Function") =>
      val targs = m.typeArguments.dropRight(1)
      val res = remap(m.typeArguments.last)
      val targsUnboxed = targs.flatMap(t => unwrapTupleStr(remap(t)))
      val sep = if (targsUnboxed.length > 0) "," else ""
      "scala.Function" + (targsUnboxed.length) + "[" + targsUnboxed.mkString(",") + sep + res + "]"

    case _ => super.remap(m)
  }*/
}
