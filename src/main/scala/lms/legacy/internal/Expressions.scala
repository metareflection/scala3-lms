package scala.lms
package internal

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer
import java.lang.{StackTraceElement,Thread}

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils {
  abstract class Typ[T] {
    def typeArguments: List[Typ[?]]
    def arrayTyp: Typ[Array[T]]
    def runtimeClass: java.lang.Class[?]
    def <:<(that: Typ[?]): Boolean
    def isArray = runtimeClass.isArray
  }

  case class ManifestTyp[T](mf: Manifest[T]) extends Typ[T] {
    def typeArguments: List[Typ[?]] = mf.typeArguments.map(ManifestTyp(_))
    def arrayTyp: Typ[Array[T]] = ManifestTyp(mf.arrayManifest)
    def runtimeClass: java.lang.Class[?] = mf.runtimeClass
    def manifest = mf
    def <:<(that: Typ[?]): Boolean = that match {
      case ManifestTyp(mf1) => mf.<:<(mf1)
      case _ => false
    }
    //override def canEqual(that: Any): Boolean = mf.canEqual(that) // TEMP
    //override def equals(that: Any): Boolean = mf.equals(that) // TEMP
    //override def hashCode = mf.hashCode
    override def toString = mf.toString
  }

  object ClassTyp {
    def unapply(t: Typ[?]): Option[(Class[?], List[Typ[?]])] = Some(t.runtimeClass, t.typeArguments)
  }

  object ArrayTyp {
    def unapply(t: Typ[?]) =
      if (t.isArray)
        Some(t.typeArguments.head)
      else
        None
  }

  def typ[T:Typ]: Typ[T] = implicitly[Typ[T]]

  abstract class Exp[+T:Typ] { // constants/symbols (atomic)
    def tp: Typ[T @uncheckedVariance] = implicitly[Typ[T]] //invariant position! but hey...
    def pos: List[SourceContext] = Nil
  }

  case class Const[+T:Typ](x: T) extends Exp[T] {
    override def equals(other: Any) = other match {
      case c: Const[_] => x == c.x && tp == c.tp
      case _ => false
    }
  }

  case class Sym[+T:Typ](val id: Int) extends Exp[T] {
    var sourceContexts: List[SourceContext] = Nil
    override def pos = sourceContexts
    def withPos(pos: List[SourceContext]) = { sourceContexts :::= pos; this }
  }

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  case class VariableTyp[T](inner: Typ[T]) extends Typ[Variable[T]] {
    def <:<(that: Typ[?]): Boolean = that match {
      case VariableTyp(inner2) => inner.<:<(inner2)
      case _ => false
    }

    def arrayTyp = throw new RuntimeException("TODO: manifest for Array[Var[T]]")

    def runtimeClass: Class[?] = throw new RuntimeException("TODO: VariableTyp.runtimeClass")

    def typeArguments = List(inner)
  }

  var nVars = 0
  def fresh[T:Typ]: Sym[T] = Sym[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }

  def fresh[T:Typ](pos: List[SourceContext]): Sym[T] = fresh[T].withPos(pos)

  def quotePos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs => {
      def all(cs: SourceContext): List[SourceContext] = cs.parent match {
        case None => List(cs)
        case Some(p) => cs::all(p)
      }
      cs.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
    }
  }


  abstract class Def[+T] { // operations (composite)
    override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }

  abstract class Stm // statement (links syms and definitions)
  
  def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TP(sym, rhs) => sym::Nil
  }
  
  def infix_rhs(stm: Stm): Any = stm match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
    case TP(sym, rhs) => rhs
  }

  def infix_defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TP(`sym`, rhs: Def[A]) => Some(rhs)
    case _ => None
  }

  def infix_defines[A: Typ](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TP(sym: Sym[A], `rhs`) if sym.tp <:< typ[A] => Some(sym)
    case _ => None
  }

  case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm

  // graph construction state
  
  var globalDefs: List[Stm] = Nil
  var localDefs: List[Stm] = Nil
  var globalDefsCache: Map[Sym[Any],Stm] = Map.empty

  def reifySubGraph[T](b: =>T): (T, List[Stm]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    val saveGlobalCache = globalDefsCache
    localDefs = Nil
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    globalDefsCache = saveGlobalCache
    (r, defs)
  }

  def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(infix_lhs(_))
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    // equivalent to: globalDefs filter (_.lhs exists (lhs contains _))
    val existing = lhs flatMap (globalDefsCache get _)
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
    for (stm <- ds; s <- infix_lhs(stm)) {      
      globalDefsCache += (s->stm)
    }
  }

  def findDefinition[T](s: Sym[T]): Option[Stm] =
    globalDefsCache.get(s)
    //globalDefs.find(x => x.defines(s).nonEmpty)

  def findDefinition[T: Typ](d: Def[T]): Option[Stm] =
    globalDefs.find(x => infix_defines(x, d).nonEmpty)

  def findOrCreateDefinition[T:Typ](d: Def[T], pos: List[SourceContext]): Stm =
    findDefinition[T](d) map { x => infix_defines(x, d).foreach(_.withPos(pos)); x } getOrElse {
      createDefinition(fresh[T](pos), d)
    }

  def findOrCreateDefinitionExp[T:Typ](d: Def[T], pos: List[SourceContext]): Exp[T] =
    infix_defines(findOrCreateDefinition(d, pos), d).get

  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    val f = TP(s, d)
    reflectSubGraph(List(f))
    f
  }
  

  protected implicit def toAtom[T:Typ](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
    findOrCreateDefinitionExp(d, List(pos)) // TBD: return Const(()) if type is Unit??
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match {
      case s @ Sym(_) =>
        findDefinition(s).flatMap(infix_defines(_, s))
      case _ =>
        None
    }
  }


  // dependencies

  // regular data (and effect) dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product => 
      // performance hotspot: this is the same as
      // p.productIterator.toList.flatMap(syms(_))
      // but faster
      val iter = p.productIterator
      val out = new ListBuffer[Sym[Any]]
      while (iter.hasNext) {
        val e = iter.next()
        out ++= syms(e)
      }
      out.result
    case _ => Nil
  }

  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }

  // symbols which are bound in a definition, but also defined elsewhere
  def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(tunnelSyms(_))
    case p: Product => p.productIterator.toList.flatMap(tunnelSyms(_))
    case _ => Nil
  }

  // symbols of effectful components of a definition
  def effectSyms(x: Any): List[Sym[Any]] = x match {
    case ss: Iterable[Any] => ss.toList.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }

  // soft dependencies: they are not required but if they occur, 
  // they must be scheduled before
  def softSyms(e: Any): List[Sym[Any]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(softSyms(_))
    case p: Product => p.productIterator.toList.flatMap(softSyms(_))
    case _ => Nil
  }

  // generic symbol traversal: f is expected to call rsyms again
  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case ss: Iterable[Any] => ss.toList.flatMap(f)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case ss: Iterable[Any] => ss.toList.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    //case _ => rsyms(e)(symsFreq)
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))


  // bookkeeping

  def reset: Unit = { // used by delite?
    nVars = 0
    globalDefs = Nil
    localDefs = Nil
    globalDefsCache = Map.empty
  }

}
