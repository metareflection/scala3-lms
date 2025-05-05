package scala.lms

import scala.reflect.ClassTag
import scala.annotation.*
import scala.quoted.*

@experimental
class virtualize extends MacroAnnotation {
  override def transform(using quotes: Quotes)(
    tree: quotes.reflect.Definition,
    companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] = {
    import quotes.reflect._

    def findMethods(owner: Symbol, name: String): List[Symbol] =
        if (owner.isNoSymbol) Nil
        else {
          owner.methodMember(name) ++ owner.companionClass.methodMember(name) match {
            case Nil => findMethods(owner.maybeOwner, name)
            case results => results
          }
        }

    def fetchEnclosingClass(s: Symbol): Symbol =
      if (s.isClassDef) s
      else if (s.isNoSymbol) Symbol.noSymbol
      else fetchEnclosingClass(s.maybeOwner)

    def unRep(t: TypeRepr): Option[TypeRepr] = t match {
      case AppliedType(f, List(arg)) => {
        // XXX - do something better
        if (!(f.show.endsWith("Exp") || f.show.endsWith("Rep"))) {
          None
        }
        else {
          Some(arg)
        }
      }
      case _ => None
    }

    def makeThis(owner: Symbol): Term = This(fetchEnclosingClass(owner))

    def makeUnit(x: Term, thist: Term, unitf: Term): Term = {
        val unitt = TypeRepr.of[Unit]
        val unitTyp = Applied(TypeSelect(thist, "Typ"), List(TypeTree.of[Unit]))

        val unitW = Implicits.search(unitTyp.tpe) match {
          case success: ImplicitSearchSuccess => success.tree
        }

        //unit[Unit](())(using Typ.of[Unit])
        Apply(Apply(TypeApply(unitf, List(TypeTree.of[Unit])), List(x)), List(unitW))
    }

    def dropTrailingUnit(t: Term, thist: Term, unitf: Term): Term = t match {
      case Block(Nil, Literal(c)) => {
        makeUnit(Literal(UnitConstant()), thist, unitf)
      }
      case Block(body, Literal(c)) => {
        // TODO: Should we typecheck this?
        Block(body, makeUnit(Literal(UnitConstant()), thist, unitf))
      }
      case Block(body, v) => {
        for (stm <- body) {
          report.error(stm.show)
        }
        report.error(v.show)
        report.errorAndAbort("body of virtualized while loop should have type Rep[Unit]")
      }
      case t => t
    }

    object Visitor extends TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match {
          case If(Apply(conv, List(x)), thenp, elsep) => {
            if (!conv.show.endsWith("__virtualizedBoolConvInternal.apply")) {
              return super.transformTerm(tree)(owner)
            }
            val xt = transformTerm(x)(owner)
            val thent = transformTerm(thenp)(owner)
            val elset = transformTerm(elsep)(owner)

            val ttype = thenp.tpe
            val trep = unRep(ttype) match {
              case Some(t) => t
              case None => report.errorAndAbort("arms of virtualized if/else must be Reps")
            }

            val thist = makeThis(owner)

            val tt = unRep(ttype)

            val t = TypeTree.of(using trep.asType)

            val ttyp = Applied(TypeSelect(thist, "Typ"), List(t))
            val typW = Implicits.search(ttyp.tpe) match {
              // Failure should be impossible, else we wouldn't have been
              // able to form the type Rep[T]
              case success: ImplicitSearchSuccess => success.tree
            }
            val srcGen = '{SourceContext.generate}.asTerm

            //Apply(Apply(ite, List(xt, thent, elset)), List(typW, srcGen))
            Apply(Select.overloaded(thist, "__ifThenElse", List(trep), List(xt, thent, elset)),
              List(typW, srcGen))
          }

          case While(Apply(conv, List(x)), bodyp) => {
            if (!conv.show.endsWith("__virtualizedBoolConvInternal.apply")) {
              return super.transformTerm(tree)(owner)
            }

            val xt = transformTerm(x)(owner)

            val bodyt = transformTerm(bodyp)(owner)

            val thist = makeThis(owner)

            val unitf = findMethods(owner, "unit") match {
              case Nil =>
                report.errorAndAbort("LMS-internal error: no [unit] found for self")
              case x :: _ => thist.select(x)
            }

            val srcGen = '{SourceContext.generate}.asTerm

            val body = dropTrailingUnit(bodyt, thist, unitf)
            // This overload currently can't find the overload for `Rep`s, even
            // though it can find it if we search manually.
            /*
            val r = Select.overloaded(
                  thist, "__whileDo",
                  Nil, List(xt, body))
            */
            val r = findMethods(owner, "__whileDo") match {
              // XXX: brittle
              case _ :: symb :: _xs => Apply(thist.select(symb), List(xt, body))
              case symb :: _xs => Apply(thist.select(symb), List(xt, body))
              case Nil => report.errorAndAbort("failed to virtualize: no __whileDo in scope")
            }

            val result = Apply(r, List(srcGen))

            result
          }

          /*
          case Assign(lhs, rhs) => {
            report.errorAndAbort(tree.show)
          }
          */

          case _ => super.transformTerm(tree)(owner)
        }
    }

    tree match {
      case ClassDef(name, constructor, parents, self, body) => {
        val vbody = Visitor.transformStats(body)(tree.symbol.owner)
        val vtree = ClassDef.copy(tree)(name, constructor, parents, self, vbody)
        List(vtree)
      }
      case DefDef(name, params, retTy, Some(rhs)) => {
        val vrhs = Visitor.transformTerm(rhs)(tree.symbol.owner)
        val vtree = DefDef.copy(tree)(name, params, retTy, Some(vrhs))
        List(vtree)
      }
      case _ => {
        report.error("@virtualize must be applied to a top-level class or function")
        List(tree)
      }
    }
  }
}
