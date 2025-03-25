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

    def findMethods(name: String, owner: Symbol): List[Symbol] =
        if (owner.isNoSymbol) Nil
        else {
          owner.methodMember(name) ++ owner.companionClass.methodMember(name) match {
            case Nil => findMethods(name, owner.maybeOwner)
            case results => results
          }
        }

    def fetchFuncInOut(name: Symbol): (List[TypeRepr], TypeRepr) =
      name.tree match {
        case d: DefDef => {
          val inps = d.paramss.flatMap { param =>
            param match {
              case paramClause: TermParamClause =>
                paramClause.params.collect { case v: ValDef => v.tpt.tpe }
              case _ => Nil
            }
          }

          (inps, d.returnTpt.tpe)
        }
        // TODO: val defs
        case _ => report.errorAndAbort(s"${name} should be a function")
      }

    // TODO: error messages
    def ifThenElseValid(paramTy: TypeRepr)(symb: Symbol): Boolean = {
      val (inps, out) = fetchFuncInOut(symb)

      inps match {
        case List(guardTy, thenTy, elseTy) => {
          // TODO: Generics
          paramTy <:< guardTy &&
          ((thenTy, elseTy) match {
            case (ByNameType(t1), ByNameType(t2)) => t1 =:= t2 && t1 =:= out
            case _ => false
          })
        }
        case _ => false
      }
    }

    object Visitor extends TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match {
          // because the guard to an `If` is always strictly a boolean, we can
          // cheat and guess that, if the guard is of the form `f(exp)` where
          // `exp: Rep[Boolean]`, then `f` must be a conversion function
          //
          // strictly speaking, it is possible for there to be a user-defined
          // implicit function from `Rep[Boolean]` to Boolean, so we should
          // check for the actual name
          case If(guard@Apply(Select(TypeApply(Ident("__virtualizedBoolConvInternal"), args), name), List(x)), thenp, elsep) => {
            val xt = transformTerm(x)(owner)
            val thent = transformTerm(thenp)(owner)
            val elset = transformTerm(elsep)(owner)

            val cands = findMethods("__ifThenElse", owner).filter(ifThenElseValid(x.tpe))

            cands match {
              case List(ifThenElseSymb) => {
                report.error(s"${Ref(ifThenElseSymb).tpe.widen}")
                Apply(Ref(ifThenElseSymb), List(xt, thent, elset))
              }
              case x :: xs => {
                report.error("failed to virtualize: multiple valid candidates for __ifThenElse")
                super.transformTerm(tree)(owner)
              }
              case Nil => {
                report.error("failed to virtualize: no valid __ifThenElse in scope")
                super.transformTerm(tree)(owner)
              }
            }
          }
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
