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
      case AppliedType(f, List(arg)) =>
        // XXX - do something better
        if (!(f.show.endsWith("Exp") || f.show.endsWith("Rep"))) {
          None
        }
        else {
          Some(arg)
        }
      case t =>
        if (t =:= TypeRepr.of[Unit]) {
          Some(TypeRepr.of[Unit])
        }
        else {
          None
        }
    }

    def unVar(t: TypeRepr): Option[TypeRepr] = t match {
      case AppliedType(f, List(arg)) =>
        if (!(f.show.endsWith("Var"))) {
          None
        }
        else {
          Some(arg)
        }
      case t =>
        if (t =:= TypeRepr.of[Unit]) {
          Some(TypeRepr.of[Unit])
        }
        else {
          None
        }
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

    def flattenBlockT(t: Statement): (List[Statement], Term) = t match {
      case Block(body, v) => {
        val flattenedBody = body.flatMap(stm => {
          val (stms, endv) = flattenBlockT(stm)
          stms ++ List(endv)
        })
        val (flattenedVStms, trueV) = flattenBlockT(v)
        (flattenedBody ++ flattenedVStms, trueV)
      }
      case t: Term => (Nil, t)
      case stm => (List(stm), Literal(UnitConstant()))
    }

    def flattenBlock(t: Term): Term = {
      val (body, v) = flattenBlockT(t)
      Block(body, v)
    }

    def dropTrailingUnitInWhileBody(t: Term, thist: Term, unitf: Term): Term =
      flattenBlock(t) match {
        case Block(Nil, Literal(c)) => {
          makeUnit(Literal(UnitConstant()), thist, unitf)
        }
        case Block(body, Literal(c)) => {
          // TODO: Should we typecheck [body] here?
          Block(body, makeUnit(Literal(UnitConstant()), thist, unitf))
        }
        case Block(body, v) => {
          report.errorAndAbort("body of virtualized while loop should have type Rep[Unit]: " + v.show)
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
            val xt = this.transformTerm(x)(owner)
            val thent = this.transformTerm(thenp)(owner)
            val elset = this.transformTerm(elsep)(owner)

            val ttype = thenp.tpe.widen
            val trep = unRep(ttype) match {
              case Some(t) => t
              case None =>
                report.errorAndAbort("arms of virtualized if/else must be Reps, instead got " + ttype.show)
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

            val xt = this.transformTerm(x)(owner)

            val bodyt = this.transformTerm(bodyp)(owner)

            val thist = makeThis(owner)

            val unitf = findMethods(owner, "unit") match {
              case Nil =>
                report.errorAndAbort("LMS-internal error: no [unit] found for self")
              case x :: _ => thist.select(x)
            }

            val srcGen = '{SourceContext.generate}.asTerm

            val body = dropTrailingUnitInWhileBody(bodyt, thist, unitf)
            // This overload currently can't find the overload for
            // `__whileDo(Rep[Boolean], Rep[Unit])` even though it can find it
            // if we search manually.
            /*
            val r = Select.overloaded(
                  thist, "__whileDo",
                  Nil, List(xt, body))
            */
            val r = findMethods(owner, "__whileDo") match {
              case _ :: symb :: _xs => Apply(thist.select(symb), List(xt, body))
              case symb :: _xs => Apply(thist.select(symb), List(xt, body))
              case Nil => report.errorAndAbort("failed to virtualize: no __whileDo in scope")
            }

            val result = Apply(r, List(srcGen))

            result
          }

          case Assign(lhs, rhsp) => {
            if (!lhs.tpe.show.endsWith("Var")) {
              return super.transformTerm(tree)(owner)
            }

            val thist = makeThis(owner)
            val rhs = transformTerm(rhsp)(owner)
            val srcGen = '{SourceContext.generate}.asTerm

            (unRep(rhs.tpe.widen), unVar(rhs.tpe.widen)) match {
              case (Some(t), _) => {
                val tytree = TypeTree.of(using t.asType)
                val ttyp = Applied(TypeSelect(thist, "Typ"), List(tytree))
                val typW = Implicits.search(ttyp.tpe) match {
                  case success: ImplicitSearchSuccess => success.tree
                }
                val overload1 = Implicits.search(TypeSelect(thist, "Overloaded1").tpe) match {
                  case success: ImplicitSearchSuccess => success.tree
                }
                val assign = Select.overloaded(thist, "__assign", List(t), List(lhs, rhs))
                Apply(assign, List(overload1, typW, srcGen))
              }
              case (_, Some(t)) => {
                val tytree = TypeTree.of(using t.asType)
                val ttyp = Applied(TypeSelect(thist, "Typ"), List(tytree))
                val typW = Implicits.search(ttyp.tpe) match {
                  case success: ImplicitSearchSuccess => success.tree
                }
                val overload2 = Implicits.search(TypeSelect(thist, "Overloaded2").tpe) match {
                  case success: ImplicitSearchSuccess => success.tree
                }
                val assign = Select.overloaded(thist, "__assign", List(t), List(lhs, rhs))
                Apply(assign, List(overload2, typW, srcGen))
              }
              case (_, _) => {
                val t = rhs.tpe.widen
                val tytree = TypeTree.of(using t.asType)
                val ttyp = Applied(TypeSelect(thist, "Typ"), List(tytree))
                val typW = Implicits.search(ttyp.tpe) match {
                  case success: ImplicitSearchSuccess => success.tree
                }
                val assign = Select.overloaded(thist, "__assign", List(t), List(rhs))
                // __newVar[T](init)(using typW, pos)
                Apply(assign, List(typW, srcGen))
              }
            }
          }

          case _ => super.transformTerm(tree)(owner)
        }

      override def transformStatement(tree: Statement)(owner: Symbol): Statement = tree match {
        // `ty` is the type of `rhs`
        case ValDef(name, ty, Some(t)) => {
          // If `Typ` is not in scope, do nothing
          if (fetchEnclosingClass(owner).typeMember("Typ").isNoSymbol) {
            return super.transformStatement(tree)(owner)
          }

          ty match {
            case _: Singleton => return super.transformStatement(tree)(owner)
            case _ => ()
          }

          val thist = makeThis(owner)
          val rhs = transformTerm(t)(owner)
          val srcGen = '{SourceContext.generate}.asTerm

          val newrhs: Term = (unRep(ty.tpe.widen), unVar(ty.tpe.widen)) match {
            case (Some(t), _) => {
              val tytree = TypeTree.of(using t.asType)
              val ttyp = Applied(TypeSelect(thist, "Typ"), List(tytree))
              val typW = Implicits.search(ttyp.tpe) match {
                case success: ImplicitSearchSuccess => success.tree
              }
              val overload1 = Implicits.search(TypeSelect(thist, "Overloaded1").tpe) match {
                case success: ImplicitSearchSuccess => success.tree
              }
              val newvar = Select.overloaded(thist, "__newVar", List(t), List(rhs))
              // __newVar[T](init)(using overload1, typW, pos)
              val result = Apply(newvar, List(overload1, typW, srcGen))
              report.error(result.show)
              result
            }
            case (_, Some(t)) => {
              val tytree = TypeTree.of(using t.asType)
              val ttyp = Applied(TypeSelect(thist, "Typ"), List(tytree))
              val typW = Implicits.search(ttyp.tpe) match {
                case success: ImplicitSearchSuccess => success.tree
              }
              val overload2 = Implicits.search(TypeSelect(thist, "Overloaded2").tpe) match {
                case success: ImplicitSearchSuccess => success.tree
              }
              val newvar = Select.overloaded(thist, "__newVar", List(t), List(rhs))
              // __newVar[T](init)(using overload2, typW, pos)
              Apply(newvar, List(overload2, typW, srcGen))
            }
            case (_, _) => {
              val t = ty.tpe.widen
              val tytree = TypeTree.of(using t.asType)
              val ttyp = Applied(TypeSelect(thist, "Typ"), List(tytree))
              val typW = Implicits.search(ttyp.tpe) match {
                case success: ImplicitSearchSuccess => success.tree
              }
              val newvar = Select.overloaded(thist, "__newVar", List(t), List(rhs))
              // __newVar[T](init)(using typW, pos)
              Apply(newvar, List(typW, srcGen))
            }
          }

          ValDef.copy(tree)(name, ty, Some(newrhs))
        }
        case _ => super.transformStatement(tree)(owner)
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
