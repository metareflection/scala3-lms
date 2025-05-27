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

    sealed trait RepOrVar
    case class RepW(t: TypeRepr) extends RepOrVar
    case class VarW(t: TypeRepr) extends RepOrVar
    case class Bare(t: TypeRepr) extends RepOrVar

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

    def repOrVar(t: TypeRepr): RepOrVar = t.widen match {
      case AppliedType(f, List(arg)) =>
        // XXX - do something better
        if (f.show.endsWith("Exp") || f.show.endsWith("Rep")) {
          RepW(arg)
        }
        else if (f.show.endsWith("Var") || f.show.endsWith("Variable")) {
          VarW(arg)
        }
        else {
          Bare(arg)
        }
      case t => Bare(t)
    }

    def unRep(t: TypeRepr): Option[TypeRepr] = repOrVar(t) match {
      case RepW(t) => Some(t)
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

    def findTypW(thist: Term, trep: TypeRepr): Term = {
      val t = TypeTree.of(using trep.asType)
      val ttyp = Applied(TypeSelect(thist, "Typ"), List(t))
      val typW = Implicits.search(ttyp.tpe) match {
        // Failure should be impossible, else we wouldn't have been
        // able to form the type Rep[T]
        case success: ImplicitSearchSuccess => success.tree
      }
      typW
    }

    def findOverload(thist: Term, n: Int): Term = {
      Implicits.search(TypeSelect(thist, "Overloaded" + n).tpe) match {
        case success: ImplicitSearchSuccess => success.tree
      }
    }

    def isVarApply(conv: Term): Boolean = {
      val convs = List(
        "__virtualizedBareVarConvInternal",
        "__virtualizedRepVarConvInternal"
      )

      def mkpattern(s: String) = raw""".*$s\[.*\]\.apply$$""".r

      convs.exists(mkpattern(_).matches(conv.show))
    }

    object Visitor extends TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match {
          case Apply(
            Select(_, "boolToBoolRep"),
            List(x@Apply(Select(_, "=="), List(_)))) => this.transformTerm(x)(owner)

          case Apply(Select(lhsp, "=="), List(rhsp)) => {
            val thist = makeThis(owner)
            val srcGen = '{SourceContext.generate}.asTerm

            val lhs = this.transformTerm(lhsp)(owner)
            val rhs = this.transformTerm(rhsp)(owner)

            // Overload order:
            //   RepRep
            //   RepVar
            //   VarRep
            //   RepBare
            //   BareRep
            //   VarBare
            //   BareVar
            //   VarVar
            val (lty, rty, n) = (repOrVar(lhs.tpe), repOrVar(rhs.tpe)) match {
              case (RepW(lty), RepW(rty)) => (lty, rty, 1)
              case (RepW(lty), VarW(rty)) => (lty, rty, 2)
              case (VarW(lty), RepW(rty)) => (lty, rty, 3)
              case (RepW(lty), Bare(rty)) => (lty, rty, 4)
              case (Bare(lty), RepW(rty)) => (lty, rty, 5)
              case (VarW(lty), Bare(rty)) => (lty, rty, 6)
              case (Bare(lty), VarW(rty)) => (lty, rty, 7)
              case (VarW(lty), VarW(rty)) => (lty, rty, 8)
              case (Bare(lty), Bare(rty)) => return tree
            }

            val overload = findOverload(thist, n)
            val ltyW = findTypW(thist, lty)
            val rtyW = findTypW(thist, rty)

            // this.__equal[A, B](a, b)(using o, mA, mB, pos)
            Apply(
              Select.overloaded(thist, "__equal", List(lty, rty), List(lhs, rhs)),
              List(overload, ltyW, rtyW, srcGen))
          }

          case If(guard@Apply(conv, List(x)), thenp, elsep) => {
            val thist = makeThis(owner)
            val srcGen = '{SourceContext.generate}.asTerm

            val xt =
              if (conv.show.endsWith("__virtualizedBoolConvInternal.apply")) {
                this.transformTerm(x)(owner)
              } else if (conv.show.endsWith("==")) {
                this.transformTerm(guard)(owner)
              } else {
                return super.transformTerm(tree)(owner)
              }

            // HACK: In the case of `if (e1 == e2)` where both `e1` and `e2` are
            // non-Rep (stage-time) expressions, don't rewrite the `if` expr.
            val guardRep = unRep(xt.tpe) match {
              case Some(_) => ()
              case None => return super.transformTerm(tree)(owner)
            }

            val thent = this.transformTerm(thenp)(owner)
            val elset = this.transformTerm(elsep)(owner)

            val ttype = thenp.tpe.widen
            val trep = unRep(ttype) match {
              case Some(t) => t
              case None =>
                report.errorAndAbort("arms of virtualized if/else must be Reps, instead got " + ttype.show)
            }

            val tt = unRep(ttype)

            val typW = findTypW(thist, trep)

            //Apply(Apply(ite, List(xt, thent, elset)), List(typW, srcGen))
            Apply(Select.overloaded(thist, "__ifThenElse", List(trep), List(xt, thent, elset)),
              List(typW, srcGen))
          }

          case While(Apply(conv, List(x)), bodyp) => {
            val thist = makeThis(owner)
            val srcGen = '{SourceContext.generate}.asTerm

            if (!conv.show.endsWith("__virtualizedBoolConvInternal.apply")) {
              return super.transformTerm(tree)(owner)
            }

            val xt = this.transformTerm(x)(owner)

            val bodyt = this.transformTerm(bodyp)(owner)

            val unitf = findMethods(owner, "unit") match {
              case Nil =>
                report.errorAndAbort("LMS-internal error: no [unit] found for self")
              case x :: _ => thist.select(x)
            }

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

          case Assign(lhsp, rhsfull@Apply(conv, List(rhsp))) => {
            if (!isVarApply(conv)) {
              return handleGenericAssign(lhsp, rhsfull)(tree, owner)
            }

            val thist = makeThis(owner)
            val srcGen = '{SourceContext.generate}.asTerm
            val lhs = this.transformTerm(lhsp)(owner)
            val rhs = this.transformTerm(rhsp)(owner)

            // start:
            //   x = __conv(y)
            // end:
            //   __assign[T](x, y)(using ...)

            unRep(rhs.tpe) match {
              case Some(t) => {
                // __assign[T](x, y)(using overloaded1, m1: Typ[T], pos)

                // m1: Typ[T]
                val ttyp = findTypW(thist, t)
                // o: Overloaded1
                val overload = findOverload(thist, 1)
                // __assign[T](lhs, rhs)
                val assignT = Select.overloaded(thist, "__assign", List(t), List(lhs, rhs))

                Apply(assignT, List(overload, ttyp, srcGen))
              }
              case None => {
                // __assign[T](x, y)(using m: Typ[T], pos)

                val trep = rhs.tpe.widen

                val t = TypeTree.of(using trep.asType)
                // this.Typ[T]
                val typt = Applied(TypeSelect(thist, "Typ"), List(t))
                // m: Typ[T]
                val ttyp = Implicits.search(typt.tpe) match {
                  case success: ImplicitSearchSuccess => success.tree
                  case failure: ImplicitSearchFailure => {
                    return super.transformTerm(tree)(owner)
                  }
                }

                // __assign[T](lhs, rhs)
                val assignT = Select.overloaded(thist, "__assign", List(trep), List(lhs, rhs))

                Apply(assignT, List(ttyp, srcGen))
              }
            }
          }

          case Assign(lhsp, rhsp) => handleGenericAssign(lhsp, rhsp)(tree, owner)

          case _ => super.transformTerm(tree)(owner)
        }
      }

      def handleGenericAssign(lhsp: Term, rhsp: Term)(tree: Term, owner: Symbol): Term = {
        val t = repOrVar(lhsp.tpe) match {
          case VarW(t) => t
          case _ => {
            report.errorAndAbort("lhsp: " + lhsp.show + " with type: " + lhsp.tpe.widen.show)
            return super.transformTerm(tree)(owner)
          }
        }

        val thist = makeThis(owner)
        val srcGen = '{SourceContext.generate}.asTerm
        val lhs = this.transformTerm(lhsp)(owner)
        val rhs = this.transformTerm(rhsp)(owner)

        // __assign[T](x, y)(using overloaded2, m2: Typ[T], pos)


        // m1: Typ[T]
        val ttyp = findTypW(thist, t)
        // o: Overloaded2
        val overload = findOverload(thist, 2)
        // __assign[T](lhs, rhs)
        val assignT = Select.overloaded(thist, "__assign", List(t), List(lhs, rhs))

        Apply(assignT, List(overload, ttyp, srcGen))
      }

      override def transformStatement(tree: Statement)(owner: Symbol): Statement = {
        tree match {
          case ValDef(name, tptp, Some(rhsfull@Apply(conv,List(rhsp)))) => {
            if (!isVarApply(conv)) {
              return handleGenericValDef(name, tptp, rhsfull)(tree, owner)
            }

            val thist = makeThis(owner)
            val srcGen = '{SourceContext.generate}.asTerm
            val rhs = this.transformTerm(rhsp)(owner)

            val (tpt, tgt, ttyp) = unRep(rhs.tpe) match {
              case Some(t) => {
                val ttyp = findTypW(thist, t)
                val overload = findOverload(thist, 1)
                val newVarT = Select.overloaded(thist, "__newVar", List(t), List(rhs))
                val varT = Applied(TypeSelect(thist, "Var"), List(TypeTree.of(using t.asType)))
                val result = Apply(newVarT, List(overload, ttyp, srcGen))

                (varT, result, ttyp)
              }
              case None => {
                val trep = rhs.tpe.widen
                // We can't use `findTypW` because we actually might be working
                // with a non-reifiable stage-time construct.
                val t = TypeTree.of(using trep.asType)
                val typt = Applied(TypeSelect(thist, "Typ"), List(t))
                val ttyp = Implicits.search(typt.tpe) match {
                  // Failure should be impossible, else we wouldn't have been
                  // able to form the type Rep[T]
                  case success: ImplicitSearchSuccess => success.tree
                  case failure: ImplicitSearchFailure => {
                    return super.transformStatement(tree)(owner)
                  }
                }
                val newVarT = Select.overloaded(thist, "__newVar", List(trep), List(rhs))
                val varT = Applied(TypeSelect(thist, "Var"), List(t))
                val result = Apply(newVarT, List(ttyp, srcGen))

                (varT, result, ttyp)
              }
            }

            ValDef.copy(tree)(name, tpt, Some(tgt))
          }

          case ValDef(name, tptp, Some(rhsp)) => handleGenericValDef(name, tptp, rhsp)(tree, owner)

          case _ => super.transformStatement(tree)(owner)
        }
      }

      def handleGenericValDef
          (name: String, tptp: TypeTree, rhsp: Term)
          (tree: Statement, owner: Symbol): Statement =
      {
        val sym = tree.symbol

        if (!sym.flags.is(Flags.Mutable)) {
          return super.transformStatement(tree)(owner)
        }

        val t = repOrVar(tptp.tpe) match {
          case VarW(t) => t
          case _ => return super.transformStatement(tree)(owner)
        }

        val srcGen = '{SourceContext.generate}.asTerm

        val thist = makeThis(owner)
        val rhs = this.transformTerm(rhsp)(owner)

        // TODO: un-copypaste this from above
        val (tpt, tgt, trep, ttyp) = {
          val ttyp = findTypW(thist, t)
          val overload = findOverload(thist, 2)
          val newVarT = Select.overloaded(thist, "__newVar", List(t), List(rhs))
          val varT = Applied(TypeSelect(thist, "Var"), List(TypeTree.of(using t.asType)))
          val result = Apply(newVarT, List(overload, ttyp, srcGen))

          (varT, result, t, ttyp)
        }

        ValDef.copy(tree)(name, tpt, Some(tgt))
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
