/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc
package typechecker

import symtab._
import Flags.{ CASE => _, _ }
import scala.collection.mutable.ListBuffer



/** Translate pattern matching into method calls (these methods form a zero-plus monad), similar in spirit to how for-comprehensions are compiled.
  *
  * For each case, express all patterns as extractor calls, guards as 0-ary extractors, and sequence them using `flatMap`
  * (lifting the body of the case into the monad using `one`).
  *
  * Cases are combined into a pattern match using the `orElse` combinator (the implicit failure case is expressed using the monad's `zero`).
  *
  * The monad `M` in which the pattern match is interpreted is determined by solving `implicitly[MatchingStrategy[M]]` for M.
  * Predef provides the default, `OptionMatching`

  * Example translation: TODO

    scrut match { case Person(father@Person(_, fatherName), name) if fatherName == name => }
    scrut match { case Person(father, name) => father match {case Person(_, fatherName) => }}
    Person.unapply(scrut) >> ((father, name) => (Person.unapply(father) >> (_, fatherName) => check(fatherName == name) >> (_ => body)))

    (a => (Person.unapply(a).>>(
      b => Person.unapply(b._1).>>(
        c => check(c._2 == b._2).>>(
          d => body)))))(scrut)

TODO:
 - Type Patterns -- Bind nested in Typed's tpe (such as in pos/t1439 `case v: View[_] =>`)
 - specialize: specialized/spec-patmatch
 - stackoverflow with actors: jvm/t3412, jvm/t3412-channel
 - anonymous classes in scrutinee (pos/t0646)
 - existentials (pos/t1843.scala, pos/t2635.scala)
 - gadt typing (pos/gadt-gilles, pos/channels)

  * (longer-term) TODO:
  *  - recover GADT typing by locally inserting implicit witnesses to type equalities derived from the current case, and considering these witnesses during subtyping (?)
  *  - recover exhaustivity and unreachability checking using a variation on the type-safe builder pattern
  */
trait PatMatVirtualiser extends ast.TreeDSL { self: Analyzer =>
  import global._
  import definitions._

  private lazy val matchingStrategyTycon = getMember(PredefModule, "MatchingStrategy".toTypeName).typeConstructor

  class MatchTranslator(typer: Typer) { translator =>
    import typer._
    import typeDebug.{ ptTree, ptBlock, ptLine }
    private var overrideUnsafe = false

    def solveContextBound(contextBoundTp: Type): (Tree, Type) = {
      val solSym = NoSymbol.newTypeParameter(NoPosition, "SolveImplicit$".toTypeName)
      val param = solSym.setInfo(contextBoundTp.typeSymbol.typeParams(0).info.cloneInfo(solSym)) // TypeBounds(NothingClass.typeConstructor, baseTp)
      val pt = appliedType(contextBoundTp, List(param.tpeHK))
      val savedUndets = context.undetparams
      context.undetparams = param :: context.undetparams
      val result = inferImplicit(EmptyTree, pt, false, false, context)
      context.undetparams = savedUndets

      (result.tree, result.subst.to(result.subst.from indexOf param))
    }

    lazy val (matchingStrategy, matchingMonadType) = solveContextBound(matchingStrategyTycon)

    /** Implement a pattern match by turning its cases (including the implicit failure case)
      * into the corresponding (monadic) extractors, and combining them with the `orElse` combinator.
      *
      * For `scrutinee match { case1 ... caseN }`, the resulting tree has the shape
      * `runOrElse(scrutinee)(x => Xcase1(x).orElse(Xcase2(x)).....orElse(zero))`
      *
      * NOTE: the resulting tree is not type checked, nor are nested pattern matches transformed
      *   thus, you must typecheck the result (and that will in turn translate nested matches)
      *   this could probably optimized... (but note that the matchingStrategy must be solved for each nested patternmatch)
      */
    def X(tree: Tree, pt: Type): Tree = {
      // we don't transform after typers
      // (that would require much more sophistication when generating trees,
      //  and the only place that emits Matches after typers is for exception handling anyway)
      assert(phase.id <= currentRun.typerPhase.id)

      val xTree = tree match {
        case Match(scrut, cases) =>
          // TODO: deal with scrut == EmptyTree
          val scrutType = if(scrut.tpe ne null) scrut.tpe.widen else {error("TODO: support match with empty scrut"); NoType} // TODO: ErrorTree
          val scrutSym = freshSym(tree.pos, scrutType)
          genRunOrElse(scrut,
                      genFun(scrutSym,
                            ((cases map Xcase(scrutSym)) ++ List(genZero)) reduceLeft genTypedOrElse(appliedType(matchingMonadType, List(pt))))) // need to propagate pt explicitly, type inferencer can't handle it
        case t => t
      }

      // println("before fixerupper: "+ xTree)
      // currentRun.trackerFactory.snapshot()
      // TODO: do this during tree construction, but that will require tracking the current owner in proto treemakers
      // TODO: assign more fine-grained positions
      // fixes symbol nesting, assigns positions
      object fixerUpper extends Traverser {
        currentOwner = context.owner

        override def traverse(t: Tree) {
          if (t != EmptyTree && t.pos == NoPosition) {
            t.setPos(tree.pos)
          }
          t match {
            case Function(_, _) if t.symbol == NoSymbol =>
              t.symbol = currentOwner.newValue(t.pos, nme.ANON_FUN_NAME).setFlag(SYNTHETIC).setInfo(NoType)
              // println("new symbol for "+ (t, t.symbol.ownerChain))
            case Function(_, _) if (t.symbol.owner == NoSymbol) || (t.symbol.owner == context.owner) =>
              // println("fundef: "+ (t, t.symbol.ownerChain, currentOwner.ownerChain))
              t.symbol.owner = currentOwner
            case d : DefTree if (d.symbol != NoSymbol) && ((d.symbol.owner == NoSymbol) || (d.symbol.owner == context.owner)) => // don't indiscriminately change existing owners! (see e.g., pos/t3440, pos/t3534, pos/unapplyContexts2)
              // println("def: "+ (d, d.symbol.ownerChain, currentOwner.ownerChain))
              if(d.symbol.isLazy) { // for lazy val's accessor -- is there no tree??
                assert(d.symbol.lazyAccessor != NoSymbol && d.symbol.lazyAccessor.owner == d.symbol.owner)
                d.symbol.lazyAccessor.owner = currentOwner
              }
              d.symbol.owner = currentOwner
            // case _ if (t.symbol != NoSymbol) && (t.symbol ne null) =>
            //   println("untouched "+ (t, t.getClass, t.symbol.ownerChain, currentOwner.ownerChain))
            case _ =>
          }
          super.traverse(t)
        }
      }
      fixerUpper(xTree) // atPos(tree.pos)(xTree) does not achieve the same effect

      // println("after fixerupper")
      // currentRun.trackerFactory.snapshot()

      xTree
    }

    type TreeXForm = Tree => Tree
    type ProtoTreeMaker = (List[Tree], TreeXForm => (TreeXForm /* wrap a Fun and subst variables to tuple sel on variable bound by that Fun */, TreeXForm /* just the subst */))

    object TreeMaker {
      def apply(trees: List[Tree], genFunAndSubst0: TreeXForm): TreeMaker = trees match {
        case Nil => new NoTreeMaker{def genFunAndSubst(next: Tree) = genFunAndSubst0(next)}
        case List(tree) => new SingleTreeMaker(tree){def genFunAndSubst(next: Tree) = genFunAndSubst0(next)}
        case _ => new AlternativeTreeMaker(trees){def genFunAndSubst(next: Tree) = genFunAndSubst0(next)}
      }
    }
    abstract class TreeMaker {
      // wrap a Fun (with binder x) around the next tree and do aggregated substitution (which
      // replaces old pattern bindings by the appropriate tuple element selection on the new binders,
      // that is, `x`, if it was bound by the immediately enclosing pattern)
      def genFunAndSubst(next: Tree): Tree

      // build Tree that chains `next` after the current extractor
      def genFlatMap(next: Tree): Tree
    }

    abstract class NoTreeMaker extends TreeMaker {
      def genFlatMap(tree: Tree) = genFunAndSubst(tree) // doesn't make a fun, only does substitution
    }

    abstract class SingleTreeMaker(extractor: Tree) extends TreeMaker {
      def genFlatMap(tree: Tree) =
        translator.genFlatMap(extractor, genFunAndSubst(tree)) setPos extractor.pos
    }

    abstract class AlternativeTreeMaker(alts: List[Tree]) extends TreeMaker {
      def genFlatMap(tree: Tree) = genOr(genFunAndSubst(tree), alts) setPos alts.head.pos
    }

    // (o => (o(foo), newO)) :: (o => (o(foo), newO')) :: (o => (o(foo), newO'')) :: (o => (o(foo), newO'''))
    // (identity(foo), newO) :: (newO(foo), newO') :: (newO'(foo), newO'') :: (newO''(foo), newO''')
    def threadSubstitution(protoTreeMakers: List[ProtoTreeMaker]): (List[TreeMaker], TreeXForm) = {
      val (treeMakers, subst) = protoTreeMakers.foldLeft((List[TreeMaker](), identity[Tree](_))){
        case ((accumTreeMakers, accumSubst), (extractors, substTreeMaker)) =>
          val (nestedTreeMaker, newSubst) = substTreeMaker(accumSubst)
          (TreeMaker(extractors, nestedTreeMaker) :: accumTreeMakers, newSubst)
      }

      (treeMakers.reverse, subst)
    }


    /**  The translation of `pat if guard => body` has two aspects:
      *     1) the substitution due to the variables bound by patterns
      *     2) the combination of the extractor calls using `flatMap`.
      *
      * 2) is easy -- it looks like: `Xpat_1.flatMap(Xpat_2....flatMap(Xpat_N.flatMap(Xguard.flatMap((x_i) => success(Xbody(x_i)))))...)`
      *     this must be right-leaning tree, as can be seen intuitively by considering the scope of bound variables:
      *     variables bound by pat_1 must be visible from the function inside the left-most flatMap right up to Xbody all the way on the right
      * 1) is tricky because Xpat_i determines the shape of Xpat_i+1:
      *    zoom in on `Xpat_1.flatMap(Xpat_2)` for example -- it actually looks more like:
      *      `Xpat_1(x_scrut).flatMap((x_1) => {y_i -> x_1._i}Xpat_2)`
      *
      *    `x_1` references the result (inside the monad) of the extractor corresponding to `pat_1`,
      *    this result holds the values for the constructor arguments, which Xpat_1 has extracted
      *    from the object pointed to by `x_scrut`. The `y_i` are the symbols bound by `pat_1` (in order)
      *    in the scope of the remainder of the pattern, and they must thus be replaced by:
      *      - (for 1-ary unapply) x_1
      *      - (for n-ary unapply, n > 1) selection of the i'th tuple component of `x_1`
      *      - (for unapplySeq) x_1.apply(i)
      *
      *    in the proto-treemakers,
      *
      *    Thus, the result type of `Xpat_i`'s extractor must conform to `M[(T_1,..., T_n)]`.
      *
      *    Operationally, phase 1) is a foldLeft, since we must consider the depth-first-flattening of
      *    the transformed patterns from left to right. For every pattern ast node, it produces a transformed ast and
      *    a function that will take care of binding and substitution of the next ast (to the right).
      *
      *    `threadSubstitution` takes these pairs and accumulates the substitution from left to right, so that the rightmost substitution (a function from Tree to Tree)
      *    will substitute each bound pattern variable in the whole case.
      */
    def Xcase(scrutSym: Symbol)(tree: Tree): Tree = {
      tree match {
        case CaseDef(pattern, guard, body) =>
          threadSubstitution(Xpat(scrutSym)(pattern) ++ Xguard(guard))._1.foldRight(genOne(body))(_ genFlatMap _) setPos tree.pos
          // TODO: if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (genOne), so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand
      }
    }

    def Xpat(scrutSym: Symbol)(pattern: Tree): List[ProtoTreeMaker] = {
      def doUnapply(args: List[Tree], extractorCall: Tree, prevBinder: Symbol, pos: Position, outerCheck: Option[Tree] = None)(implicit res: ListBuffer[ProtoTreeMaker]): (List[Symbol], List[Tree]) = {
        assert((extractorCall.tpe ne null) && (extractorCall.tpe ne NoType) && (extractorCall.tpe ne ErrorType), "args: "+ args +" extractorCall: "+ extractorCall)
        val extractorType = extractorCall.tpe
        val isSeq = extractorCall.symbol.name == nme.unapplySeq

        // what's the extractor's result type in the monad?
        val typeInMonad = extractorResultInMonad(extractorType)

        if(typeInMonad == ErrorType) {
          error("Unsupported extractor type: "+ extractorType)
          return (Nil, Nil)
        }

        // `patBinders` are the variables bound by this pattern in the following patterns
        // patBinders are replaced by references to the relevant part of the extractor's result (tuple component, seq element, the result as-is)
        val sub@(patBinders, _) = args map {
          case BoundSym(b, p) => (b, p)
          case p => (freshSym(pos, prefix = "p"), p)
        } unzip

        // the types for the binders corresponding to my subpatterns
        // subPatTypes != args map (_.tpe) since the args may have more specific types than the constructor's parameter types
        val (subPatTypes, subPatRefs, lenGuard) = monadTypeToSubPatTypesAndRefs(typeInMonad, isSeq, args, patBinders)

        // must use type `tp`, which is provided by extractor's result, not the type expected by binder,
        // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
        // (it will later result in a type test when `tp` is not a subtype of `b.info`)
        (patBinders, subPatTypes).zipped foreach { case (b, tp) => b setInfo tp } // println("changing "+ b +" : "+ b.info +" -> "+ tp);

        val extractorParamType = extractorType.paramTypes.head

        // println("doUnapply (subPatTypes, typeInMonad, prevBinder, prevBinder.info.widen, extractorCall.symbol, extractorType, prevBinder.info.widen <:< extractorParamType) =\n"+
        //   (subPatTypes, typeInMonad, prevBinder, prevBinder.info.widen, extractorCall.symbol, extractorType, prevBinder.info.widen <:< extractorParamType))

        // println("doUnapply checking parameter type: "+ (prevBinder, prevBinder.info.widen, extractorParamType, prevBinder.info.widen <:< extractorParamType))
        // example check: List[Int] <:< ::[Int]
        // TODO: extractorParamType may contain unbound type params (run/t2800, run/t3530)
        val prevBinderOrCasted =
          if(!(prevBinder.info.widen <:< extractorParamType)) { import CODE._
            val castedBinder = freshSym(pos, extractorParamType, "cp")
            // cast
            val condTp = gen.mkIsInstanceOf(REF(prevBinder), extractorParamType, true, false)
            val cond = outerCheck map (_ AND condTp) getOrElse condTp

            // chain a cast before the actual extractor call
            // we control which binder is used in the nested tree (patTree -- created below), so no need to substitute
            res += (List(genTypedGuard(cond, extractorParamType, prevBinder)),
                      { outerSubst: TreeXForm =>
                          (nestedTree => genFun(castedBinder,outerSubst(nestedTree)), outerSubst)
                      })

            castedBinder
          } else prevBinder

        // the extractor call (applied to the binder bound by the flatMap corresponding to the previous (i.e., enclosing/outer) pattern)
        val extractorApply = atPos(pos)(genApply(extractorCall, prevBinderOrCasted))
        val patTree =
          if(extractorType.finalResultType.typeSymbol == BooleanClass) genGuard(extractorApply)
          else extractorApply

        // println("patTree= "+ patTree)

        res += Pair(List(patTree),
          if(patBinders isEmpty)
            { outerSubst: TreeXForm =>
                val binder = freshSym(patTree.pos, typeInMonad) // UnitClass.tpe is definitely wrong when isSeq, and typeInMonad should always be correct since it comes directly from the extractor's result type
                (nestedTree => genFun(binder, lenGuard(binder, outerSubst(nestedTree))), outerSubst)
            }
          else
            { outerSubst: TreeXForm =>
                val binder = freshSym(patTree.pos, typeInMonad)
                val theSubst = typedSubst(patBinders, subPatRefs(binder))
                def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                (nestedTree => genFun(binder, lenGuard(binder, nextSubst(nestedTree))), nextSubst)
            })

        sub
      }

      def singleBinderProtoTreeMaker(binderToSubst: Symbol, patTrees: Tree*): ProtoTreeMaker = singleBinderProtoTreeMakerWithTp(binderToSubst, binderToSubst.info.widen, false, patTrees : _*)
      def singleBinderProtoTreeMakerWithTp(binderToSubst: Symbol, binderType: Type, unsafe: Boolean, patTrees: Tree*): ProtoTreeMaker = {
        assert(patTrees.head.pos != NoPosition, "proto-tree for "+(binderToSubst, patTrees.toList))

        (patTrees.toList,
            { outerSubst: TreeXForm =>
                val binder = freshSym(patTrees.head.pos, binderType)
                val theSubst = typedSubst(List(binderToSubst), List(CODE.REF(binder)), unsafe)
                // println("theSubst: "+ theSubst)
                def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                (nestedTree => genFun(binder, nextSubst(nestedTree)), nextSubst)
            })
      }

      /** Decompose the pattern in `tree`, of shape C(p_1, ..., p_N), into a list of N symbols, and a list of its N sub-trees
        * The list of N symbols contains symbols for every bound name as well as the un-named sub-patterns (fresh symbols are generated here for these)
        *
        * @arg prevBinder  symbol used to refer to the result of the previous pattern's extractor (will later be replaced by the outer tree with the correct tree to refer to that patterns result)
        */
      def transformPat(prevBinder: Symbol, patTree: Tree)(implicit res: ListBuffer[ProtoTreeMaker]): (List[Symbol], List[Tree]) = { // TODO zip the returned lists here?
        object MaybeBoundTyped {
          def unapply(tree: Tree): Option[(Symbol, Tree)] = tree match {
            case BoundSym(patBinder, Typed(expr, tpt)) => Some((patBinder, tpt))
            case Bind(_, Typed(expr, tpt)) => Some((prevBinder, tpt))
            case Typed(expr, tpt) =>  Some((prevBinder, tpt))
            case _ => None
          }
        }

        def unwrapExtractorApply(t: Tree)(implicit extractor: Symbol): Tree = t match {
          case Apply(x, _) => unwrapExtractorApply(x) // could be implicit arg apply
          case x if x.symbol == extractor => x
        }

        patTree match {
          case UnApply(Apply(unfun0, unargs), args) =>
            // TODO: check unargs == args
            // println("unfun: "+ (unfun.tpe, unfun.symbol.ownerChain, unfun.symbol.info, prevBinder.info))
            val unfun = unwrapExtractorApply(unfun0)(unfun0.symbol)

            doUnapply(args, unfun, prevBinder, patTree.pos)

          case Apply(fun, args)     =>
            val orig = if(fun.isInstanceOf[TypeTree]) fun.asInstanceOf[TypeTree].original else fun
            val origSym = orig.symbol // undo rewrite performed in (5) of adapt
            val extractor = unapplyMember(origSym.filter(sym => reallyExists(unapplyMember(sym.tpe))).tpe)
            if((fun.tpe eq null) || fun.tpe.isError || (extractor eq NoSymbol)) {
               error("cannot find unapply member for "+ fun +" with args "+ args) // TODO: ErrorTree
               (Nil, Nil)
            } else {
              // println("needs outer test? "+(needsOuterTest(patTree.tpe, prevBinder.info, context.owner), patTree.tpe, prevBinder, prevBinder.info, context.owner))
              val outerCheck =
                if (needsOuterTest(patTree.tpe, prevBinder.info, context.owner)) { import CODE._
                  Some(genOuterCheck(prevBinder, patTree.tpe))
                } else None

              val extractorSel = genSelect(orig, extractor)

              // this is a tricky balance: pos/t602.scala, pos/sudoku.scala, run/virtpatmat_alts.scala must all be happy
              // bypass typing at own risk: val extractorCall = genSelect(orig, extractor) setType caseClassApplyToUnapplyTp(fun.tpe)
              // can't always infer type arguments (pos/t602):
              /*  case class Span[K <: Ordered[K]](low: Option[K]) {
                    override def equals(x: Any): Boolean = x match {
                      case Span((low0 @ _)) if low0 equals low => true
                    }
                  }*/
              // so... leave undetermined type params floating around if we have to, but forego type-safe substitution when overrideUnsafe
              // (if we don't infer types, uninstantiated type params show up later: pos/sudoku.scala)
              // (see also run/virtpatmat_alts.scala)
              val savedUndets = context.undetparams
              val extractorCall = try {
                context.undetparams = Nil
                silent(_.typed(Apply(extractorSel, List(Ident("<argument>") setType fun.tpe.finalResultType)), EXPRmode, WildcardType)) match {
                  case Apply(extractorCall, _)  =>
                    unwrapExtractorApply(extractorCall)(extractor)
                  case ex =>
                   // error("cannot type unapply call for "+ extractorSel +" error: "+ ex) // TODO: ErrorTree
                   typedOperator(extractorSel)
                }
              } finally context.undetparams = savedUndets

              overrideUnsafe ||= extractorCall.tpe.typeParams.nonEmpty // all bets are off when you have unbound type params floating around
              doUnapply(args, extractorCall, prevBinder, patTree.pos, outerCheck)
            }

          case MaybeBoundTyped(patBinder, tpt) => // must treat Typed and Bind together -- we need to know the prevBinder of the Bind pattern to get at the actual type
            val tpe = tpt.tpe // TODO: handle Binds nested in tpt (spec: 8.2 Type Patterns)
            val prevTp = prevBinder.info.widen
            val accumType = intersectionType(List(prevTp, tpe))

            val condTp = genTypeDirectedEquals(prevBinder, prevTp, tpe)
            // println("needs outer test? "+(needsOuterTest(tpe, prevBinder.info, context.owner), (tpe, prevBinder.info, context.owner)))
            val cond =
              if (needsOuterTest(tpe, prevBinder.info, context.owner)) { import CODE._
                genOuterCheck(prevBinder, tpe) AND condTp
              } else condTp

            val extractor = atPos(patTree.pos)(genTypedGuard(cond, accumType, prevBinder))

            res += singleBinderProtoTreeMakerWithTp(patBinder, accumType, unsafe = true, extractor)

            (Nil, Nil) // a typed pattern never has any subtrees

          case BoundSym(patBinder, p)          =>
            // don't generate an extractor, TreeMaker only performs the substitution patBinder --> prevBinder
            // println("rebind "+ patBinder +" to "+ prevBinder)
            res += (List(),
                      { outerSubst: TreeXForm =>
                          val theSubst = typedSubst(List(patBinder), List(CODE.REF(prevBinder)), unsafe = true)
                          // println("proto subst of: "+ patBinder)
                          def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                          (nestedTree => nextSubst(nestedTree), nextSubst)
                      })
            // the symbols are markers that may be used to refer to the result of the extractor in which the corresponding tree is nested
            // it's the responsibility of the treemaker (added to res in the previous line) to replace this symbol by a reference that
            // selects that result on the function symbol of the flatMap call that binds to the result of this extractor
            (List(prevBinder), List(p)) // must be prevBinder, as patBinder has the wrong info: even if the bind assumes a better type, this is not guaranteed until we cast
          case Bind(n, p) => (Nil, Nil) // there's no symbol -- something wrong?

          case Literal(Constant(_)) | Ident(_) | Select(_, _) => // it was folly to think we can unify this with type tests
            val prevTp = prevBinder.info.widen
            val tpe = patTree.tpe

            val condEq = genEquals(prevBinder, patTree)
            // println("needs outer test? "+(needsOuterTest(tpe, prevBinder.info, context.owner), tpe.prefix, (tpe, prevBinder.info, context.owner)))
            val cond =
              if (needsOuterTest(tpe, prevBinder.info, context.owner)) { import CODE._
                genOuterCheck(prevBinder, tpe) AND condEq
              } else condEq

            // NOTE: this generates `patTree == prevBinder`, since the extractor must be in control of the equals method
            // equals need not be well-behaved, so don't intersect with pattern's (stabilized) type (unlike MaybeBoundTyped's accumType, where its required)
            val extractor = atPos(patTree.pos)(genTypedGuard(cond, prevTp, prevBinder))

            res += singleBinderProtoTreeMakerWithTp(prevBinder, prevTp, unsafe = false, extractor)

            (Nil, Nil)

          case Alternative(alts)    =>
            val altTrees = alts map { alt =>
              // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
              val resAlts = new ListBuffer[ProtoTreeMaker]
              traverseDepthFirst(prevBinder, alt)(resAlts)

              // currently we ignore subst, since alternatives may not bind variables (except wildcards)
              val (treeMakers, subst) = threadSubstitution(resAlts.toList)

              atPos(alt.pos)(treeMakers.foldRight (genOne(CODE.REF(prevBinder))) (_ genFlatMap _))
            }

            res += singleBinderProtoTreeMaker(prevBinder, altTrees : _*)

        /* TODO: Paul says about future version: I think this should work, and always intended to implement if I can get away with it.
            case class Foo(x: Int, y: String)
            case class Bar(z: Int)

            def f(x: Any) = x match { case Foo(x, _) | Bar(x) => x } // x is lub of course.
        */

            (Nil, Nil)

          // case Star(x)              => // no need to handle this because it's always a wildcard nested in a bind (?)
          // case x: ArrayValue        => // TODO?
          // case x: This              => // TODO?

          case _                       =>
            error("UNHANDLED pattern: "+ (prevBinder, patTree, patTree.getClass))
            (Nil, Nil)
        }
      }

      def traverseDepthFirst(prevBinder: Symbol, patTree: Tree)(implicit res: ListBuffer[ProtoTreeMaker]): Unit =
        if (!isWildcardPattern(patTree)) // skip wildcard trees -- no point in checking them
          transformPat(prevBinder, patTree).zipped foreach traverseDepthFirst

      val res = new ListBuffer[ProtoTreeMaker]
      traverseDepthFirst(scrutSym, pattern)(res)
      res.toList
    }

    def Xguard(guard: Tree): List[ProtoTreeMaker] = {
      if (guard == EmptyTree) List()
      else List(
        (List(genGuard(guard)),
          { outerSubst =>
            val binder = freshSym(guard.pos, UnitClass.tpe)
            (nestedTree => genFun(binder, outerSubst(nestedTree)), outerSubst) // guard does not bind any variables, so next subst is the current one
          }))
    }

// tree exegesis, rephrasing everything in terms of extractors
    def extractorResultInMonad(extractorTp: Type): Type = {
      val res = extractorTp.finalResultType
      if(res.typeSymbol == BooleanClass) UnitClass.tpe
      else {
        val monadArgs = res.baseType(matchingMonadType.typeSymbol).typeArgs
        // assert(monadArgs.length == 1, "unhandled extractor type: "+ extractorTp) // TODO: overloaded unapply??
        if(monadArgs.length == 1) monadArgs(0)
        else ErrorType
      }
    }

    // require(patBinders.nonEmpty)
    def monadTypeToSubPatTypesAndRefs(typeInMonad: Type, isSeq: Boolean, subPats: List[Tree], subPatBinders: List[Symbol]): (List[Type], Symbol => List[Tree], (Symbol, Tree) => Tree) = {
      val nbSubPatBinders = subPatBinders.length
      val lastIsStar = subPats.nonEmpty && treeInfo.isStar(subPats.last)
      val nbSubPats = subPats.length

      val ts =
        if(typeInMonad.typeSymbol eq UnitClass) Nil
        else if(nbSubPatBinders == 1) List(typeInMonad)
        else getProductArgs(typeInMonad) getOrElse List(typeInMonad)

      // replace last type (of shape Seq[A]) with RepeatedParam[A] so that formalTypes will
      // repeat the last argument type to align the formals with the number of arguments
      val subPatTypes = if(isSeq) {
        val TypeRef(pre, SeqClass, args) = (ts.last baseType SeqClass)
        formalTypes(ts.init :+ typeRef(pre, RepeatedParamClass, args), nbSubPats)
      } else ts

      // println("subPatTypes (typeInMonad, isSeq, nbSubPats, ts, subPatTypes)= "+(typeInMonad, isSeq, nbSubPats, ts, subPatTypes))
      // only relevant if isSeq: (here to avoid capturing too much in the returned closure)
      val firstIndexingBinder = ts.length - 1 // ts.last is the Seq, thus there are `ts.length - 1` non-seq elements in the tuple
      val lastIndexingBinder = if(lastIsStar) nbSubPatBinders-2 else nbSubPatBinders-1
      def seqTree(binder: Symbol): Tree = if(firstIndexingBinder == 0) CODE.REF(binder) else genTupleSel(binder)(firstIndexingBinder+1)
      def seqLenCmp = ts.last member nme.lengthCompare

      def subPatRefs(binder: Symbol): List[Tree] =
        (if(isSeq) {
          ((1 to firstIndexingBinder) map genTupleSel(binder)) ++  // there are `firstIndexingBinder` non-seq tuple elements preceding the Seq
          ((firstIndexingBinder to lastIndexingBinder) map {i => genIndex(seqTree(binder))(i-firstIndexingBinder)}) ++  // then we have to index the binder that represents the sequence for the remaining subpatterns, except for...
          ((lastIndexingBinder+1 to nbSubPatBinders-1) map {n => if(n == 0) seqTree(binder) else genDrop(seqTree(binder))(n)}) // the last one -- if the last subpattern is a sequence wildcard: drop the prefix (indexed by the refs on the line above), return the remainder
        }
        else if(nbSubPatBinders == 1) List(CODE.REF(binder))
        else ((1 to nbSubPatBinders) map genTupleSel(binder))).toList

      // len may still be -1 even if isSeq
      val len = if(!isSeq) -1 else lastIndexingBinder - firstIndexingBinder + 1
      def unapplySeqLengthGuard(binder: Symbol, then: Tree) = { import CODE._
        // the comparison to perform.  If the pivot is right ignoring, then a scrutinee sequence
        // of >= pivot length could match it; otherwise it must be exactly equal.
        def compareOp: (Tree, Tree) => Tree = if (lastIsStar) _ INT_>= _ else _ INT_== _

        // scrutinee.lengthCompare(pivotLength) [== | >=] 0
        def lenOk                        = compareOp( (seqTree(binder) DOT seqLenCmp)(LIT(len)), ZERO )

        // wrapping in a null check on the scrutinee
        // only check if minimal length is non-trivially satisfied
        val minLenToCheck = if(lastIsStar) 1 else 0
        if (len >= minLenToCheck) IF ((seqTree(binder) ANY_!= NULL) AND lenOk) THEN then ELSE genZero
        else then
      }

      (subPatTypes, subPatRefs, unapplySeqLengthGuard)
    }

    // generate the tree for the run-time test that follows from the fact that
    // a `scrut` of known type `scrutTp` is expected to have type `expectedTp`
    def genTypeDirectedEquals(scrut: Symbol, scrutTp: Type, expectedTp: Type): Tree = { import CODE._
      def isMatchUnlessNull = scrutTp <:< expectedTp && (expectedTp <:< AnyRefClass.tpe)
      def isRef             = scrutTp <:< AnyRefClass.tpe
      def genEqualsAndInstanceOf(sym: Symbol): Tree
        = genEquals(scrut, REF(sym)) AND gen.mkIsInstanceOf(REF(scrut), expectedTp.widen, true, false)

      expectedTp match {
          case ConstantType(Constant(null)) if isRef  => REF(scrut) OBJ_EQ NULL
          case ConstantType(const)                    => genEquals(scrut, Literal(const))
// TODO: align with spec -- the cases below implement what the full pattern matcher does, not the spec
// A singleton type p.type. This type pattern matches only the value denoted by the path p
// [...] comparison of the matched value with p using method eq in class AnyRef
          case SingleType(_, sym) => assert(sym.isStable); genEqualsAndInstanceOf(sym) // pre is checked elsewhere (genOuterCheck)
          case ThisType(sym) if sym.isModule          => genEqualsAndInstanceOf(sym)
          case ThisType(sym)                          => REF(scrut) OBJ_EQ This(sym) // TODO: this matches the actual pattern matcher, but why not use equals as in the object case above? (see run/t576)
          case _ if isMatchUnlessNull                 => REF(scrut) OBJ_NE NULL
          case _                                      => gen.mkIsInstanceOf(REF(scrut), expectedTp, true, false)
        }
    }

    /** adds a test comparing the dynamic outer to the static outer */
    def genOuterCheck(scrut: Symbol, expectedTp: Type): Tree = { import CODE._
      val expectedPrefix = expectedTp.prefix match {
        case ThisType(clazz)  => THIS(clazz)
        case pre              => REF(pre.prefix, pre.termSymbol)
      }

      // Select(q, outerSym) is replaced by Select(q, outerAccessor(outerSym.owner)) in ExplicitOuter
      val outer = expectedTp.typeSymbol.newMethod("<outer>".toTermName) setInfo expectedTp.prefix setFlag SYNTHETIC
      (Select(gen.mkAsInstanceOf(REF(scrut), expectedTp, true, false), outer)) OBJ_EQ expectedPrefix
    }

    /** A conservative approximation of which patterns do not discern anything.
      * A corrolary of this is that they do not entail any variable binding.
      */
    def isWildcardPattern(pat: Tree): Boolean = pat match {
      case Bind(nme.WILDCARD, body) => isWildcardPattern(body) // don't skip when binding an interesting symbol!
      case Ident(nme.WILDCARD)  => true
      case Star(x)              => isWildcardPattern(x)
      case x: Ident             => treeInfo.isVarPattern(x)
      case Alternative(ps)      => ps forall isWildcardPattern
      case EmptyTree            => true
      case _                    => false
    }

    object BoundSym {
      def unapply(t: Tree): Option[(Symbol, Tree)] = t match {
        case t@Bind(n, p) if (t.symbol ne null) && (t.symbol ne NoSymbol) => // pos/t2429 does not satisfy these conditions
          Some((t.symbol, p))
        case _ => None
      }
    }


// code gen

    var ctr = 0
    def freshSym(pos: Position, tp: Type = NoType, prefix: String = "x") = {ctr += 1;
      // assert(owner ne null)
      // assert(owner ne NoSymbol)
      new TermSymbol(NoSymbol, pos, (prefix+ctr).toTermName) setInfo tp
    }

    // We must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed,
    // and will thus not be retyped. This means we might end up with untyped subtrees inside bigger, typed trees.
    def typedSubst(from: List[Symbol], to: List[Tree], unsafe: Boolean = false) = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(_) =>
          def subst(from: List[Symbol], to: List[Tree]): Tree =
            if (from.isEmpty) tree
            else if (tree.symbol == from.head) {
              if(tree.tpe != null && tree.tpe != NoType)
                // this whole "unsafe" business and the more precise pt are only for debugging (to detect iffy substitutions)
                // could in principle always assume unsafe and use pt = WildcardType
                if(overrideUnsafe || unsafe) typed(to.head.shallowDuplicate, EXPRmode, WildcardType)
                else silent(_.typed(to.head.shallowDuplicate, EXPRmode, tree.tpe.widen)) match {
                  case t: Tree => t
                  case ex: TypeError => // these should be relatively rare
                    // not necessarily a bug: e.g., in Node(_, md @ UnprefixedAttribute(_, _, _), _*),
                    // md.info == UnprefixedAttribute, whereas x._2 : MetaData
                    // (where x is the binder of the function that'll be flatMap'ed over Node's unapply;
                    //  the unapply has sig (x: Node) Option[(String, MetaData, Seq[Node])])
                    // (it's okay because doUnapply will insert a cast when x._2 is passed to the UnprefixedAttribute extractor)
                    // println("subst unsafely replacing "+ tree.symbol +": "+ tree.tpe.widen +" by "+ to.head +" in: "+ tree)
                    typed(to.head.shallowDuplicate, EXPRmode, WildcardType)
                }
              else
                to.head.shallowDuplicate
            }
            else subst(from.tail, to.tail);
          subst(from, to)
        case _ =>
          super.transform(tree)
      }
    }

    import CODE._
    // methods in MatchingStrategy (the monad companion)
    def genZero: Tree = matchingStrategy DOT "zero".toTermName                                          // matchingStrategy.zero
    def genOne(res: Tree): Tree = (matchingStrategy DOT "one".toTermName)(res)                          // matchingStrategy.one(res)
    def genOr(f: Tree, as: List[Tree]): Tree = (matchingStrategy DOT "or".toTermName)((f :: as): _*)    // matchingStrategy.or(f, as)
    def genGuard(t: Tree, then: Tree = UNIT): Tree = (matchingStrategy DOT "guard".toTermName)(t, then) // matchingStrategy.guard(t, then)
    def genRunOrElse(scrut: Tree, matcher: Tree): Tree = (matchingStrategy DOT "runOrElse".toTermName)(scrut) APPLY (matcher) // matchingStrategy.runOrElse(scrut)(matcher)
    def genCast(expectedTp: Type, binder: Symbol): Tree = genTypedGuard(gen.mkIsInstanceOf(REF(binder), expectedTp, true, false), expectedTp, binder) // TODO: use genTypeDirectedEquals(binder, binder.info.widen, expectedTp) instead of gen.mkIsInstanceOf?
    def genTypedGuard(cond: Tree, expectedTp: Type, binder: Symbol): Tree  = genGuard(cond, gen.mkAsInstanceOf(REF(binder), expectedTp, true, false))


    // methods in the monad instance
    def genFlatMap(a: Tree, b: Tree): Tree = (a DOT "flatMap".toTermName)(b)
    def genTypedOrElse(pt: Type)(thisCase: Tree, elseCase: Tree): Tree = (Typed(thisCase, TypeTree(pt)) DOT "orElse".toTermName)(Typed(elseCase, TypeTree(pt)))

    // misc
    def genApply(fun: Tree, arg: Symbol): Tree = fun APPLY REF(arg)
    def genSelect(tgt: Tree, mem: Symbol): Tree = tgt DOT mem
    def genFun(arg: Symbol, body: Tree): Tree = Function(List(ValDef(arg)), body)
    def genIndex(tgt: Tree)(i: Int): Tree = tgt APPLY (LIT(i))
    def genDrop(tgt: Tree)(n: Int): Tree = (tgt DOT "drop".toTermName) (LIT(n))
    def genTupleSel(binder: Symbol)(i: Int): Tree = (REF(binder) DOT ("_"+i).toTermName) // make tree that accesses the i'th component of the tuple referenced by binder
    def genEquals(binder: Symbol, checker: Tree): Tree = checker MEMBER_== REF(binder) // NOTE: checker must be the target of the ==, that's the patmat semantics for ya
  }
}