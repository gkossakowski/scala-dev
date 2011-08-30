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
  * (lifting the body of the case into the monad using `success`).
  *
  * Cases are combined into a pattern match using the `orElse` combinator (the implicit failure case is expressed using the monad's `fail`).
  *
  * The monad `M` in which the pattern match is interpreted is determined by solving `implicitly[MatchingStrategy[M]]` for M.
  * Predef provides the default, `Option`:

      implicit object OptionMatching extends MatchingStrategy[Option] {
        def fail: Option[Nothing] = None // TODO: throw new MatchError?
        def success[T](x: T): Option[T] = Some(x)
      }

  * Example translation: TODO

    scrut match { case Person(father@Person(_, fatherName), name) if fatherName == name => }
    scrut match { case Person(father, name) => father match {case Person(_, fatherName) => }}
    Person.unapply(scrut) >> ((father, name) => (Person.unapply(father) >> (_, fatherName) => check(fatherName == name) >> (_ => body)))

    (a => (Person.unapply(a).>>(
      b => Person.unapply(b._1).>>(
        c => check(c._2 == b._2).>>(
          d => body)))))(scrut)


  * (longer-term) TODO:
  *  - recover exhaustivity and unreachability checking using a variation on the type-safe builder pattern
  *  - recover GADT typing by locally inserting implicit witnesses to type equalities derived from the current case, and considering these witnesses during subtyping (?)
  */
trait PatMatVirtualiser extends ast.TreeDSL { self: Analyzer =>
  import global._
  import definitions._

  // TODO handle nested matches:  a match { case p => b match { case p => ... } }
  // note that the matching strategy may change -- need to defer to type checker to set up context
  def typedMatch(typer: Typer, tree: Tree, selector: Tree, cases: List[CaseDef], mode: Int, pt: Type): Tree = {
    import typer._
    def solveImplicit(contextBoundTp: Type, tree: Tree): (Tree, Type) = {
      val solSym = NoSymbol.newTypeParameter(tree.pos, "SolveImplicit$".toTypeName)
      val param = solSym.setInfo(contextBoundTp.typeSymbol.typeParams(0).info.cloneInfo(solSym)) // TypeBounds(NothingClass.typeConstructor, baseTp)
      val pt = appliedType(contextBoundTp, List(param.tpeHK))
      val savedUndets = context.undetparams
      context.undetparams = param :: context.undetparams
      val result = inferImplicit(tree, pt, false, false, context)
      context.undetparams = savedUndets

      (result.tree, result.subst.to(result.subst.from indexOf param))
    }

    val matchingStrategyTycon = getMember(PredefModule, "MatchingStrategy".toTypeName).typeConstructor
    val (matchingStrategy, matchingMonadType) = solveImplicit(matchingStrategyTycon, tree)

    val selector1 = typed(selector, EXPRmode | BYVALmode, WildcardType) // TODO: handle empty selector (just remove outer apply node from xTree?)
    val xTree = new MatchTranslator(typer, matchingStrategy, matchingMonadType).X(treeCopy.Match(tree, selector1, typedCases(tree, cases, selector1.tpe.widen, pt)))
    // println("xformed patmat: "+ xTree)

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
          case Function(_, _) => 
            if (t.symbol == NoSymbol) {
              t.symbol = currentOwner.newValue(t.pos, nme.ANON_FUN_NAME).setFlag(SYNTHETIC).setInfo(NoType)
              // println("new symbol for "+ (t, t.symbol.ownerChain))
            }
          case d : DefTree => 
            // println("def: "+ (d, d.symbol.ownerChain, currentOwner.ownerChain))
            assert(d.symbol.owner == NoSymbol || currentOwner.hasTransOwner(d.symbol.owner)) // don't uproot anything, goal is to nest deeper
            d.symbol.owner = currentOwner
          case _ => 
        }
        super.traverse(t)
      }
    }
    
    fixerUpper(xTree) // atPos(tree.pos)(xTree) does not achieve the same effect
    // printTypings = true
    val res = typed(xTree, mode, pt)
    // printTypings = false
    res
  }

  private class MatchTranslator(typer: Typer, matchStrategy: Tree, matchingMonadType: Type) { translator =>
    import typer._
    import typeDebug.{ ptTree, ptBlock, ptLine }

    /** Implement a pattern match by turning its cases (including the implicit failure case)
      * into the corresponding (monadic) extractors, and combining them with the `orElse` combinator.
      *
      * For `scrutinee match { case1 ... caseN }`, the resulting tree has the shape
      * `runOrElse(scrutinee, x => Xcase1(x).orElse(Xcase2(x)).....orElse(zero))`
      */
    def X(tree: Tree): Tree = {
      tree match {
        case Match(scrut, cases) =>
          val scrutSym = freshSym(tree.pos) setInfo scrut.tpe.widen // TODO: deal with scrut == EmptyTree
          mkRunOrElse(scrut,
                      mkFun(scrutSym,
                            ((cases map Xcase(scrutSym)) ++ List(mkZero)) reduceLeft mkOrElse))
        case t => t
      }
    }

    type TreeXForm = Tree => Tree
    type ProtoTreeMaker = (List[Tree], TreeXForm => (TreeXForm /* wrap a Fun and subst variables to tuple sel on variable bound by that Fun */, TreeXForm /* just the subst */))

    object TreeMaker {
      def apply(trees: List[Tree], mkFunAndSubst0: TreeXForm): TreeMaker =
        if (trees isEmpty) new NoTreeMaker{def mkFunAndSubst(next: Tree) = mkFunAndSubst0(next)}
        else if (trees.length == 1) new SingleTreeMaker(trees.head){def mkFunAndSubst(next: Tree) = mkFunAndSubst0(next)}
        else new AlternativeTreeMaker(trees){def mkFunAndSubst(next: Tree) = mkFunAndSubst0(next)}
    }
    abstract class TreeMaker {
      // wrap a Fun (with binder x) around the next tree and do aggregated substitution (which
      // replaces old pattern bindings by the appropriate tuple element selection on the new binders,
      // that is, `x`, if it was bound by the immediately enclosing pattern)
      def mkFunAndSubst(next: Tree): Tree

      // build Tree that chains `next` after the current extractor
      def mkFlatMap(next: Tree): Tree
    }

    abstract class NoTreeMaker extends TreeMaker {
      def mkFlatMap(tree: Tree) = mkFunAndSubst(tree) // doesn't make a fun, only does substitution
    }

    abstract class SingleTreeMaker(extractor: Tree) extends TreeMaker {
      def mkFlatMap(tree: Tree) =
        if(tree == EmptyTree) extractor  // when combining treemakers in an alternative
        else translator.mkFlatMap(extractor, mkFunAndSubst(tree)) setPos extractor.pos
    }

    abstract class AlternativeTreeMaker(alts: List[Tree]) extends TreeMaker {
      def mkFlatMap(tree: Tree) = mkOr(alts, mkFunAndSubst(tree)) setPos alts.head.pos
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
          threadSubstitution(Xpat(scrutSym)(pattern) ++ Xguard(guard))._1.foldRight(mkOne(X(body)))(_ mkFlatMap _) setPos tree.pos
          // TODO: if we want to support a generalisation of Kotlin's patmat continue, must not hard-wire lifting into the monad (mkOne), so that user can generate failure when needed -- use implicit conversion to lift into monad on-demand
      }
    }

    def Xpat(scrutSym: Symbol)(pattern: Tree): List[ProtoTreeMaker] = {
      def doUnapply(args: List[Tree], extractorCall: Tree, prevBinder: Symbol, pos: Position)(implicit res: ListBuffer[ProtoTreeMaker]): (List[Symbol], List[Tree]) = {
        assert((extractorCall.tpe ne null) && (extractorCall.tpe ne NoType) && (extractorCall.tpe ne ErrorType), "args: "+ args +" extractorCall: "+ extractorCall) 
        val extractorType = extractorCall.tpe
        val isSeq = extractorCall.symbol.name == nme.unapplySeq

        // what's the extractor's result type in the monad?
        val typeInMonad = extractorResultInMonad(extractorType)

        // the types for the binders corresponding to my subpatterns
        // subPatTypes != args map (_.tpe) since the args may have more specific types than the constructor's parameter types
        val subPatTypes = monadTypeToSubPatTypes(typeInMonad, isSeq, args.length)

        // `patBinders` are the variables bound by this pattern in the following patterns
        // patBinders are replaced by references to the relevant part of the extractor's result (tuple component, seq element, the result as-is)
        val sub@(patBinders, _) =
            (args, subPatTypes).zipped map {
              case (BoundSym(b, p), tp) =>
                // must use type `tp`, which is provided by extractor's result, not the type expected by binder,
                // as b.info may be based on a Typed type ascription, which has not been taken into account yet by the translation
                // (it will later result in a type test when `tp` is not a subtype of `b.info`)
                (b setInfo tp, p) 
              case (p, tp) => (freshSym(pos, "p") setInfo tp, p)
            } unzip

        val untupledSolo = (patBinders.length == 1) && getProductArgs(typeInMonad).isEmpty // typeInMonad is not a tuple

        val extractorParamType = extractorType.paramTypes.head

        // println("doUnapply (subPatTypes, typeInMonad, prevBinder.info.widen, extractorParamType, prevBinder.info.widen <:< extractorParamType) =\n"+(subPatTypes, typeInMonad, prevBinder.info.widen, extractorParamType, prevBinder.info.widen <:< extractorParamType))

        // example check: List[Int] <:< ::[Int]
        val prevBinderOrCasted =
          if(!(prevBinder.info.widen <:< extractorParamType)) {
            val castedBinder = freshSym(pos, "cp") setInfo extractorParamType
            val castTree = mkCast(extractorParamType, prevBinder) // cast (with failure expressed using the monad)
            // chain a cast before the actual extractor call
            // we control which binder is used in the nested tree (patTree -- created below), so no need to substitute
            res += (List(castTree),
                      { outerSubst: TreeXForm =>
                          (nestedTree => mkFun(castedBinder,outerSubst(nestedTree)), outerSubst)
                      })

            castedBinder
          } else prevBinder


        /* TODO: check length of sequence for an unapplySeq
          if(isSeq) {
            if(isSequenceWildCard(args.last)) {
              if (args.length > 1) // avoid redundent check: length is always > 0
                res += Xguard(`binder`.length >= args.length - 1)
            } else
              res += Xguard(`binder`.length == args.length)
          }*/


        // the extractor call (applied to the binder bound by the flatMap corresponding to the previous (i.e., enclosing/outer) pattern)
        val extractorApply = atPos(pos)(mkApply(extractorCall, prevBinderOrCasted))
        val patTree =
          if(extractorType.finalResultType.typeSymbol == BooleanClass) mkGuard(extractorApply)
          else extractorApply

        // println("patTree= "+ patTree)

        res += Pair(List(patTree),
          if(patBinders isEmpty)
            { outerSubst: TreeXForm =>
                val binder = freshSym(patTree.pos) setInfo UnitClass.tpe
                (nestedTree => mkFun(binder, outerSubst(nestedTree)), outerSubst)
            }
          else
            { outerSubst: TreeXForm =>
                val binder = freshSym(patTree.pos) setInfo typeInMonad
                val theSubst = mkTypedSubst(patBinders, subPatRefs(binder, patBinders, isSeq, untupledSolo))
                def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                (nestedTree => mkFun(binder, nextSubst(nestedTree)), nextSubst)
            })

        sub
      }

      def singleBinderProtoTreeMaker(binderToSubst: Symbol, patTrees: Tree*): ProtoTreeMaker = singleBinderProtoTreeMakerWithTp(binderToSubst, binderToSubst.info.widen, false, patTrees : _*)
      def singleBinderProtoTreeMakerWithTp(binderToSubst: Symbol, binderType: Type, unsafe: Boolean, patTrees: Tree*): ProtoTreeMaker = {
        assert(patTrees.head.pos != NoPosition, "proto-tree for "+(binderToSubst, patTrees.toList))

        (patTrees.toList,
            { outerSubst: TreeXForm =>
                val binder = freshSym(patTrees.head.pos) setInfo binderType
                val theSubst = mkTypedSubst(List(binderToSubst), List(CODE.REF(binder)), unsafe)
                // println("theSubst: "+ theSubst)
                def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                (nestedTree => mkFun(binder, nextSubst(nestedTree)), nextSubst)
            })
      }

      /** Decompose the pattern in `tree`, of shape C(p_1, ..., p_N), into a list of N symbols, and a list of its N sub-trees
        * The list of N symbols contains symbols for every bound name as well as the un-named sub-patterns (fresh symbols are generated here for these)
        *
        * @arg prevBinder  symbol used to refer to the result of the previous pattern's extractor (will later be replaced by the outer tree with the correct tree to refer to that patterns result)
        */
      def transformPat(prevBinder: Symbol, patTree: Tree)(implicit res: ListBuffer[ProtoTreeMaker]): (List[Symbol], List[Tree]) = patTree match {
        case UnApply(Apply(unfun, unargs), args) =>
        // TODO: check unargs == args
          // println("unfun: "+ (unfun.tpe, unfun.symbol.ownerChain, unfun.symbol.info, prevBinder.info))

          doUnapply(args, unfun, prevBinder, patTree.pos)

        case Apply(fun, args)     =>
          val orig = fun.asInstanceOf[TypeTree].original
          val origSym = orig.symbol // undo rewrite performed in (5) of adapt
          val extractor = unapplyMember(origSym.filter(sym => reallyExists(unapplyMember(sym.tpe))).tpe)
          val extractorCall = typed(mkSelect(orig, extractor), FUNmode, WildcardType) // mkSelect(orig, extractor) setType caseClassApplyToUnapplyTp(fun.tpe)
          // println("orig: "+ (orig, extractor, mkSelect(orig, extractor), typed(mkSelect(orig, extractor), FUNmode, WildcardType).tpe, caseClassApplyToUnapplyTp(fun.tpe)))
          // println("apply extractor: "+ (extractor, extractorCall.tpe, fun.tpe, fun.asInstanceOf[TypeTree].original.tpe, origSym))

          doUnapply(args, extractorCall, prevBinder, patTree.pos)


        case BoundSym(patBinder, p)          =>
          // don't generate an extractor, TreeMaker only performs the substitution patBinder --> prevBinder
          res += (List(),
                    { outerSubst: TreeXForm =>
                        val theSubst = mkTypedSubst(List(patBinder), List(CODE.REF(prevBinder)), unsafe = true)
                        def nextSubst(tree: Tree): Tree = outerSubst(theSubst.transform(tree))
                        (nestedTree => nextSubst(nestedTree), nextSubst)
                    })
          // the symbols are markers that may be used to refer to the result of the extractor in which the corresponding tree is nested
          // it's the responsibility of the treemaker (added to res in the previous line) to replace this symbol by a reference that
          // selects that result on the function symbol of the flatMap call that binds to the result of this extractor
          (List(patBinder), List(p))

        case Typed(expr, tpt)                 =>
          // println("Typed: expr is wildcard, right? "+ (expr, tpt.tpe, prevBinder, prevBinder.info))
          res += singleBinderProtoTreeMakerWithTp(prevBinder, tpt.tpe, unsafe = true, atPos(patTree.pos)(mkCast(tpt.tpe, prevBinder)))

          (Nil, Nil) // a typed pattern never has any subtrees

        case Literal(Constant(_)) | Ident(_) | Select(_, _) =>
          res += singleBinderProtoTreeMaker(prevBinder, atPos(patTree.pos)(
                    mkGuard(mkEquals(prevBinder, patTree),
                            CODE.REF(prevBinder) setType prevBinder.info)))

          (Nil, Nil)

        case Alternative(alts)    =>
          val altTrees = alts map { alt =>
            // one alternative may still generate multiple trees (e.g., an extractor call + equality test)
            val resAlts = new ListBuffer[ProtoTreeMaker]
            traverseDepthFirst(prevBinder, alt)(resAlts)

            // currently we ignore subst, since alternatives may not bind variables (except wildcards)
            val (treeMakers, subst) = threadSubstitution(resAlts.toList)

            atPos(alt.pos)(treeMakers.foldRight (EmptyTree: Tree) (_ mkFlatMap _)) // EmptyTree is treated specially in mkFlatMap in SingleTreeMaker (fuses flatMap'ing the identity)
          }

          res += singleBinderProtoTreeMaker(prevBinder, altTrees : _*)

      /* TODO: Paul says about future version: I think this should work, and always intended to implement if I can get away with it.
          case class Foo(x: Int, y: String)
          case class Bar(z: Int)

          def f(x: Any) = x match { case Foo(x, _) | Bar(x) => x } // x is lub of course.
      */

          (Nil, Nil)

        // case x: ArrayValue        => // SequencePattern(x)
        // case x: Star              =>
        // case x: This              =>

        case _                       =>  // TODO
          println("UNHANDLED: "+ (prevBinder, patTree))
          (Nil, Nil)
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
        (List(mkGuard(X(guard))),
          { outerSubst =>
            val binder = freshSym(guard.pos) setInfo UnitClass.tpe
            (nestedTree => mkFun(binder, outerSubst(nestedTree)), outerSubst) // guard does not bind any variables, so next subst is the current one
          }))
    }

// tree exegesis, rephrasing everything in terms of extractors

    def extractorResultInMonad(extractorTp: Type): Type = {
      val res = extractorTp.finalResultType
      if(res.typeSymbol == BooleanClass) UnitClass.tpe
      else {
        val monadArgs = res.baseType(matchingMonadType.typeSymbol).typeArgs
        if(monadArgs.length == 1) monadArgs(0)
        else {println("unhandled extractor: "+ extractorTp); NoType}
      }
    }

    def monadTypeToSubPatTypes(typeInMonad: Type, isSeq: Boolean, nbArgs: Int): List[Type] = {
      val ts = 
        if(typeInMonad.typeSymbol eq UnitClass) Nil
        else getProductArgs(typeInMonad) getOrElse List(typeInMonad)

      // replace last type (of shape Seq[A]) with RepeatedParam[A] so that formalTypes will
      // repeat the last argument type to align the formals with the number of arguments
      if(isSeq) { 
        val TypeRef(pre, SeqClass, args) = (ts.last baseType SeqClass) 
        formalTypes(ts.init :+ typeRef(pre, RepeatedParamClass, args), nbArgs)
      } else ts
    }

    // require(patBinders.nonEmpty)
    def subPatRefs(binder: Symbol, patBinders: List[Symbol], isSeq: Boolean, untupledSolo: Boolean): List[Tree] =
      if(isSeq) ((0 to patBinders.length-1) map mkIndex(binder)).toList
      else if(untupledSolo) List(CODE.REF(binder))
      else ((1 to patBinders.length) map mkTupleSel(binder)).toList
      // else List() -- never called when patBinders are empty

    // not needed: now typing the unapply call
    // inverse of monadTypeToSubPatTypes(extractorResultInMonad(_))
    // minus the padding of repeated args for unapplySeq (TODO?)
    // def subPatTypesToExtractorResultType(tps: List[Type]): Type = {
    //   tps match {
    //     case Nil => BooleanClass.tpe
    //     case List(x) => optionType(x)
    //     case xs => optionType(tupleType(xs))
    //   }
    // }
    // def caseClassApplyToUnapplyTp(tp: Type) = {
    //   val dummyMethod = new TermSymbol(NoSymbol, NoPosition, "$dummy")
    //   val resTp = subPatTypesToExtractorResultType(tp.paramTypes)
    //   MethodType(List(dummyMethod newSyntheticValueParam(tp.finalResultType)), if(tp.paramTypes nonEmpty) resTp else BooleanClass.tpe)
    // }

    /** A conservative approximation of which patterns do not discern anything.
      * A corrolary of this is that they do not entail any variable binding.
      */
    def isWildcardPattern(pat: Tree): Boolean = pat match {
      case Bind(_, body)        => isWildcardPattern(body)
      case Ident(nme.WILDCARD)  => true
      case x: Ident             => treeInfo.isVarPattern(x)
      case Alternative(ps)      => ps forall isWildcardPattern
      case EmptyTree            => true
      case _                    => false
    }

    object BoundSym {
      def unapply(t: Tree): Option[(Symbol, Tree)] = t match {
        case t@Bind(n, p) =>
          val b = t.symbol
          assert(b ne null, t); assert(b ne NoSymbol, t)
          Some((b, p))
        case _ => None
      }
    }


// code gen

    var ctr = 0
    def freshSym(pos: Position, prefix: String = "x") = {ctr += 1;
      // assert(owner ne null)
      // assert(owner ne NoSymbol)
      new TermSymbol(NoSymbol, pos, (prefix+ctr).toTermName)
    }

    // we must explicitly type the trees that we replace inside some other tree, since the latter may already have been typed, and will thus not be retyped
    // thus, we might end up with untyped subtrees inside bigger, typed trees
    def mkTypedSubst(from: List[Symbol], to: List[Tree], unsafe: Boolean = false) = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(_) =>
          def subst(from: List[Symbol], to: List[Tree]): Tree =
            if (from.isEmpty) tree
            else if (tree.symbol == from.head) {
              if(tree.tpe != null && tree.tpe != NoType) typed(to.head.shallowDuplicate, EXPRmode, if(unsafe) WildcardType else tree.tpe.widen)
              else to.head.shallowDuplicate
            }
            else subst(from.tail, to.tail);
          subst(from, to)
        case _ =>
          super.transform(tree)
      }
    }

    import CODE._
    // methods in MatchingStrategy (the monad companion)
    def mkZero: Tree = matchStrategy DOT "zero".toTermName                                          // matchStrategy.zero
    def mkOne(res: Tree): Tree = (matchStrategy DOT "one".toTermName)(res)                          // matchStrategy.one(res)
    def mkOr(as: List[Tree], f: Tree): Tree = (matchStrategy DOT "or".toTermName)((f :: as): _*)    // matchStrategy.or(f, as)
    def mkGuard(t: Tree, then: Tree = UNIT): Tree = (matchStrategy DOT "guard".toTermName)(t, then) // matchStrategy.guard(t, then)
    def mkRunOrElse(scrut: Tree, matcher: Tree): Tree = (matchStrategy DOT "runOrElse".toTermName)(scrut) APPLY (matcher) // matchStrategy.runOrElse(scrut)(matcher)
    def mkCast(expectedTp: Type, binder: Symbol): Tree =
      mkGuard(gen.mkIsInstanceOf(REF(binder), expectedTp, true, false), gen.mkAsInstanceOf(REF(binder), expectedTp, true, false))


    // methods in the monad instance
    def mkFlatMap(a: Tree, b: Tree): Tree = (a DOT "flatMap".toTermName)(b)
    def mkOrElse(thisCase: Tree, elseCase: Tree): Tree = (thisCase DOT "orElse".toTermName)(elseCase)

    // misc
    def mkApply(fun: Tree, arg: Symbol): Tree = fun APPLY REF(arg)
    def mkSelect(tgt: Tree, mem: Symbol): Tree = tgt DOT mem
    def mkFun(arg: Symbol, body: Tree): Tree = Function(List(ValDef(arg)), body)
    def mkIndex(binder: Symbol)(i: Int): Tree = REF(binder) APPLY (LIT(i))
    def mkTupleSel(binder: Symbol)(i: Int): Tree = (REF(binder) DOT ("_"+i).toTermName) // make tree that accesses the i'th component of the tuple referenced by binder
    def mkEquals(binder: Symbol, other: Tree): Tree = REF(binder) MEMBER_== other
  }
}