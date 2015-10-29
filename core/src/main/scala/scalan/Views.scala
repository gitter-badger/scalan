package scalan

import scala.language.higherKinds
import scala.collection.mutable.{Map => MutMap}
import scala.reflect.ClassTag
import scalan.common.Lazy
import scalan.meta.ScalanAst.STraitOrClassDef
import scalan.staged.BaseExp

trait Views extends Elems { self: ViewsDsl with Scalan =>
  type Iso[From, To] = Rep[Iso0[From, To]]

  // eFrom0 is used to avoid making eFrom implicit in subtypes
  // and support recursive types
  trait Iso0[From, To]/*(implicit eFrom0: Elem[From])*/ extends Def[Iso0[From, To]] {
    def eFrom: Elem[From]
    def eTo: Elem[To]
    def from(p: Rep[To]): Rep[From]
    def to(p: Rep[From]): Rep[To]
    override def toString = s"${eFrom.name} <-> ${eTo.name}"
    override def equals(other: Any) = other match {
      case i: Iso0[_, _] => (this eq i) || (eFrom == i.eFrom && eTo == i.eTo)
      case _ => false
    }
    override def canEqual(other: Any) = other.isInstanceOf[Iso0[_, _]]
    override lazy val hashCode = 41 * eFrom.hashCode + eTo.hashCode
    def isIdentity: Boolean = false
    lazy val fromFun = fun { x: Rep[To] => from(x) }(Lazy(eTo), eFrom)
    lazy val toFun = fun { x: Rep[From] => to(x) }(Lazy(eFrom), eTo)

    //    if (isDebug) {
    //      debug$IsoCounter(this) += 1
    //    }
  }

  abstract class IdentityIso0[A](implicit eA: Elem[A]) extends Iso0[A, A]/*()(eTo)*/ {
    def eFrom: Elem[A] = eA
    def eTo: Elem[A] = eA
    def from(x: Rep[A]) = x
    def to(x: Rep[A]) = x
    override def isIdentity = true
  }

  // TODO we can get eA1 etc. from iso1 and iso2, but this won't work as default arguments
  // because this creates a compiler-generated companion object and conflicts with `def PairIso0`
  // in ViewsImpl.scala
  abstract class PairIso0[A1, A2, B1, B2](val iso1: Iso[A1, B1], val iso2: Iso[A2, B2])(
    implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends Iso0[(A1, A2), (B1, B2)]/*()(pairElement(iso1.eFrom, iso2.eFrom))*/ {
//    implicit def eA1 = iso1.eFrom
//    implicit def eA2 = iso2.eFrom
//    implicit def eB1 = iso1.eTo
//    implicit def eB2 = iso2.eTo
    lazy val eFrom: Elem[(A1, A2)] = element[(A1, A2)]
    lazy val eTo: Elem[(B1, B2)] = element[(B1, B2)]

    var fromCacheKey:Option[Rep[(B1,B2)]] = None
    var fromCacheValue:Option[Rep[(A1,A2)]] = None
    var toCacheKey:Option[Rep[(A1,A2)]] = None
    var toCacheValue:Option[Rep[(B1,B2)]] = None

    def from(b: Rep[(B1, B2)]) = {
      if (fromCacheKey.isEmpty || b != fromCacheKey.get) {
        fromCacheKey = Some(b)
        fromCacheValue = Some((iso1.from(b._1), iso2.from(b._2)))
      }
      fromCacheValue.get
    }
    def to(a: Rep[(A1, A2)]) = {
      if (toCacheKey.isEmpty || a != toCacheKey.get) {
        toCacheKey = Some(a)
        toCacheValue = Some((iso1.to(a._1), iso2.to(a._2)))
      }
      toCacheValue.get
    }
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
  }
  trait PairIso0Companion

  abstract class SumIso0[A1, A2, B1, B2](val iso1: Iso[A1, B1], val iso2: Iso[A2, B2])(
    implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends Iso0[A1 | A2, B1 | B2]/*()(sumElement(iso1.eFrom, iso2.eFrom))*/ {
    lazy val eFrom: Elem[A1 | A2] = element[A1 | A2]
    lazy val eTo: Elem[B1 | B2] = element[B1 | B2]
    def from(b: Rep[B1 | B2]) =
      b.mapSumBy(iso1.fromFun, iso2.fromFun)
    def to(a: Rep[A1 | A2]) =
      a.mapSumBy(iso1.toFun, iso2.toFun)
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
  }

  abstract class ComposeIso0[A, B, C](val iso2: Iso[B, C], val iso1: Iso[A, B])(
    implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C]) extends Iso0[A, C]/*()(iso1.eFrom)*/ {
    def eFrom: Elem[A] = iso1.eFrom
    def eTo: Elem[C] = iso2.eTo
    def from(c: Rep[C]) = iso1.from(iso2.from(c))
    def to(a: Rep[A]) = iso2.to(iso1.to(a))
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
  }

  abstract class FuncIso0[A, B, C, D](val iso1: Iso[A, B], val iso2: Iso[C, D])(
    implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C], val eD: Elem[D])
    extends Iso0[A => C, B => D]/*()(funcElement(iso1.eFrom, iso2.eFrom))*/ {
    lazy val eFrom: Elem[A => C] = element[A => C]
    lazy val eTo: Elem[B => D] = element[B => D]
    def from(f: Rep[B => D]): Rep[A => C] = {
      fun { b => iso2.from(f(iso1.to(b))) }
    }
    def to(f: Rep[A => C]): Rep[B => D] = {
      fun { a => iso2.to(f(iso1.from(a))) }
    }
    override def isIdentity = iso1.isIdentity && iso2.isIdentity
  }

  abstract class ConverterIso0[A, B](val convTo: Conv[A,B], val convFrom: Conv[B,A])(
    implicit val eA: Elem[A], val eB: Elem[B])
    extends Iso0[A,B]/*()(convTo.eT)*/ {
    def eFrom: Elem[A] = eA
    def eTo: Elem[B] = eB
    def to(a: Rep[A]) = convTo(a)
    def from(b: Rep[B]) = convFrom(b)
    override lazy val toFun = convTo.convFun
    override lazy val fromFun = convFrom.convFun
    override def isIdentity = false
  }

  type Iso1[A, B, C[_]] = Rep[Iso10[A, B, C]]

  trait Iso10[A, B, C[_]]
    extends Iso0[C[A], C[B]]/*()(cC.lift(innerIso.eFrom))*/ {
    def innerIso: Iso[A, B]
    implicit def cC: Container[C]
    implicit def eA: Elem[A] = innerIso.eFrom
    implicit def eB: Elem[B] = innerIso.eTo
    lazy val eFrom: Elem[C[A]] = cC.lift(innerIso.eFrom)
    lazy val eTo: Elem[C[B]] = cC.lift(innerIso.eTo)
    override def isIdentity = innerIso.isIdentity
  }

  // TODO currently need implicit args for ArrayIsoElem to be generated, should be removed
  abstract class ArrayIso[A,B](innerIso: Iso[A,B])(implicit override val eA: Elem[A], override val eB: Elem[B]) extends Iso10[A, B, Array] {
    def cC = container[Array]
    def from(x: Arr[B]) = x.mapBy(innerIso.fromFun)
    def to(x: Arr[A]) = x.mapBy(innerIso.toFun)
  }
  
}

trait ViewsDsl extends impl.ViewsAbs { self: Scalan =>

  implicit def viewElement[From, To](implicit iso: Iso[From, To]): Elem[To] = iso.eTo // always ask elem from Iso0

  trait ViewElem[From, To] extends Elem[To] { _: scala.Equals =>
    def iso: Iso[From, To]
    override def isEntityType = shouldUnpack(this)
  }

  object ViewElem {
    def unapply[From, To](ve: ViewElem[From, To]): Option[Iso0[From, To]] = Some(ve.iso)
  }

  trait ViewElem1[A,From,To,C[_]] extends ViewElem[From, To] { _: scala.Equals =>
    def eItem: Elem[A]
    def cont: Cont[C]
  }

  class ConcreteIso0Elem[From, To, IsoType <: Iso0[From, To]](_eFrom: Elem[From], _eTo: Elem[To]) extends Iso0Elem[From, To, IsoType]()(_eFrom, _eTo)

  object UnpackableElem {
    def unapply(e: Elem[_]) =
      if (shouldUnpack(e)) {
        val iso = getIsoByElem(e)
        if (iso.isIdentity)
          None
        else
          Some(iso)
      }
      else None
  }

  def shouldUnpack(e: Elem[_]): Boolean

  private[this] val isoCache = MutMap.empty[Elem[_], Iso[_, _]]

  def getIsoByElem[T](e: Elem[T]): Iso[_, T] = isoCache.getOrElseUpdate(e,
    e match {
      case ve: ViewElem[_,_] =>
        val eFrom = ve.iso.eFrom
        val deepIso = getIsoByElem(eFrom)
        if (deepIso.isIdentity)
          ve.iso
        else
          composeIso(ve.iso, deepIso)
      case pe: PairElem[a,b] =>
        val iso1 = getIsoByElem(pe.eFst)
        val iso2 = getIsoByElem(pe.eSnd)
        pairIso(iso1,iso2)
      case pe: SumElem[a,b] =>
        val iso1 = getIsoByElem(pe.eLeft)
        val iso2 = getIsoByElem(pe.eRight)
        sumIso(iso1,iso2)
      case fe: FuncElem[a,b] =>
        val iso1 = getIsoByElem(fe.eDom)
        val iso2 = getIsoByElem(fe.eRange)
        funcIso(iso1,iso2)
//      case ae: ArrayElem[_] =>
//        val iso0 = getIsoByElem(ae.eItem)
//        arrayIso0(iso0)
//      case ae: ListElem[_] =>
//        val iso0 = getIsoByElem(ae.eItem)
//        listIso0(iso0)
//      case ae: ArrayBufferElem[_] =>
//        val iso0 = getIsoByElem(ae.eItem)
//        arrayBufferIso0(iso0)
//      case ae: ThunkElem[_] =>
//        val iso0 = getIsoByElem(ae.eItem)
//        thunkIso0(iso0)
      case me: MMapElem[_,_] =>
        identityIso(me)

      case we: WrapperElem1[a, Def[ext], cbase, cw] @unchecked =>
        val eExt = we.eTo
        val iso = getIsoByElem(eExt)
        iso

      case we: WrapperElem[Def[base],Def[ext]] @unchecked =>
        val eExt = we.eTo
        val iso = getIsoByElem(eExt)
        iso

      //    case ee1: EntityElem1[_,_,_] =>
      //      val iso0 = getIsoByElem(ee1.eItem)
      //      TODO implement using ContainerIso0

      case ee: EntityElem[_] =>
        identityIso(ee)
      case be: BaseElem[_] =>
        identityIso(be)
      case _ => !!!(s"Don't know how to build iso0 for element $e")
    }
  ).asInstanceOf[Iso[_, T]]

  def identityIso[A](implicit elem: Elem[A]): Iso[A, A] = IdentityIso0[A]()(elem)

  def pairIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[(A1, A2), (B1, B2)] =
    PairIso0[A1, A2, B1, B2](iso1, iso2)(iso1.eFrom, iso2.eFrom, iso1.eTo, iso2.eTo)

  def sumIso[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2]): Iso[A1 | A2, B1 | B2] =
    ???
//    cachedIso[SumIso0[A1, A2, B1, B2]](iso1, iso2)

  def composeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]): Iso[A, C] = {
    ???
//    ((iso2, iso1) match {
//        // TODO uncomment and make it compile
////      case (IdentityIso0Matcher(_), _) => iso1
////      case (_, IdentityIso0Matcher(_)) => iso2
////      case (PairIso0Matcher(iso21, iso22), PairIso0Matcher(iso11, iso12)) =>
////        pairIso(composeIso(iso21, iso11), composeIso(iso22, iso12))
//      case _ => cachedIso[ComposeIso0[A, B, C]](iso2, iso1)
//    }).asInstanceOf[Iso0[A, C]]
  }

  def funcIso[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D]): Iso[A => C, B => D] =
    ??? // cachedIso[FuncIso0[A, B, C, D]](iso1, iso2)

  def converterIso[A, B](convTo: Conv[A,B], convFrom: Conv[B,A]): Iso[A,B] =
    ??? // cachedIso[ConverterIso0[A, B]](convTo.asInstanceOf[AnyRef], convFrom.asInstanceOf[AnyRef])

  def convertBeforeIso[A, B, C](convTo: Conv[A,B], convFrom: Conv[B,A], iso0: Iso[B,C]): Iso[A, C] = composeIso(iso0, converterIso(convTo, convFrom))

  def convertAfterIso[A,B,C](iso0: Iso[A,B], convTo: Conv[B,C], convFrom: Conv[C,B]): Iso[A, C] = composeIso(converterIso(convTo, convFrom), iso0)

  def unifyIsos[A,B,C,D](iso1: Iso[A,C], iso2: Iso[B,D],
                         toD: Conv[C,D], toC: Conv[D,C]): (Iso[A,C], Iso[B,C]) = {
    val ea = iso1.eFrom
    val eb = iso2.eFrom
    implicit val ec = iso1.eTo
    val (i1, i2) =
      if (ec == iso2.eTo)
        (iso1, iso2.asInstanceOf[Iso[B,C]])
      else
        (iso1, convertAfterIso(iso2, toC, toD))
    (i1, i2)
  }
}

trait ViewsDslSeq extends impl.ViewsSeq { self: ScalanSeq =>
  def shouldUnpack(e: Elem[_]) = true
}

trait ViewsDslExp extends impl.ViewsExp with BaseExp { self: ScalanExp =>
  type Unpacked[T] = (Rep[Source], Iso[Source, T]) forSome { type Source }
  type UnpackedLambdaResult[T,R] = (Rep[T => R], Iso[Source, R]) forSome { type Source }

  type UnpackTester = Element[_] => Boolean

  private var unpackTesters: Set[UnpackTester] = Set.empty

  def addUnpackTester(tester: UnpackTester): Unit =
    unpackTesters += tester
  def removeUnpackTester(tester: UnpackTester): Unit =
    unpackTesters -= tester

  def shouldUnpack(e: Elem[_]) = unpackTesters.exists(_(e))

  object HasViews {
    def unapply[T](s: Exp[T]): Option[Unpacked[T]] =
      unapplyViews(s)
  }

  // for simplifying unapplyViews
  protected def trivialUnapply[T](s: Exp[T]) = (s, identityIso(s.elem))

  def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
//    case Def(Tup(s1, s2)) =>
//      (unapplyViews(s1), unapplyViews(s2)) match {
//        case (None, None) => None
//        case (opt1, opt2) =>
//          val (sv1, iso1) = opt1.getOrElse(trivialUnapply(s1))
//          val (sv2, iso2) = opt2.getOrElse(trivialUnapply(s2))
//          Some((Pair(sv1, sv2), pairIso(iso1, iso2)))
//      }
//    case Def(l @ SLeft(s)) =>
//      (unapplyViews(s), UnpackableElem.unapply(l.eRight)) match {
//        case (None, None) => None
//        case (opt1, opt2) =>
//          val (sv1, iso1) = opt1.getOrElse(trivialUnapply(s))
//          val iso2 = opt2.getOrElse(identityIso(l.eRight))
//          Some((sv1.asLeft(iso2.eFrom), sumIso(iso1, iso2)))
//      }
//    case Def(r @ SRight(s)) =>
//      (UnpackableElem.unapply(r.eLeft), unapplyViews(s)) match {
//        case (None, None) => None
//        case (opt1, opt2) =>
//          val (sv2, iso2) = opt2.getOrElse(trivialUnapply(s))
//          val iso1 = opt1.getOrElse(identityIso(r.eLeft))
//          Some((sv2.asRight(iso1.eFrom), sumIso(iso1, iso2)))
//      }
    case _ =>
      UnpackableExp.unapply(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  object UnpackableDef {
    def unapply[T](d: Def[T]): Option[Unpacked[T]] =
      d match {
//        case view: View[a, T] => Some((view.source, view.iso0))
        // TODO make UserTypeDef extend View with lazy iso0/source?
        case _ =>
          val eT = d.selfType
          eT match {
//            case UnpackableElem(iso0: Iso[a, T @unchecked]) =>
//              Some((iso0.from(d.self), iso0))
            case _ => None
          }
      }
  }

  object UnpackableExp {
    def unapply[T](e: Exp[T]): Option[Unpacked[T]] =
      e match {
        case Def(d) => UnpackableDef.unapply(d)
        case _ =>
          val eT = e.elem
          eT match {
//            case UnpackableElem(iso0: Iso[a, T @unchecked]) =>
//              Some((iso0.from(e), iso0))
            case _ => None
          }
      }
  }

  object LambdaResultHasViews {
    def unapply[A,C](l: Rep[A => C]): Option[UnpackedLambdaResult[A,C]] = l match {
//      case Def(Lambda(_, _, _, HasViews(_, iso0: Iso[b, _]))) =>
//        Some((l, iso0))
      case _ => None
    }
  }

  abstract class View[From, To] extends Def[To] {
    def source: Rep[From]
    def iso: Iso[From, To]
    implicit lazy val selfType = iso.eTo
  }

  case class UnpackView[A, B](view: Rep[B])(implicit iso: Iso[A, B]) extends Def[A] {
    implicit def selfType = iso.eFrom
  }

  abstract class View1[A, B, C[_]](val iso: Iso1[A,B,C]) extends View[C[A], C[B]] {
    def innerIso = iso.innerIso
  }

  abstract class View2[A1, A2, B1, B2, C[_, _]](implicit val iso1: Iso[A1, B1], val iso2: Iso[A2, B2]) extends View[C[A1, A2], C[B1, B2]]

  case class PairView[A1, A2, B1, B2](source: Rep[(A1, A2)])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, Tuple2] {
    lazy val iso = pairIso(iso1, iso2)
  }

  case class SumView[A1, A2, B1, B2](source: Rep[A1|A2])(implicit iso1: Iso[A1, B1], iso2: Iso[A2, B2]) extends View2[A1, A2, B1, B2, | ] {
    lazy val iso = sumIso(iso1, iso2)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    // Rule: (V(a, iso1), V(b, iso2)) ==> V((a,b), PairIso0(iso1, iso2))
    case Tup(Def(UnpackableDef(a, iso1: Iso[a, c])), Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
      PairView((a.asRep[a], b.asRep[b]))(iso1, iso2)

    // Rule: (V(a, iso1), b) ==> V((a,b), PairIso0(iso1, id))
    case Tup(Def(UnpackableDef(a, iso1: Iso[a, c])), b: Rep[b]) =>
      PairView((a.asRep[a], b))(iso1, identityIso(b.elem)).self

    // Rule: (a, V(b, iso2)) ==> V((a,b), PairIso0(id, iso2))
    case Tup(a: Rep[a], Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
      PairView((a, b.asRep[b]))(identityIso(a.elem), iso2).self

//    // Rule: V(a, iso1) ; V(b, iso2)) ==> iso2.to(a ; b)
//    case block@Semicolon(Def(UnpackableDef(a, iso1: Iso[a, c])), Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
//      iso2.to(Semicolon(a.asRep[a], b.asRep[b])(iso2.eFrom))
//
//    // Rule: a ; V(b, iso2)) ==> iso2.to(a ; b)
//    case block@Semicolon(a: Rep[a], Def(UnpackableDef(b, iso2: Iso[b, d]))) =>
//      iso2.to(Semicolon(a, b.asRep[b])(iso2.eFrom))

    // Rule: V(a, iso1) ; b ==> a ; b
    case block@Semicolon(Def(UnpackableDef(a, iso1: Iso[a, c])), b: Rep[b]) =>
      Semicolon(a.asRep[a], b)(block.selfType.asElem[b])

    // Rule: PairView(source, iso1, _)._1  ==> iso1.to(source._1)
    case First(Def(view@PairView(source))) =>
      view.iso1.to(source._1)

    // Rule: PairView(source, _, iso2)._2  ==> iso2.to(source._2)
    case Second(Def(view@PairView(source))) =>
      view.iso2.to(source._2)

    // Rule: PairView(PairView(source, i2), i1)  ==> PairView(source, PairIso0(composeIso(i1.iso1, i2.iso1), composeIso(i1.iso2, i2.iso2)))
    case v1@PairView(Def(v2@PairView(source))) => {
      val pIso1 = composeIso(v1.iso1, v2.iso1)
      val pIso2 = composeIso(v1.iso2, v2.iso2)
      PairView(source)(pIso1, pIso2)
    }

    // Rule: UnpackView(V(source, iso0))  ==> source
    case UnpackView(Def(UnpackableDef(source, iso0))) => source

//    // Rule: ParExec(nJobs, f @ i => ... V(_, iso0)) ==> V(ParExec(nJobs, f >> iso0.from), arrayIso0(iso0))
//    case ParallelExecute(nJobs:Rep[Int], f@Def(Lambda(_, _, _, UnpackableExp(_, iso0: Iso0[a, b])))) =>
//      implicit val ea = iso0.eFrom
//      val parRes = ParallelExecute(nJobs, fun { i => iso0.from(f(i)) })(iso0.eFrom)
//      ViewArray(parRes)(arrayIso0(iso0))
//
//    // Rule: ArrayFold(xs, V(init, iso0), step) ==> iso0.to(ArrayFold(xs, init, p => iso0.from(step(iso0.to(p._1), p._2)) ))
//    case ArrayFold(xs: Rep[Array[t]] @unchecked, HasViews(init, iso0: Iso0[a, b]), step) =>
//      val init1 = init.asRep[a]
//      implicit val eT = xs.elem.asElem[Array[t]].eItem
//      implicit val eA = iso0.eFrom
//      implicit val eB = iso0.eTo
//      val step1 = fun { (p: Rep[(a,t)]) =>
//        val x_viewed = (iso0.to(p._1), p._2)
//        val res_viewed = step.asRep[((b,t)) => b](x_viewed)
//        val res = iso0.from(res_viewed)
//        res
//      }
//      val foldRes = ArrayFold(xs, init1, step1)
//      iso0.to(foldRes)
//
//    // Rule: loop(V(start, iso0), step, isMatch) ==> iso0.to(loop(start, iso0.to >> step >> iso0.from, iso0.to >> isMatch))
//    case LoopUntil(HasViews(startWithoutViews, iso0: Iso0[a, b]), step, isMatch) =>
//      val start1 = startWithoutViews.asRep[a]
//      implicit val eA = iso0.eFrom
//      implicit val eB = iso0.eTo
//      val step1 = fun { (x: Rep[a]) =>
//        val x_viewed = iso0.to(x)
//        val res_viewed = step.asRep[b => b](x_viewed) // mirrorApply(step.asRep[b => b], x_viewed)
//        val res = iso0.from(res_viewed)
//        res
//      }
//      val isMatch1 = fun { (x: Rep[a]) =>
//        val x_viewed = iso0.to(x)
//        val res = isMatch.asRep[b => Boolean](x_viewed) // mirrorApply(isMatch.asRep[b => Boolean], x_viewed)
//        res
//      }
//      val loopRes = LoopUntil(start1, step1, isMatch1)
//      iso0.to(loopRes)

    case call @ MethodCall(Def(obj), m, args, neverInvoke) =>
      call.tryInvoke match {
        // Rule: obj.m(args) ==> body(m).subst{xs -> args}
        case InvokeSuccess(res) => res
        case InvokeFailure(e) =>
          if (e.isInstanceOf[DelayInvokeException])
            super.rewriteDef(d)
          else
            !!!(s"Failed to invoke $call", e)
        case InvokeImpossible =>
          implicit val resultElem: Elem[T] = d.selfType
          // asRep[T] cast below should be safe
          // explicit resultElem to make sure both branches have the same type
          def copyMethodCall(newReceiver: Exp[_]) =
            mkMethodCall(newReceiver, m, args, neverInvoke, resultElem).asRep[T]

          obj match {
            // Rule: (if(c) t else e).m(args) ==> if (c) t.m(args) else e.m(args)
            case IfThenElse(cond, t, e) =>
              IF (cond) {
                copyMethodCall(t)
              } ELSE {
                copyMethodCall(e)
              }
            case _ =>
              super.rewriteDef(d)
          }
      }
    case _ => super.rewriteDef(d)
  }

//  override def rewriteVar[T](v: Exp[T]) = v.elem match {
//    case UnpackableElem(iso0: Iso0[a, T @unchecked]) =>
//      iso0.to(fresh[a](Lazy(iso0.eFrom)))
//    case _ => super.rewriteVar(v)
//  }
}
