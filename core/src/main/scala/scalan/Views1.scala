package scalan

import scala.language.higherKinds
import scala.collection.mutable.{Map => MutMap}
import scala.reflect.ClassTag
import scalan.common.Lazy
import scalan.meta.ScalanAst.STraitOrClassDef
import scalan.staged.BaseExp

trait Views1 extends Elems { self: Views1Dsl with Scalan =>
  // TODO type Iso[From, To] = Rep[Iso0[From, To]]
  type RIso[From, To] = Rep[Iso0[From, To]]

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
    override lazy val hashCode = 41 * eFrom.hashCode + eTo.hashCode
    def isIdentity: Boolean = false
    lazy val fromFun = fun { x: Rep[To] => from(x) }(Lazy(eTo), eFrom)
    lazy val toFun = fun { x: Rep[From] => to(x) }(Lazy(eFrom), eTo)

    //    if (isDebug) {
    //      debug$Iso0Counter(this) += 1
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
  // in Views1Impl.scala
  abstract class PairIso0[A1, A2, B1, B2](val iso01: RIso[A1, B1], val iso02: RIso[A2, B2])(
    implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends Iso0[(A1, A2), (B1, B2)]/*()(pairElement(iso01.eFrom, iso02.eFrom))*/ {
//    implicit def eA1 = iso01.eFrom
//    implicit def eA2 = iso02.eFrom
//    implicit def eB1 = iso01.eTo
//    implicit def eB2 = iso02.eTo
    lazy val eFrom: Elem[(A1, A2)] = element[(A1, A2)]
    lazy val eTo: Elem[(B1, B2)] = element[(B1, B2)]

    var fromCacheKey:Option[Rep[(B1,B2)]] = None
    var fromCacheValue:Option[Rep[(A1,A2)]] = None
    var toCacheKey:Option[Rep[(A1,A2)]] = None
    var toCacheValue:Option[Rep[(B1,B2)]] = None

    def from(b: Rep[(B1, B2)]) = {
      if (fromCacheKey.isEmpty || b != fromCacheKey.get) {
        fromCacheKey = Some(b)
        fromCacheValue = Some((iso01.from(b._1), iso02.from(b._2)))
      }
      fromCacheValue.get
    }
    def to(a: Rep[(A1, A2)]) = {
      if (toCacheKey.isEmpty || a != toCacheKey.get) {
        toCacheKey = Some(a)
        toCacheValue = Some((iso01.to(a._1), iso02.to(a._2)))
      }
      toCacheValue.get
    }
    override def isIdentity = iso01.isIdentity && iso02.isIdentity
  }
  trait PairIso0Companion

  abstract class SumIso0[A1, A2, B1, B2](val iso01: RIso[A1, B1], val iso02: RIso[A2, B2])(
    implicit val eA1: Elem[A1], val eA2: Elem[A2], val eB1: Elem[B1], val eB2: Elem[B2])
    extends Iso0[A1 | A2, B1 | B2]/*()(sumElement(iso01.eFrom, iso02.eFrom))*/ {
    lazy val eFrom: Elem[A1 | A2] = element[A1 | A2]
    lazy val eTo: Elem[B1 | B2] = element[B1 | B2]
    def from(b: Rep[B1 | B2]) =
      b.mapSumBy(iso01.fromFun, iso02.fromFun)
    def to(a: Rep[A1 | A2]) =
      a.mapSumBy(iso01.toFun, iso02.toFun)
    override def isIdentity = iso01.isIdentity && iso02.isIdentity
  }

  abstract class ComposeIso0[A, B, C](val iso02: RIso[B, C], val iso01: RIso[A, B])(
    implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C]) extends Iso0[A, C]/*()(iso01.eFrom)*/ {
    def eFrom: Elem[A] = iso01.eFrom
    def eTo: Elem[C] = iso02.eTo
    def from(c: Rep[C]) = iso01.from(iso02.from(c))
    def to(a: Rep[A]) = iso02.to(iso01.to(a))
    override def isIdentity = iso01.isIdentity && iso02.isIdentity
  }

  abstract class FuncIso0[A, B, C, D](val iso01: RIso[A, B], val iso02: RIso[C, D])(
    implicit val eA: Elem[A], val eB: Elem[B], val eC: Elem[C], val eD: Elem[D])
    extends Iso0[A => C, B => D]/*()(funcElement(iso01.eFrom, iso02.eFrom))*/ {
    lazy val eFrom: Elem[A => C] = element[A => C]
    lazy val eTo: Elem[B => D] = element[B => D]
    def from(f: Rep[B => D]): Rep[A => C] = {
      fun { b => iso02.from(f(iso01.to(b))) }
    }
    def to(f: Rep[A => C]): Rep[B => D] = {
      fun { a => iso02.to(f(iso01.from(a))) }
    }
    override def isIdentity = iso01.isIdentity && iso02.isIdentity
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

}

trait Views1Dsl extends impl.Views1Abs { self: Scalan =>

  private val iso0Cache = MutMap.empty[(Class[_], Seq[AnyRef]), AnyRef]

  def cachedIso0[I <: Iso0[_, _]](args: AnyRef*)(implicit tag: ClassTag[I]) = {
    val clazz = tag.runtimeClass
    iso0Cache.getOrElseUpdate(
    (clazz, args), {
      val constructors = clazz.getDeclaredConstructors()
      if (constructors.length != 1) {
        !!!(s"Iso0 class $clazz has ${constructors.length} constructors, 1 expected")
      } else {
        val constructor = constructors(0)
        val constructorArgs = self +: args
        constructor.newInstance(constructorArgs: _*).asInstanceOf[Iso0[_, _]]
      }
    }).asInstanceOf[I]
  }

  abstract class Iso01[A, B, C[_]](val innerIso0: Iso0[A,B])(implicit cC: Cont[C])
    extends Iso0[C[A], C[B]]/*()(cC.lift(innerIso0.eFrom))*/ {
    implicit val eA = innerIso0.eFrom
    implicit val eB = innerIso0.eTo
    lazy val eFrom = cC.lift(innerIso0.eFrom)
    lazy val eTo = cC.lift(innerIso0.eTo)
    override def isIdentity = innerIso0.isIdentity
  }

  implicit def viewElem0ent[From, To](implicit iso0: Iso0[From, To]): Elem[To] = iso0.eTo // always ask elem from Iso0

  trait ViewElem0[From, To] extends Elem[To] { _: scala.Equals =>
    def iso0: Iso0[From, To]
    override def isEntityType = shouldUnpack(this)
  }

  object ViewElem0 {
    def unapply[From, To](ve: ViewElem0[From, To]): Option[Iso0[From, To]] = Some(ve.iso0)
  }

  trait ViewElem01[A,From,To,C[_]] extends ViewElem0[From, To] { _: scala.Equals =>
    def eItem: Elem[A]
    def cont: Cont[C]
  }

  object UnpackableElem {
    def unapply(e: Elem[_]) =
      if (shouldUnpack(e)) {
        val iso0 = getIso0ByElem(e)
        if (iso0.isIdentity)
          None
        else
          Some(iso0)
      }
      else None
  }

  def shouldUnpack(e: Elem[_]): Boolean

  def getIso0ByElem[T](e: Elem[T]): Iso0[_, T] = iso0Cache.getOrElseUpdate(
    (classOf[Iso0[_, _]], Seq(e)),
    e match {
      case ve: ViewElem0[_,_] =>
        val eFrom = ve.iso0.eFrom
        val deepIso0 = getIso0ByElem(eFrom)
        if (deepIso0.isIdentity)
          ve.iso0
        else
          composeIso0(ve.iso0, deepIso0)
      case pe: PairElem[a,b] =>
        val iso01 = getIso0ByElem(pe.eFst)
        val iso02 = getIso0ByElem(pe.eSnd)
        pairIso0(iso01,iso02)
      case pe: SumElem[a,b] =>
        val iso01 = getIso0ByElem(pe.eLeft)
        val iso02 = getIso0ByElem(pe.eRight)
        sumIso0(iso01,iso02)
      case fe: FuncElem[a,b] =>
        val iso01 = getIso0ByElem(fe.eDom)
        val iso02 = getIso0ByElem(fe.eRange)
        funcIso0(iso01,iso02)
//      case ae: ArrayElem[_] =>
//        val iso0 = getIso0ByElem(ae.eItem)
//        arrayIso0(iso0)
//      case ae: ListElem[_] =>
//        val iso0 = getIso0ByElem(ae.eItem)
//        listIso0(iso0)
//      case ae: ArrayBufferElem[_] =>
//        val iso0 = getIso0ByElem(ae.eItem)
//        arrayBufferIso0(iso0)
//      case ae: ThunkElem[_] =>
//        val iso0 = getIso0ByElem(ae.eItem)
//        thunkIso0(iso0)
      case me: MMapElem[_,_] =>
        identityIso0(me)

      case we: WrapperElem1[a, Def[ext], cbase, cw] @unchecked =>
        val eExt = we.eTo
        val iso0 = getIso0ByElem(eExt)
        iso0

      case we: WrapperElem[Def[base],Def[ext]] @unchecked =>
        val eExt = we.eTo
        val iso0 = getIso0ByElem(eExt)
        iso0

      //    case ee1: EntityElem1[_,_,_] =>
      //      val iso0 = getIso0ByElem(ee1.eItem)
      //      TODO implement using ContainerIso0

      case ee: EntityElem[_] =>
        identityIso0(ee)
      case be: BaseElem[_] =>
        identityIso0(be)
      case _ => !!!(s"Don't know how to build iso0 for element $e")
    }
  ).asInstanceOf[Iso0[_,T]]

  def isConcreteElem[T](e: Elem[T]): Boolean = e match {
    case e: PairElem[_, _] => e.eFst.isConcrete && e.eSnd.isConcrete
    case e: SumElem[_, _] => e.eLeft.isConcrete && e.eRight.isConcrete
    case e: FuncElem[_, _] => e.eDom.isConcrete && e.eRange.isConcrete
    case e: ArrayElem[_] => e.eItem.isConcrete
    case e: ListElem[_] => e.eItem.isConcrete
    case e: ArrayBufferElem[_] => e.eItem.isConcrete
    case _: ViewElem0[_,_] => true
    case _: EntityElem[_] => false
    case _: BaseElem[_] => true
    case _ => !!!(s"isConcrete is not defined for Elem $e")
  }

  implicit class ElemOps[T](e: Elem[T]) {
    def isConcrete = isConcreteElem(e)
    def getDataIso0 = getIso0ByElem(e)
  }

  def identityIso0[A](implicit elem: Elem[A]): Iso0[A, A] = cachedIso0[IdentityIso0[A]](elem)

  def pairIso0[A1, A2, B1, B2](iso01: Iso0[A1, B1], iso02: Iso0[A2, B2]): Iso0[(A1, A2), (B1, B2)] =
    cachedIso0[PairIso0[A1, A2, B1, B2]](iso01, iso02)

  def sumIso0[A1, A2, B1, B2](iso01: Iso0[A1, B1], iso02: Iso0[A2, B2]): Iso0[A1 | A2, B1 | B2] =
    ???
//    cachedIso0[SumIso0[A1, A2, B1, B2]](iso01, iso02)

  def composeIso0[A, B, C](iso02: Iso0[B, C], iso01: Iso0[A, B]): Iso0[A, C] = {
    ???
//    ((iso02, iso01) match {
//        // TODO uncomment and make it compile
////      case (IdentityIso0Matcher(_), _) => iso01
////      case (_, IdentityIso0Matcher(_)) => iso02
////      case (PairIso0Matcher(iso021, iso022), PairIso0Matcher(iso011, iso012)) =>
////        pairIso0(composeIso0(iso021, iso011), composeIso0(iso022, iso012))
//      case _ => cachedIso0[ComposeIso0[A, B, C]](iso02, iso01)
//    }).asInstanceOf[Iso0[A, C]]
  }

  def funcIso0[A, B, C, D](iso01: Iso0[A, B], iso02: Iso0[C, D]): Iso0[A => C, B => D] =
    ??? // cachedIso0[FuncIso0[A, B, C, D]](iso01, iso02)

  def converterIso0[A, B](convTo: Conv[A,B], convFrom: Conv[B,A]): Iso0[A,B] =
    ??? // cachedIso0[ConverterIso0[A, B]](convTo.asInstanceOf[AnyRef], convFrom.asInstanceOf[AnyRef])

  def convertBeforeIso0[A, B, C](convTo: Conv[A,B], convFrom: Conv[B,A], iso0: Iso0[B,C]): Iso0[A, C] = composeIso0(iso0, converterIso0(convTo, convFrom))

  def convertAfterIso0[A,B,C](iso0: Iso0[A,B], convTo: Conv[B,C], convFrom: Conv[C,B]): Iso0[A, C] = composeIso0(converterIso0(convTo, convFrom), iso0)

  def unifyIso0s[A,B,C,D](iso01: Iso0[A,C], iso02: Iso0[B,D],
                         toD: Conv[C,D], toC: Conv[D,C]): (Iso0[A,C], Iso0[B,C]) = {
    val ea = iso01.eFrom
    val eb = iso02.eFrom
    implicit val ec = iso01.eTo
    val (i1, i2) =
      if (ec == iso02.eTo)
        (iso01, iso02.asInstanceOf[Iso0[B,C]])
      else
        (iso01, convertAfterIso0(iso02, toC, toD))
    (i1, i2)
  }

//  implicit class RepDefView0Ops[T](x: Rep[T]) {
//    def convertTo[R <: Def[_]](implicit eR: Elem[R]): Rep[R] =
//      eR match {
//        case entE: EntityElem[R] @unchecked => entE.convert(x)
//        case _ => !!!(s"Cannot convert $x to a value of type ${eR.name}: EntityElem expected but ${eR.getClass.getSimpleName} found")
//      }
//  }
}

trait Views1DslSeq extends impl.Views1Seq { self: ScalanSeq =>
  def shouldUnpack(e: Elem[_]) = true
}

trait Views1DslExp extends impl.Views1Exp with BaseExp { self: ScalanExp =>
  type Unpacked[T] = (Rep[Source], Iso0[Source, T]) forSome { type Source }
  type UnpackedLambdaResult[T,R] = (Rep[T => R], Iso0[Source, R]) forSome { type Source }

  type UnpackTester = Element[_] => Boolean

  private var unpackTesters: Set[UnpackTester] = Set.empty

  def addUnpackTester(tester: UnpackTester): Unit =
    unpackTesters += tester
  def removeUnpackTester(tester: UnpackTester): Unit =
    unpackTesters -= tester

  def shouldUnpack(e: Elem[_]) = unpackTesters.exists(_(e))

  object HasView0s {
    def unapply[T](s: Exp[T]): Option[Unpacked[T]] =
      unapplyView0s(s)
  }

  // for simplifying unapplyView0s
  protected def trivialUnapply[T](s: Exp[T]) = (s, identityIso0(s.elem))

  def unapplyView0s[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
//    case Def(Tup(s1, s2)) =>
//      (unapplyView0s(s1), unapplyView0s(s2)) match {
//        case (None, None) => None
//        case (opt1, opt2) =>
//          val (sv1, iso01) = opt1.getOrElse(trivialUnapply(s1))
//          val (sv2, iso02) = opt2.getOrElse(trivialUnapply(s2))
//          Some((Pair(sv1, sv2), pairIso0(iso01, iso02)))
//      }
//    case Def(l @ SLeft(s)) =>
//      (unapplyView0s(s), UnpackableElem.unapply(l.eRight)) match {
//        case (None, None) => None
//        case (opt1, opt2) =>
//          val (sv1, iso01) = opt1.getOrElse(trivialUnapply(s))
//          val iso02 = opt2.getOrElse(identityIso0(l.eRight))
//          Some((sv1.asLeft(iso02.eFrom), sumIso0(iso01, iso02)))
//      }
//    case Def(r @ SRight(s)) =>
//      (UnpackableElem.unapply(r.eLeft), unapplyView0s(s)) match {
//        case (None, None) => None
//        case (opt1, opt2) =>
//          val (sv2, iso02) = opt2.getOrElse(trivialUnapply(s))
//          val iso01 = opt1.getOrElse(identityIso0(r.eLeft))
//          Some((sv2.asRight(iso01.eFrom), sumIso0(iso01, iso02)))
//      }
    case _ =>
      UnpackableExp.unapply(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  object UnpackableDef {
    def unapply[T](d: Def[T]): Option[Unpacked[T]] =
      d match {
//        case view0: View0[a, T] => Some((view0.source, view0.iso0))
        // TODO make UserTypeDef extend View0 with lazy iso0/source?
        case _ =>
          val eT = d.selfType
          eT match {
//            case UnpackableElem(iso0: Iso0[a, T @unchecked]) =>
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
//            case UnpackableElem(iso0: Iso0[a, T @unchecked]) =>
//              Some((iso0.from(e), iso0))
            case _ => None
          }
      }
  }

  object LambdaResultHasView0s {
    def unapply[A,C](l: Rep[A => C]): Option[UnpackedLambdaResult[A,C]] = l match {
//      case Def(Lambda(_, _, _, HasView0s(_, iso0: Iso0[b, _]))) =>
//        Some((l, iso0))
      case _ => None
    }
  }

  abstract class View0[From, To] extends Def[To] {
    def source: Rep[From]
    def iso0: Iso0[From, To]
    implicit def selfType = iso0.eTo
    def copy(source: Rep[From]): View0[From, To]
    def mirror(t: Transformer) = copy(t(source))
  }

  case class UnpackView0[A, B](view0: Rep[B])(implicit iso0: Iso0[A, B]) extends Def[A] {
    implicit def selfType = iso0.eFrom
  }

  abstract class View01[A, B, C[_]](val iso0: Iso01[A,B,C]) extends View0[C[A], C[B]] {
    def innerIso0 = iso0.innerIso0
  }

  abstract class View02[A1, A2, B1, B2, C[_, _]](implicit val iso01: Iso0[A1, B1], val iso02: Iso0[A2, B2]) extends View0[C[A1, A2], C[B1, B2]]

  case class PairView0[A1, A2, B1, B2](source: Rep[(A1, A2)])(implicit iso01: Iso0[A1, B1], iso02: Iso0[A2, B2]) extends View02[A1, A2, B1, B2, Tuple2] {
    lazy val iso0 = pairIso0(iso01, iso02)
    def copy(source: Rep[(A1, A2)]) = PairView0(source)
  }

  case class SumView0[A1, A2, B1, B2](source: Rep[A1|A2])(implicit iso01: Iso0[A1, B1], iso02: Iso0[A2, B2]) extends View02[A1, A2, B1, B2, | ] {
    lazy val iso0 = sumIso0(iso01, iso02)
    def copy(source: Rep[A1|A2]) = SumView0(source)
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    // Rule: (V(a, iso01), V(b, iso02)) ==> V((a,b), PairIso0(iso01, iso02))
    case Tup(Def(UnpackableDef(a, iso01: Iso0[a, c])), Def(UnpackableDef(b, iso02: Iso0[b, d]))) =>
      PairView0((a.asRep[a], b.asRep[b]))(iso01, iso02)

    // Rule: (V(a, iso01), b) ==> V((a,b), PairIso0(iso01, id))
    case Tup(Def(UnpackableDef(a, iso01: Iso0[a, c])), b: Rep[b]) =>
      PairView0((a.asRep[a], b))(iso01, identityIso0(b.elem)).self

    // Rule: (a, V(b, iso02)) ==> V((a,b), PairIso0(id, iso02))
    case Tup(a: Rep[a], Def(UnpackableDef(b, iso02: Iso0[b, d]))) =>
      PairView0((a, b.asRep[b]))(identityIso0(a.elem), iso02).self

//    // Rule: V(a, iso01) ; V(b, iso02)) ==> iso02.to(a ; b)
//    case block@Semicolon(Def(UnpackableDef(a, iso01: Iso0[a, c])), Def(UnpackableDef(b, iso02: Iso0[b, d]))) =>
//      iso02.to(Semicolon(a.asRep[a], b.asRep[b])(iso02.eFrom))
//
//    // Rule: a ; V(b, iso02)) ==> iso02.to(a ; b)
//    case block@Semicolon(a: Rep[a], Def(UnpackableDef(b, iso02: Iso0[b, d]))) =>
//      iso02.to(Semicolon(a, b.asRep[b])(iso02.eFrom))

    // Rule: V(a, iso01) ; b ==> a ; b
    case block@Semicolon(Def(UnpackableDef(a, iso01: Iso0[a, c])), b: Rep[b]) =>
      Semicolon(a.asRep[a], b)(block.selfType.asElem[b])

    // Rule: PairView0(source, iso01, _)._1  ==> iso01.to(source._1)
    case First(Def(view0@PairView0(source))) =>
      view0.iso01.to(source._1)

    // Rule: PairView0(source, _, iso02)._2  ==> iso02.to(source._2)
    case Second(Def(view0@PairView0(source))) =>
      view0.iso02.to(source._2)

    // Rule: PairView0(PairView0(source, i2), i1)  ==> PairView0(source, PairIso0(composeIso0(i1.iso01, i2.iso01), composeIso0(i1.iso02, i2.iso02)))
    case v1@PairView0(Def(v2@PairView0(source))) => {
      val pIso01 = composeIso0(v1.iso01,v2.iso01)
      val pIso02 = composeIso0(v1.iso02, v2.iso02)
      PairView0(source)(pIso01, pIso02)
    }

    // Rule: UnpackView0(V(source, iso0))  ==> source
    case UnpackView0(Def(UnpackableDef(source, iso0))) => source

//    // Rule: ParExec(nJobs, f @ i => ... V(_, iso0)) ==> V(ParExec(nJobs, f >> iso0.from), arrayIso0(iso0))
//    case ParallelExecute(nJobs:Rep[Int], f@Def(Lambda(_, _, _, UnpackableExp(_, iso0: Iso0[a, b])))) =>
//      implicit val ea = iso0.eFrom
//      val parRes = ParallelExecute(nJobs, fun { i => iso0.from(f(i)) })(iso0.eFrom)
//      View0Array(parRes)(arrayIso0(iso0))
//
//    // Rule: ArrayFold(xs, V(init, iso0), step) ==> iso0.to(ArrayFold(xs, init, p => iso0.from(step(iso0.to(p._1), p._2)) ))
//    case ArrayFold(xs: Rep[Array[t]] @unchecked, HasView0s(init, iso0: Iso0[a, b]), step) =>
//      val init1 = init.asRep[a]
//      implicit val eT = xs.elem.asElem[Array[t]].eItem
//      implicit val eA = iso0.eFrom
//      implicit val eB = iso0.eTo
//      val step1 = fun { (p: Rep[(a,t)]) =>
//        val x_view0ed = (iso0.to(p._1), p._2)
//        val res_view0ed = step.asRep[((b,t)) => b](x_view0ed)
//        val res = iso0.from(res_view0ed)
//        res
//      }
//      val foldRes = ArrayFold(xs, init1, step1)
//      iso0.to(foldRes)
//
//    // Rule: loop(V(start, iso0), step, isMatch) ==> iso0.to(loop(start, iso0.to >> step >> iso0.from, iso0.to >> isMatch))
//    case LoopUntil(HasView0s(startWithoutView0s, iso0: Iso0[a, b]), step, isMatch) =>
//      val start1 = startWithoutView0s.asRep[a]
//      implicit val eA = iso0.eFrom
//      implicit val eB = iso0.eTo
//      val step1 = fun { (x: Rep[a]) =>
//        val x_view0ed = iso0.to(x)
//        val res_view0ed = step.asRep[b => b](x_view0ed) // mirrorApply(step.asRep[b => b], x_view0ed)
//        val res = iso0.from(res_view0ed)
//        res
//      }
//      val isMatch1 = fun { (x: Rep[a]) =>
//        val x_view0ed = iso0.to(x)
//        val res = isMatch.asRep[b => Boolean](x_view0ed) // mirrorApply(isMatch.asRep[b => Boolean], x_view0ed)
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
