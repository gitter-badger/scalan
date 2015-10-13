package scalan

import scala.language.higherKinds
import scala.collection.mutable.{Map=>MutMap}
import scala.reflect.ClassTag
import scalan.common.Lazy
import scalan.meta.ScalanAst.STraitOrClassDef
import scalan.staged.BaseExp
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait Views1Abs extends Views1  {
  self: Views1Dsl with Scalan =>

  // single proxy for each type family
  implicit def proxyIso0[From, To](p: Rep[Iso0[From, To]]): Iso0[From, To] = {
    proxyOps[Iso0[From, To]](p)(scala.reflect.classTag[Iso0[From, To]])
  }

  // familyElem
  class Iso0Elem[From, To, To0 <: Iso0[From, To]](implicit _eFrom: Elem[From], _eTo: Elem[To])
    extends EntityElem[To0] {
    def eFrom = _eFrom
    def eTo = _eTo
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("From" -> Left(eFrom), "To" -> Left(eTo))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagFrom = eFrom.tag
      implicit val tagTo = eTo.tag
      weakTypeTag[Iso0[From, To]].asInstanceOf[WeakTypeTag[To0]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo0: Elem[To0] = this
      val conv = fun {x: Rep[Iso0[From, To]] => convertIso0(x) }
      tryConvert(element[Iso0[From, To]], this, x, conv)
    }

    def convertIso0(x: Rep[Iso0[From, To]]): Rep[To0] = {
      x.selfType1 match {
        case _: Iso0Elem[_, _, _] => x.asRep[To0]
        case e => !!!(s"Expected $x to have Iso0Elem[_, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To0] = ???
  }

  implicit def iso0Element[From, To](implicit eFrom: Elem[From], eTo: Elem[To]): Elem[Iso0[From, To]] =
    cachedElem[Iso0Elem[From, To, Iso0[From, To]]](eFrom, eTo)

  implicit case object Iso0CompanionElem extends CompanionElem[Iso0CompanionAbs] {
    lazy val tag = weakTypeTag[Iso0CompanionAbs]
    protected def getDefaultRep = Iso0
  }

  abstract class Iso0CompanionAbs extends CompanionDef[Iso0CompanionAbs] {
    def selfType = Iso0CompanionElem
    override def toString = "Iso0"
  }
  def Iso0: Rep[Iso0CompanionAbs]
  implicit def proxyIso0CompanionAbs(p: Rep[Iso0CompanionAbs]): Iso0CompanionAbs =
    proxyOps[Iso0CompanionAbs](p)

  abstract class AbsIdentityIso0[A]
      ()(implicit eA: Elem[A])
    extends IdentityIso0[A]() with Def[IdentityIso0[A]] {
    lazy val selfType = element[IdentityIso0[A]]
  }
  // elem for concrete class
  class IdentityIso0Elem[A](val iso: Iso[IdentityIso0Data[A], IdentityIso0[A]])(implicit eA: Elem[A])
    extends Iso0Elem[A, A, IdentityIso0[A]]
    with ConcreteElem[IdentityIso0Data[A], IdentityIso0[A]] {
    override lazy val parent: Option[Elem[_]] = Some(iso0Element(element[A], element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }

    override def convertIso0(x: Rep[Iso0[A, A]]) = IdentityIso0()
    override def getDefaultRep = IdentityIso0()
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[IdentityIso0[A]]
    }
  }

  // state representation type
  type IdentityIso0Data[A] = Unit

  // 3) Iso for concrete class
  class IdentityIso0Iso[A](implicit eA: Elem[A])
    extends Iso[IdentityIso0Data[A], IdentityIso0[A]] {
    override def from(p: Rep[IdentityIso0[A]]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      IdentityIso0()
    }
    lazy val eTo = new IdentityIso0Elem[A](this)
  }
  // 4) constructor and deconstructor
  class IdentityIso0CompanionAbs extends CompanionDef[IdentityIso0CompanionAbs] {
    def selfType = IdentityIso0CompanionElem
    override def toString = "IdentityIso0"
    def apply[A](p: Rep[IdentityIso0Data[A]])(implicit eA: Elem[A]): Rep[IdentityIso0[A]] =
      isoIdentityIso0(eA).to(p)
    def apply[A]()(implicit eA: Elem[A]): Rep[IdentityIso0[A]] =
      mkIdentityIso0()
  }
  object IdentityIso0Matcher {
    def unapply[A](p: Rep[Iso0[A, A]]) = unmkIdentityIso0(p)
  }
  lazy val IdentityIso0: Rep[IdentityIso0CompanionAbs] = new IdentityIso0CompanionAbs
  implicit def proxyIdentityIso0Companion(p: Rep[IdentityIso0CompanionAbs]): IdentityIso0CompanionAbs = {
    proxyOps[IdentityIso0CompanionAbs](p)
  }

  implicit case object IdentityIso0CompanionElem extends CompanionElem[IdentityIso0CompanionAbs] {
    lazy val tag = weakTypeTag[IdentityIso0CompanionAbs]
    protected def getDefaultRep = IdentityIso0
  }

  implicit def proxyIdentityIso0[A](p: Rep[IdentityIso0[A]]): IdentityIso0[A] =
    proxyOps[IdentityIso0[A]](p)

  implicit class ExtendedIdentityIso0[A](p: Rep[IdentityIso0[A]])(implicit eA: Elem[A]) {
    def toData: Rep[IdentityIso0Data[A]] = isoIdentityIso0(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIdentityIso0[A](implicit eA: Elem[A]): Iso[IdentityIso0Data[A], IdentityIso0[A]] =
    cachedIso[IdentityIso0Iso[A]](eA)

  // 6) smart constructor and deconstructor
  def mkIdentityIso0[A]()(implicit eA: Elem[A]): Rep[IdentityIso0[A]]
  def unmkIdentityIso0[A](p: Rep[Iso0[A, A]]): Option[(Rep[Unit])]

  abstract class AbsPairIso0[A1, A2, B1, B2]
      (iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairIso0[A1, A2, B1, B2](iso01, iso02) with Def[PairIso0[A1, A2, B1, B2]] {
    lazy val selfType = element[PairIso0[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class PairIso0Elem[A1, A2, B1, B2](val iso: Iso[PairIso0Data[A1, A2, B1, B2], PairIso0[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends Iso0Elem[(A1, A2), (B1, B2), PairIso0[A1, A2, B1, B2]]
    with ConcreteElem[PairIso0Data[A1, A2, B1, B2], PairIso0[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(iso0Element(pairElement(element[A1],element[A2]), pairElement(element[B1],element[B2])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A1" -> Left(eA1), "A2" -> Left(eA2), "B1" -> Left(eB1), "B2" -> Left(eB2))
    }

    override def convertIso0(x: Rep[Iso0[(A1, A2), (B1, B2)]]) = // Converter is not generated by meta
!!!("Cannot convert from Iso0 to PairIso0: missing fields List(iso01, iso02)")
    override def getDefaultRep = PairIso0(element[Iso0[A1, B1]].defaultRepValue, element[Iso0[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[PairIso0[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type PairIso0Data[A1, A2, B1, B2] = (Iso0[A1, B1], Iso0[A2, B2])

  // 3) Iso for concrete class
  class PairIso0Iso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends Iso[PairIso0Data[A1, A2, B1, B2], PairIso0[A1, A2, B1, B2]]()(pairElement(implicitly[Elem[Iso0[A1, B1]]], implicitly[Elem[Iso0[A2, B2]]])) {
    override def from(p: Rep[PairIso0[A1, A2, B1, B2]]) =
      (p.iso01, p.iso02)
    override def to(p: Rep[(Iso0[A1, B1], Iso0[A2, B2])]) = {
      val Pair(iso01, iso02) = p
      PairIso0(iso01, iso02)
    }
    lazy val eTo = new PairIso0Elem[A1, A2, B1, B2](this)
  }
  // 4) constructor and deconstructor
  class PairIso0CompanionAbs extends CompanionDef[PairIso0CompanionAbs] with PairIso0Companion {
    def selfType = PairIso0CompanionElem
    override def toString = "PairIso0"
    def apply[A1, A2, B1, B2](p: Rep[PairIso0Data[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
      isoPairIso0(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
      mkPairIso0(iso01, iso02)
  }
  object PairIso0Matcher {
    def unapply[A1, A2, B1, B2](p: Rep[Iso0[(A1, A2), (B1, B2)]]) = unmkPairIso0(p)
  }
  lazy val PairIso0: Rep[PairIso0CompanionAbs] = new PairIso0CompanionAbs
  implicit def proxyPairIso0Companion(p: Rep[PairIso0CompanionAbs]): PairIso0CompanionAbs = {
    proxyOps[PairIso0CompanionAbs](p)
  }

  implicit case object PairIso0CompanionElem extends CompanionElem[PairIso0CompanionAbs] {
    lazy val tag = weakTypeTag[PairIso0CompanionAbs]
    protected def getDefaultRep = PairIso0
  }

  implicit def proxyPairIso0[A1, A2, B1, B2](p: Rep[PairIso0[A1, A2, B1, B2]]): PairIso0[A1, A2, B1, B2] =
    proxyOps[PairIso0[A1, A2, B1, B2]](p)

  implicit class ExtendedPairIso0[A1, A2, B1, B2](p: Rep[PairIso0[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[PairIso0Data[A1, A2, B1, B2]] = isoPairIso0(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairIso0[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[PairIso0Data[A1, A2, B1, B2], PairIso0[A1, A2, B1, B2]] =
    cachedIso[PairIso0Iso[A1, A2, B1, B2]](eA1, eA2, eB1, eB2)

  // 6) smart constructor and deconstructor
  def mkPairIso0[A1, A2, B1, B2](iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]]
  def unmkPairIso0[A1, A2, B1, B2](p: Rep[Iso0[(A1, A2), (B1, B2)]]): Option[(Rep[Iso0[A1, B1]], Rep[Iso0[A2, B2]])]

  abstract class AbsSumIso0[A1, A2, B1, B2]
      (iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumIso0[A1, A2, B1, B2](iso01, iso02) with Def[SumIso0[A1, A2, B1, B2]] {
    lazy val selfType = element[SumIso0[A1, A2, B1, B2]]
  }
  // elem for concrete class
  class SumIso0Elem[A1, A2, B1, B2](val iso: Iso[SumIso0Data[A1, A2, B1, B2], SumIso0[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends Iso0Elem[$bar[A1, A2], $bar[B1, B2], SumIso0[A1, A2, B1, B2]]
    with ConcreteElem[SumIso0Data[A1, A2, B1, B2], SumIso0[A1, A2, B1, B2]] {
    override lazy val parent: Option[Elem[_]] = Some(iso0Element(sumElement(element[A1],element[A2]), sumElement(element[B1],element[B2])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A1" -> Left(eA1), "A2" -> Left(eA2), "B1" -> Left(eB1), "B2" -> Left(eB2))
    }

    override def convertIso0(x: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]) = // Converter is not generated by meta
!!!("Cannot convert from Iso0 to SumIso0: missing fields List(iso01, iso02)")
    override def getDefaultRep = SumIso0(element[Iso0[A1, B1]].defaultRepValue, element[Iso0[A2, B2]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA1 = eA1.tag
      implicit val tagA2 = eA2.tag
      implicit val tagB1 = eB1.tag
      implicit val tagB2 = eB2.tag
      weakTypeTag[SumIso0[A1, A2, B1, B2]]
    }
  }

  // state representation type
  type SumIso0Data[A1, A2, B1, B2] = (Iso0[A1, B1], Iso0[A2, B2])

  // 3) Iso for concrete class
  class SumIso0Iso[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends Iso[SumIso0Data[A1, A2, B1, B2], SumIso0[A1, A2, B1, B2]]()(pairElement(implicitly[Elem[Iso0[A1, B1]]], implicitly[Elem[Iso0[A2, B2]]])) {
    override def from(p: Rep[SumIso0[A1, A2, B1, B2]]) =
      (p.iso01, p.iso02)
    override def to(p: Rep[(Iso0[A1, B1], Iso0[A2, B2])]) = {
      val Pair(iso01, iso02) = p
      SumIso0(iso01, iso02)
    }
    lazy val eTo = new SumIso0Elem[A1, A2, B1, B2](this)
  }
  // 4) constructor and deconstructor
  class SumIso0CompanionAbs extends CompanionDef[SumIso0CompanionAbs] {
    def selfType = SumIso0CompanionElem
    override def toString = "SumIso0"
    def apply[A1, A2, B1, B2](p: Rep[SumIso0Data[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
      isoSumIso0(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
      mkSumIso0(iso01, iso02)
  }
  object SumIso0Matcher {
    def unapply[A1, A2, B1, B2](p: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]) = unmkSumIso0(p)
  }
  lazy val SumIso0: Rep[SumIso0CompanionAbs] = new SumIso0CompanionAbs
  implicit def proxySumIso0Companion(p: Rep[SumIso0CompanionAbs]): SumIso0CompanionAbs = {
    proxyOps[SumIso0CompanionAbs](p)
  }

  implicit case object SumIso0CompanionElem extends CompanionElem[SumIso0CompanionAbs] {
    lazy val tag = weakTypeTag[SumIso0CompanionAbs]
    protected def getDefaultRep = SumIso0
  }

  implicit def proxySumIso0[A1, A2, B1, B2](p: Rep[SumIso0[A1, A2, B1, B2]]): SumIso0[A1, A2, B1, B2] =
    proxyOps[SumIso0[A1, A2, B1, B2]](p)

  implicit class ExtendedSumIso0[A1, A2, B1, B2](p: Rep[SumIso0[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]) {
    def toData: Rep[SumIso0Data[A1, A2, B1, B2]] = isoSumIso0(eA1, eA2, eB1, eB2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSumIso0[A1, A2, B1, B2](implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Iso[SumIso0Data[A1, A2, B1, B2], SumIso0[A1, A2, B1, B2]] =
    cachedIso[SumIso0Iso[A1, A2, B1, B2]](eA1, eA2, eB1, eB2)

  // 6) smart constructor and deconstructor
  def mkSumIso0[A1, A2, B1, B2](iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]]
  def unmkSumIso0[A1, A2, B1, B2](p: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]): Option[(Rep[Iso0[A1, B1]], Rep[Iso0[A2, B2]])]

  abstract class AbsComposeIso0[A, B, C]
      (iso02: RIso[B, C], iso01: RIso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends ComposeIso0[A, B, C](iso02, iso01) with Def[ComposeIso0[A, B, C]] {
    lazy val selfType = element[ComposeIso0[A, B, C]]
  }
  // elem for concrete class
  class ComposeIso0Elem[A, B, C](val iso: Iso[ComposeIso0Data[A, B, C], ComposeIso0[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends Iso0Elem[A, C, ComposeIso0[A, B, C]]
    with ConcreteElem[ComposeIso0Data[A, B, C], ComposeIso0[A, B, C]] {
    override lazy val parent: Option[Elem[_]] = Some(iso0Element(element[A], element[C]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB), "C" -> Left(eC))
    }

    override def convertIso0(x: Rep[Iso0[A, C]]) = // Converter is not generated by meta
!!!("Cannot convert from Iso0 to ComposeIso0: missing fields List(iso02, iso01)")
    override def getDefaultRep = ComposeIso0(element[Iso0[B, C]].defaultRepValue, element[Iso0[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      weakTypeTag[ComposeIso0[A, B, C]]
    }
  }

  // state representation type
  type ComposeIso0Data[A, B, C] = (Iso0[B, C], Iso0[A, B])

  // 3) Iso for concrete class
  class ComposeIso0Iso[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends Iso[ComposeIso0Data[A, B, C], ComposeIso0[A, B, C]]()(pairElement(implicitly[Elem[Iso0[B, C]]], implicitly[Elem[Iso0[A, B]]])) {
    override def from(p: Rep[ComposeIso0[A, B, C]]) =
      (p.iso02, p.iso01)
    override def to(p: Rep[(Iso0[B, C], Iso0[A, B])]) = {
      val Pair(iso02, iso01) = p
      ComposeIso0(iso02, iso01)
    }
    lazy val eTo = new ComposeIso0Elem[A, B, C](this)
  }
  // 4) constructor and deconstructor
  class ComposeIso0CompanionAbs extends CompanionDef[ComposeIso0CompanionAbs] {
    def selfType = ComposeIso0CompanionElem
    override def toString = "ComposeIso0"
    def apply[A, B, C](p: Rep[ComposeIso0Data[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
      isoComposeIso0(eA, eB, eC).to(p)
    def apply[A, B, C](iso02: RIso[B, C], iso01: RIso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
      mkComposeIso0(iso02, iso01)
  }
  object ComposeIso0Matcher {
    def unapply[A, B, C](p: Rep[Iso0[A, C]]) = unmkComposeIso0(p)
  }
  lazy val ComposeIso0: Rep[ComposeIso0CompanionAbs] = new ComposeIso0CompanionAbs
  implicit def proxyComposeIso0Companion(p: Rep[ComposeIso0CompanionAbs]): ComposeIso0CompanionAbs = {
    proxyOps[ComposeIso0CompanionAbs](p)
  }

  implicit case object ComposeIso0CompanionElem extends CompanionElem[ComposeIso0CompanionAbs] {
    lazy val tag = weakTypeTag[ComposeIso0CompanionAbs]
    protected def getDefaultRep = ComposeIso0
  }

  implicit def proxyComposeIso0[A, B, C](p: Rep[ComposeIso0[A, B, C]]): ComposeIso0[A, B, C] =
    proxyOps[ComposeIso0[A, B, C]](p)

  implicit class ExtendedComposeIso0[A, B, C](p: Rep[ComposeIso0[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]) {
    def toData: Rep[ComposeIso0Data[A, B, C]] = isoComposeIso0(eA, eB, eC).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoComposeIso0[A, B, C](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Iso[ComposeIso0Data[A, B, C], ComposeIso0[A, B, C]] =
    cachedIso[ComposeIso0Iso[A, B, C]](eA, eB, eC)

  // 6) smart constructor and deconstructor
  def mkComposeIso0[A, B, C](iso02: RIso[B, C], iso01: RIso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]]
  def unmkComposeIso0[A, B, C](p: Rep[Iso0[A, C]]): Option[(Rep[Iso0[B, C]], Rep[Iso0[A, B]])]

  abstract class AbsFuncIso0[A, B, C, D]
      (iso01: RIso[A, B], iso02: RIso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends FuncIso0[A, B, C, D](iso01, iso02) with Def[FuncIso0[A, B, C, D]] {
    lazy val selfType = element[FuncIso0[A, B, C, D]]
  }
  // elem for concrete class
  class FuncIso0Elem[A, B, C, D](val iso: Iso[FuncIso0Data[A, B, C, D], FuncIso0[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends Iso0Elem[A => C, B => D, FuncIso0[A, B, C, D]]
    with ConcreteElem[FuncIso0Data[A, B, C, D], FuncIso0[A, B, C, D]] {
    override lazy val parent: Option[Elem[_]] = Some(iso0Element(funcElement(element[A],element[C]), funcElement(element[B],element[D])))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB), "C" -> Left(eC), "D" -> Left(eD))
    }

    override def convertIso0(x: Rep[Iso0[A => C, B => D]]) = // Converter is not generated by meta
!!!("Cannot convert from Iso0 to FuncIso0: missing fields List(iso01, iso02)")
    override def getDefaultRep = FuncIso0(element[Iso0[A, B]].defaultRepValue, element[Iso0[C, D]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      implicit val tagC = eC.tag
      implicit val tagD = eD.tag
      weakTypeTag[FuncIso0[A, B, C, D]]
    }
  }

  // state representation type
  type FuncIso0Data[A, B, C, D] = (Iso0[A, B], Iso0[C, D])

  // 3) Iso for concrete class
  class FuncIso0Iso[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends Iso[FuncIso0Data[A, B, C, D], FuncIso0[A, B, C, D]]()(pairElement(implicitly[Elem[Iso0[A, B]]], implicitly[Elem[Iso0[C, D]]])) {
    override def from(p: Rep[FuncIso0[A, B, C, D]]) =
      (p.iso01, p.iso02)
    override def to(p: Rep[(Iso0[A, B], Iso0[C, D])]) = {
      val Pair(iso01, iso02) = p
      FuncIso0(iso01, iso02)
    }
    lazy val eTo = new FuncIso0Elem[A, B, C, D](this)
  }
  // 4) constructor and deconstructor
  class FuncIso0CompanionAbs extends CompanionDef[FuncIso0CompanionAbs] {
    def selfType = FuncIso0CompanionElem
    override def toString = "FuncIso0"
    def apply[A, B, C, D](p: Rep[FuncIso0Data[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
      isoFuncIso0(eA, eB, eC, eD).to(p)
    def apply[A, B, C, D](iso01: RIso[A, B], iso02: RIso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
      mkFuncIso0(iso01, iso02)
  }
  object FuncIso0Matcher {
    def unapply[A, B, C, D](p: Rep[Iso0[A => C, B => D]]) = unmkFuncIso0(p)
  }
  lazy val FuncIso0: Rep[FuncIso0CompanionAbs] = new FuncIso0CompanionAbs
  implicit def proxyFuncIso0Companion(p: Rep[FuncIso0CompanionAbs]): FuncIso0CompanionAbs = {
    proxyOps[FuncIso0CompanionAbs](p)
  }

  implicit case object FuncIso0CompanionElem extends CompanionElem[FuncIso0CompanionAbs] {
    lazy val tag = weakTypeTag[FuncIso0CompanionAbs]
    protected def getDefaultRep = FuncIso0
  }

  implicit def proxyFuncIso0[A, B, C, D](p: Rep[FuncIso0[A, B, C, D]]): FuncIso0[A, B, C, D] =
    proxyOps[FuncIso0[A, B, C, D]](p)

  implicit class ExtendedFuncIso0[A, B, C, D](p: Rep[FuncIso0[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]) {
    def toData: Rep[FuncIso0Data[A, B, C, D]] = isoFuncIso0(eA, eB, eC, eD).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoFuncIso0[A, B, C, D](implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Iso[FuncIso0Data[A, B, C, D], FuncIso0[A, B, C, D]] =
    cachedIso[FuncIso0Iso[A, B, C, D]](eA, eB, eC, eD)

  // 6) smart constructor and deconstructor
  def mkFuncIso0[A, B, C, D](iso01: RIso[A, B], iso02: RIso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]]
  def unmkFuncIso0[A, B, C, D](p: Rep[Iso0[A => C, B => D]]): Option[(Rep[Iso0[A, B]], Rep[Iso0[C, D]])]

  abstract class AbsConverterIso0[A, B]
      (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends ConverterIso0[A, B](convTo, convFrom) with Def[ConverterIso0[A, B]] {
    lazy val selfType = element[ConverterIso0[A, B]]
  }
  // elem for concrete class
  class ConverterIso0Elem[A, B](val iso: Iso[ConverterIso0Data[A, B], ConverterIso0[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends Iso0Elem[A, B, ConverterIso0[A, B]]
    with ConcreteElem[ConverterIso0Data[A, B], ConverterIso0[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso0Element(element[A], element[B]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertIso0(x: Rep[Iso0[A, B]]) = // Converter is not generated by meta
!!!("Cannot convert from Iso0 to ConverterIso0: missing fields List(convTo, convFrom)")
    override def getDefaultRep = ConverterIso0(element[Converter[A, B]].defaultRepValue, element[Converter[B, A]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ConverterIso0[A, B]]
    }
  }

  // state representation type
  type ConverterIso0Data[A, B] = (Converter[A, B], Converter[B, A])

  // 3) Iso for concrete class
  class ConverterIso0Iso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[ConverterIso0Data[A, B], ConverterIso0[A, B]]()(pairElement(implicitly[Elem[Converter[A, B]]], implicitly[Elem[Converter[B, A]]])) {
    override def from(p: Rep[ConverterIso0[A, B]]) =
      (p.convTo, p.convFrom)
    override def to(p: Rep[(Converter[A, B], Converter[B, A])]) = {
      val Pair(convTo, convFrom) = p
      ConverterIso0(convTo, convFrom)
    }
    lazy val eTo = new ConverterIso0Elem[A, B](this)
  }
  // 4) constructor and deconstructor
  class ConverterIso0CompanionAbs extends CompanionDef[ConverterIso0CompanionAbs] {
    def selfType = ConverterIso0CompanionElem
    override def toString = "ConverterIso0"
    def apply[A, B](p: Rep[ConverterIso0Data[A, B]])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso0[A, B]] =
      isoConverterIso0(eA, eB).to(p)
    def apply[A, B](convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso0[A, B]] =
      mkConverterIso0(convTo, convFrom)
  }
  object ConverterIso0Matcher {
    def unapply[A, B](p: Rep[Iso0[A, B]]) = unmkConverterIso0(p)
  }
  lazy val ConverterIso0: Rep[ConverterIso0CompanionAbs] = new ConverterIso0CompanionAbs
  implicit def proxyConverterIso0Companion(p: Rep[ConverterIso0CompanionAbs]): ConverterIso0CompanionAbs = {
    proxyOps[ConverterIso0CompanionAbs](p)
  }

  implicit case object ConverterIso0CompanionElem extends CompanionElem[ConverterIso0CompanionAbs] {
    lazy val tag = weakTypeTag[ConverterIso0CompanionAbs]
    protected def getDefaultRep = ConverterIso0
  }

  implicit def proxyConverterIso0[A, B](p: Rep[ConverterIso0[A, B]]): ConverterIso0[A, B] =
    proxyOps[ConverterIso0[A, B]](p)

  implicit class ExtendedConverterIso0[A, B](p: Rep[ConverterIso0[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ConverterIso0Data[A, B]] = isoConverterIso0(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConverterIso0[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ConverterIso0Data[A, B], ConverterIso0[A, B]] =
    cachedIso[ConverterIso0Iso[A, B]](eA, eB)

  // 6) smart constructor and deconstructor
  def mkConverterIso0[A, B](convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso0[A, B]]
  def unmkConverterIso0[A, B](p: Rep[Iso0[A, B]]): Option[(Rep[Converter[A, B]], Rep[Converter[B, A]])]

  registerModule(Views1_Module)
}

// Seq -----------------------------------
trait Views1Seq extends Views1Dsl  {
  self: Views1Dsl with ScalanSeq =>
  lazy val Iso0: Rep[Iso0CompanionAbs] = new Iso0CompanionAbs {
  }

  case class SeqIdentityIso0[A]
      ()(implicit eA: Elem[A])
    extends AbsIdentityIso0[A]() {
  }

  def mkIdentityIso0[A]
    ()(implicit eA: Elem[A]): Rep[IdentityIso0[A]] =
    new SeqIdentityIso0[A]()
  def unmkIdentityIso0[A](p: Rep[Iso0[A, A]]) = p match {
    case p: IdentityIso0[A] @unchecked =>
      Some(())
    case _ => None
  }

  case class SeqPairIso0[A1, A2, B1, B2]
      (override val iso01: RIso[A1, B1], override val iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairIso0[A1, A2, B1, B2](iso01, iso02) {
  }

  def mkPairIso0[A1, A2, B1, B2]
    (iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
    new SeqPairIso0[A1, A2, B1, B2](iso01, iso02)
  def unmkPairIso0[A1, A2, B1, B2](p: Rep[Iso0[(A1, A2), (B1, B2)]]) = p match {
    case p: PairIso0[A1, A2, B1, B2] @unchecked =>
      Some((p.iso01, p.iso02))
    case _ => None
  }

  case class SeqSumIso0[A1, A2, B1, B2]
      (override val iso01: RIso[A1, B1], override val iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumIso0[A1, A2, B1, B2](iso01, iso02) {
  }

  def mkSumIso0[A1, A2, B1, B2]
    (iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
    new SeqSumIso0[A1, A2, B1, B2](iso01, iso02)
  def unmkSumIso0[A1, A2, B1, B2](p: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]) = p match {
    case p: SumIso0[A1, A2, B1, B2] @unchecked =>
      Some((p.iso01, p.iso02))
    case _ => None
  }

  case class SeqComposeIso0[A, B, C]
      (override val iso02: RIso[B, C], override val iso01: RIso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeIso0[A, B, C](iso02, iso01) {
  }

  def mkComposeIso0[A, B, C]
    (iso02: RIso[B, C], iso01: RIso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
    new SeqComposeIso0[A, B, C](iso02, iso01)
  def unmkComposeIso0[A, B, C](p: Rep[Iso0[A, C]]) = p match {
    case p: ComposeIso0[A, B, C] @unchecked =>
      Some((p.iso02, p.iso01))
    case _ => None
  }

  case class SeqFuncIso0[A, B, C, D]
      (override val iso01: RIso[A, B], override val iso02: RIso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends AbsFuncIso0[A, B, C, D](iso01, iso02) {
  }

  def mkFuncIso0[A, B, C, D]
    (iso01: RIso[A, B], iso02: RIso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
    new SeqFuncIso0[A, B, C, D](iso01, iso02)
  def unmkFuncIso0[A, B, C, D](p: Rep[Iso0[A => C, B => D]]) = p match {
    case p: FuncIso0[A, B, C, D] @unchecked =>
      Some((p.iso01, p.iso02))
    case _ => None
  }

  case class SeqConverterIso0[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsConverterIso0[A, B](convTo, convFrom) {
  }

  def mkConverterIso0[A, B]
    (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso0[A, B]] =
    new SeqConverterIso0[A, B](convTo, convFrom)
  def unmkConverterIso0[A, B](p: Rep[Iso0[A, B]]) = p match {
    case p: ConverterIso0[A, B] @unchecked =>
      Some((p.convTo, p.convFrom))
    case _ => None
  }
}

// Exp -----------------------------------
trait Views1Exp extends Views1Dsl  {
  self: Views1Dsl with ScalanExp =>
  lazy val Iso0: Rep[Iso0CompanionAbs] = new Iso0CompanionAbs {
  }

  case class ExpIdentityIso0[A]
      ()(implicit eA: Elem[A])
    extends AbsIdentityIso0[A]()

  object IdentityIso0Methods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[IdentityIso0[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[IdentityIso0Elem[_]] && method.getName == "from" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[IdentityIso0[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IdentityIso0[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[IdentityIso0[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[IdentityIso0Elem[_]] && method.getName == "to" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[IdentityIso0[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IdentityIso0[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[IdentityIso0[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IdentityIso0Elem[_]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[IdentityIso0[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IdentityIso0[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIdentityIso0[A]
    ()(implicit eA: Elem[A]): Rep[IdentityIso0[A]] =
    new ExpIdentityIso0[A]()
  def unmkIdentityIso0[A](p: Rep[Iso0[A, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IdentityIso0Elem[A] @unchecked =>
      Some(())
    case _ =>
      None
  }

  case class ExpPairIso0[A1, A2, B1, B2]
      (override val iso01: RIso[A1, B1], override val iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairIso0[A1, A2, B1, B2](iso01, iso02)

  object PairIso0Methods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[PairIso0[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[PairIso0Elem[_, _, _, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[PairIso0[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairIso0[A1, A2, B1, B2]], Rep[(B1, B2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[PairIso0[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[PairIso0Elem[_, _, _, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[PairIso0[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairIso0[A1, A2, B1, B2]], Rep[(A1, A2)]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[PairIso0[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairIso0Elem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[PairIso0[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairIso0[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairIso0CompanionMethods {
  }

  def mkPairIso0[A1, A2, B1, B2]
    (iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
    new ExpPairIso0[A1, A2, B1, B2](iso01, iso02)
  def unmkPairIso0[A1, A2, B1, B2](p: Rep[Iso0[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairIso0Elem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[PairIso0[A1, A2, B1, B2]].iso01, p.asRep[PairIso0[A1, A2, B1, B2]].iso02))
    case _ =>
      None
  }

  case class ExpSumIso0[A1, A2, B1, B2]
      (override val iso01: RIso[A1, B1], override val iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumIso0[A1, A2, B1, B2](iso01, iso02)

  object SumIso0Methods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[SumIso0[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[SumIso0Elem[_, _, _, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[SumIso0[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SumIso0[A1, A2, B1, B2]], Rep[$bar[B1, B2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[SumIso0[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[SumIso0Elem[_, _, _, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[SumIso0[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SumIso0[A1, A2, B1, B2]], Rep[$bar[A1, A2]]) forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[SumIso0[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SumIso0Elem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[SumIso0[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SumIso0[A1, A2, B1, B2]] forSome {type A1; type A2; type B1; type B2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkSumIso0[A1, A2, B1, B2]
    (iso01: RIso[A1, B1], iso02: RIso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
    new ExpSumIso0[A1, A2, B1, B2](iso01, iso02)
  def unmkSumIso0[A1, A2, B1, B2](p: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumIso0Elem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[SumIso0[A1, A2, B1, B2]].iso01, p.asRep[SumIso0[A1, A2, B1, B2]].iso02))
    case _ =>
      None
  }

  case class ExpComposeIso0[A, B, C]
      (override val iso02: RIso[B, C], override val iso01: RIso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeIso0[A, B, C](iso02, iso01)

  object ComposeIso0Methods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[ComposeIso0[A, B, C]], Rep[C]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(c, _*), _) if receiver.elem.isInstanceOf[ComposeIso0Elem[_, _, _]] && method.getName == "from" =>
          Some((receiver, c)).asInstanceOf[Option[(Rep[ComposeIso0[A, B, C]], Rep[C]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ComposeIso0[A, B, C]], Rep[C]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[ComposeIso0[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[ComposeIso0Elem[_, _, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[ComposeIso0[A, B, C]], Rep[A]) forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ComposeIso0[A, B, C]], Rep[A]) forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[ComposeIso0[A, B, C]] forSome {type A; type B; type C}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ComposeIso0Elem[_, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[ComposeIso0[A, B, C]] forSome {type A; type B; type C}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ComposeIso0[A, B, C]] forSome {type A; type B; type C}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkComposeIso0[A, B, C]
    (iso02: RIso[B, C], iso01: RIso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
    new ExpComposeIso0[A, B, C](iso02, iso01)
  def unmkComposeIso0[A, B, C](p: Rep[Iso0[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeIso0Elem[A, B, C] @unchecked =>
      Some((p.asRep[ComposeIso0[A, B, C]].iso02, p.asRep[ComposeIso0[A, B, C]].iso01))
    case _ =>
      None
  }

  case class ExpFuncIso0[A, B, C, D]
      (override val iso01: RIso[A, B], override val iso02: RIso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends AbsFuncIso0[A, B, C, D](iso01, iso02)

  object FuncIso0Methods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[FuncIso0[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[FuncIso0Elem[_, _, _, _]] && method.getName == "from" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[FuncIso0[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[FuncIso0[A, B, C, D]], Rep[B => D]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[FuncIso0[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[FuncIso0Elem[_, _, _, _]] && method.getName == "to" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[FuncIso0[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[FuncIso0[A, B, C, D]], Rep[A => C]) forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[FuncIso0[A, B, C, D]] forSome {type A; type B; type C; type D}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[FuncIso0Elem[_, _, _, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[FuncIso0[A, B, C, D]] forSome {type A; type B; type C; type D}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[FuncIso0[A, B, C, D]] forSome {type A; type B; type C; type D}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkFuncIso0[A, B, C, D]
    (iso01: RIso[A, B], iso02: RIso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
    new ExpFuncIso0[A, B, C, D](iso01, iso02)
  def unmkFuncIso0[A, B, C, D](p: Rep[Iso0[A => C, B => D]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FuncIso0Elem[A, B, C, D] @unchecked =>
      Some((p.asRep[FuncIso0[A, B, C, D]].iso01, p.asRep[FuncIso0[A, B, C, D]].iso02))
    case _ =>
      None
  }

  case class ExpConverterIso0[A, B]
      (override val convTo: Conv[A, B], override val convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsConverterIso0[A, B](convTo, convFrom)

  object ConverterIso0Methods {
    object to {
      def unapply(d: Def[_]): Option[(Rep[ConverterIso0[A, B]], Rep[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(a, _*), _) if receiver.elem.isInstanceOf[ConverterIso0Elem[_, _]] && method.getName == "to" =>
          Some((receiver, a)).asInstanceOf[Option[(Rep[ConverterIso0[A, B]], Rep[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConverterIso0[A, B]], Rep[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object from {
      def unapply(d: Def[_]): Option[(Rep[ConverterIso0[A, B]], Rep[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(b, _*), _) if receiver.elem.isInstanceOf[ConverterIso0Elem[_, _]] && method.getName == "from" =>
          Some((receiver, b)).asInstanceOf[Option[(Rep[ConverterIso0[A, B]], Rep[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ConverterIso0[A, B]], Rep[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isIdentity {
      def unapply(d: Def[_]): Option[Rep[ConverterIso0[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ConverterIso0Elem[_, _]] && method.getName == "isIdentity" =>
          Some(receiver).asInstanceOf[Option[Rep[ConverterIso0[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ConverterIso0[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkConverterIso0[A, B]
    (convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso0[A, B]] =
    new ExpConverterIso0[A, B](convTo, convFrom)
  def unmkConverterIso0[A, B](p: Rep[Iso0[A, B]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConverterIso0Elem[A, B] @unchecked =>
      Some((p.asRep[ConverterIso0[A, B]].convTo, p.asRep[ConverterIso0[A, B]].convFrom))
    case _ =>
      None
  }

  object Iso0Methods {
    object from {
      def unapply(d: Def[_]): Option[(Rep[Iso0[From, To]], Rep[To]) forSome {type From; type To}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[Iso0Elem[_, _, _]] && method.getName == "from" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[Iso0[From, To]], Rep[To]) forSome {type From; type To}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iso0[From, To]], Rep[To]) forSome {type From; type To}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[Iso0[From, To]], Rep[From]) forSome {type From; type To}] = d match {
        case MethodCall(receiver, method, Seq(p, _*), _) if receiver.elem.isInstanceOf[Iso0Elem[_, _, _]] && method.getName == "to" =>
          Some((receiver, p)).asInstanceOf[Option[(Rep[Iso0[From, To]], Rep[From]) forSome {type From; type To}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iso0[From, To]], Rep[From]) forSome {type From; type To}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toString`: Overrides Object method

    // WARNING: Cannot generate matcher for method `equals`: Overrides Object method

    // WARNING: Cannot generate matcher for method `isIdentity`: Method's return type Boolean is not a Rep
  }
}

object Views1_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAO1YTWwbRRQeb/wbh6ahUARVmxAZUBHEiYXUQw6V104gyE2ibEDIVK3G60m6ZXdnszMONocKcagQcEJcOHCoxLEXxAUJiRsS4sCpqpA4ceBUilAPVBxAzMz+eL3eXTuJQKiQw2p38/Z7733ve29mfPMuyBAbPE1UqENzwUAULijivkpoSVkxqUZ7F3C7o6M62nnn5BfqBVMmEphuguwVSOpEb4KCc7PStfx7Be01QAGaKiIU24SCJxvCQ1nFuo5UqmGzrBlGh8KWjsoNjdDlBki3cLu3B66BVAMcV7Gp2ogipaZDQhBx3+cRj0jznwviubdh9X2YZZ5FOZDFtg01ysJnPo479lvIUnomNnsGBcfc0DYsHhazyWmGhW3qucgxuCu47T2mTchegIcbV+E+LDMXu2WF2pq5y74sWlB9A+6idWbCzdMsYIL0ne2eJZ4nGmCSoD1G0Jph6eJN1wIAsApURBALfX4WfH4WOD8lBdka1LW3IP/npo27PeD8pSYA6FoM4rkREB4CWjHbpfcuqq/fV4qGxD/u8lByIsMsA5qNUYMoBePxm60Pyb0Xb5yTwGQTTGqk2iLUhioNltxlqwhNE1MRs08gtHdZtebjqiW8VJlNSBIFFRsWNBmSS+UUq5OuqRrlxvzdlFudGOpz1EKeaaprpfx852LyFbqpQV3fvPP480/9vPKaBKRBFwUGqTDh2x4oBek1ghddaH6dpiBV7fPbf+SXQrd/zSVE4nPyzJ1f2l8vgouSz6Tr2IfkMEVHCevYRKXVzdJvyrcf3eSFtsGU8x9H639q5/744dgOFRoQQI+NpwEWSYZ8f7t46+x5CeSbomNWdbjbZDUhKzoyNuwaNmkT5PE+sp3/5Pahzu8iNZFrox3Y0albqSDFE4xiCuZie9tCnP9lVk/WCj4LpyiQUNVjPM1jiqxJsAgUTK21nfnAayhgfD5Ox2lEaOrkVuMR/e75rySQeRlkdliapAEyLdwx255Y2UCjqEtl711qME0mTmhDwyun09pzQAQhIg2FLAyLqcGkgtqLLyELebtj6eiFL3+/9O7bL1lCS0NKHkSWqksDGpaqlXAwS6Ev5NAXcmWI8aD4g/Lj1zMgVM+MxpJb8lPdYrmOiNGNIBBjNGglEbQyVhq87Sad5lKwgWbm72mXbrxPBbep7uB6sdG6ygb0svjuiaRCuevWZ9evP/rrp5dPiHmbb2nUgFZp8QDT1huOf+M0BYEBJErZfxZUs8k4swk1myu0FnR9Kqb84XaeQNV+8Yf72St+sNgREJVEiCFNR0DIiVFESC4CIjGKCIFRkPe4C04lfj0bNSkGOiHGojLKQh6JIQ/xlTSQvFelFrQPPFxiPvyPzZh+8f9vldhWySkd44HolPC2sRYtijFVnSTAlBzv6ZCNEg5ePpKiD7CXiwOQkwDkMQBqSQDDtaGgyNc5TNDYYgxyFm0gjzIYqlzyFvFM/BZxtWOqt9Y+PjF9+vKP4tyTbWMDamLNnmWbW5vtZMReYNbdK8Yqlj+mZuK0Vj+Sqg8rwcM2Sm2s4B9EVccB1JMAhvlhWxmurH++JxIM6uE0DzGcY+bbaBln2cFwfxv7rtjBef+oOs5zzFUbG0mooT6M+VHiXyhlT0gP8aSQTdH4G+Nx1RR0FMB8BQyGNcEOZyOVkuaFGOBa2saRZA9oxrfwz0/psdMMuYzek0QHkfqgb+RaZl/V0Jtkid04q4XA3GKrx3zM6qG4J0XGzrX7n6w/+93nP4kVZJKfObGJTP+31f4BM/yDQ8FxWif6AHVZx00gcpYtP5mKqP8C/4IJMc4WAAA="
}
}

