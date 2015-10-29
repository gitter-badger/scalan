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
trait ViewsAbs extends Views  {
  self: ViewsDsl with Scalan =>

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

  // single proxy for each type family
  implicit def proxyIso10[A, B, C[_]](p: Rep[Iso10[A, B, C]]): Iso10[A, B, C] = {
    proxyOps[Iso10[A, B, C]](p)(scala.reflect.classTag[Iso10[A, B, C]])
  }
  // familyElem
  class Iso10Elem[A, B, C[_], To <: Iso10[A, B, C]](implicit _eA: Elem[A], _eB: Elem[B], _cC: Container[C])
    extends Iso0Elem[C[A], C[B], To] {
    def eA = _eA
    def eB = _eB
    def cC = _cC
    override lazy val parent: Option[Elem[_]] = Some(iso0Element(element[C[A]], element[C[B]]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB), "C" -> Right(cC.asInstanceOf[SomeCont]))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[Iso10[A, B, C]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Iso10[A, B, C]] => convertIso10(x) }
      tryConvert(element[Iso10[A, B, C]], this, x, conv)
    }

    def convertIso10(x: Rep[Iso10[A, B, C]]): Rep[To] = {
      x.selfType1.asInstanceOf[Element[_]] match {
        case _: Iso10Elem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have Iso10Elem[_, _, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iso10Element[A, B, C[_]](implicit eA: Elem[A], eB: Elem[B], cC: Container[C]): Elem[Iso10[A, B, C]] =
    cachedElem[Iso10Elem[A, B, C, Iso10[A, B, C]]](eA, eB, cC)

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
    extends Iso0[IdentityIso0Data[A], IdentityIso0[A]] {
    override def from(p: Rep[IdentityIso0[A]]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      IdentityIso0()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new IdentityIso0Elem[A](self)
    lazy val selfType = new ConcreteIso0Elem[IdentityIso0Data[A], IdentityIso0[A], IdentityIso0Iso[A]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[IdentityIso0Data[A], IdentityIso0[A]]]]
    def productArity = 1
    def productElement(n: Int) = eA
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
    reifyObject(new IdentityIso0Iso[A]()(eA))

  // 6) smart constructor and deconstructor
  def mkIdentityIso0[A]()(implicit eA: Elem[A]): Rep[IdentityIso0[A]]
  def unmkIdentityIso0[A](p: Rep[Iso0[A, A]]): Option[(Rep[Unit])]

  abstract class AbsPairIso0[A1, A2, B1, B2]
      (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends PairIso0[A1, A2, B1, B2](iso1, iso2) with Def[PairIso0[A1, A2, B1, B2]] {
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
!!!("Cannot convert from Iso0 to PairIso0: missing fields List(iso1, iso2)")
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
    extends Iso0[PairIso0Data[A1, A2, B1, B2], PairIso0[A1, A2, B1, B2]] {
    override def from(p: Rep[PairIso0[A1, A2, B1, B2]]) =
      (p.iso1, p.iso2)
    override def to(p: Rep[(Iso0[A1, B1], Iso0[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      PairIso0(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[Iso0[A1, B1]], element[Iso0[A2, B2]])
    lazy val eTo = new PairIso0Elem[A1, A2, B1, B2](self)
    lazy val selfType = new ConcreteIso0Elem[PairIso0Data[A1, A2, B1, B2], PairIso0[A1, A2, B1, B2], PairIso0Iso[A1, A2, B1, B2]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[PairIso0Data[A1, A2, B1, B2], PairIso0[A1, A2, B1, B2]]]]
    def productArity = 4
    def productElement(n: Int) = (eA1, eA2, eB1, eB2).productElement(n)
  }
  // 4) constructor and deconstructor
  class PairIso0CompanionAbs extends CompanionDef[PairIso0CompanionAbs] with PairIso0Companion {
    def selfType = PairIso0CompanionElem
    override def toString = "PairIso0"
    def apply[A1, A2, B1, B2](p: Rep[PairIso0Data[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
      isoPairIso0(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
      mkPairIso0(iso1, iso2)
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
    reifyObject(new PairIso0Iso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  // 6) smart constructor and deconstructor
  def mkPairIso0[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]]
  def unmkPairIso0[A1, A2, B1, B2](p: Rep[Iso0[(A1, A2), (B1, B2)]]): Option[(Rep[Iso0[A1, B1]], Rep[Iso0[A2, B2]])]

  abstract class AbsSumIso0[A1, A2, B1, B2]
      (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends SumIso0[A1, A2, B1, B2](iso1, iso2) with Def[SumIso0[A1, A2, B1, B2]] {
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
!!!("Cannot convert from Iso0 to SumIso0: missing fields List(iso1, iso2)")
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
    extends Iso0[SumIso0Data[A1, A2, B1, B2], SumIso0[A1, A2, B1, B2]] {
    override def from(p: Rep[SumIso0[A1, A2, B1, B2]]) =
      (p.iso1, p.iso2)
    override def to(p: Rep[(Iso0[A1, B1], Iso0[A2, B2])]) = {
      val Pair(iso1, iso2) = p
      SumIso0(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[Iso0[A1, B1]], element[Iso0[A2, B2]])
    lazy val eTo = new SumIso0Elem[A1, A2, B1, B2](self)
    lazy val selfType = new ConcreteIso0Elem[SumIso0Data[A1, A2, B1, B2], SumIso0[A1, A2, B1, B2], SumIso0Iso[A1, A2, B1, B2]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[SumIso0Data[A1, A2, B1, B2], SumIso0[A1, A2, B1, B2]]]]
    def productArity = 4
    def productElement(n: Int) = (eA1, eA2, eB1, eB2).productElement(n)
  }
  // 4) constructor and deconstructor
  class SumIso0CompanionAbs extends CompanionDef[SumIso0CompanionAbs] {
    def selfType = SumIso0CompanionElem
    override def toString = "SumIso0"
    def apply[A1, A2, B1, B2](p: Rep[SumIso0Data[A1, A2, B1, B2]])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
      isoSumIso0(eA1, eA2, eB1, eB2).to(p)
    def apply[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
      mkSumIso0(iso1, iso2)
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
    reifyObject(new SumIso0Iso[A1, A2, B1, B2]()(eA1, eA2, eB1, eB2))

  // 6) smart constructor and deconstructor
  def mkSumIso0[A1, A2, B1, B2](iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]]
  def unmkSumIso0[A1, A2, B1, B2](p: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]): Option[(Rep[Iso0[A1, B1]], Rep[Iso0[A2, B2]])]

  abstract class AbsComposeIso0[A, B, C]
      (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends ComposeIso0[A, B, C](iso2, iso1) with Def[ComposeIso0[A, B, C]] {
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
!!!("Cannot convert from Iso0 to ComposeIso0: missing fields List(iso2, iso1)")
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
    extends Iso0[ComposeIso0Data[A, B, C], ComposeIso0[A, B, C]] {
    override def from(p: Rep[ComposeIso0[A, B, C]]) =
      (p.iso2, p.iso1)
    override def to(p: Rep[(Iso0[B, C], Iso0[A, B])]) = {
      val Pair(iso2, iso1) = p
      ComposeIso0(iso2, iso1)
    }
    lazy val eFrom = pairElement(element[Iso0[B, C]], element[Iso0[A, B]])
    lazy val eTo = new ComposeIso0Elem[A, B, C](self)
    lazy val selfType = new ConcreteIso0Elem[ComposeIso0Data[A, B, C], ComposeIso0[A, B, C], ComposeIso0Iso[A, B, C]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[ComposeIso0Data[A, B, C], ComposeIso0[A, B, C]]]]
    def productArity = 3
    def productElement(n: Int) = (eA, eB, eC).productElement(n)
  }
  // 4) constructor and deconstructor
  class ComposeIso0CompanionAbs extends CompanionDef[ComposeIso0CompanionAbs] {
    def selfType = ComposeIso0CompanionElem
    override def toString = "ComposeIso0"
    def apply[A, B, C](p: Rep[ComposeIso0Data[A, B, C]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
      isoComposeIso0(eA, eB, eC).to(p)
    def apply[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
      mkComposeIso0(iso2, iso1)
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
    reifyObject(new ComposeIso0Iso[A, B, C]()(eA, eB, eC))

  // 6) smart constructor and deconstructor
  def mkComposeIso0[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]]
  def unmkComposeIso0[A, B, C](p: Rep[Iso0[A, C]]): Option[(Rep[Iso0[B, C]], Rep[Iso0[A, B]])]

  abstract class AbsFuncIso0[A, B, C, D]
      (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends FuncIso0[A, B, C, D](iso1, iso2) with Def[FuncIso0[A, B, C, D]] {
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
!!!("Cannot convert from Iso0 to FuncIso0: missing fields List(iso1, iso2)")
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
    extends Iso0[FuncIso0Data[A, B, C, D], FuncIso0[A, B, C, D]] {
    override def from(p: Rep[FuncIso0[A, B, C, D]]) =
      (p.iso1, p.iso2)
    override def to(p: Rep[(Iso0[A, B], Iso0[C, D])]) = {
      val Pair(iso1, iso2) = p
      FuncIso0(iso1, iso2)
    }
    lazy val eFrom = pairElement(element[Iso0[A, B]], element[Iso0[C, D]])
    lazy val eTo = new FuncIso0Elem[A, B, C, D](self)
    lazy val selfType = new ConcreteIso0Elem[FuncIso0Data[A, B, C, D], FuncIso0[A, B, C, D], FuncIso0Iso[A, B, C, D]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[FuncIso0Data[A, B, C, D], FuncIso0[A, B, C, D]]]]
    def productArity = 4
    def productElement(n: Int) = (eA, eB, eC, eD).productElement(n)
  }
  // 4) constructor and deconstructor
  class FuncIso0CompanionAbs extends CompanionDef[FuncIso0CompanionAbs] {
    def selfType = FuncIso0CompanionElem
    override def toString = "FuncIso0"
    def apply[A, B, C, D](p: Rep[FuncIso0Data[A, B, C, D]])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
      isoFuncIso0(eA, eB, eC, eD).to(p)
    def apply[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
      mkFuncIso0(iso1, iso2)
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
    reifyObject(new FuncIso0Iso[A, B, C, D]()(eA, eB, eC, eD))

  // 6) smart constructor and deconstructor
  def mkFuncIso0[A, B, C, D](iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]]
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
    extends Iso0[ConverterIso0Data[A, B], ConverterIso0[A, B]] {
    override def from(p: Rep[ConverterIso0[A, B]]) =
      (p.convTo, p.convFrom)
    override def to(p: Rep[(Converter[A, B], Converter[B, A])]) = {
      val Pair(convTo, convFrom) = p
      ConverterIso0(convTo, convFrom)
    }
    lazy val eFrom = pairElement(element[Converter[A, B]], element[Converter[B, A]])
    lazy val eTo = new ConverterIso0Elem[A, B](self)
    lazy val selfType = new ConcreteIso0Elem[ConverterIso0Data[A, B], ConverterIso0[A, B], ConverterIso0Iso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[ConverterIso0Data[A, B], ConverterIso0[A, B]]]]
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
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
    reifyObject(new ConverterIso0Iso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkConverterIso0[A, B](convTo: Conv[A, B], convFrom: Conv[B, A])(implicit eA: Elem[A], eB: Elem[B]): Rep[ConverterIso0[A, B]]
  def unmkConverterIso0[A, B](p: Rep[Iso0[A, B]]): Option[(Rep[Converter[A, B]], Rep[Converter[B, A]])]

  abstract class AbsArrayIso[A, B]
      (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends ArrayIso[A, B](innerIso) with Def[ArrayIso[A, B]] {
    lazy val selfType = element[ArrayIso[A, B]]
  }
  // elem for concrete class
  class ArrayIsoElem[A, B](val iso: Iso[ArrayIsoData[A, B], ArrayIso[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends Iso10Elem[A, B, Array, ArrayIso[A, B]]
    with ConcreteElem[ArrayIsoData[A, B], ArrayIso[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(iso10Element(element[A], element[B], container[Array]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertIso10(x: Rep[Iso10[A, B, Array]]) = ArrayIso(x.innerIso)
    override def getDefaultRep = ArrayIso(element[Iso0[A, B]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[ArrayIso[A, B]]
    }
  }

  // state representation type
  type ArrayIsoData[A, B] = Iso0[A, B]

  // 3) Iso for concrete class
  class ArrayIsoIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso0[ArrayIsoData[A, B], ArrayIso[A, B]] {
    override def from(p: Rep[ArrayIso[A, B]]) =
      p.innerIso
    override def to(p: Rep[Iso0[A, B]]) = {
      val innerIso = p
      ArrayIso(innerIso)
    }
    lazy val eFrom = element[Iso0[A, B]]
    lazy val eTo = new ArrayIsoElem[A, B](self)
    lazy val selfType = new ConcreteIso0Elem[ArrayIsoData[A, B], ArrayIso[A, B], ArrayIsoIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[ArrayIsoData[A, B], ArrayIso[A, B]]]]
    def productArity = 2
    def productElement(n: Int) = (eA, eB).productElement(n)
  }
  // 4) constructor and deconstructor
  class ArrayIsoCompanionAbs extends CompanionDef[ArrayIsoCompanionAbs] {
    def selfType = ArrayIsoCompanionElem
    override def toString = "ArrayIso"

    def apply[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
      mkArrayIso(innerIso)
  }
  object ArrayIsoMatcher {
    def unapply[A, B](p: Rep[Iso10[A, B, Array]]) = unmkArrayIso(p)
  }
  lazy val ArrayIso: Rep[ArrayIsoCompanionAbs] = new ArrayIsoCompanionAbs
  implicit def proxyArrayIsoCompanion(p: Rep[ArrayIsoCompanionAbs]): ArrayIsoCompanionAbs = {
    proxyOps[ArrayIsoCompanionAbs](p)
  }

  implicit case object ArrayIsoCompanionElem extends CompanionElem[ArrayIsoCompanionAbs] {
    lazy val tag = weakTypeTag[ArrayIsoCompanionAbs]
    protected def getDefaultRep = ArrayIso
  }

  implicit def proxyArrayIso[A, B](p: Rep[ArrayIso[A, B]]): ArrayIso[A, B] =
    proxyOps[ArrayIso[A, B]](p)

  implicit class ExtendedArrayIso[A, B](p: Rep[ArrayIso[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[ArrayIsoData[A, B]] = isoArrayIso(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoArrayIso[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[ArrayIsoData[A, B], ArrayIso[A, B]] =
    reifyObject(new ArrayIsoIso[A, B]()(eA, eB))

  // 6) smart constructor and deconstructor
  def mkArrayIso[A, B](innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]]
  def unmkArrayIso[A, B](p: Rep[Iso10[A, B, Array]]): Option[(Rep[Iso0[A, B]])]

  registerModule(Views_Module)
}

// Seq -----------------------------------
trait ViewsSeq extends ViewsDsl  {
  self: ViewsDsl with ScalanSeq =>
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
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairIso0[A1, A2, B1, B2](iso1, iso2) {
  }

  def mkPairIso0[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
    new SeqPairIso0[A1, A2, B1, B2](iso1, iso2)
  def unmkPairIso0[A1, A2, B1, B2](p: Rep[Iso0[(A1, A2), (B1, B2)]]) = p match {
    case p: PairIso0[A1, A2, B1, B2] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class SeqSumIso0[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumIso0[A1, A2, B1, B2](iso1, iso2) {
  }

  def mkSumIso0[A1, A2, B1, B2]
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
    new SeqSumIso0[A1, A2, B1, B2](iso1, iso2)
  def unmkSumIso0[A1, A2, B1, B2](p: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]) = p match {
    case p: SumIso0[A1, A2, B1, B2] @unchecked =>
      Some((p.iso1, p.iso2))
    case _ => None
  }

  case class SeqComposeIso0[A, B, C]
      (override val iso2: Iso[B, C], override val iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeIso0[A, B, C](iso2, iso1) {
  }

  def mkComposeIso0[A, B, C]
    (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
    new SeqComposeIso0[A, B, C](iso2, iso1)
  def unmkComposeIso0[A, B, C](p: Rep[Iso0[A, C]]) = p match {
    case p: ComposeIso0[A, B, C] @unchecked =>
      Some((p.iso2, p.iso1))
    case _ => None
  }

  case class SeqFuncIso0[A, B, C, D]
      (override val iso1: Iso[A, B], override val iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends AbsFuncIso0[A, B, C, D](iso1, iso2) {
  }

  def mkFuncIso0[A, B, C, D]
    (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
    new SeqFuncIso0[A, B, C, D](iso1, iso2)
  def unmkFuncIso0[A, B, C, D](p: Rep[Iso0[A => C, B => D]]) = p match {
    case p: FuncIso0[A, B, C, D] @unchecked =>
      Some((p.iso1, p.iso2))
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

  case class SeqArrayIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayIso[A, B](innerIso) {
  }

  def mkArrayIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
    new SeqArrayIso[A, B](innerIso)
  def unmkArrayIso[A, B](p: Rep[Iso10[A, B, Array]]) = p match {
    case p: ArrayIso[A, B] @unchecked =>
      Some((p.innerIso))
    case _ => None
  }
}

// Exp -----------------------------------
trait ViewsExp extends ViewsDsl  {
  self: ViewsDsl with ScalanExp =>
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
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsPairIso0[A1, A2, B1, B2](iso1, iso2)

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
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[PairIso0[A1, A2, B1, B2]] =
    new ExpPairIso0[A1, A2, B1, B2](iso1, iso2)
  def unmkPairIso0[A1, A2, B1, B2](p: Rep[Iso0[(A1, A2), (B1, B2)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairIso0Elem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[PairIso0[A1, A2, B1, B2]].iso1, p.asRep[PairIso0[A1, A2, B1, B2]].iso2))
    case _ =>
      None
  }

  case class ExpSumIso0[A1, A2, B1, B2]
      (override val iso1: Iso[A1, B1], override val iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2])
    extends AbsSumIso0[A1, A2, B1, B2](iso1, iso2)

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
    (iso1: Iso[A1, B1], iso2: Iso[A2, B2])(implicit eA1: Elem[A1], eA2: Elem[A2], eB1: Elem[B1], eB2: Elem[B2]): Rep[SumIso0[A1, A2, B1, B2]] =
    new ExpSumIso0[A1, A2, B1, B2](iso1, iso2)
  def unmkSumIso0[A1, A2, B1, B2](p: Rep[Iso0[$bar[A1, A2], $bar[B1, B2]]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SumIso0Elem[A1, A2, B1, B2] @unchecked =>
      Some((p.asRep[SumIso0[A1, A2, B1, B2]].iso1, p.asRep[SumIso0[A1, A2, B1, B2]].iso2))
    case _ =>
      None
  }

  case class ExpComposeIso0[A, B, C]
      (override val iso2: Iso[B, C], override val iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C])
    extends AbsComposeIso0[A, B, C](iso2, iso1)

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
    (iso2: Iso[B, C], iso1: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C]): Rep[ComposeIso0[A, B, C]] =
    new ExpComposeIso0[A, B, C](iso2, iso1)
  def unmkComposeIso0[A, B, C](p: Rep[Iso0[A, C]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ComposeIso0Elem[A, B, C] @unchecked =>
      Some((p.asRep[ComposeIso0[A, B, C]].iso2, p.asRep[ComposeIso0[A, B, C]].iso1))
    case _ =>
      None
  }

  case class ExpFuncIso0[A, B, C, D]
      (override val iso1: Iso[A, B], override val iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D])
    extends AbsFuncIso0[A, B, C, D](iso1, iso2)

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
    (iso1: Iso[A, B], iso2: Iso[C, D])(implicit eA: Elem[A], eB: Elem[B], eC: Elem[C], eD: Elem[D]): Rep[FuncIso0[A, B, C, D]] =
    new ExpFuncIso0[A, B, C, D](iso1, iso2)
  def unmkFuncIso0[A, B, C, D](p: Rep[Iso0[A => C, B => D]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: FuncIso0Elem[A, B, C, D] @unchecked =>
      Some((p.asRep[FuncIso0[A, B, C, D]].iso1, p.asRep[FuncIso0[A, B, C, D]].iso2))
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

  case class ExpArrayIso[A, B]
      (override val innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B])
    extends AbsArrayIso[A, B](innerIso)

  object ArrayIsoMethods {
    object cC {
      def unapply(d: Def[_]): Option[Rep[ArrayIso[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ArrayIsoElem[_, _]] && method.getName == "cC" =>
          Some(receiver).asInstanceOf[Option[Rep[ArrayIso[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[ArrayIso[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object from {
      def unapply(d: Def[_]): Option[(Rep[ArrayIso[A, B]], Arr[B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ArrayIsoElem[_, _]] && method.getName == "from" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ArrayIso[A, B]], Arr[B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ArrayIso[A, B]], Arr[B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object to {
      def unapply(d: Def[_]): Option[(Rep[ArrayIso[A, B]], Arr[A]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, _*), _) if receiver.elem.isInstanceOf[ArrayIsoElem[_, _]] && method.getName == "to" =>
          Some((receiver, x)).asInstanceOf[Option[(Rep[ArrayIso[A, B]], Arr[A]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[ArrayIso[A, B]], Arr[A]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkArrayIso[A, B]
    (innerIso: Iso[A, B])(implicit eA: Elem[A], eB: Elem[B]): Rep[ArrayIso[A, B]] =
    new ExpArrayIso[A, B](innerIso)
  def unmkArrayIso[A, B](p: Rep[Iso10[A, B, Array]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ArrayIsoElem[A, B] @unchecked =>
      Some((p.asRep[ArrayIso[A, B]].innerIso))
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

    object canEqual {
      def unapply(d: Def[_]): Option[(Rep[Iso0[From, To]], Any) forSome {type From; type To}] = d match {
        case MethodCall(receiver, method, Seq(other, _*), _) if receiver.elem.isInstanceOf[Iso0Elem[_, _, _]] && method.getName == "canEqual" =>
          Some((receiver, other)).asInstanceOf[Option[(Rep[Iso0[From, To]], Any) forSome {type From; type To}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iso0[From, To]], Any) forSome {type From; type To}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `isIdentity`: Method's return type Boolean is not a Rep
  }
}

object Views_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAO1ZT2zbVBh/cZImacq6MhiCaWupAmgImjZC2qGHKUlbKMrWqi4IhWnVi/Paedh+rt9LSThMiMOE4Ia4cOAwieMuiMskJG5IiAOnCU3ixIHTGEI7MHEA8d7zn9iO7aQtQ6jQg2U73/t9f36/7/Oze/MeyBILPEsUqEFjTkcUzsnivEpoSV42qEp7F3C7o6EltP3eyVvKBaNGJDDZBGNXIFkiWhMU7JPlrumdy2i3AQrQUBCh2CIUPN0QHsoK1jSkUBUbZVXXOxS2NFRuqIQuNkCmhdu9XXANpBrguIINxUIUyXUNEoKIcz+PeESqd10Q1701s+/DKPMsyr4sNi2oUhY+83Hctt9AptwzsNHTKTjmhLZm8rCYTU7VTWxR10WOwV3BbfcyY0B2AzzauAr3YJm52CnL1FKNHbayaELlLbiDLjITbp5hAROkbW/2THGdboBxgnZZgVZ1UxN3uiYAgDFQEUHM9esz59VnjtenJCNLhZr6DuQ/rlu42wP2XyoNQNdkEC8MgXAR0LLRLn1wSXnzgVzUJb64y0PJiQzHGNB0jBoEFayO32x8RO6/fOOcBMabYFwl1RahFlSon3KnWkVoGJiKmL0CQmuHsTUbx5bwUmU2IUkUFKyb0GBITiknGE+aqqiUG/N7Ew47MaXPURO5pqmumfLynYnJV+imDjVt/e6TLz7z8/IbEpCCLgoMUmbCt1xQCjKrBM870Pw4SUGq2q9v/5IfCt3+MZcQiVeT5+7+0v56HlySvEo6jj1IDlO0lXARG6i0sl76Tf7245ucaAtM2L/YWv9TPffHD8e2qdCAAHpiNA2wSLLkzvfF22fPSyDfFB2zosGdJuOELGtIX7Pq2KBNkMd7yLJ/ye1BjZ9FaiLXRtuwo1GHKX+J06zEFMzE9raJeP0XGZ+sFbwqnKJAQlW34hkeUyQnfhIomFht2/OBcyhgvHqcjtOI0NTJjcZj2r3zX0kg+yrIbrM0SQNkW7hjtF2xsoFGUZfW3HupYJpMnNCCukun3dozQAQhIg2FLAyLqWBSfu3FU8hC3uyYGnrpy98vv//uK6bQ0oCSg8hSdSGgYalaCQezEFpRC62oVQYq7he/X378eAaE+MyoBC+4aGmW6ZAIHf++CCMhK0mQlZFS4C03bjeWjHU0NXtfvXzjQyrqmuoGnxVrratsOC+KdU8lkeQ8sz6/fv3xXz/bOiFmbb6lUh2apfl9TFp3MD7ESQp8w0fQ2L8WhWZTcWodqhZXZ93v+lQM9eFWTqOqx3xEL7vU+6mOgKgkQgzoOQKilhhFhOAiIBKjiBAYBXm3dv6JxI9no6ZEoA9iLCrDLGpDMWoD9UoaRu6tUgta+x4sMQv/Q/OlT/z/bRLbJjm5ox+JLglvF+vRohhN0QnyS9Xi/RyoRcKB1w6l5n3s3+IAakkAtREA6kkAg7xQUOTPN0zQyEL01yzaoDbMYIC35G3hmfht4UrHUG6vfnJi8vTWj+JdZ6yNdaiKZ/U029BabAcj9gDTzv4wVq38MjUVp7Slwyj6gAI8WIvURwr8KCo6DmApCWCwPmz7wlX1z/dDgsFSOM0DDOWY2TZcwmPsRXBvE3uu2Ivy3mFVnOeYKxbWk1BDPRjzEeJfKGVXSI/wpJBF0eib4VHVlKCFLHO2MJIYvBVVy4K9g+ojrxqGyPAoPmddKvOiRCyxh8CiL1Qb8zUQjCjNXquH9nuGt1OgzNImjqxzgFnPwnvzzYws1pDL6B1lIIjI9KKGV30YqVFG0bIakq7dLn8rrUmjfqiPrfgkUrf6lo559nUVvU3YkLb3SAJ3g+2ZZmP2TLLzXYQp6tqDTy8+/90XP4l90zj/woINZHj/Reh/Tgl/WssLn0tEC6htzPbii54xyz/DiJj/AnZ3A+i3GQAA"
}
}

