package scalan.primitives

import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait AbstractStringsAbs extends AbstractStrings with scalan.Scalan {
  self: AbstractStringsDsl =>

  // single proxy for each type family
  implicit def proxyAString(p: Rep[AString]): AString = {
    proxyOps[AString](p)(scala.reflect.classTag[AString])
  }

  // familyElem
  class AStringElem[To <: AString]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[AString].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[AString] => convertAString(x) }
      tryConvert(element[AString], this, x, conv)
    }

    def convertAString(x: Rep[AString]): Rep[To] = {
      x.selfType1 match {
        case _: AStringElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have AStringElem[_], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def aStringElement: Elem[AString] =
    cachedElem[AStringElem[AString]]()

  implicit case object AStringCompanionElem extends CompanionElem[AStringCompanionAbs] {
    lazy val tag = weakTypeTag[AStringCompanionAbs]
    protected def getDefaultRep = AString
  }

  abstract class AStringCompanionAbs extends CompanionDef[AStringCompanionAbs] with AStringCompanion {
    def selfType = AStringCompanionElem
    override def toString = "AString"
  }
  def AString: Rep[AStringCompanionAbs]
  implicit def proxyAStringCompanionAbs(p: Rep[AStringCompanionAbs]): AStringCompanionAbs =
    proxyOps[AStringCompanionAbs](p)

  abstract class AbsSString
      (wrappedValue: Rep[String])
    extends SString(wrappedValue) with Def[SString] {
    lazy val selfType = element[SString]
  }
  // elem for concrete class
  class SStringElem(val iso: Iso[SStringData, SString])
    extends AStringElem[SString]
    with ConcreteElem[SStringData, SString] {
    override lazy val parent: Option[Elem[_]] = Some(aStringElement)
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertAString(x: Rep[AString]) = SString(x.wrappedValue)
    override def getDefaultRep = SString("")
    override lazy val tag = {
      weakTypeTag[SString]
    }
  }

  // state representation type
  type SStringData = String

  // 3) Iso for concrete class
  class SStringIso
    extends Iso0[SStringData, SString] {
    override def from(p: Rep[SString]) =
      p.wrappedValue
    override def to(p: Rep[String]) = {
      val wrappedValue = p
      SString(wrappedValue)
    }
    lazy val eFrom = element[String]
    lazy val eTo = new SStringElem(self)
    lazy val selfType = new ConcreteIso0Elem[SStringData, SString, SStringIso](eFrom, eTo).
      asInstanceOf[Elem[Iso0[SStringData, SString]]]
    def productArity = 0
    def productElement(n: Int) = ???
  }
  // 4) constructor and deconstructor
  class SStringCompanionAbs extends CompanionDef[SStringCompanionAbs] with SStringCompanion {
    def selfType = SStringCompanionElem
    override def toString = "SString"

    def apply(wrappedValue: Rep[String]): Rep[SString] =
      mkSString(wrappedValue)
  }
  object SStringMatcher {
    def unapply(p: Rep[AString]) = unmkSString(p)
  }
  lazy val SString: Rep[SStringCompanionAbs] = new SStringCompanionAbs
  implicit def proxySStringCompanion(p: Rep[SStringCompanionAbs]): SStringCompanionAbs = {
    proxyOps[SStringCompanionAbs](p)
  }

  implicit case object SStringCompanionElem extends CompanionElem[SStringCompanionAbs] {
    lazy val tag = weakTypeTag[SStringCompanionAbs]
    protected def getDefaultRep = SString
  }

  implicit def proxySString(p: Rep[SString]): SString =
    proxyOps[SString](p)

  implicit class ExtendedSString(p: Rep[SString]) {
    def toData: Rep[SStringData] = isoSString.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSString: Iso[SStringData, SString] =
    reifyObject(new SStringIso())

  // 6) smart constructor and deconstructor
  def mkSString(wrappedValue: Rep[String]): Rep[SString]
  def unmkSString(p: Rep[AString]): Option[(Rep[String])]

  abstract class AbsCString
      (wrappedValue: Rep[String])
    extends CString(wrappedValue) with Def[CString] {
    lazy val selfType = element[CString]
  }
  // elem for concrete class
  class CStringElem(val iso: Iso[CStringData, CString])
    extends AStringElem[CString]
    with ConcreteElem[CStringData, CString] {
    override lazy val parent: Option[Elem[_]] = Some(aStringElement)
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertAString(x: Rep[AString]) = CString(x.wrappedValue)
    override def getDefaultRep = CString("")
    override lazy val tag = {
      weakTypeTag[CString]
    }
  }

  // state representation type
  type CStringData = String

  // 3) Iso for concrete class
  class CStringIso
    extends Iso0[CStringData, CString] {
    override def from(p: Rep[CString]) =
      p.wrappedValue
    override def to(p: Rep[String]) = {
      val wrappedValue = p
      CString(wrappedValue)
    }
    lazy val eFrom = element[String]
    lazy val eTo = new CStringElem(self)
    lazy val selfType = new ConcreteIso0Elem[CStringData, CString, CStringIso](eFrom, eTo).
      asInstanceOf[Elem[Iso0[CStringData, CString]]]
    def productArity = 0
    def productElement(n: Int) = ???
  }
  // 4) constructor and deconstructor
  class CStringCompanionAbs extends CompanionDef[CStringCompanionAbs] with CStringCompanion {
    def selfType = CStringCompanionElem
    override def toString = "CString"

    def apply(wrappedValue: Rep[String]): Rep[CString] =
      mkCString(wrappedValue)
  }
  object CStringMatcher {
    def unapply(p: Rep[AString]) = unmkCString(p)
  }
  lazy val CString: Rep[CStringCompanionAbs] = new CStringCompanionAbs
  implicit def proxyCStringCompanion(p: Rep[CStringCompanionAbs]): CStringCompanionAbs = {
    proxyOps[CStringCompanionAbs](p)
  }

  implicit case object CStringCompanionElem extends CompanionElem[CStringCompanionAbs] {
    lazy val tag = weakTypeTag[CStringCompanionAbs]
    protected def getDefaultRep = CString
  }

  implicit def proxyCString(p: Rep[CString]): CString =
    proxyOps[CString](p)

  implicit class ExtendedCString(p: Rep[CString]) {
    def toData: Rep[CStringData] = isoCString.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCString: Iso[CStringData, CString] =
    reifyObject(new CStringIso())

  // 6) smart constructor and deconstructor
  def mkCString(wrappedValue: Rep[String]): Rep[CString]
  def unmkCString(p: Rep[AString]): Option[(Rep[String])]

  registerModule(AbstractStrings_Module)
}

// Seq -----------------------------------
trait AbstractStringsSeq extends AbstractStringsDsl with scalan.ScalanSeq {
  self: AbstractStringsDslSeq =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs {
  }

  case class SeqSString
      (override val wrappedValue: Rep[String])
    extends AbsSString(wrappedValue) {
  }

  def mkSString
    (wrappedValue: Rep[String]): Rep[SString] =
    new SeqSString(wrappedValue)
  def unmkSString(p: Rep[AString]) = p match {
    case p: SString @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  case class SeqCString
      (override val wrappedValue: Rep[String])
    extends AbsCString(wrappedValue) {
  }

  def mkCString
    (wrappedValue: Rep[String]): Rep[CString] =
    new SeqCString(wrappedValue)
  def unmkCString(p: Rep[AString]) = p match {
    case p: CString @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }
}

// Exp -----------------------------------
trait AbstractStringsExp extends AbstractStringsDsl with scalan.ScalanExp {
  self: AbstractStringsDslExp =>
  lazy val AString: Rep[AStringCompanionAbs] = new AStringCompanionAbs {
  }

  case class ExpSString
      (override val wrappedValue: Rep[String])
    extends AbsSString(wrappedValue)

  object SStringMethods {
  }

  object SStringCompanionMethods {
  }

  def mkSString
    (wrappedValue: Rep[String]): Rep[SString] =
    new ExpSString(wrappedValue)
  def unmkSString(p: Rep[AString]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SStringElem @unchecked =>
      Some((p.asRep[SString].wrappedValue))
    case _ =>
      None
  }

  case class ExpCString
      (override val wrappedValue: Rep[String])
    extends AbsCString(wrappedValue)

  object CStringMethods {
  }

  object CStringCompanionMethods {
  }

  def mkCString
    (wrappedValue: Rep[String]): Rep[CString] =
    new ExpCString(wrappedValue)
  def unmkCString(p: Rep[AString]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CStringElem @unchecked =>
      Some((p.asRep[CString].wrappedValue))
    case _ =>
      None
  }

  object AStringMethods {
    object wrappedValue {
      def unapply(d: Def[_]): Option[Rep[AString]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[AStringElem[_]] && method.getName == "wrappedValue" =>
          Some(receiver).asInstanceOf[Option[Rep[AString]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[AString]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AStringCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Rep[String]] = d match {
        case MethodCall(receiver, method, Seq(msg, _*), _) if receiver.elem == AStringCompanionElem && method.getName == "apply" =>
          Some(msg).asInstanceOf[Option[Rep[String]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[String]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object AbstractStrings_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVVPWwcRRR+92Of70exYxSkuMGcDyMiuLOQUApLoONyQUgX28oGFB0R0dze+DJhdna8MzZ3FCkoUkCHaCkiUaZBNEhINAgJUVAhhERNFUBRClKBeDP7c3uHF5uCLUY7M2/ez/e9b+b+b7CgAthULuFEND2qSdOx/22lG05XaKYnV/zhIaeX6P77T37hXhGvqjws92HxFlGXFO9DOfzpjmXy79CDHpSJcKnSfqA0PN2zEVquzzl1NfNFi3neoSYDTls9pvR2D4oDfzg5gDuQ68GK6ws3oJo6HU6UoipaX6ImI5bMy3Y+2ZXTGKJlqmilqrgWEKYxfYyxEtpfpdKZCF9MPA1notR2pUkLbUrMk36g4xAldHfLH8bToiC4AKu92+SItDDEqOXogIkRnqxK4r5DRnQHTYx5ERNWlO9fm0g7L/SgougBAvS6J7ldGUsAQAZetEk0p/g0E3yaBp+GQwNGOHuPmM29wB9PIPxyBYCxRBfPn+Ai9kC7Ytj44Ib71mOn6uXN4bFJpWQrXERHT2V0g6UCcfz26kfq0Wv3Luah0ocKU+2B0gFxdZryCK0qEcLXNucEQBKMkK16Fls2Shtt5lqi7PqeJAI9RVDWkCfOXKaNsVmrRexkQF/SksamubHMJfWuZ9Rr+6ZDON97cP6FZ37tXs9DfjZEGV062PhB7FRDqR22g4XUDOUI3ew4ScXPPvh9+M0W3MgnOEVuT0cNulhQP/1Y/eG5V/Kw1LeNfJmTUR+hUl1Ovd2g4wvdhyX/iAbhTumIcPN3LFWlId0nh1xHAKYrL2DlGtYzJSepgWXbtncuBqAaduiOL2jj8l7jD+e7j++bBgygFu6EGvyLXfzz5zP72vamhtq7AZGSDt8k/DAU/rKGAko4QWUji0BJ9wLm4YVxRF/6+ss3Hn61s2A5XI0Ksy6nfBXTNZrQ+Xpdw+LUIORyymglTNvxPXq2/oi9fe9DbbnLjWdviN3BbZTktj13/l9ojG+qz+7ePffw05tPWIUtDZj2iGxs/Qd9xXL4H/UDSYeHN8fqdG6GNYRvxQmh66Qjr80fQc04U4hT29VYohHnmdKyvlK255Kmsx5PbCEzrP+TXzNu2HHzNMV2TllsZ77YMGYq/02YLbyALXEqKGYAmUt541jXte44SXjrRDSOK7udWfbcbbg2l+zLs4sIzXL8koSH8JU4GwlFxjJWUS0B1DM05ERdi5jdefzJzoXvP//Far5i+h9vHpG87Gmtz6KyOpcIvtip5DUUjTRs+n8DZ0t2dkEJAAA="
}
}

trait AbstractStringsDsl extends impl.AbstractStringsAbs {self: AbstractStringsDsl =>}
trait AbstractStringsDslSeq extends impl.AbstractStringsSeq {self: AbstractStringsDslSeq =>}
trait AbstractStringsDslExp extends impl.AbstractStringsExp {self: AbstractStringsDslExp =>}
