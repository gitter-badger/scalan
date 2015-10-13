package scalan.collections

import scalan._
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait BitSetsAbs extends BitSets with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxyBitSet(p: Rep[BitSet]): BitSet = {
    proxyOps[BitSet](p)(scala.reflect.classTag[BitSet])
  }

  // familyElem
  class BitSetElem[To <: BitSet]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[BitSet].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[BitSet] => convertBitSet(x) }
      tryConvert(element[BitSet], this, x, conv)
    }

    def convertBitSet(x: Rep[BitSet]): Rep[To] = {
      x.selfType1 match {
        case _: BitSetElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have BitSetElem[_], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def bitSetElement: Elem[BitSet] =
    cachedElem[BitSetElem[BitSet]]()

  implicit case object BitSetCompanionElem extends CompanionElem[BitSetCompanionAbs] {
    lazy val tag = weakTypeTag[BitSetCompanionAbs]
    protected def getDefaultRep = BitSet
  }

  abstract class BitSetCompanionAbs extends CompanionDef[BitSetCompanionAbs] with BitSetCompanion {
    def selfType = BitSetCompanionElem
    override def toString = "BitSet"
  }
  def BitSet: Rep[BitSetCompanionAbs]
  implicit def proxyBitSetCompanionAbs(p: Rep[BitSetCompanionAbs]): BitSetCompanionAbs =
    proxyOps[BitSetCompanionAbs](p)

  abstract class AbsBoolCollBitSet
      (bits: Rep[Collection[Boolean]])
    extends BoolCollBitSet(bits) with Def[BoolCollBitSet] {
    lazy val selfType = element[BoolCollBitSet]
  }
  // elem for concrete class
  class BoolCollBitSetElem(val iso: Iso[BoolCollBitSetData, BoolCollBitSet])
    extends BitSetElem[BoolCollBitSet]
    with ConcreteElem[BoolCollBitSetData, BoolCollBitSet] {
    override lazy val parent: Option[Elem[_]] = Some(bitSetElement)
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertBitSet(x: Rep[BitSet]) = BoolCollBitSet(x.bits)
    override def getDefaultRep = BoolCollBitSet(element[Collection[Boolean]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[BoolCollBitSet]
    }
  }

  // state representation type
  type BoolCollBitSetData = Collection[Boolean]

  // 3) Iso for concrete class
  class BoolCollBitSetIso
    extends Iso[BoolCollBitSetData, BoolCollBitSet] {
    override def from(p: Rep[BoolCollBitSet]) =
      p.bits
    override def to(p: Rep[Collection[Boolean]]) = {
      val bits = p
      BoolCollBitSet(bits)
    }
    lazy val eTo = new BoolCollBitSetElem(this)
  }
  // 4) constructor and deconstructor
  class BoolCollBitSetCompanionAbs extends CompanionDef[BoolCollBitSetCompanionAbs] with BoolCollBitSetCompanion {
    def selfType = BoolCollBitSetCompanionElem
    override def toString = "BoolCollBitSet"

    def apply(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
      mkBoolCollBitSet(bits)
  }
  object BoolCollBitSetMatcher {
    def unapply(p: Rep[BitSet]) = unmkBoolCollBitSet(p)
  }
  lazy val BoolCollBitSet: Rep[BoolCollBitSetCompanionAbs] = new BoolCollBitSetCompanionAbs
  implicit def proxyBoolCollBitSetCompanion(p: Rep[BoolCollBitSetCompanionAbs]): BoolCollBitSetCompanionAbs = {
    proxyOps[BoolCollBitSetCompanionAbs](p)
  }

  implicit case object BoolCollBitSetCompanionElem extends CompanionElem[BoolCollBitSetCompanionAbs] {
    lazy val tag = weakTypeTag[BoolCollBitSetCompanionAbs]
    protected def getDefaultRep = BoolCollBitSet
  }

  implicit def proxyBoolCollBitSet(p: Rep[BoolCollBitSet]): BoolCollBitSet =
    proxyOps[BoolCollBitSet](p)

  implicit class ExtendedBoolCollBitSet(p: Rep[BoolCollBitSet]) {
    def toData: Rep[BoolCollBitSetData] = isoBoolCollBitSet.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBoolCollBitSet: Iso[BoolCollBitSetData, BoolCollBitSet] =
    cachedIso[BoolCollBitSetIso]()

  // 6) smart constructor and deconstructor
  def mkBoolCollBitSet(bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet]
  def unmkBoolCollBitSet(p: Rep[BitSet]): Option[(Rep[Collection[Boolean]])]

  registerModule(BitSets_Module)
}

// Seq -----------------------------------
trait BitSetsSeq extends BitSetsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs {
  }

  case class SeqBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])
    extends AbsBoolCollBitSet(bits) {
  }

  def mkBoolCollBitSet
    (bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
    new SeqBoolCollBitSet(bits)
  def unmkBoolCollBitSet(p: Rep[BitSet]) = p match {
    case p: BoolCollBitSet @unchecked =>
      Some((p.bits))
    case _ => None
  }
}

// Exp -----------------------------------
trait BitSetsExp extends BitSetsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val BitSet: Rep[BitSetCompanionAbs] = new BitSetCompanionAbs {
  }

  case class ExpBoolCollBitSet
      (override val bits: Rep[Collection[Boolean]])
    extends AbsBoolCollBitSet(bits)

  object BoolCollBitSetMethods {
  }

  object BoolCollBitSetCompanionMethods {
  }

  def mkBoolCollBitSet
    (bits: Rep[Collection[Boolean]]): Rep[BoolCollBitSet] =
    new ExpBoolCollBitSet(bits)
  def unmkBoolCollBitSet(p: Rep[BitSet]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BoolCollBitSetElem @unchecked =>
      Some((p.asRep[BoolCollBitSet].bits))
    case _ =>
      None
  }

  object BitSetMethods {
    object bits {
      def unapply(d: Def[_]): Option[Rep[BitSet]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "bits" =>
          Some(receiver).asInstanceOf[Option[Rep[BitSet]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BitSet]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object union {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Rep[BitSet])] = d match {
        case MethodCall(receiver, method, Seq(that, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "union" =>
          Some((receiver, that)).asInstanceOf[Option[(Rep[BitSet], Rep[BitSet])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Rep[BitSet])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[BitSet]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[BitSet]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BitSet]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object contains {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "contains" =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[BitSet], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(n, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "add" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some((receiver, n)).asInstanceOf[Option[(Rep[BitSet], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object add_many {
      def unapply(d: Def[_]): Option[(Rep[BitSet], Coll[Int])] = d match {
        case MethodCall(receiver, method, Seq(ns, _*), _) if receiver.elem.isInstanceOf[BitSetElem[_]] && method.getName == "add" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some((receiver, ns)).asInstanceOf[Option[(Rep[BitSet], Coll[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BitSet], Coll[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BitSetCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Coll[Boolean]] = d match {
        case MethodCall(receiver, method, Seq(flags, _*), _) if receiver.elem == BitSetCompanionElem && method.getName == "apply" && method.getAnnotation(classOf[scalan.OverloadId]) == null =>
          Some(flags).asInstanceOf[Option[Coll[Boolean]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Coll[Boolean]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply_many {
      def unapply(d: Def[_]): Option[Rep[Array[Boolean]]] = d match {
        case MethodCall(receiver, method, Seq(flags, _*), _) if receiver.elem == BitSetCompanionElem && method.getName == "apply" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "many" } =>
          Some(flags).asInstanceOf[Option[Rep[Array[Boolean]]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Array[Boolean]]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object empty {
      def unapply(d: Def[_]): Option[Rep[Int]] = d match {
        case MethodCall(receiver, method, Seq(range, _*), _) if receiver.elem == BitSetCompanionElem && method.getName == "empty" =>
          Some(range).asInstanceOf[Option[Rep[Int]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Int]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object BitSets_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVVPWwTSRR+tpM4tiMSjACRBi5nQCCwIyREkQIlxqCTTBKxgJBBoMl6bAZmZzY748imoKCggA7RUiBR0qBrTjrpmtNJpyuo0Omkq6/iQIgCKhBvZtdrO8ryU7DFamf27Xvv+3mzT1/BuArggHIJJ6LsUU3Kjn1eVLrk1IRmundWNjucnqKtO7t+cc+KJZWG6QZMXCfqlOINyIUPta4fPzt0vQ45IlyqtAyUhh/qtkLFlZxTVzMpKszzOpqscVqpM6UX6jC2Jpu9dbgNqTrMuFK4AdXUqXKiFFXR/iQ1HbF4nbPr3oo/qCEqBkVlCMX5gDCN7WONmTD+HPWdnpCi52nYFrW24pu2MCbLPF8Gul8ii+muy2Z/OSYIbkCxfoNskAqWaFccHTDRxi8LPnFvkjZdxhATPoYNK8pb53u+XWfqkFd0HQn6yfO53en6AIAKHLNNlAf8lGN+yoafkkMDRji7RczL1UB2exBeqQxA18cUR76Qop+B1kSzdO+Ke/m9U/DS5uOuaSVrEU5gor0JbrBSII9/nnug3p55fCIN+QbkmVpcUzogrh6WPGKrQISQ2vYcE0iCNqo1l6SWrbKIMZsskXOl5xOBmSIqp1AnzlymTbDZm4rUSaA+q33aD011/VSMd18CXuubKuF89eWeo/v/r11KQ3q0RA5TOmj8oJ9Uw8QSQ6a1ZdTcchG5yWViwAdfvm7+MQ9X0jFNUdavUwZTjKt//i68OHQyDZMN6+PTnLQbyJSqceqtBFUpdAMm5QYNwjfZDcLN05ZKZZu0RTpcR/wNA88gcA37EifOp4aVBevuVJ+AQmjQZSlo6fRq6Z3z18Onxn8BTIVvwhH8yE58+HdbS1travQA08q2NK0hg5MbsRHt5Kux32OafkwS1KerAfPwANmgx3//9cKb35bHrabFCOlFwjs0HOcI6AC06WW8RbhC4NklKTklYiDw8N1gzYeIHOnR7XNv2dXH97WVNdUdPTtW1m5g8wv2uz2fUbh/hj27e3fnmyfXdtjZm0RqPOKX5r9h8vqD8h0nC2Lzh2dKcbA2t1kkcrch0CgXTkt1uIHZzV/iGT0aPogKSR9yxCEYtUcGORvd2XpAc90t+7XrA1HmL8OaToQzejLMbqo+P7qZNQ6zwXhQFiNHDM51FXUVwFyCW5xIHwR/+/2j5cPPf/7P+jxvlMbxE/HfbdjfozwVw3yIxesI/GfiX2uoa5xLYwLb9ydh3tpCRQgAAA=="
}
}

trait BitSetsDsl extends impl.BitSetsAbs {self: ScalanCommunityDsl =>}
trait BitSetsDslSeq extends impl.BitSetsSeq {self: ScalanCommunityDslSeq =>}
trait BitSetsDslExp extends impl.BitSetsExp {self: ScalanCommunityDslExp =>}
