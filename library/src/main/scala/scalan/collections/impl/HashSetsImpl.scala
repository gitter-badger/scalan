package scalan.collections

import scala.collection.immutable.HashSet
import scalan._
import scalan.common.Default
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait HashSetsAbs extends HashSets with scalan.Scalan {
  self: ScalanCommunityDsl =>

  // single proxy for each type family
  implicit def proxySHashSet[A](p: Rep[SHashSet[A]]): SHashSet[A] = {
    proxyOps[SHashSet[A]](p)(scala.reflect.classTag[SHashSet[A]])
  }

  // TypeWrapper proxy
  //implicit def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOps[SHashSet[A]](p.asRep[SHashSet[A]])

  implicit def unwrapValueOfSHashSet[A](w: Rep[SHashSet[A]]): Rep[HashSet[A]] = w.wrappedValueOfBaseType

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]]

  implicit def castSHashSetElement[A](elem: Elem[SHashSet[A]]): SHashSetElem[A, SHashSet[A]] = elem.asInstanceOf[SHashSetElem[A, SHashSet[A]]]

  implicit lazy val containerHashSet: Cont[HashSet] = new Container[HashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[HashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[HashSet[A]]
  }

  implicit lazy val containerSHashSet: Cont[SHashSet] = new Container[SHashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SHashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[SHashSet[A]]
  }
  case class SHashSetIso[A,B](iso: Iso[A,B]) extends Iso1[A, B, SHashSet](iso) {
    def from(x: Rep[SHashSet[B]]) = x.map(iso.fromFun)
    def to(x: Rep[SHashSet[A]]) = x.map(iso.toFun)
    lazy val defaultRepTo = SHashSet.empty[B]
  }

  // familyElem
  abstract class SHashSetElem[A, To <: SHashSet[A]](implicit val eA: Elem[A])
    extends WrapperElem1[A, To, HashSet, SHashSet]()(eA, container[HashSet], container[SHashSet]) {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("HashSets")
      module.entities.find(_.name == "SHashSet").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSet[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SHashSet[A]] => convertSHashSet(x) }
      tryConvert(element[SHashSet[A]], this, x, conv)
    }

    def convertSHashSet(x : Rep[SHashSet[A]]): Rep[To] = {
      assert(x.selfType1 match { case _: SHashSetElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sHashSetElement[A](implicit eA: Elem[A]): Elem[SHashSet[A]] =
    elemCache.getOrElseUpdate(
      (classOf[SHashSetElem[A, SHashSet[A]]], Seq(eA)),
      new SHashSetElem[A, SHashSet[A]] {
        lazy val eTo = element[SHashSetImpl[A]]
      }).asInstanceOf[Elem[SHashSet[A]]]

  implicit case object SHashSetCompanionElem extends CompanionElem[SHashSetCompanionAbs] {
    lazy val tag = weakTypeTag[SHashSetCompanionAbs]
    protected def getDefaultRep = SHashSet
  }

  abstract class SHashSetCompanionAbs extends CompanionBase[SHashSetCompanionAbs] with SHashSetCompanion {
    override def toString = "SHashSet"
  }
  def SHashSet: Rep[SHashSetCompanionAbs]
  implicit def proxySHashSetCompanion(p: Rep[SHashSetCompanion]): SHashSetCompanion =
    proxyOps[SHashSetCompanion](p)

  // default wrapper implementation
  abstract class SHashSetImpl[A](val wrappedValueOfBaseType: Rep[HashSet[A]])(implicit val eA: Elem[A]) extends SHashSet[A] {
    def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("$plus", classOf[AnyRef]),
        List(elem.asInstanceOf[AnyRef]))

    def map[B:Elem](f: Rep[A => B]): Rep[SHashSet[B]] =
      methodCallEx[SHashSet[B]](self,
        this.getClass.getMethod("map", classOf[AnyRef], classOf[Elem[B]]),
        List(f.asInstanceOf[AnyRef], element[B]))

    def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      methodCallEx[A](self,
        this.getClass.getMethod("fold", classOf[AnyRef], classOf[AnyRef]),
        List(z.asInstanceOf[AnyRef], f.asInstanceOf[AnyRef]))
  }
  trait SHashSetImplCompanion
  // elem for concrete class
  class SHashSetImplElem[A](val iso: Iso[SHashSetImplData[A], SHashSetImpl[A]])(implicit eA: Elem[A])
    extends SHashSetElem[A, SHashSetImpl[A]]
    with ConcreteElem1[A, SHashSetImplData[A], SHashSetImpl[A], SHashSet] {
    override lazy val parent: Option[Elem[_]] = Some(sHashSetElement(element[A]))
    override lazy val entityDef = {
      val module = getModules("HashSets")
      module.concreteSClasses.find(_.name == "SHashSetImpl").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    lazy val eTo = this
    override def convertSHashSet(x: Rep[SHashSet[A]]) = SHashSetImpl(x.wrappedValueOfBaseType)
    override def getDefaultRep = super[ConcreteElem1].getDefaultRep
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSetImpl[A]]
    }
  }

  // state representation type
  type SHashSetImplData[A] = HashSet[A]

  // 3) Iso for concrete class
  class SHashSetImplIso[A](implicit eA: Elem[A])
    extends Iso[SHashSetImplData[A], SHashSetImpl[A]] {
    override def from(p: Rep[SHashSetImpl[A]]) =
      p.wrappedValueOfBaseType
    override def to(p: Rep[HashSet[A]]) = {
      val wrappedValueOfBaseType = p
      SHashSetImpl(wrappedValueOfBaseType)
    }
    lazy val defaultRepTo: Rep[SHashSetImpl[A]] = SHashSetImpl(DefaultOfHashSet[A].value)
    lazy val eTo = new SHashSetImplElem[A](this)
  }
  // 4) constructor and deconstructor
  abstract class SHashSetImplCompanionAbs extends CompanionBase[SHashSetImplCompanionAbs] {
    override def toString = "SHashSetImpl"

    def apply[A](wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      mkSHashSetImpl(wrappedValueOfBaseType)
  }
  object SHashSetImplMatcher {
    def unapply[A](p: Rep[SHashSet[A]]) = unmkSHashSetImpl(p)
  }
  def SHashSetImpl: Rep[SHashSetImplCompanionAbs]
  implicit def proxySHashSetImplCompanion(p: Rep[SHashSetImplCompanionAbs]): SHashSetImplCompanionAbs = {
    proxyOps[SHashSetImplCompanionAbs](p)
  }

  implicit case object SHashSetImplCompanionElem extends CompanionElem[SHashSetImplCompanionAbs] {
    lazy val tag = weakTypeTag[SHashSetImplCompanionAbs]
    protected def getDefaultRep = SHashSetImpl
  }

  implicit def proxySHashSetImpl[A](p: Rep[SHashSetImpl[A]]): SHashSetImpl[A] =
    proxyOps[SHashSetImpl[A]](p)

  implicit class ExtendedSHashSetImpl[A](p: Rep[SHashSetImpl[A]])(implicit eA: Elem[A]) {
    def toData: Rep[SHashSetImplData[A]] = isoSHashSetImpl(eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSHashSetImpl[A](implicit eA: Elem[A]): Iso[SHashSetImplData[A], SHashSetImpl[A]] =
    cachedIso[SHashSetImplIso[A]](eA)

  // 6) smart constructor and deconstructor
  def mkSHashSetImpl[A](wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]]
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]): Option[(Rep[HashSet[A]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(HashSets_Module.dump))
}

// Seq -----------------------------------
trait HashSetsSeq extends HashSetsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs with UserTypeSeq[SHashSetCompanionAbs] {
    lazy val selfType = element[SHashSetCompanionAbs]
    override def empty[A:Elem]: Rep[SHashSet[A]] =
      SHashSetImpl(HashSet.empty[A])
  }

    // override proxy if we deal with TypeWrapper
  //override def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOpsEx[HashSet[A],SHashSet[A], SeqSHashSetImpl[A]](p, bt => SeqSHashSetImpl(bt))

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]] =
    new SeqBaseElemEx1[A, SHashSet[A], HashSet](element[SHashSet[A]])(
      element[A], container[HashSet], DefaultOfHashSet[A])

  case class SeqSHashSetImpl[A]
      (override val wrappedValueOfBaseType: Rep[HashSet[A]])
      (implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValueOfBaseType)
       with SeqSHashSet[A] with UserTypeSeq[SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]]
    override def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      SHashSetImpl(wrappedValueOfBaseType.$plus(elem))

    override def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      wrappedValueOfBaseType.fold(z)(scala.Function.untupled(f))
  }
  lazy val SHashSetImpl = new SHashSetImplCompanionAbs with UserTypeSeq[SHashSetImplCompanionAbs] {
    lazy val selfType = element[SHashSetImplCompanionAbs]
  }

  def mkSHashSetImpl[A]
      (wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      new SeqSHashSetImpl[A](wrappedValueOfBaseType)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p match {
    case p: SHashSetImpl[A] @unchecked =>
      Some((p.wrappedValueOfBaseType))
    case _ => None
  }

  implicit def wrapHashSetToSHashSet[A:Elem](v: HashSet[A]): SHashSet[A] = SHashSetImpl(v)
}

// Exp -----------------------------------
trait HashSetsExp extends HashSetsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs with UserTypeDef[SHashSetCompanionAbs] {
    lazy val selfType = element[SHashSetCompanionAbs]
    override def mirror(t: Transformer) = this

    def empty[A:Elem]: Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))
  }

  case class ViewSHashSet[A, B](source: Rep[SHashSet[A]])(iso: Iso1[A, B, SHashSet])
    extends View1[A, B, SHashSet](iso) {
    def copy(source: Rep[SHashSet[A]]) = ViewSHashSet(source)(iso)
    override def toString = s"ViewSHashSet[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSHashSet[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]] =
    new ExpBaseElemEx1[A, SHashSet[A], HashSet](element[SHashSet[A]])(
      element[A], container[HashSet], DefaultOfHashSet[A])

  case class ExpSHashSetImpl[A]
      (override val wrappedValueOfBaseType: Rep[HashSet[A]])
      (implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValueOfBaseType) with UserTypeDef[SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]]
    override def mirror(t: Transformer) = ExpSHashSetImpl[A](t(wrappedValueOfBaseType))
  }

  lazy val SHashSetImpl: Rep[SHashSetImplCompanionAbs] = new SHashSetImplCompanionAbs with UserTypeDef[SHashSetImplCompanionAbs] {
    lazy val selfType = element[SHashSetImplCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object SHashSetImplMethods {
  }

  def mkSHashSetImpl[A]
    (wrappedValueOfBaseType: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
    new ExpSHashSetImpl[A](wrappedValueOfBaseType)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SHashSetImplElem[A] @unchecked =>
      Some((p.asRep[SHashSetImpl[A]].wrappedValueOfBaseType))
    case _ =>
      None
  }

  object SHashSetMethods {
    object wrappedValueOfBaseType {
      def unapply(d: Def[_]): Option[Rep[SHashSet[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "wrappedValueOfBaseType" =>
          Some(receiver).asInstanceOf[Option[Rep[SHashSet[A]] forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SHashSet[A]] forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object + {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(elem, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "$plus" =>
          Some((receiver, elem)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A => B]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fold {
      def unapply(d: Def[_]): Option[(Rep[SHashSet[A]], Rep[A], Rep[((A, A)) => A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, Seq(z, f, _*), _) if receiver.elem.isInstanceOf[SHashSetElem[_, _]] && method.getName == "fold" =>
          Some((receiver, z, f)).asInstanceOf[Option[(Rep[SHashSet[A]], Rep[A], Rep[((A, A)) => A]) forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SHashSet[A]], Rep[A], Rep[((A, A)) => A]) forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object SHashSetCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == SHashSetCompanionElem && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UserTypeSHashSet {
    def unapply(s: Exp[_]): Option[Iso[_, _]] = {
      s.elem match {
        case e: SHashSetElem[a,to] => e.eItem match {
          case UnpackableElem(iso) => Some(iso)
          case _ => None
        }
        case _ => None
      }
    }
  }

  override def unapplyViews[T](s: Exp[T]): Option[Unpacked[T]] = (s match {
    case Def(view: ViewSHashSet[_, _]) =>
      Some((view.source, view.iso))
    case UserTypeSHashSet(iso: Iso[a, b]) =>
      val newIso = SHashSetIso(iso)
      val repr = reifyObject(UnpackView(s.asRep[SHashSet[b]])(newIso))
      Some((repr, newIso))
    case _ =>
      super.unapplyViews(s)
  }).asInstanceOf[Option[Unpacked[T]]]

  override def rewriteDef[T](d: Def[T]) = d match {
    case SHashSetMethods.map(xs, Def(IdentityLambda())) => xs

    case view1@ViewSHashSet(Def(view2@ViewSHashSet(arr))) =>
      val compIso = composeIso(view1.innerIso, view2.innerIso)
      implicit val eAB = compIso.eTo
      ViewSHashSet(arr)(SHashSetIso(compIso))

    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
    case mc @ MethodCall(Def(wrapper: ExpSHashSetImpl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
      val resultElem = mc.selfType
      val wrapperIso = getIsoByElem(resultElem)
      wrapperIso match {
        case iso: Iso[base,ext] =>
          val eRes = iso.eFrom
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValueOfBaseType, eRes)
          iso.to(newCall)
      }

    case SHashSetMethods.map(xs, f) => (xs, f) match {
      case (xs: RHS[a] @unchecked, LambdaResultHasViews(f, iso: Iso[b, c])) =>
        val f1 = f.asRep[a => c]
        implicit val eA = xs.elem.eItem
        implicit val eB = iso.eFrom
        val s = xs.map(fun { x =>
          val tmp = f1(x)
          iso.from(tmp)
        })
        val res = ViewSHashSet(s)(SHashSetIso(iso))
        res
      case (HasViews(source, contIso: SHashSetIso[a, b]), f: Rep[Function1[_, c] @unchecked]) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.iso
        implicit val eA = iso.eFrom
        implicit val eB = iso.eTo
        implicit val eC = f1.elem.eRange
        source.asRep[SHashSet[a]].map(fun { x => f1(iso.to(x)) })
      case _ =>
        super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }
}

object HashSets_Module {
  val packageName = "scalan.collections"
  val name = "HashSets"
  val dump = "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"
}
}

trait HashSetsDsl extends impl.HashSetsAbs {self: ScalanCommunityDsl =>}
trait HashSetsDslExp extends impl.HashSetsExp {self: ScalanCommunityDslExp =>}
