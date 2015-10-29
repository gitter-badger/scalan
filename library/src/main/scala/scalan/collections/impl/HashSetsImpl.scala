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

  implicit def unwrapValueOfSHashSet[A](w: Rep[SHashSet[A]]): Rep[HashSet[A]] = w.wrappedValue

  implicit def hashSetElement[A:Elem]: Elem[HashSet[A]] =
    element[SHashSet[A]].asInstanceOf[WrapperElem1[_, _, CBase, CW] forSome { type CBase[_]; type CW[_] }].baseElem.asInstanceOf[Elem[HashSet[A]]]

  implicit def castSHashSetElement[A](elem: Elem[SHashSet[A]]): SHashSetElem[A, SHashSet[A]] =
    elem.asInstanceOf[SHashSetElem[A, SHashSet[A]]]

  implicit lazy val containerHashSet: Container[HashSet] = new Container[HashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[HashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[HashSet[A]]
  }

  implicit lazy val containerSHashSet: Container[SHashSet] = new Container[SHashSet] {
    def tag[A](implicit evA: WeakTypeTag[A]) = weakTypeTag[SHashSet[A]]
    def lift[A](implicit evA: Elem[A]) = element[SHashSet[A]]
  }

  case class SHashSetIso[A, B](innerIso: Iso[A, B]) extends Iso10[A, B, SHashSet] {
    lazy val selfType = new ConcreteIso0Elem[SHashSet[A], SHashSet[B], SHashSetIso[A, B]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[SHashSet[A], SHashSet[B]]]]
    def cC = container[SHashSet]
    def from(x: Rep[SHashSet[B]]) = x.map(innerIso.fromFun)
    def to(x: Rep[SHashSet[A]]) = x.map(innerIso.toFun)
  }

  def sHashSetIso[A, B](innerIso: Iso[A, B]) =
    reifyObject(SHashSetIso[A, B](innerIso)).asInstanceOf[Iso1[A, B, SHashSet]]

  // familyElem
  class SHashSetElem[A, To <: SHashSet[A]](implicit _eA: Elem[A])
    extends WrapperElem1[A, To, HashSet, SHashSet](_eA, container[HashSet], container[SHashSet]) {
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSet[A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[SHashSet[A]] => convertSHashSet(x) }
      tryConvert(element[SHashSet[A]], this, x, conv)
    }

    def convertSHashSet(x: Rep[SHashSet[A]]): Rep[To] = {
      x.selfType1 match {
        case _: SHashSetElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have SHashSetElem[_, _], but got $e")
      }
    }
    lazy val baseElem =
      new BaseTypeElem1[A, HashSet, SHashSet[A]](this.asInstanceOf[Element[SHashSet[A]]])(
        element[A], container[HashSet], DefaultOfHashSet[A])
    lazy val eTo: Elem[_] = new SHashSetImplElem[A](isoSHashSetImpl(eA))(eA)
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def sHashSetElement[A](implicit eA: Elem[A]): Elem[SHashSet[A]] =
    elemCache.getOrElseUpdate(
      (classOf[SHashSetElem[A, SHashSet[A]]], Seq(eA)),
      new SHashSetElem[A, SHashSet[A]]).asInstanceOf[Elem[SHashSet[A]]]

  implicit case object SHashSetCompanionElem extends CompanionElem[SHashSetCompanionAbs] {
    lazy val tag = weakTypeTag[SHashSetCompanionAbs]
    protected def getDefaultRep = SHashSet
  }

  abstract class SHashSetCompanionAbs extends CompanionDef[SHashSetCompanionAbs] with SHashSetCompanion {
    def selfType = SHashSetCompanionElem
    override def toString = "SHashSet"
  }
  def SHashSet: Rep[SHashSetCompanionAbs]
  implicit def proxySHashSetCompanionAbs(p: Rep[SHashSetCompanionAbs]): SHashSetCompanionAbs =
    proxyOps[SHashSetCompanionAbs](p)

  // default wrapper implementation
  abstract class SHashSetImpl[A](val wrappedValue: Rep[HashSet[A]])(implicit val eA: Elem[A]) extends SHashSet[A] with Def[SHashSetImpl[A]] {
    lazy val selfType = element[SHashSetImpl[A]]

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
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA))
    }
    override lazy val eTo: Elem[_] = this
    override def convertSHashSet(x: Rep[SHashSet[A]]) = // Converter is not generated by meta
!!!("Cannot convert from SHashSet to SHashSetImpl: missing fields List(wrappedValue)")
    override def getDefaultRep = SHashSetImpl(DefaultOfHashSet[A].value)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[SHashSetImpl[A]]
    }
  }

  // state representation type
  type SHashSetImplData[A] = HashSet[A]

  // 3) Iso for concrete class
  class SHashSetImplIso[A](implicit eA: Elem[A])
    extends Iso0[SHashSetImplData[A], SHashSetImpl[A]] {
    override def from(p: Rep[SHashSetImpl[A]]) =
      p.wrappedValue
    override def to(p: Rep[HashSet[A]]) = {
      val wrappedValue = p
      SHashSetImpl(wrappedValue)
    }
    lazy val eFrom = element[HashSet[A]]
    lazy val eTo = new SHashSetImplElem[A](self)
    lazy val selfType = new ConcreteIso0Elem[SHashSetImplData[A], SHashSetImpl[A], SHashSetImplIso[A]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[SHashSetImplData[A], SHashSetImpl[A]]]]
    def productArity = 1
    def productElement(n: Int) = eA
  }
  // 4) constructor and deconstructor
  class SHashSetImplCompanionAbs extends CompanionDef[SHashSetImplCompanionAbs] {
    def selfType = SHashSetImplCompanionElem
    override def toString = "SHashSetImpl"

    def apply[A](wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
      mkSHashSetImpl(wrappedValue)
  }
  object SHashSetImplMatcher {
    def unapply[A](p: Rep[SHashSet[A]]) = unmkSHashSetImpl(p)
  }
  lazy val SHashSetImpl: Rep[SHashSetImplCompanionAbs] = new SHashSetImplCompanionAbs
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
    reifyObject(new SHashSetImplIso[A]()(eA))

  // 6) smart constructor and deconstructor
  def mkSHashSetImpl[A](wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]]
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]): Option[(Rep[HashSet[A]])]

  registerModule(HashSets_Module)
}

// Seq -----------------------------------
trait HashSetsSeq extends HashSetsDsl with scalan.ScalanSeq {
  self: ScalanCommunityDslSeq =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs {
    override def empty[A:Elem]: Rep[SHashSet[A]] =
      SHashSetImpl(HashSet.empty[A])
  }

  // override proxy if we deal with TypeWrapper
  //override def proxyHashSet[A:Elem](p: Rep[HashSet[A]]): SHashSet[A] =
  //  proxyOpsEx[HashSet[A], SHashSet[A], SeqSHashSetImpl[A]](p, bt => SeqSHashSetImpl(bt))

  case class SeqSHashSetImpl[A]
      (override val wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValue) with SeqSHashSet[A] {
    override def $plus(elem: Rep[A]): Rep[SHashSet[A]] =
      SHashSetImpl(wrappedValue.$plus(elem))

    override def fold(z: Rep[A])(f: Rep[((A, A)) => A]): Rep[A] =
      wrappedValue.fold(z)(scala.Function.untupled(f))
  }

  def mkSHashSetImpl[A]
    (wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
    new SeqSHashSetImpl[A](wrappedValue)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p match {
    case p: SHashSetImpl[A] @unchecked =>
      Some((p.wrappedValue))
    case _ => None
  }

  implicit def wrapHashSetToSHashSet[A:Elem](v: HashSet[A]): SHashSet[A] = SHashSetImpl(v)
}

// Exp -----------------------------------
trait HashSetsExp extends HashSetsDsl with scalan.ScalanExp {
  self: ScalanCommunityDslExp =>
  lazy val SHashSet: Rep[SHashSetCompanionAbs] = new SHashSetCompanionAbs {
    def empty[A:Elem]: Rep[SHashSet[A]] =
      methodCallEx[SHashSet[A]](self,
        this.getClass.getMethod("empty", classOf[Elem[A]]),
        List(element[A]))
  }

  case class ViewSHashSet[A, B](source: Rep[SHashSet[A]])(iso: Iso[A, B])
    extends View1[A, B, SHashSet](sHashSetIso(iso)) {
    override def toString = s"ViewSHashSet[${innerIso.eTo.name}]($source)"
    override def equals(other: Any) = other match {
      case v: ViewSHashSet[_, _] => source == v.source && innerIso.eTo == v.innerIso.eTo
      case _ => false
    }
  }

  case class ExpSHashSetImpl[A]
      (override val wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A])
    extends SHashSetImpl[A](wrappedValue)

  object SHashSetImplMethods {
  }

  def mkSHashSetImpl[A]
    (wrappedValue: Rep[HashSet[A]])(implicit eA: Elem[A]): Rep[SHashSetImpl[A]] =
    new ExpSHashSetImpl[A](wrappedValue)
  def unmkSHashSetImpl[A](p: Rep[SHashSet[A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SHashSetImplElem[A] @unchecked =>
      Some((p.asRep[SHashSetImpl[A]].wrappedValue))
    case _ =>
      None
  }

  object SHashSetMethods {
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
      ViewSHashSet(arr)(compIso)

    // Rule: W(a).m(args) ==> iso.to(a.m(unwrap(args)))
    case mc @ MethodCall(Def(wrapper: ExpSHashSetImpl[_]), m, args, neverInvoke) if !isValueAccessor(m) =>
      val resultElem = mc.selfType
      val wrapperIso = getIsoByElem(resultElem)
      wrapperIso match {
        case iso: Iso[base,ext] =>
          val eRes = iso.eFrom
          val newCall = unwrapMethodCall(mc, wrapper.wrappedValue, eRes)
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
        val res = ViewSHashSet(s)(iso)
        res
      case (HasViews(source, contIso: SHashSetIso[a, b]), f: Rep[Function1[_, c] @unchecked]) =>
        val f1 = f.asRep[b => c]
        val iso = contIso.innerIso
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

object HashSets_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWT2wUVRh/O9vtdru1paWgGKrQrNYq7UKjEsMB+2eLlS1tOoBaScnbmbdlYP7tzFuc1YhJo8TATYmJJEZJ1BMX40HjwYOJiZHEREPURDl40IOAIRwkRlG/92be7M7SaamJe5idN/Pm+773+/2+P+evopTroPtdBevYHDYIxcMyvx91aU4umFSjtWlLrepkgpSXNn6sTJtjroS65lHrEexOuPo8yvg3Bc8O72VSKaIMNhXiUstxKdpa5B7yiqXrRKGaZeY1w6hSXNJJvqi5dFcRtZQstVZBJ1CiiNYplqk4hBJ5XMeuS9zgeRthEWnhOsPXtRm77sPMs1PkG06x38EahfDBxzp//xyx5ZppmTWDos4gtBmbhQV70pphWw4VLtJg7oilimWLieEB6ikexcdxHlws5mXqaOYifJm1sXIML5J9sIVtb4GAXaKX99dsvk4WUbtLKgDQlGHr/IlnI4SAgREexHAdn+EQn2GGT04mjoZ17XnMXs46lldD/i+RRMizwcS2VUwIC6RgqrlTh5Rnb8hZQ2IfeyyUND9hKxi6N0YNnArA8Yu519zre87tlFD7PGrX3NGSSx2s0EbKA7Sy2DQtymMOAcTOIrDVH8cW9zIKe5okkVEsw8YmWAqg7ACedE3RKNvMnnUE7MRAn6Y2EVsTnp0Iz7sl5rxcN+NY12cvbxq670rhaQlJURcZMCmD8B1hlKI2+QnsHpEJDcyzaxdFiVGOMbtkvPo1vYL7EIiBy7+pn29Hh6QQvsDb7TEGJlLu999mLw7ullDbPNf3pI4X5wFBt6ATY8YZt0w6j9qs48Tx36SPY53dLctgWiVlXNVpgGsjIEkAhKItsZloE4bWLq76hAAg6wt3n2WS3ORs7nf5yzPnmS4d1OG/8VPzb23nzR86y5RLlqKO5xxs20Q9iPUqETAnIbOjwKfXQkdACrv08a0bGj67KyFi5u8pksiosNfCcFzVBQQt1CEKQF/IYV+cCrlqN84Ve/Wruz+VUOpJlCoDNW4RpUpW1VRFOkDJpMSjY+JZIkoNyB872BDa8YvHFsSDCCPtvSXmVTUmqusHJ09uuPbu4fW8KrSVNGpgO7d9DTVBpPD/mPMoSlGW7XyKy8iPrpVd+sXrtaVyA1RDK0EF2cazMgRBy2177JeJM3t5demqg8O3BedqzHqK7mD5ijWTOOKo6YYINoQPWHK1+ykkWwbp7r+uLZw7TXkdSXjRJjZTOgpdYxc/ymZu55EmtDoK3rigY0f0VYhUTJFrCotrDs7RLT4bb6TZV6PNrt3hekfd3KNRyabBveAJjhRAX++CPoYFgKI/hhY50AyI+MSNt/Y9+NWHP3Mu2pn6oCiZ4SxQl5rXVGZ6fHtwEKNqwoQBPb4BCygQTJhhJA/ERlJhhYEY4JNrYAkdPD8x8o7BKesini/+6YaJpKELxDUSfzuc7tIzlcpH2x/q5CnaVPyhj08FucUXM9ALHE0ly+ZrFuQoB/AKaETaNnWEdtZUdAurU6JQRXKyiFphOecXcH8aapCvwGtzzMnGdEs5dmDo13Kl9+GbfnfWAH0REiQQpagvrhsFrUhkSpwTwMeld0/3fl3au3jKzx2FfTPJp647/fxyqjBYGmR4zPKIegBioO+/uKfvyoWzQbdqzbHAclHp+sV/IWyFgsitKxLJsBv8bOCNheonp+NHgpXFADaSP/5z4XXHTUoofTszwX+ZBNjtpiin0DYT5WW6tYPuie9/k1VTuTj15vquvsM/cZZbVcuA+sftQxt0oIAJZ01VOlyOrVS0V9LYqG3rtZfLg29/c/avlyR2zBTDW0CQLFd5HKUikmgTC/XrC5En8c6mVMj8gZ73/ryW+W5Gai457M+OBB8/NkBRgwTNXtIIefyPV/zcgPrhBLGmaHMpW2oaqYAe6ODLzwNLy0wyfNerdVn7hx2JzBn+i2BcEqUxQk1Ax60CWaUnx9H7L/f659DhDgAA"
}
}

trait HashSetsDsl extends impl.HashSetsAbs {self: ScalanCommunityDsl =>}
trait HashSetsDslExp extends impl.HashSetsExp {self: ScalanCommunityDslExp =>}
