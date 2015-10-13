package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait CoproductsAbs extends Coproducts with scalan.Scalan {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyCoproduct[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]): Coproduct[F, G, A] = {
    proxyOps[Coproduct[F, G, A]](p)(scala.reflect.classTag[Coproduct[F, G, A]])
  }

  // familyElem
  class CoproductElem[F[_], G[_], A, To <: Coproduct[F, G, A]](implicit _cF: Cont[F], _cG: Cont[G], _eA: Elem[A])
    extends EntityElem[To] {
    def cF = _cF
    def cG = _cG
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("F" -> Right(cF.asInstanceOf[SomeCont]), "G" -> Right(cG.asInstanceOf[SomeCont]), "A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[Coproduct[F, G, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Coproduct[F, G, A]] => convertCoproduct(x) }
      tryConvert(element[Coproduct[F, G, A]], this, x, conv)
    }

    def convertCoproduct(x: Rep[Coproduct[F, G, A]]): Rep[To] = {
      x.selfType1.asInstanceOf[Element[_]] match {
        case _: CoproductElem[_, _, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have CoproductElem[_, _, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def coproductElement[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Elem[Coproduct[F, G, A]] =
    cachedElem[CoproductElem[F, G, A, Coproduct[F, G, A]]](cF, cG, eA)

  implicit case object CoproductCompanionElem extends CompanionElem[CoproductCompanionAbs] {
    lazy val tag = weakTypeTag[CoproductCompanionAbs]
    protected def getDefaultRep = Coproduct
  }

  abstract class CoproductCompanionAbs extends CompanionDef[CoproductCompanionAbs] with CoproductCompanion {
    def selfType = CoproductCompanionElem
    override def toString = "Coproduct"
  }
  def Coproduct: Rep[CoproductCompanionAbs]
  implicit def proxyCoproductCompanionAbs(p: Rep[CoproductCompanionAbs]): CoproductCompanionAbs =
    proxyOps[CoproductCompanionAbs](p)

  abstract class AbsCoproductImpl[F[_], G[_], A]
      (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductImpl[F, G, A](run) with Def[CoproductImpl[F, G, A]] {
    lazy val selfType = element[CoproductImpl[F, G, A]]
  }
  // elem for concrete class
  class CoproductImplElem[F[_], G[_], A](val iso: Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends CoproductElem[F, G, A, CoproductImpl[F, G, A]]
    with ConcreteElem[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] {
    override lazy val parent: Option[Elem[_]] = Some(coproductElement(container[F], container[G], element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("F" -> Right(cF.asInstanceOf[SomeCont]), "G" -> Right(cG.asInstanceOf[SomeCont]), "A" -> Left(eA))
    }

    override def convertCoproduct(x: Rep[Coproduct[F, G, A]]) = CoproductImpl(x.run)
    override def getDefaultRep = CoproductImpl(element[Either[F[A], G[A]]].defaultRepValue)
    override lazy val tag = {
      implicit val tagA = eA.tag
      weakTypeTag[CoproductImpl[F, G, A]]
    }
  }

  // state representation type
  type CoproductImplData[F[_], G[_], A] = Either[F[A], G[A]]

  // 3) Iso for concrete class
  class CoproductImplIso[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] {
    override def from(p: Rep[CoproductImpl[F, G, A]]) =
      p.run
    override def to(p: Rep[Either[F[A], G[A]]]) = {
      val run = p
      CoproductImpl(run)
    }
    lazy val eTo = new CoproductImplElem[F, G, A](this)
  }
  // 4) constructor and deconstructor
  class CoproductImplCompanionAbs extends CompanionDef[CoproductImplCompanionAbs] with CoproductImplCompanion {
    def selfType = CoproductImplCompanionElem
    override def toString = "CoproductImpl"

    def apply[F[_], G[_], A](run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
      mkCoproductImpl(run)
  }
  object CoproductImplMatcher {
    def unapply[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = unmkCoproductImpl(p)
  }
  lazy val CoproductImpl: Rep[CoproductImplCompanionAbs] = new CoproductImplCompanionAbs
  implicit def proxyCoproductImplCompanion(p: Rep[CoproductImplCompanionAbs]): CoproductImplCompanionAbs = {
    proxyOps[CoproductImplCompanionAbs](p)
  }

  implicit case object CoproductImplCompanionElem extends CompanionElem[CoproductImplCompanionAbs] {
    lazy val tag = weakTypeTag[CoproductImplCompanionAbs]
    protected def getDefaultRep = CoproductImpl
  }

  implicit def proxyCoproductImpl[F[_], G[_], A](p: Rep[CoproductImpl[F, G, A]]): CoproductImpl[F, G, A] =
    proxyOps[CoproductImpl[F, G, A]](p)

  implicit class ExtendedCoproductImpl[F[_], G[_], A](p: Rep[CoproductImpl[F, G, A]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]) {
    def toData: Rep[CoproductImplData[F, G, A]] = isoCoproductImpl(cF, cG, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoCoproductImpl[F[_], G[_], A](implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Iso[CoproductImplData[F, G, A], CoproductImpl[F, G, A]] =
    cachedIso[CoproductImplIso[F, G, A]](cF, cG, eA)

  // 6) smart constructor and deconstructor
  def mkCoproductImpl[F[_], G[_], A](run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]]
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]): Option[(Rep[Either[F[A], G[A]]])]

  registerModule(Coproducts_Module)
}

// Seq -----------------------------------
trait CoproductsSeq extends CoproductsDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val Coproduct: Rep[CoproductCompanionAbs] = new CoproductCompanionAbs {
  }

  case class SeqCoproductImpl[F[_], G[_], A]
      (override val run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends AbsCoproductImpl[F, G, A](run) {
  }

  def mkCoproductImpl[F[_], G[_], A]
    (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
    new SeqCoproductImpl[F, G, A](run)
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = p match {
    case p: CoproductImpl[F, G, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait CoproductsExp extends CoproductsDsl with scalan.ScalanExp {
  self: MonadsDslExp =>
  lazy val Coproduct: Rep[CoproductCompanionAbs] = new CoproductCompanionAbs {
  }

  case class ExpCoproductImpl[F[_], G[_], A]
      (override val run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A])
    extends AbsCoproductImpl[F, G, A](run)

  object CoproductImplMethods {
  }

  object CoproductImplCompanionMethods {
  }

  def mkCoproductImpl[F[_], G[_], A]
    (run: Rep[Either[F[A], G[A]]])(implicit cF: Cont[F], cG: Cont[G], eA: Elem[A]): Rep[CoproductImpl[F, G, A]] =
    new ExpCoproductImpl[F, G, A](run)
  def unmkCoproductImpl[F[_], G[_], A](p: Rep[Coproduct[F, G, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CoproductImplElem[F, G, A] @unchecked =>
      Some((p.asRep[CoproductImpl[F, G, A]].run))
    case _ =>
      None
  }

  object CoproductMethods {
    object run {
      def unapply(d: Def[_]): Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}] = d match {
        case MethodCall(receiver, method, _, _) if (receiver.elem.asInstanceOf[Element[_]] match { case _: CoproductElem[_, _, _, _] => true; case _ => false }) && method.getName == "run" =>
          Some(receiver).asInstanceOf[Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Coproduct[F, G, A]] forSome {type F[_]; type G[_]; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object CoproductCompanionMethods {
  }
}

object Coproducts_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWPYwbRRR+ts/ns30kIcBJkUDcnQwRCOwIJFJcETmO7wTy/eg2RWQiwng99m3YndmbGZ/WFCkoUkCHaCkiUaZBNFR0SIiCCiEkaqoQhFKQCsSb8e561/FPIhEXo32zb795833vm/Hd+5CXAl6VNnEJq3pUkaplnutSVawmU44a7vLuwKVXaO+TtW/tXXZZZuF0G5aPiLwi3TYURw/NwI+fLXrcgiJhNpWKC6lgo2VWqNncdamtHM5qjucNFOm4tNZypNpqwVKHd4fHcAsyLThjc2YLqqjVcImUVIbzK1RX5MRx0cTDfX+8BqvpXdQSu7gqiKOwfFzjzCj/kPrWkHE29BScCkvb93VZmFNwPJ8LFS1RQLgj3o3CJUZwAs62bpITUsMl+jVLCYf18cuyT+yPSJ/uYYpOX8KCJXV7V4e+iXMtKEl6jAS96/mumQl8AEAF3jJFVMf8VGN+qpqfikWFQ1znY6JfHggeDGH0y+QAAh8h3lgAESHQJutWPr1uv//QKntZ/XGgSymYHS4j0MszusFIgTz+cPi5fLBz52IWSm0oObLekUoQWyUlD9kqE8a4MjXHBBLRR7U2Z6llVqljzkRLFG3u+YQhUkjlKurkOrajdLKeWw3VmUF9Qfk0Ss0Efibe7/qM/Zq+aRDXPbh37s1X/mhey0I2vUQRIS1sfBGBKig2uC/QMLYK8fV4WkFme0yyDnfSYd2EeigG47Ewp7qYp/P3/ux+fwGuZ2N2w2IeT1CEyMtffyn//NqlLKy0Tftvu6TfRoJl06Xevmhwptqwwk+oGL0pnBBXP00VuNClPTJwVUh7kq8c8qVgfaZRfarJ3DKmyEQElEd9vccZrWwfVP62fvzirm5bAaujNyPn/utc/Oe3Uz1lOlpBTgxYxG4O/Z5WY7npqCMqJiWaiJOijIWbkzR91JsojUq1uEef3XzgfHDnM2X0ygTps2S/cxPNu2W+W58jXXSmfX379gt/fXXjOePFlY6jPOJXLjyBEyPjPEWnQczQqN/PjWM9bKBaa7Fr9MnYSK6/kfgwQf2LmahFTJKCrL0dabKkG3aq/RJaTgHYmQewsxiA1mMAbZyFjaLgmdS+DU7s2pdmSW/IXTtsPe/ev/RdFvLvQb6HZpQtyHf4gHUj1fAOVTRQl6O5TFo1VIkI4sUqmd86jDmfcIQ1NePGJC3T03bmA+nh2uMhPUpjAvodSJOeQ4ukZ/7X87mYuDkTXW3it8OCFjf/2bikKY2fulKSDTKboQWiPQHXT1M1PX6YxsLE0lgedEfkAM5IV4asCticYQwrPIVQ81sPv9x7/advfjc3dkmfZ3h7sPg/3dgGwcTFUNw1a+FftETB6Gd9wpli/wPgisG5MgsAAA=="
}
}

