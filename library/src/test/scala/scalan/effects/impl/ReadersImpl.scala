package scalan.monads

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ReadersAbs extends Readers with scalan.Scalan {
  self: MonadsDsl =>

  // single proxy for each type family
  implicit def proxyReader[Env, A](p: Rep[Reader[Env, A]]): Reader[Env, A] = {
    proxyOps[Reader[Env, A]](p)(scala.reflect.classTag[Reader[Env, A]])
  }

  // familyElem
  class ReaderElem[Env, A, To <: Reader[Env, A]](implicit _eEnv: Elem[Env], _eA: Elem[A])
    extends EntityElem[To] {
    def eEnv = _eEnv
    def eA = _eA
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Env" -> Left(eEnv), "A" -> Left(eA))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagA = eA.tag
      weakTypeTag[Reader[Env, A]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Reader[Env, A]] => convertReader(x) }
      tryConvert(element[Reader[Env, A]], this, x, conv)
    }

    def convertReader(x: Rep[Reader[Env, A]]): Rep[To] = {
      x.selfType1 match {
        case _: ReaderElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have ReaderElem[_, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def readerElement[Env, A](implicit eEnv: Elem[Env], eA: Elem[A]): Elem[Reader[Env, A]] =
    cachedElem[ReaderElem[Env, A, Reader[Env, A]]](eEnv, eA)

  implicit case object ReaderCompanionElem extends CompanionElem[ReaderCompanionAbs] {
    lazy val tag = weakTypeTag[ReaderCompanionAbs]
    protected def getDefaultRep = Reader
  }

  abstract class ReaderCompanionAbs extends CompanionDef[ReaderCompanionAbs] with ReaderCompanion {
    def selfType = ReaderCompanionElem
    override def toString = "Reader"
  }
  def Reader: Rep[ReaderCompanionAbs]
  implicit def proxyReaderCompanionAbs(p: Rep[ReaderCompanionAbs]): ReaderCompanionAbs =
    proxyOps[ReaderCompanionAbs](p)

  abstract class AbsReaderBase[Env, A]
      (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderBase[Env, A](run) with Def[ReaderBase[Env, A]] {
    lazy val selfType = element[ReaderBase[Env, A]]
  }
  // elem for concrete class
  class ReaderBaseElem[Env, A](val iso: Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends ReaderElem[Env, A, ReaderBase[Env, A]]
    with ConcreteElem[ReaderBaseData[Env, A], ReaderBase[Env, A]] {
    override lazy val parent: Option[Elem[_]] = Some(readerElement(element[Env], element[A]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("Env" -> Left(eEnv), "A" -> Left(eA))
    }

    override def convertReader(x: Rep[Reader[Env, A]]) = ReaderBase(x.run)
    override def getDefaultRep = ReaderBase(constFun[Env, A](element[A].defaultRepValue))
    override lazy val tag = {
      implicit val tagEnv = eEnv.tag
      implicit val tagA = eA.tag
      weakTypeTag[ReaderBase[Env, A]]
    }
  }

  // state representation type
  type ReaderBaseData[Env, A] = Env => A

  // 3) Iso for concrete class
  class ReaderBaseIso[Env, A](implicit eEnv: Elem[Env], eA: Elem[A])
    extends Iso0[ReaderBaseData[Env, A], ReaderBase[Env, A]] {
    override def from(p: Rep[ReaderBase[Env, A]]) =
      p.run
    override def to(p: Rep[Env => A]) = {
      val run = p
      ReaderBase(run)
    }
    lazy val eFrom = element[Env => A]
    lazy val eTo = new ReaderBaseElem[Env, A](self)
    lazy val selfType = new ConcreteIso0Elem[ReaderBaseData[Env, A], ReaderBase[Env, A], ReaderBaseIso[Env, A]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[ReaderBaseData[Env, A], ReaderBase[Env, A]]]]
    def productArity = 2
    def productElement(n: Int) = (eEnv, eA).productElement(n)
  }
  // 4) constructor and deconstructor
  class ReaderBaseCompanionAbs extends CompanionDef[ReaderBaseCompanionAbs] with ReaderBaseCompanion {
    def selfType = ReaderBaseCompanionElem
    override def toString = "ReaderBase"

    def apply[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
      mkReaderBase(run)
  }
  object ReaderBaseMatcher {
    def unapply[Env, A](p: Rep[Reader[Env, A]]) = unmkReaderBase(p)
  }
  lazy val ReaderBase: Rep[ReaderBaseCompanionAbs] = new ReaderBaseCompanionAbs
  implicit def proxyReaderBaseCompanion(p: Rep[ReaderBaseCompanionAbs]): ReaderBaseCompanionAbs = {
    proxyOps[ReaderBaseCompanionAbs](p)
  }

  implicit case object ReaderBaseCompanionElem extends CompanionElem[ReaderBaseCompanionAbs] {
    lazy val tag = weakTypeTag[ReaderBaseCompanionAbs]
    protected def getDefaultRep = ReaderBase
  }

  implicit def proxyReaderBase[Env, A](p: Rep[ReaderBase[Env, A]]): ReaderBase[Env, A] =
    proxyOps[ReaderBase[Env, A]](p)

  implicit class ExtendedReaderBase[Env, A](p: Rep[ReaderBase[Env, A]])(implicit eEnv: Elem[Env], eA: Elem[A]) {
    def toData: Rep[ReaderBaseData[Env, A]] = isoReaderBase(eEnv, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoReaderBase[Env, A](implicit eEnv: Elem[Env], eA: Elem[A]): Iso[ReaderBaseData[Env, A], ReaderBase[Env, A]] =
    reifyObject(new ReaderBaseIso[Env, A]()(eEnv, eA))

  // 6) smart constructor and deconstructor
  def mkReaderBase[Env, A](run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]]
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]): Option[(Rep[Env => A])]

  registerModule(Readers_Module)
}

// Seq -----------------------------------
trait ReadersSeq extends ReadersDsl with scalan.ScalanSeq {
  self: MonadsDslSeq =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs {
  }

  case class SeqReaderBase[Env, A]
      (override val run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends AbsReaderBase[Env, A](run) {
  }

  def mkReaderBase[Env, A]
    (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
    new SeqReaderBase[Env, A](run)
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]) = p match {
    case p: ReaderBase[Env, A] @unchecked =>
      Some((p.run))
    case _ => None
  }
}

// Exp -----------------------------------
trait ReadersExp extends ReadersDsl with scalan.ScalanExp {
  self: MonadsDslExp =>
  lazy val Reader: Rep[ReaderCompanionAbs] = new ReaderCompanionAbs {
  }

  case class ExpReaderBase[Env, A]
      (override val run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A])
    extends AbsReaderBase[Env, A](run)

  object ReaderBaseMethods {
  }

  object ReaderBaseCompanionMethods {
  }

  def mkReaderBase[Env, A]
    (run: Rep[Env => A])(implicit eEnv: Elem[Env], eA: Elem[A]): Rep[ReaderBase[Env, A]] =
    new ExpReaderBase[Env, A](run)
  def unmkReaderBase[Env, A](p: Rep[Reader[Env, A]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ReaderBaseElem[Env, A] @unchecked =>
      Some((p.asRep[ReaderBase[Env, A]].run))
    case _ =>
      None
  }

  object ReaderMethods {
    object run {
      def unapply(d: Def[_]): Option[Rep[Reader[Env, A]] forSome {type Env; type A}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ReaderElem[_, _, _]] && method.getName == "run" =>
          Some(receiver).asInstanceOf[Option[Rep[Reader[Env, A]] forSome {type Env; type A}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Reader[Env, A]] forSome {type Env; type A}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object ReaderCompanionMethods {
    object ask {
      def unapply(d: Def[_]): Option[Unit forSome {type R}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == ReaderCompanionElem && method.getName == "ask" =>
          Some(()).asInstanceOf[Option[Unit forSome {type R}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type R}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Readers_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWPYwbRRR+3rPPZ/vIBQ6CIEruODlBRMSOaFJcEfkcHzrk+9FtCmQiovHu2NmwO7s3M7ZsihQUKaBDNBQUkSjTIBoqOiREQRUhJCoKqhCEUpAKxJvZ3zt5LwTEFqOZ2Tfv5/u+N7v3HkJJcDgvLOIS1vCoJA1Tz1tC1s0Ok46cbvv2yKVX6eCDU19Z22xDGLDUg/mbRFwVbg8q4aQzCZK5SQ+6UCHMokL6XEh4pasjNC3fdaklHZ81Hc8bSdJ3abPrCLnehWLft6cHcBsKXThp+cziVFKz7RIhqIj2F6jKyEnWFb2e7gZpDNZUVTQzVVzjxJGYPsY4Gdrv08CcMp9NPQknotR2A5UW2pQdL/C5jEOU0d1N346XRUZwA57r3iJj0sQQw6YpucOGeLIWEOs9MqQ7aKLMi5iwoO7g2jTQ67kuVAU9QIC2vMDVO5MAAJCBN3QSjRSfRoJPQ+FTNyl3iOu8T9TLPe5PphA+hTmASYAuXn+Ci9gD7TC7/uF1653HZs0z1OGJSqWsK5xHRys5atBUII7f7n8sHr1597IB1R5UHdHqC8mJJbOUR2jVCGO+1DknABI+RLbW8tjSUVpoc0QSFcv3AsLQUwTlIvLkOpYjlbHaW4zYyYG+LAMamxYmQSGpdzWnXq2bNnHdvQcvXTz3a+dtA4zDISro0kTh89iphPl9SmzKI+dqXJIw12HjFGPcKLT0Ug2VSTqWj8kmweXVB7/Z31yC60aCZhT8nxGILkrixx9q91+7YsBCT8t90yXDHgIqOi71dnnbZ7IHC/6Y8vBNeUxcNZtJaNmmAzJyZQRzFp85xEfCam5jBlSBt66boBADUAt1vOMzWt/cq/9hfvfJPSVTDovhm7BT/3Iu//nTiYHUCkaI+YglcGN/J2CczWM3oJsjZt3f+nR56cyNnzW387bvEUcL7HQXShy7W5dyOgL3KamshvmavkefXXvkvHv3I6lJK0wOXyC7/VvYsev63Nlj+Isvsi/u3Hnh989vLOsGXOg70iNB/dJTtF/cLf9je0GCSojUi+laDStI2XLYKhtE0HY2+ErmVKaPXi7EItFGEop4k41jGopKujldF7Iy24lBW8e4mEGuhGqat3aSSO1MvtQQmVP73efdh1e+NqD0FpQG2E4CNdb3R8yOIcevnqQTuRHvFQ5DjhATTrwEYv2sQorZUXm2Z5nMqClT9EU4AiLq7fDOf7rhKplvTUYSen0hSuDJylkKU5ihmvQGzlKTi8W/g0uNW6lNZFiOcJHwTKwDnxFbRLVxWMuRhxk1EiJ9+/FnOxe+//IXfRtVVUviLciSf5FUDMltH8Fd2dax8Nciky1KWjWpzvRv9V3R3eoJAAA="
}
}

trait ReadersDslSeq extends impl.ReadersSeq {self: MonadsDslSeq =>}
trait ReadersDslExp extends impl.ReadersExp {self: MonadsDslExp =>}
