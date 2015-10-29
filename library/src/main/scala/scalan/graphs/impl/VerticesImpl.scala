package scalan.graphs

import scalan.collections.CollectionsDsl
import scalan.{Scalan, ScalanExp, ScalanSeq}
import scalan.ScalanCommunityDsl
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait VerticesAbs extends Vertices with scalan.Scalan {
  self: GraphsDsl =>

  // single proxy for each type family
  implicit def proxyVertex[V, E](p: Rep[Vertex[V, E]]): Vertex[V, E] = {
    proxyOps[Vertex[V, E]](p)(scala.reflect.classTag[Vertex[V, E]])
  }

  // familyElem
  class VertexElem[V, E, To <: Vertex[V, E]](implicit _eV: Elem[V], _eE: Elem[E])
    extends EntityElem[To] {
    def eV = _eV
    def eE = _eE
    lazy val parent: Option[Elem[_]] = None
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[Vertex[V, E]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Vertex[V, E]] => convertVertex(x) }
      tryConvert(element[Vertex[V, E]], this, x, conv)
    }

    def convertVertex(x: Rep[Vertex[V, E]]): Rep[To] = {
      x.selfType1 match {
        case _: VertexElem[_, _, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have VertexElem[_, _, _], but got $e")
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def vertexElement[V, E](implicit eV: Elem[V], eE: Elem[E]): Elem[Vertex[V, E]] =
    cachedElem[VertexElem[V, E, Vertex[V, E]]](eV, eE)

  implicit case object VertexCompanionElem extends CompanionElem[VertexCompanionAbs] {
    lazy val tag = weakTypeTag[VertexCompanionAbs]
    protected def getDefaultRep = Vertex
  }

  abstract class VertexCompanionAbs extends CompanionDef[VertexCompanionAbs] with VertexCompanion {
    def selfType = VertexCompanionElem
    override def toString = "Vertex"
  }
  def Vertex: Rep[VertexCompanionAbs]
  implicit def proxyVertexCompanionAbs(p: Rep[VertexCompanionAbs]): VertexCompanionAbs =
    proxyOps[VertexCompanionAbs](p)

  abstract class AbsSVertex[V, E]
      (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends SVertex[V, E](id, graph) with Def[SVertex[V, E]] {
    lazy val selfType = element[SVertex[V, E]]
  }
  // elem for concrete class
  class SVertexElem[V, E](val iso: Iso[SVertexData[V, E], SVertex[V, E]])(implicit eV: Elem[V], eE: Elem[E])
    extends VertexElem[V, E, SVertex[V, E]]
    with ConcreteElem[SVertexData[V, E], SVertex[V, E]] {
    override lazy val parent: Option[Elem[_]] = Some(vertexElement(element[V], element[E]))
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("V" -> Left(eV), "E" -> Left(eE))
    }

    override def convertVertex(x: Rep[Vertex[V, E]]) = SVertex(x.id, x.graph)
    override def getDefaultRep = SVertex(0, element[Graph[V, E]].defaultRepValue)
    override lazy val tag = {
      implicit val tagV = eV.tag
      implicit val tagE = eE.tag
      weakTypeTag[SVertex[V, E]]
    }
  }

  // state representation type
  type SVertexData[V, E] = (Int, Graph[V, E])

  // 3) Iso for concrete class
  class SVertexIso[V, E](implicit eV: Elem[V], eE: Elem[E])
    extends Iso0[SVertexData[V, E], SVertex[V, E]] {
    override def from(p: Rep[SVertex[V, E]]) =
      (p.id, p.graph)
    override def to(p: Rep[(Int, Graph[V, E])]) = {
      val Pair(id, graph) = p
      SVertex(id, graph)
    }
    lazy val eFrom = pairElement(element[Int], element[Graph[V, E]])
    lazy val eTo = new SVertexElem[V, E](self)
    lazy val selfType = new ConcreteIso0Elem[SVertexData[V, E], SVertex[V, E], SVertexIso[V, E]](eFrom, eTo).
      asInstanceOf[Elem[Iso0[SVertexData[V, E], SVertex[V, E]]]]
    def productArity = 2
    def productElement(n: Int) = (eV, eE).productElement(n)
  }
  // 4) constructor and deconstructor
  class SVertexCompanionAbs extends CompanionDef[SVertexCompanionAbs] with SVertexCompanion {
    def selfType = SVertexCompanionElem
    override def toString = "SVertex"
    def apply[V, E](p: Rep[SVertexData[V, E]])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      isoSVertex(eV, eE).to(p)
    def apply[V, E](id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
      mkSVertex(id, graph)
  }
  object SVertexMatcher {
    def unapply[V, E](p: Rep[Vertex[V, E]]) = unmkSVertex(p)
  }
  lazy val SVertex: Rep[SVertexCompanionAbs] = new SVertexCompanionAbs
  implicit def proxySVertexCompanion(p: Rep[SVertexCompanionAbs]): SVertexCompanionAbs = {
    proxyOps[SVertexCompanionAbs](p)
  }

  implicit case object SVertexCompanionElem extends CompanionElem[SVertexCompanionAbs] {
    lazy val tag = weakTypeTag[SVertexCompanionAbs]
    protected def getDefaultRep = SVertex
  }

  implicit def proxySVertex[V, E](p: Rep[SVertex[V, E]]): SVertex[V, E] =
    proxyOps[SVertex[V, E]](p)

  implicit class ExtendedSVertex[V, E](p: Rep[SVertex[V, E]])(implicit eV: Elem[V], eE: Elem[E]) {
    def toData: Rep[SVertexData[V, E]] = isoSVertex(eV, eE).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSVertex[V, E](implicit eV: Elem[V], eE: Elem[E]): Iso[SVertexData[V, E], SVertex[V, E]] =
    reifyObject(new SVertexIso[V, E]()(eV, eE))

  // 6) smart constructor and deconstructor
  def mkSVertex[V, E](id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]]
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]): Option[(Rep[Int], Rep[Graph[V, E]])]

  registerModule(Vertices_Module)
}

// Seq -----------------------------------
trait VerticesSeq extends VerticesDsl with scalan.ScalanSeq {
  self: GraphsDslSeq =>
  lazy val Vertex: Rep[VertexCompanionAbs] = new VertexCompanionAbs {
  }

  case class SeqSVertex[V, E]
      (override val id: Rep[Int], override val graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsSVertex[V, E](id, graph) {
  }

  def mkSVertex[V, E]
    (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
    new SeqSVertex[V, E](id, graph)
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]) = p match {
    case p: SVertex[V, E] @unchecked =>
      Some((p.id, p.graph))
    case _ => None
  }
}

// Exp -----------------------------------
trait VerticesExp extends VerticesDsl with scalan.ScalanExp {
  self: GraphsDslExp =>
  lazy val Vertex: Rep[VertexCompanionAbs] = new VertexCompanionAbs {
  }

  case class ExpSVertex[V, E]
      (override val id: Rep[Int], override val graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E])
    extends AbsSVertex[V, E](id, graph)

  object SVertexMethods {
  }

  object SVertexCompanionMethods {
  }

  def mkSVertex[V, E]
    (id: Rep[Int], graph: PG[V, E])(implicit eV: Elem[V], eE: Elem[E]): Rep[SVertex[V, E]] =
    new ExpSVertex[V, E](id, graph)
  def unmkSVertex[V, E](p: Rep[Vertex[V, E]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SVertexElem[V, E] @unchecked =>
      Some((p.asRep[SVertex[V, E]].id, p.asRep[SVertex[V, E]].graph))
    case _ =>
      None
  }

  object VertexMethods {
    object graph {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "graph" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object id {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "id" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object value {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "value" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outNbrs {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "outNbrs" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object outEdges {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "outEdges" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hasEdgeTo {
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "hasEdgeTo" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numOutNbrs {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "numOutNbrs" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object numInNbrs {
      def unapply(d: Def[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "numInNbrs" =>
          Some(receiver).asInstanceOf[Option[Rep[Vertex[V, E]] forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Vertex[V, E]] forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrs {
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "commonNbrs" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object commonNbrsNum {
      def unapply(d: Def[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = d match {
        case MethodCall(receiver, method, Seq(v, _*), _) if receiver.elem.isInstanceOf[VertexElem[_, _, _]] && method.getName == "commonNbrsNum" =>
          Some((receiver, v)).asInstanceOf[Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Vertex[V, E]], Rep[Vertex[V, E]]) forSome {type V; type E}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object VertexCompanionMethods {
  }
}

object Vertices_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTWwbRRR+u47jv5CEAkVUAodgQCCwQwTqIYcqdZ2oyE2sbLGQqUDj9diZMju72RlHNoceOPQAN8SVQyWOvSAuSEi9ICTEgRNCSJw5laKqB3oCMTP743XiDbSIPYx2Zt68n+/73uzevANZ7sML3EYUsaqDBapa+n2Ti4rVYIKI8SW3N6T4Au5/ePor+xI7z01Y6sD8PuIXOO1AIXhpjLz43cIHTSggZmMuXJ8LeLapI9Rsl1JsC+KyGnGcoUBdimtNwsVGE+a6bm98ANfAaMKy7TLbxwJbdYo4xzxcz2OVEYnnBT0f73qTGKymqqglqrjsIyJk+jLGcmC/hz1rzFw2dgQshqnteiotaZMjjuf6IgqRk+723V40nWNILsCp5lV0iGoyxKBmCZ+wgTxZ8pD9PhrgHWmizOdkwhzT/uWxp+eZJhQ5PpAAXXQ8qldGHgBIBtZ1EtUJPtUYn6rCp2JhnyBKPkBqs+W7ozEEj5EBGHnSxSv/4CLygBusV/noiv3OfavkmOrwSKWS0xXOS0flFDVoKiSO3+19wu9t3zhrQrEDRcI3u1z4yBZJykO0SogxV+icYwCRP5BsraaxpaNsSpsjkijYruMhJj2FUC5IniixiVDGam0hZCcF+pzwcGRqjDwjrnclpV6tmzqitHX7qVef/63xtgnmdIiCdGlJ4fuRUwHzbewLPAqdq3FJgNGeIKymDT1VQ2E0GXMn5BKj8uLt33vfrsEVM8YyDP3v6JMusvznn0o/vnTOhHxHi32LokFHwskbFDu7ft1logN59xD7wU7uEFH1NpPOXA/30ZCKEOQkOhmJjoCV1Lb0sIJuQ7eAEQFQClS84zJc2WpV/rC+//SmEqkPC8FO0Kd/kbN//rLYF1q/AkzSi8DNyOaOsXgujVoPt3ziyKvkEL/xzddv3b21k9XsngrLaSM6xEFjh9VMKlMBjTUZ6SITAXs63pm4DDWUBWQHPvL2o7zM1vbDiqIYVG65Dn509R5598bHQtNvjKYvot3uVdn5G/rcMycoIboQv7h+/Ym7n7/3mG7kfJcIB3mVtQdo46jr/sc2hWnQFuvhh0GreX16M2cFzZcCpBqfjPcCliSVy+GpejLxcuJIIsgZ4wjHJm5H0edU+8ykOCmS4w4aJzk4LorpMsux1J9Ol7qE8/Re83F659wtE7JvQrYvu5k3Idt1h6wX8SQ/udKpOB+tGdM8SV6Qj5yYF/2swASsaUW3ZhocLydR7+tHuM5IiU6v/IfLNUUEev7azPAlJcot5BA6Xp+ZxoPJbClVZd6Ux3Ii1mwQHwJlNbYnNqFhXkUlqtXhkVA9wZUVouLDaoqorLBnJUPX7n+28/IPX/6qr8+i6n55dbP49yl5bU6jWNjWseTfUCJd2QXqPtCp/g3/QmkjnQoAAA=="
}
}

trait VerticesDsl extends impl.VerticesAbs {self: GraphsDsl =>}
trait VerticesDslSeq extends impl.VerticesSeq {self: GraphsDslSeq =>}
trait VerticesDslExp extends impl.VerticesExp {self: GraphsDslExp =>}
